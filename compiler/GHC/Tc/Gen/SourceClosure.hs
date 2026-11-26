-- | Certify and publish closed type-family equations for scope-indexed GADTs,
-- rank-2 renaming and substitution closures, and indexed environment lookups.
--
-- See Note [Certifying a checked source closure] and
-- Note [Indexed GADT decision-tree and continuation compilation].
module GHC.Tc.Gen.SourceClosure
  ( publishCheckedSourceClosures
  , earlyClosureCandidateType
  , earlyIndexedLookups
  , pruneUnusedIndexedLookups
  , sourceEarlyClosureTables
  , checkLocalIndexedLookupExports
  , validateEarlyIndexedLookups
  , finalizeEarlyClosureTypes
  ) where

import GHC.Prelude

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, unless, when, zipWithM, zipWithM_)
import Control.Monad.Fix (mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable (for_)
import Data.List (find, isPrefixOf, nubBy, sortBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)

import GHC.Builtin.KnownKeys (patErrorIdKey)
import GHC.Builtin.WiredIn.Types (manyDataConTy, matchableDataConTy, unmatchableDataConTy)
import GHC.Core (CoreBind, CoreExpr, CoreRule)
import qualified GHC.Core as Core
import GHC.Core.Coercion (coercionLKind, coercionRKind, coercionRole)
import GHC.Core.Coercion.Axiom
  ( CoAxBranch, ClosedTyFam(CTF), Role(Nominal)
  , coAxiomBranches, coAxiomName, coAxBranchLHS, coAxBranchRHS, coAxBranchTyVars, fromBranches )
import GHC.Core.ConLike (ConLike(RealDataCon))
import qualified GHC.Core.DataCon as DC
import GHC.Core.Dependent (hasExistentialExpr, hasNativeDependentExpr)
import GHC.Core.FamInstEnv (emptyFamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch, normaliseTcApp)
import GHC.Core.FVs (exprFreeVars, exprSomeFreeVarsList)
import GHC.Core.OpenWhnf (checkedMultiplicityCast, openDataConFieldTypes, openIndexedDataCon)
import GHC.Core.Predicate (tyCoVarsOfTypesWellScoped)
import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.SimpleOpt (SimpleOpts, simpleOptExpr)
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, mkOpenSubst, substExpr, substTy)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCo.FVs (tyConsOfType, tyConsOfTypes)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(MRefl), Scaled(..), Type(..), scaledThing)
import GHC.Core.TyCon
  ( AlgTyConFlav(VanillaAlgTyCon), FamTyConFlav(ClosedTypeFamilyTyCon), Injectivity(NotInjective)
  , TyCon, TyConBinder, TyConBndrVis(AnonTCB, NamedTCB), closedFamilyTyConCoAxiom_maybe
  , isAlgTyCon, isPromotedDataCon, isPromotedDataCon_maybe, mkAlgTyCon, mkAnonTyConBinder
  , mkFamilyTyCon, mkLevPolyDataTyConRhs, mkNamedTyConBinder, mkNamedTyConBinders, mkTyConKind
  , mkTyConKindWithMatchability, synTyConRhs_maybe, tyConArity, tyConBinders, tyConDataCons
  , tyConKind, tyConName, tyConRepName_maybe )
import GHC.Core.TyCon.Set (TyConSet)
import GHC.Core.Type
  ( FunTyFlag(FTF_T_T), Kind, coreFullView, getTyVar_maybe, isLiftedTypeKind, isManyTy, isOneTy
  , liftedTypeKind, lookupTyVar, mkAppTys, mkSpecForAllTys, mkTyConApp, mkTyVarTy, mkTyVarTys
  , mkVisFunTyManyU, splitForAllForAllTyBinders, splitForAllTyVars, splitFunTy_maybe, splitFunTys
  , splitTyConApp_maybe, substTyUnchecked, substTyWith, tyCoVarsOfTypes, typeKind )
import GHC.Core.Unify (alwaysBindTv, tcMatchTys, tcUnifyTys)
import GHC.Core.Utils (exprType, stripTicksE)
import GHC.Driver.Config (initSimpleOpts)
import GHC.Driver.DynFlags (GeneralFlag(Opt_Hpc), HasDynFlags(getDynFlags))
import GHC.Driver.Env (visibleSourceRuleHeads)
import GHC.Driver.Plugins (hasActiveLatePlugins)
import GHC.Hs.Utils (CollectFlag(CollNoDictBinders), collectHsBindBinders)
import GHC.Tc.Errors.Types (RetainedQuantificationError(RetainedSourceRule), TcRnMessage(..))
import GHC.Tc.Gen.SourceTable
  ( hasSourceMethodRule, ordinaryImport, publicCaseType, sourceEquationDatatypes
  , sourceTableCallees, sourceTableOwner, visibleSourceMethodRules )
import GHC.Tc.TyCl.Build (buildDataCon, newImplicitBinder, newTyConRepName)
import GHC.Tc.Types (tcVisibleOrphanMods)
import GHC.Tc.Utils.Env (tcLookupIdMaybe)
import GHC.Tc.Utils.Instantiate (freshenTyVarBndrs)
import GHC.Tc.Utils.Monad
  ( TcGblEnv(..), TcM, addErr, failIfErrsM, getGblEnv, getGlobalRdrEnv, getTopEnv, goptM, liftIO
  , newSysName, readTcRef, setSrcSpan )
import GHC.Tc.Utils.TcType (isTyFamFree)
import GHC.Types.Avail (availsToNameSet)
import GHC.Types.Basic (SourceTableOrigin(NominalSourceTable))
import GHC.Types.Id
  ( Id, idCoreRules, idDetails, idName, isDataConWorkId_maybe, isDataConWrapId_maybe, mkLocalId
  , setIdExported, sourceTableId_maybe )
import GHC.Types.Id.Info (IdDetails(CheckedClosureSourceTableId, SourceTableId, VanillaId))
import GHC.Types.Id.Make (DataConBangOpts(FixedBangOpts))
import GHC.Types.InlinePragma (isNeverActive)
import GHC.Types.Name
  ( Name, NamedThing(getName), isExternalName, isWiredInName, mkDataOcc, mkInstTyCoOcc
  , mkSourceTableOcc, mkTcOcc, mkTyVarOcc, mkVarOcc, nameModule_maybe, nameOccName, nameSrcSpan
  , nameStableString, occNameString, stableNameCmp )
import GHC.Types.Name.Env (NameEnv, filterNameEnv, lookupNameEnv, mapNameEnv, mkNameEnv, nonDetNameEnvElts)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkOccEnv_C)
import GHC.Types.Name.Reader (isLocalGRE, lookupGRE_Name)
import GHC.Types.Name.Set (NameSet, elemNameSet, extendNameSetList, mkNameSet, nameSetElemsStable)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Types.SrcLoc (noSrcSpan, unLoc)
import GHC.Types.Tickish (GenTickish(Breakpoint))
import GHC.Types.TyThing (TyThing(AConLike, ATyCon, AnId), implicitTyConThings)
import GHC.Types.TypeEnv (TypeEnv, lookupTypeEnv, typeEnvElts, typeEnvIds)
import GHC.Types.Unique (hasKey)
import GHC.Types.Unique.Set (addOneToUniqSet, elementOfUniqSet, emptyUniqSet, mkUniqSet, unionUniqSets)
import GHC.Types.Var
  ( ForAllTyFlag(Inferred, Specified), TyVar, Var, VarBndr(Bndr), eqForAllVis, isCoVar, isId
  , isLocalId, isTyVar, mkTyVar, mkTyVarBinders, setIdDetails, varType )
import GHC.Types.Var.Env (VarEnv, elemVarEnv, emptyVarEnv, lookupVarEnv, mkInScopeSet, mkVarEnv)
import GHC.Types.Var.Set (VarSet, elemVarSet, emptyVarSet, extendVarSet, mkVarSet, subVarSet, unionVarSet)

{- Note [Certifying a checked source closure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functions that substitute or rename de Bruijn variables pass higher-order
callbacks under binders. Direct promotion cannot express a fresh lambda at each
recursive step because promoted Haskell types lack general type-level lambdas
(GHC Proposal #378, §4.6 and §4.11; GHC Proposal #509). Following Eisenberg and
Stolarek (Haskell Symposium 2014) and Danvy and Nielsen (PPDP 2001 / BRICS
RS-01-23, "Defunctionalization at Work"), `GHC.Tc.Gen.SourceClosure` certifies
well-scoped unary (`UTerm n`, `Fin n`) and binary (`Term g a`, `Lookup g a`)
renaming and substitution chains by defunctionalizing inline replacement
callbacks into a private closure GADT (`$closure:`) interpreted by two nominal
closed type families (`$apply:` and `$lookup:`).

When a provider exports the underlying renaming and substitution operations
(`ext`, `rename`, `weakenLookup`, `exts`, `subst` or their unary counterparts),
each operation receives its own nominal closed type family over unmatchable
(`%U`, GHC Proposal #242; Kiss et al., ICFP 2019) callback kinds. Forwarding
wrappers (`substSingle n w = subst (singleSubst w) n` or inline untyped
`substSingleU`) reduce directly through `T:subst` / `T:substU` so open
equations over abstract terms unify without exposing private closure tokens.

Every generated equation must respect call-by-need definedness (Johann and
Voigtländer, Fundam. Inform. 2006; Schmidt-Schauß, Sabel, and Machkasova, LMCS
2015):
  1. Both desugared Core and post-Simplifier Core must produce alpha-identical
     nominal `CoAxBranch` equations (`sameIndexedRow`) or identical closure
     traversal recipes (`sameScopeShape`).
  2. Constructor fields and scrutinized branches must stay lazy; any `seq` or
     banged field withholds publication.
  3. All referenced source datatypes and constructors must be exported or
     ordinarily imported, and no active user `RULES` or `SPECIALISE` pragmas
     may rewrite the owner or any reachable external helper.
  4. GHCi `Breakpoint` ticks and dead lifted local bindings left behind after
     stripping ticks are ignored during inspection while leaving the executable
     Core untouched.
-}

{- Note [Indexed GADT decision-tree and continuation compilation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Scope-indexed functions outside inline-closure chains share one Core compiler
(`compileIndexedCore`):
  1. Mixed `forall`/value telescopes (`splitMixedPiTys`) preserve `Inferred`
     and `Specified` binders, rank-2 callback domains, and prenex or rank-2
     return quantifiers (`singleSubst`, `ren`, `consSubst`, `seqSubst`).
     Point-free unary lazy constructors (`ids = UVar`, `shift = FS`) are
     eta-expanded to match their declared value binders.
  2. One-constructor existential input packages (`Marked g -> Found g`) unwrap
     a local worker (`$wfindMarked`) when the wrapper forwards all existential
     and value fields in order.
  3. Exhaustive `Case (Var v)` trees refine nominal GADT `EqSpec` equalities
     along each constructor path (`tcUnifyTys alwaysBindTv`). Missing or
     `patError` branches are refuted when unifying that constructor's return
     type leaves another value parameter uninhabited (`dataConCannotMatch`, as
     in `lookupEnv :: Env n -> Fin n -> Clos` at `ENil :: Env 'Zero`).
  4. A direct single-constructor existential `Case` on a structurally smaller
     recursive call (`markFound`) compiles to a private `$case:` continuation
     family so reduction blocks until the recursive package constructor appears.
  5. Leaf expressions support bound variables, neutral callback applications,
     lazy promoted constructors, floated closed constructor-only type helpers,
     calls (full or partial after eta-contraction) to certified `SourceTable`
     families, and structurally decreasing recursive calls. Under `-O2`, when
     GHC inlines a certified helper callback into a caller (`substSingle`),
     `matchesInlinedFamily` compiles the inlined lambda decision tree and
     verifies every branch against `normaliseTcApp` of the helper family.
-}

-- ---------------------------------------------------------------------------
-- Core environments, casts, and telescope helpers
-- ---------------------------------------------------------------------------

type BindEnv = VarEnv CoreExpr
type MixedPiSpec = Either (ForAllTyFlag, TyVar) Type

mkBindEnv :: [(Id, CoreExpr)] -> BindEnv
mkBindEnv pairs = mkVarEnv (reverse pairs)

isSourceBreakpoint :: GenTickish pass -> Bool
isSourceBreakpoint Breakpoint{} = True
isSourceBreakpoint _            = False

stripBreakpointTicks :: [CoreBind] -> [(Id, CoreExpr)]
stripBreakpointTicks binds =
    [(v, stripTicksE isSourceBreakpoint rhs) | (v, rhs) <- Core.flattenBinds binds]

safeSimpleOptBind :: SimpleOpts -> (Id, CoreExpr) -> (Id, CoreExpr)
safeSimpleOptBind opts (v, rhs)
  | hasNativeDependentExpr (Core.Type (varType v))
  || hasExistentialExpr rhs
  || hasNativeDependentExpr rhs
  = (v, rhs)
  | otherwise
  = (v, simpleOptExpr opts rhs)

isValueVar :: Id -> Bool
isValueVar v = isId v && not (isCoVar v)

isConstructorId :: Id -> Bool
isConstructorId v = isJust (isDataConWorkId_maybe v <|> isDataConWrapId_maybe v)

lazyConstructor :: DC.DataCon -> Bool
lazyConstructor con =
    not (any DC.isBanged (DC.dataConImplBangs con)) &&
    null (DC.dataConRepArgBinders con) &&
    null (DC.dataConStupidTheta con)

constructorOnlyExpr :: CoreExpr -> Bool
constructorOnlyExpr expr = case constructorCall expr of
  Just (_, args) -> all constructorOnlyExpr args
  Nothing        -> False

-- | Test whether `expr` is a refuted `patError` call or an empty case on one.
isPatErrorExpr :: BindEnv -> CoreExpr -> Bool
isPatErrorExpr bindings = check (2 :: Int)
  where
    check 0 _ = False
    check d expr = case snd (collectMixedBinders expr) of
      Core.Case scrut _ _ [] -> check d scrut
      body
        | (Core.Var v, _) <- Core.collectArgs body ->
            v `hasKey` patErrorIdKey ||
            maybe False (check (d - 1)) (lookupVarEnv bindings v)
        | otherwise -> False

isPatErrorHelper :: BindEnv -> Id -> Bool
isPatErrorHelper bind_env v
  | Just decl <- lookupVarEnv bind_env v = isPatErrorExpr bind_env decl
  | otherwise                            = False

isFloatedConstructorHelper :: (Id -> Bool) -> BindEnv -> Id -> Bool
isFloatedConstructorHelper is_public bind_env v
  | not (is_public v)
  , Just decl <- lookupVarEnv bind_env v
  = (case function decl of
       Just (_ : _, [], body) -> constructorOnlyExpr body
       _ -> False) || isJust (validConstructorAliasBody decl)
  | otherwise = False

hasOnlyAllowedCalls :: (Id -> Bool) -> [Name] -> CoreExpr -> Bool
hasOnlyAllowedCalls allow_extra allowed rhs =
    null $ exprSomeFreeVarsList
      (\v -> isValueVar v &&
             not (v `hasKey` patErrorIdKey) &&
             idName v `notElem` allowed &&
             not (isConstructorId v) &&
             not (allow_extra v))
      rhs

reachable :: BindEnv -> Id -> [Name]
reachable bind_env root = walk emptyVarSet [] [root]
  where
    walk !_ acc [] = acc
    walk !seen acc (v : rest)
      | v `elemVarSet` seen = walk seen acc rest
      | Just body <- lookupVarEnv bind_env v =
          let fvs = exprSomeFreeVarsList
                      (\u -> u `elemVarEnv` bind_env && not (isPatErrorHelper bind_env u))
                      body
          in walk (extendVarSet seen v) (idName v : acc) (fvs ++ rest)
      | otherwise = walk seen acc rest

expandTypeHelper :: BindEnv -> CoreExpr -> CoreExpr
expandTypeHelper bindings rhs
  | (Core.Var helper, args) <- Core.collectArgs rhs
  , Just declaration <- lookupVarEnv bindings helper
  , Just (tvs, [], body) <- function declaration
  , Just tys <- traverse indexedTypeArg args
  , length tvs == length tys
  , let in_scope = mkInScopeSet (foldl' unionVarSet (exprFreeVars body)
                                    (map (exprFreeVars . Core.Type) tys))
        substitution = foldl' (\sub (tv, ty) -> extendTvSubst sub tv ty)
                             (mkEmptySubst in_scope) (zip tvs tys)
        expanded = substExpr substitution body
  , exprType expanded `eqType` exprType rhs
  = expanded
  | otherwise = rhs

indexedTypeArg :: CoreExpr -> Maybe Type
indexedTypeArg (Core.Type ty) = Just ty
indexedTypeArg _              = Nothing

isSafeMultOrReflCo :: Coercion -> Bool
isSafeMultOrReflCo co
  | coercionLKind co `eqType` coercionRKind co = True
  | checkedMultiplicityCast co = True
isSafeMultOrReflCo (UnivCo _ _ lhs rhs _)
  | Just (FTF_T_T, ma, mult, arg, res) <- splitFunTy_maybe lhs
  , Just (FTF_T_T, ma', mult', arg', res') <- splitFunTy_maybe rhs =
      (ma `eqType` ma' || (ma `eqType` matchableDataConTy && ma' `eqType` unmatchableDataConTy)) &&
      (mult `eqType` mult' || (isOneTy mult && isManyTy mult')) &&
      arg `eqType` arg' && res `eqType` res'
isSafeMultOrReflCo (SubCo c) = isSafeMultOrReflCo c
isSafeMultOrReflCo (TransCo c1 c2) = isSafeMultOrReflCo c1 && isSafeMultOrReflCo c2
isSafeMultOrReflCo ForAllCo{ fco_tcv = tv, fco_visL = l, fco_visR = r
                            , fco_ma = ma, fco_kind = MRefl, fco_body = body } =
    isTyVar tv && (eqForAllVis l r || (isInvisiblePiFlag l && isInvisiblePiFlag r)) &&
    (coercionLKind ma `eqType` coercionRKind ma ||
     (coercionLKind ma `eqType` matchableDataConTy && coercionRKind ma `eqType` unmatchableDataConTy)) &&
    isSafeMultOrReflCo body
isSafeMultOrReflCo FunCo{ fco_afl = FTF_T_T, fco_afr = FTF_T_T
                         , fco_ma = cm, fco_mult = cw, fco_arg = c1, fco_res = c2 } =
    (coercionLKind cm `eqType` coercionRKind cm ||
     (coercionLKind cm `eqType` matchableDataConTy && coercionRKind cm `eqType` unmatchableDataConTy)) &&
    (coercionLKind cw `eqType` coercionRKind cw ||
     (isOneTy (coercionLKind cw) && isManyTy (coercionRKind cw))) &&
    coercionLKind c1 `eqType` coercionRKind c1 &&
    coercionLKind c2 `eqType` coercionRKind c2
isSafeMultOrReflCo _ = False

weakeningBody :: CoreExpr -> Maybe CoreExpr
weakeningBody (Core.Cast body co) = guard (isSafeMultOrReflCo co) *> weakeningBody body
weakeningBody body                = Just body

caseCoercion :: [Var] -> Coercion -> Bool
caseCoercion scope co = case co of
    Refl _            -> True
    GRefl _ _ MRefl   -> True
    CoVarCo cv        -> cv `elem` scope
    TyConAppCo _ _ cs -> all (caseCoercion scope) cs
    TransCo a b       -> caseCoercion scope a && caseCoercion scope b
    SubCo c           -> caseCoercion scope c
    SymCo c           -> caseCoercion scope c
    SelCo _ c         -> caseCoercion scope c
    _                 -> False

branchBody :: [Var] -> CoreExpr -> Maybe CoreExpr
branchBody binders (Core.Cast body co)
  | caseCoercion (filter isCoVar binders) co = branchBody binders body
  | otherwise                                = Nothing
branchBody _ body = Just body

function :: CoreExpr -> Maybe ([TyVar], [Id], CoreExpr)
function expr = do
    let (binders, body) = collect [] expr
        (ty_binders, tm_binders) = span isTyVar binders
    guard (all isValueVar tm_binders)
    pure (ty_binders, tm_binders, body)
  where
    collect binders (Core.Lam binder body) = collect (binder : binders) body
    collect binders (Core.Let (Core.NonRec binder _) body)
      | isLocalId binder
      , isLiftedTypeKind (typeKind (varType binder))
      , not (binder `elemVarSet` exprFreeVars body)
      = collect binders body
    collect binders body = (reverse binders, body)

collectMixedBinders :: CoreExpr -> ([Var], CoreExpr)
collectMixedBinders = go []
  where
    go acc (Core.Lam b e) | isTyVar b || isValueVar b = go (b : acc) e
    go acc (Core.Let (Core.NonRec b _) e)
      | isLocalId b
      , isLiftedTypeKind (typeKind (varType b))
      , not (b `elemVarSet` exprFreeVars e)
      = go acc e
    go acc (Core.Cast e co) | isSafeMultOrReflCo co = go acc e
    go acc e = (reverse acc, e)

application :: CoreExpr -> Maybe (CoreExpr, [CoreExpr])
application expr = do
    let (head_expr, args) = Core.collectArgs expr
        (_, values) = span Core.isTyCoArg args
    guard (all (not . Core.isTyCoArg) values)
    pure (head_expr, values)

constructorCall :: CoreExpr -> Maybe (DC.DataCon, [CoreExpr])
constructorCall rhs = do
    unwrapped <- weakeningBody rhs
    (head_expr, fields) <- application unwrapped
    Core.Var make <- weakeningBody head_expr
    con <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
    pure (con, fields)

caseOn :: Id -> CoreExpr -> Maybe [(DC.DataCon, [Var], CoreExpr)]
caseOn target (Core.Case (Core.Var scrutinee) _ _ alts)
  | target == scrutinee = traverse dataAlt alts
  where
    dataAlt (Core.Alt (Core.DataAlt con) bs rhs) = Just (con, bs, rhs)
    dataAlt _                                    = Nothing
caseOn _ _ = Nothing

unwrapLocalCallbackLets :: CoreExpr -> CoreExpr
unwrapLocalCallbackLets expr
  | hasExistentialExpr expr || hasNativeDependentExpr expr = expr
  | otherwise = unwrapCallbackLetsGo expr

unwrapCallbackLetsGo :: CoreExpr -> CoreExpr
unwrapCallbackLetsGo (Core.Let (Core.NonRec b b_rhs) inner)
  | isLocalId b, isValueVar b
  , VanillaId <- idDetails b
  , null (idCoreRules b)
  , not (b `elemVarSet` exprFreeVars b_rhs)
  = let free = exprFreeVars inner `unionVarSet` exprFreeVars b_rhs
        sub = mkOpenSubst (mkInScopeSet free) [(b, b_rhs)]
    in unwrapCallbackLetsGo (substExpr sub inner)
unwrapCallbackLetsGo (Core.Lam b body)   = Core.Lam b (unwrapCallbackLetsGo body)
unwrapCallbackLetsGo (Core.App f a)      = Core.App (unwrapCallbackLetsGo f) (unwrapCallbackLetsGo a)
unwrapCallbackLetsGo (Core.Cast body co) = Core.Cast (unwrapCallbackLetsGo body) co
unwrapCallbackLetsGo expr                = expr

betaReduceTypeApps :: CoreExpr -> CoreExpr
betaReduceTypeApps expr
  | hasExistentialExpr expr || hasNativeDependentExpr expr = expr
  | otherwise = reduceUnwrapped (unwrapCallbackLetsGo expr)
  where
    reduceUnwrapped unwrapped
      | (Core.Lam b body, Core.Type ty : rest) <- collect [] unwrapped
      , isTyVar b
      = let free = exprFreeVars body `unionVarSet` exprFreeVars (Core.Type ty)
            sub = extendTvSubst (mkEmptySubst (mkInScopeSet free)) b ty
        in reduceUnwrapped (unwrapCallbackLetsGo (foldl' Core.App (substExpr sub body) rest))
      | Core.Lam b body <- unwrapped   = Core.Lam b (reduceUnwrapped body)
      | Core.Cast body co <- unwrapped = Core.Cast (reduceUnwrapped body) co
      | otherwise                      = unwrapped

    collect args (Core.App f a) = collect (a : args) f
    collect args f              = (f, args)

peelEtaCall :: CoreExpr -> CoreExpr
peelEtaCall raw_e
  | Just e <- weakeningBody raw_e
  , (ebndrs@(_ : _), raw_ebody) <- collectMixedBinders (unwrapLocalCallbackLets e)
  , Just ebody <- weakeningBody raw_ebody
  , (raw_fn, all_args) <- Core.collectArgs ebody
  , Just fn <- weakeningBody raw_fn
  , length all_args >= length ebndrs
  , let (prefix_args, suffix_args) = splitAt (length all_args - length ebndrs) all_args
  , and (zipWith matchesEta ebndrs suffix_args)
  , let prefix_expr = foldl' Core.App fn prefix_args
        fvs = exprFreeVars prefix_expr
  , all (not . (`elemVarSet` fvs)) ebndrs
  = peelEtaCall prefix_expr
  | otherwise = raw_e
  where
    matchesEta b (Core.Type (TyVarTy tv)) = isTyVar b && tv == b
    matchesEta b arg
      | isValueVar b, Just (Core.Var v) <- weakeningBody arg = v == b
    matchesEta _ _ = False

peelEtaTarget :: CoreExpr -> Maybe Id
peelEtaTarget raw_e
  | Just (Core.Var fn) <- weakeningBody (peelEtaCall raw_e) = Just fn
  | otherwise = Nothing

resolveForwardCseAlias :: BindEnv -> Id -> CoreExpr -> (Maybe Id, CoreExpr)
resolveForwardCseAlias bind_env actual rhs
  | Just av <- peelEtaTarget rhs
  , idName av /= idName actual
  , varType av `eqType` varType actual
  , Just ar <- lookupVarEnv bind_env av
  = (Just av, ar)
  | otherwise = (Nothing, rhs)

validConstructorAliasBody :: CoreExpr -> Maybe DC.DataCon
validConstructorAliasBody raw = do
    expr <- weakeningBody raw
    let (bndrs, raw_body) = collectMixedBinders (betaReduceTypeApps expr)
        val_bndrs = filter isValueVar bndrs
    body <- weakeningBody raw_body
    guard (length bndrs == length (filter isTyVar bndrs) + length val_bndrs &&
           length val_bndrs <= 1)
    (con, raw_values) <- constructorCall body
    values <- traverse weakeningBody raw_values
    guard (lazyConstructor con && length val_bndrs == length values)
    zipWithM_ (\i v -> case v of { Core.Var y -> guard (i == y); _ -> Nothing }) val_bndrs values
    pure con

isInvisiblePiFlag :: ForAllTyFlag -> Bool
isInvisiblePiFlag Inferred  = True
isInvisiblePiFlag Specified = True
isInvisiblePiFlag _         = False

splitMixedPiTys :: Type -> Maybe ([MixedPiSpec], Type)
splitMixedPiTys raw_ty = case splitForAllForAllTyBinders raw_ty of
    ((match, Bndr tv flag) : rest_bndrs, inner)
      | isTyVar tv, isInvisiblePiFlag flag, match `eqType` unmatchableDataConTy -> do
          let rebuilt = foldr (\(m, b) acc -> ForAllTy m b acc) inner rest_bndrs
          (more, res) <- splitMixedPiTys rebuilt
          pure (Left (flag, tv) : more, res)
    (_ : _, _) -> Nothing
    ([], tau)
      | Just (FTF_T_T, match, mult, dom, cod) <- splitFunTy_maybe tau
      , match `eqType` unmatchableDataConTy, isManyTy mult -> do
          (more, res) <- splitMixedPiTys cod
          pure (Right dom : more, res)
      | Just _ <- splitFunTy_maybe tau -> Nothing
      | otherwise -> Just ([], coreFullView tau)

isIndexedAlgDomain :: Type -> Bool
isIndexedAlgDomain ty
  | Just (tc, args@(_ : _)) <- splitTyConApp_maybe ty
  = isAlgTyCon tc &&
    (any (not . DC.isVanillaDataCon) (tyConDataCons tc) ||
     any (not . isLiftedTypeKind . typeKind) args)
  | otherwise = False

isIndexedCallbackTy :: Type -> Bool
isIndexedCallbackTy ty
  | Just (pis, cod) <- splitMixedPiTys ty
  , isIndexedAlgDomain cod
  = case pis of
      [Right dom]         -> isIndexedAlgDomain dom
      [Left _, Right dom] -> isIndexedAlgDomain dom
      _                   -> False
  | otherwise = False

-- | Select functions whose signatures can head an early indexed lookup,
-- GADT structural traversal, or scope-indexed closure chain.
earlyClosureCandidateType :: Type -> Bool
earlyClosureCandidateType ty
  | Just (pis, out_ty) <- splitMixedPiTys ty
  , any (\case { Left _ -> True; _ -> False }) pis
  , Right first_dom : _ <- [Right d | Right d <- pis]
  , isLiftedTypeKind (typeKind out_ty)
  = isIndexedAlgDomain first_dom || isIndexedCallbackTy first_dom
  | otherwise = False

-- ---------------------------------------------------------------------------
-- Family, branch, and companion helpers
-- ---------------------------------------------------------------------------

companionTyConLookup :: [TyCon] -> Id -> [TyCon]
companionTyConLookup tcs =
    let !by_occ = mkOccEnv_C (++)
          [ (nameOccName (tyConName tc), [(nameModule_maybe (tyConName tc), tc)])
          | tc <- tcs ]
    in \fun ->
      let mod_mb = nameModule_maybe (idName fun)
          occ_str = occNameString (nameOccName (idName fun))
          pick prefix
            | Just bucket <- lookupOccEnv by_occ (mkTcOcc (prefix ++ occ_str))
            = lookup mod_mb bucket
            | otherwise = Nothing
      in mapMaybe pick ["$closure:", "$apply:", "$lookup:", "$weaken:", "$substMap:", "$case:"]

mkClosureTableAxiom :: Bool -> Name -> TcM Name
mkClosureTableAxiom False root = newImplicitBinder root mkInstTyCoOcc
mkClosureTableAxiom True root =
    newImplicitBinder root (mkInstTyCoOcc . mkTcOcc . ("early:" ++) . occNameString)

isEarlyClosureTable :: TyCon -> Bool
isEarlyClosureTable table
  | Just axiom <- closedFamilyTyConCoAxiom_maybe table
  = "D:early:" `isPrefixOf` occNameString (nameOccName (coAxiomName axiom))
  | otherwise = False

freshVar :: String -> Kind -> TcM TyVar
freshVar str kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc str)

freshLocalId :: String -> Type -> TcM Id
freshLocalId str ty = do
    nm <- newSysName (mkVarOcc str)
    pure (mkLocalId nm manyDataConTy ty)

mkFamily :: Name -> Name -> [TyConBinder] -> Kind -> [CoAxBranch] -> TyCon
mkFamily name axname binders result branches =
    let family = mkFamilyTyCon name (mkTyConKindWithMatchability unmatchableDataConTy binders result)
                    binders 0 result Nothing (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal axname family branches
    in family

mkBranch :: [TyVar] -> [Type] -> Type -> CoAxBranch
mkBranch vars lhs rhs =
    mkCoAxBranch vars [] [] lhs rhs (map (const Nominal) vars) noSrcSpan

renamingBranch :: [Type] -> Type -> CoAxBranch
renamingBranch lhs = mkBranch (tyCoVarsOfTypesWellScoped lhs) lhs

validBranch :: CoAxBranch -> Bool
validBranch branch =
    all isTyFamFree lhs &&
    tyCoVarsOfTypes (coAxBranchRHS branch : lhs ++ map varType tvs) `subVarSet` mkVarSet tvs
  where
    lhs = coAxBranchLHS branch
    tvs = coAxBranchTyVars branch

sameIndexedRow :: CoAxBranch -> CoAxBranch -> MaybeT TcM ()
sameIndexedRow left right = do
    let old_lhs = coAxBranchLHS left
        new_lhs = coAxBranchLHS right
    subst <- MaybeT (pure (tcMatchTys old_lhs new_lhs))
    renamed <- MaybeT $ pure $ traverse (\v -> lookupTyVar subst v >>= getTyVar_maybe) (coAxBranchTyVars left)
    guard (length renamed == length (coAxBranchTyVars right) &&
           mkVarSet renamed == mkVarSet (coAxBranchTyVars right) &&
           substTyUnchecked subst (coAxBranchRHS left) `eqType` coAxBranchRHS right)

conAt :: DC.DataCon -> Type -> [Type] -> Maybe Type
conAt con result fields = do
    let promoted = DC.promoteDataCon con
        (annotated, telescope) = splitForAllTyVars (tyConKind promoted)
        (_, inputs, output) = splitFunTys telescope
    guard (length inputs == length fields)
    subst <- tcMatchTys (output : map scaledThing inputs) (result : map typeKind fields)
    args <- traverse (lookupTyVar subst . snd) annotated
    let value = mkTyConApp promoted (args ++ fields)
    value <$ guard (typeKind value `eqType` result)

checkedPromoted :: DC.DataCon -> Type -> [Type] -> [Type] -> Maybe Type
checkedPromoted con kind source fields = do
    (tc, parameters) <- splitTyConApp_maybe kind
    guard (tc == DC.dataConTyCon con)
    let names = DC.dataConUserTyVars con
        value = mkTyConApp (DC.promoteDataCon con) (source ++ fields)
        vanilla = do
          expected <- traverse (`lookup` zip (DC.dataConUnivTyVars con) parameters) names
          guard (eqTypes expected source)
          openDataConFieldTypes con parameters
    expected <- vanilla <|> (\(_, _, tys) -> tys) <$> openIndexedDataCon con kind source
    guard (eqTypes (map typeKind fields) expected && typeKind value `eqType` kind)
    pure value

publishRenamingTable :: Bool -> (TyCon -> Bool) -> Id -> [Id] -> [TyConBinder] -> Kind
                     -> (TyCon -> Maybe [CoAxBranch]) -> MaybeT TcM (Id, [TyCon])
publishRenamingTable is_early public owner helpers binders result rows = do
    root <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (mkClosureTableAxiom is_early root)
    let equations = rows table
        table = mkFamily root axiom binders result (fromMaybe [] equations)
        local_helpers = nameSetElemsStable $ mkNameSet
          [ idName h
          | h <- helpers
          , idName h /= idName owner
          , nameModule_maybe (idName h) == nameModule_maybe (idName owner) ]
        details = case local_helpers of
          []  -> SourceTableId (idName owner) table NominalSourceTable
          nms -> CheckedClosureSourceTableId (idName owner) table nms
    guard (tyConKind table `eqType` varType owner)
    branches <- MaybeT (pure equations)
    guard (all validBranch branches && all public (sourceEquationDatatypes [table]))
    pure (setIdDetails owner details, [table])

-- ---------------------------------------------------------------------------
-- Unified indexed GADT decision-tree & continuation compiler (SC2 + SC5)
-- ---------------------------------------------------------------------------

data IndexedPattern
  = IndexedVariable Id
  | IndexedConstructor Type DC.DataCon [Type] [IndexedPattern]

data IndexedResultCase = IndexedResultCase [Type] Id IndexedPattern CoreExpr

data IndexedEquation = IndexedEquation [TyVar] [Type] (TyCon -> Type)

applyRow :: IndexedEquation -> TyCon -> CoAxBranch
applyRow (IndexedEquation vars lhs rhs) family = mkBranch vars lhs (rhs family)

data CompiledIndexed = CompiledIndexed
  { ci_binders       :: [TyConBinder]
  , ci_res_kind      :: Kind
  , ci_rows          :: [(IndexedEquation, Maybe (TyCon, CoAxBranch))]
  , ci_case_names    :: Maybe (Name, Name)
  , ci_mb_worker     :: Maybe Id
  , ci_table_callees :: [Id]
  }

indexedFields :: IndexedPattern -> [Id]
indexedFields pat = go pat []
  where
    go (IndexedVariable v) acc           = v : acc
    go (IndexedConstructor _ _ _ fs) acc = foldr go acc fs

indexedAlt :: Id -> DC.DataCon -> [Var] -> Maybe (IndexedPattern, [Var])
indexedAlt scrutinee con binders = do
    let kind = varType scrutinee
    (tc, args) <- splitTyConApp_maybe kind
    (universals, exs, specs, [], fields, _) <- pure (DC.dataConFullSig con)
    let (locals, remaining) = splitAt (length exs) binders
        (proofs, values) = splitAt (length specs) remaining
        instantiate = substTyWith (universals ++ exs) (args ++ mkTyVarTys locals)
    guard (tc == DC.dataConTyCon con && length args == length universals &&
           length locals == length exs && all isTyVar locals &&
           eqTypes (map (instantiate . varType) exs) (map varType locals) &&
           length proofs == length specs && all isCoVar proofs &&
           all isValueVar values && eqTypes (map (instantiate . scaledThing) fields) (map varType values) &&
           lazyConstructor con)
    zipWithM_ (check_proof instantiate) specs proofs
    let source = map (instantiate . mkTyVarTy) (DC.dataConUserTyVars con)
    pure (IndexedConstructor kind con source (map IndexedVariable values), proofs)
  where
    check_proof instantiate spec binder = do
      let (tv, ty) = DC.eqSpecPair spec
          co = CoVarCo binder
      guard (coercionRole co == Nominal &&
             coercionLKind co `eqType` instantiate (mkTyVarTy tv) &&
             coercionRKind co `eqType` instantiate ty)

indexedPatternTypes :: (Type -> Type) -> IndexedPattern -> MaybeT TcM (Type, [(Id, Type)])
indexedPatternTypes specialize pat = do
    variables <- lift $ traverse (freshVar "field" . specialize . varType) ids
    let fields = zip ids (mkTyVarTys variables)
        write_pattern (IndexedVariable v) = lookup v fields
        write_pattern (IndexedConstructor kind con source children) =
          checkedPromoted con (specialize kind) (map specialize source) =<< traverse write_pattern children
    packed <- MaybeT (pure (write_pattern pat))
    pure (packed, fields)
  where
    ids = indexedFields pat

indexedConstructor :: (Type -> Type) -> Id -> [CoreExpr] -> Maybe (DC.DataCon, [Type], [CoreExpr])
indexedConstructor specialize callee args = do
    let (erased, values) = span Core.isTyCoArg args
        (tys, proofs) = span (isJust . indexedTypeArg) erased
    types <- traverse (fmap specialize . indexedTypeArg) tys
    let check_proof (Core.Coercion co) = coercionRole co == Nominal &&
              specialize (coercionLKind co) `eqType` specialize (coercionRKind co)
        check_proof _ = False
    (con, supplied) <- case isDataConWrapId_maybe callee of
      Just con -> (con, types) <$ guard (null proofs)
      Nothing -> do
        con <- isDataConWorkId_maybe callee
        let variables = DC.dataConUnivTyVars con ++ DC.dataConExTyCoVars con
        (_, _, specs, [], _, _) <- pure (DC.dataConFullSig con)
        guard (length variables == length types && length specs == length proofs &&
               all check_proof proofs)
        supplied <- traverse (`lookup` zip variables types) (DC.dataConUserTyVars con)
        pure (con, supplied)
    guard (length supplied == length (DC.dataConUserTyVars con))
    pure (con, supplied, values)

indexedResultCase :: Id -> [TyVar] -> IndexedPattern -> CoreExpr -> Maybe IndexedResultCase
indexedResultCase owner parameters pat (Core.Case call binder kind [Core.Alt (Core.DataAlt con) binders body]) = do
    (Core.Var callee, args) <- pure (Core.collectArgs call)
    let (prefix, suffix) = splitAt (length parameters) args
    tys <- traverse indexedTypeArg prefix
    [Core.Var operand] <- pure suffix
    (tc, _) <- splitTyConApp_maybe (varType binder)
    (_, _ : _, [], [], _, _) <- pure (DC.dataConFullSig con)
    guard (callee == owner && length tys == length parameters && operand `elem` indexedFields pat &&
           varType binder `eqType` exprType call && kind `eqType` exprType body &&
           tyConDataCons tc == [con])
    (res_pat, []) <- indexedAlt binder con binders
    pure (IndexedResultCase tys operand res_pat body)
indexedResultCase _ _ _ _ = Nothing

-- | Compile a mixed-telescope indexed Core definition into closed-family equations.
-- See Note [Indexed GADT decision-tree and continuation compilation].
compileIndexedCore
  :: BindEnv
  -> (Id -> Maybe TyCon)
  -> Maybe (Name, Name)
  -> Maybe [CoAxBranch]
  -> Id
  -> CoreExpr
  -> MaybeT TcM CompiledIndexed
compileIndexedCore bindings resolveTable mb_case_names mb_expected owner rhs = do
    (pis, sig_out_ty) <- MaybeT (pure (splitMixedPiTys (varType owner)))
    let val_sig_doms = [d | Right d <- pis]
    guard (any (\case { Left _ -> True; _ -> False }) pis &&
           not (null val_sig_doms) &&
           all (isLiftedTypeKind . typeKind) (sig_out_ty : val_sig_doms) &&
           any (\d -> isIndexedAlgDomain d || isIndexedCallbackTy d) val_sig_doms)
    (hdr_binders, hdr_res) <- lift (buildHeaderTelescope pis sig_out_ty)
    let init_in_scope = mkInScopeSet (tyCoVarsOfTypes [varType owner, exprType rhs])
    compileConAlias pis hdr_binders hdr_res <|> do
      guard (exprType rhs `eqType` varType owner)
      (bndrs, inst_out_ty, body0) <- peelMixedBinders pis sig_out_ty (mkEmptySubst init_in_scope) (betaReduceTypeApps rhs)
      guard (exprType body0 `eqType` inst_out_ty)
      let val_params = [v | Right v <- bndrs]
          prenex_tvs = [tv | Left (_, tv) <- takeWhile (\case { Left _ -> True; _ -> False }) bndrs]
          is_prenex = length prenex_tvs + length val_params == length bndrs
      (mb_pack, pats0, tree0) <- unwrapPackWorker is_prenex prenex_tvs val_params body0
      raw_rows <- descendTree is_prenex prenex_tvs mb_pack pats0 [] tree0
      guard (not (null raw_rows) && (isNothing mb_pack || length raw_rows > 1))
      case_names <- case [() | (_, _, Right _) <- raw_rows] of
        [] -> pure Nothing
        [_] | isNothing mb_pack -> case mb_case_names of
          Just nms -> pure (Just nms)
          Nothing  -> Just <$> lift (allocateCaseNames owner)
        _ -> MaybeT (pure Nothing)
      dummy_name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
      dummy_ax <- lift (newImplicitBinder dummy_name mkInstTyCoOcc)
      let header = mkFamily dummy_name dummy_ax hdr_binders hdr_res []
      compiled <- traverse (compileRow header bndrs inst_out_ty is_prenex prenex_tvs val_params mb_pack case_names) raw_rows
      let (eq_pairs, self_flags, callee_lists) = unzip3 compiled
          has_self = or self_flags
          all_callees = nubBy (\a b -> idName a == idName b) (concat callee_lists)
      guard (certifiedShape has_self all_callees bndrs val_params inst_out_ty raw_rows eq_pairs header)
      let mb_worker = case mb_pack of
            Just (_, _, w) -> w
            Nothing        -> Nothing
      pure CompiledIndexed
        { ci_binders       = hdr_binders
        , ci_res_kind      = hdr_res
        , ci_rows          = eq_pairs
        , ci_case_names    = case_names
        , ci_mb_worker     = mb_worker
        , ci_table_callees = all_callees
        }
  where
    compileConAlias pis hdr_binders hdr_res = do
      let (left_pis, rest_pis) = span (\case { Left _ -> True; _ -> False }) pis
      [Right _] <- pure rest_pis
      dc <- MaybeT (pure (validConstructorAliasBody rhs))
      let hdr_tvs = [tv | Bndr tv (NamedTCB _) <- hdr_binders]
      [Bndr arg_tv AnonTCB] <- pure [b | b@(Bndr _ AnonTCB) <- hdr_binders]
      guard (length hdr_tvs == length left_pis && not (null hdr_tvs) && length hdr_tvs <= 3 &&
             mkVarSet hdr_tvs `subVarSet` tyCoVarsOfTypes [varType arg_tv, hdr_res])
      out <- MaybeT (pure (conAt dc hdr_res [mkTyVarTy arg_tv]))
      let lhs_tys = mkTyVarTys (hdr_tvs ++ [arg_tv])
          vars = tyCoVarsOfTypesWellScoped lhs_tys
          eq = IndexedEquation vars lhs_tys (const out)
      dummy_name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
      dummy_ax <- lift (newImplicitBinder dummy_name mkInstTyCoOcc)
      let header = mkFamily dummy_name dummy_ax hdr_binders hdr_res []
      guard (isScopeConAlias (varType arg_tv) hdr_res eq header && validBranch (applyRow eq header))
      pure CompiledIndexed
        { ci_binders       = hdr_binders
        , ci_res_kind      = hdr_res
        , ci_rows          = [(eq, Nothing)]
        , ci_case_names    = Nothing
        , ci_mb_worker     = Nothing
        , ci_table_callees = []
        }
    allocateCaseNames fn = do
      nm <- newImplicitBinder (idName fn) (mkTcOcc . ("$case:" ++) . occNameString)
      ax <- newImplicitBinder nm mkInstTyCoOcc
      pure (nm, ax)

    buildHeaderTelescope pis out_ty = go (mkEmptySubst (mkInScopeSet (tyCoVarsOfTypes [varType owner]))) [] pis
      where
        go sub acc [] = pure (reverse acc, substTy sub out_ty)
        go sub acc (Left (flag, tv) : rest) = do
          tv' <- freshVar (occNameString (nameOccName (getName tv))) (substTy sub (varType tv))
          go (extendTvSubst sub tv (mkTyVarTy tv')) (mkNamedTyConBinder flag tv' : acc) rest
        go sub acc (Right dom : rest) = do
          v' <- freshVar "arg" (substTy sub dom)
          go sub (mkAnonTyConBinder v' : acc) rest

    stripNoise (Core.Let (Core.NonRec b _) e)
      | isLocalId b
      , isLiftedTypeKind (typeKind (varType b))
      , not (b `elemVarSet` exprFreeVars e)
      = stripNoise e
    stripNoise (Core.Cast e co) | isSafeMultOrReflCo co = stripNoise e
    stripNoise e = e

    pointFreeCon e = do
      inner <- weakeningBody e
      let (raw_hd, args) = Core.collectArgs inner
      Core.Var make <- weakeningBody raw_hd
      dc <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
      guard (lazyConstructor dc && all Core.isTyCoArg args)
      pure inner

    peelMixedBinders [] sig_out sub expr =
      pure ([], substTy sub sig_out, stripNoise expr)
    peelMixedBinders (Left (flag, sig_tv) : rest) sig_out sub expr = do
      let exp_k = substTy sub (varType sig_tv)
          e' = stripNoise expr
      case e' of
        Core.Lam b body | isTyVar b, varType b `eqType` exp_k -> do
          (more, out, final_b) <- peelMixedBinders rest sig_out (extendTvSubst sub sig_tv (mkTyVarTy b)) body
          pure (Left (flag, b) : more, out, final_b)
        _ | Just con_fn <- pointFreeCon e' -> do
          b <- lift (freshVar "tv" exp_k)
          (more, out, final_b) <- peelMixedBinders rest sig_out
                                    (extendTvSubst sub sig_tv (mkTyVarTy b))
                                    (Core.App con_fn (Core.Type (mkTyVarTy b)))
          pure (Left (flag, b) : more, out, final_b)
        _ -> MaybeT (pure Nothing)
    peelMixedBinders (Right sig_dom : rest) sig_out sub expr = do
      let inst_dom = substTy sub sig_dom
          e' = stripNoise expr
      case e' of
        Core.Lam v body | isValueVar v, varType v `eqType` inst_dom -> do
          (more, out, final_b) <- peelMixedBinders rest sig_out sub body
          pure (Right v : more, out, final_b)
        _ | null rest, Just con_fn <- pointFreeCon e' -> do
          v <- lift (freshLocalId "eta" inst_dom)
          (more, out, final_b) <- peelMixedBinders [] sig_out sub (Core.App con_fn (Core.Var v))
          pure (Right v : more, out, final_b)
        _ -> MaybeT (pure Nothing)

    unwrapPackWorker True prenex_tvs [input] body
      | Just (in_tc, _) <- splitTyConApp_maybe (varType input)
      , [pack] <- tyConDataCons in_tc
      , (_, exs@(_ : _), [], [], _, _) <- DC.dataConFullSig pack
      , Just [(pack', alt_bs, nested)] <- caseOn input body
      , pack' == pack = do
          (outer_pat, []) <- MaybeT (pure (indexedAlt input pack alt_bs))
          let (existentials, fields) = splitAt (length exs) alt_bs
              tys = prenex_tvs ++ existentials
          (mb_w, tree) <- case nested of
            Core.Case{} -> pure (Nothing, nested)
            _ -> do
              (Core.Var worker, args) <- MaybeT (pure (Just (Core.collectArgs nested)))
              guard (worker /= owner &&
                     (not (isExternalName (idName worker)) ||
                      nameModule_maybe (idName worker) == nameModule_maybe (idName owner)))
              let (passed_tys, passed_fs) = splitAt (length tys) args
              supplied <- MaybeT (pure (traverse indexedTypeArg passed_tys))
              guard (eqTypes supplied (mkTyVarTys tys) &&
                     length passed_fs == length fields &&
                     and (zipWith (\a e -> case a of { Core.Var u -> u == e; _ -> False }) passed_fs fields))
              decl <- MaybeT (pure (lookupVarEnv bindings worker))
              (w_tys, w_fs, w_tree) <- MaybeT (pure (function decl))
              let wbndrs = w_tys ++ w_fs
                  free = foldl' unionVarSet (exprFreeVars w_tree) (map exprFreeVars args)
                  expanded = substExpr (mkOpenSubst (mkInScopeSet free) (zip wbndrs args)) w_tree
              guard (length w_tys == length tys && length w_fs == length fields &&
                     exprType decl `eqType` varType worker &&
                     exprType expanded `eqType` exprType nested)
              pure (Just worker, expanded)
          pure (Just (pack, outer_pat, mb_w), [(input, outer_pat)], tree)
    unwrapPackWorker _ _ val_params body =
      pure (Nothing, [(v, IndexedVariable v) | v <- val_params], body)

    replacePat v node (IndexedVariable u) | v == u = node
    replacePat v node (IndexedConstructor ty con src fs) =
      IndexedConstructor ty con src (map (replacePat v node) fs)
    replacePat _ _ pat = pat

    descendTree is_prenex prenex_tvs mb_pack pats proofs expr
      | is_prenex
      , isNothing mb_pack
      , [(_, outer_pat@IndexedConstructor{})] <- pats
      , Just rc <- indexedResultCase owner prenex_tvs outer_pat expr
      = pure [(pats, proofs, Right rc)]
    descendTree is_prenex prenex_tvs mb_pack pats proofs expr@(Core.Case (Core.Var scrut) bndr res_ty alts) = do
      let leaf_vars = concatMap (indexedFields . snd) pats
      guard (not (null alts) &&
             varType bndr `eqType` varType scrut &&
             res_ty `eqType` exprType expr &&
             scrut `elem` leaf_vars)
      (tc, _) <- MaybeT (pure (splitTyConApp_maybe (varType scrut)))
      let cons = tyConDataCons tc
      guard (not (null cons))
      parsed <- fmap catMaybes $ MaybeT $ pure $ traverse (\case
        Core.Alt (Core.DataAlt dc) bs rhs_e | dc `elem` cons -> Just (Just (dc, (bs, rhs_e)))
        Core.Alt Core.DEFAULT [] rhs_e | isPatErrorExpr bindings rhs_e -> Just Nothing
        _ -> Nothing) alts
      guard (length parsed == length (nubBy (\a b -> fst a == fst b) parsed))
      fmap concat $ traverse (\dc -> case lookup dc parsed of
        Just (bs, rhs_e) | not (isPatErrorExpr bindings rhs_e) -> do
          (node, eqs) <- MaybeT (pure (indexedAlt scrut dc bs))
          let pats' = [(p, replacePat scrut node pat) | (p, pat) <- pats]
          descendTree is_prenex prenex_tvs mb_pack pats' (proofs ++ eqs) rhs_e
        _ -> do
          refuted <- lift (isRefutedBranch scrut dc leaf_vars proofs)
          guard refuted
          pure []) cons
    descendTree _ _ _ _ _ Core.Case{} = MaybeT (pure Nothing)
    descendTree _ _ _ pats proofs expr = pure [(pats, proofs, Left (unwrapLocalCallbackLets expr))]

    isRefutedBranch scrut dc leaf_vars proofs = do
      let (univs, exs, specs, _, _, orig_res) = DC.dataConFullSig dc
      (ren, _) <- freshenTyVarBndrs (univs ++ exs)
      let inst = substTyUnchecked ren
          spec_lhs = [inst (mkTyVarTy tv) | spec <- specs, let (tv, _) = DC.eqSpecPair spec]
          spec_rhs = [inst rhs_t | spec <- specs, let (_, rhs_t) = DC.eqSpecPair spec]
          proof_cos = map CoVarCo proofs
          lhs_tys = map coercionLKind proof_cos ++ spec_lhs ++ [varType scrut]
          rhs_tys = map coercionRKind proof_cos ++ spec_rhs ++ [inst orig_res]
      pure $ case tcUnifyTys alwaysBindTv lhs_tys rhs_tys of
        Nothing -> True
        Just subst ->
          let spec_ty = substTyUnchecked subst
          in any (\v -> v /= scrut && case splitTyConApp_maybe (spec_ty (varType v)) of
                   Just (otc, oargs) -> isAlgTyCon otc && all (DC.dataConCannotMatch oargs) (tyConDataCons otc)
                   Nothing           -> False) leaf_vars

    buildRowLhs specialize mixed_bndrs row_pats = go [] [] mixed_bndrs
      where
        go lhs_acc env_acc [] = pure (reverse lhs_acc, env_acc)
        go lhs_acc env_acc (Left (_, tv) : rest) =
          go (specialize (mkTyVarTy tv) : lhs_acc) env_acc rest
        go lhs_acc env_acc (Right v : rest) = do
          pat <- MaybeT (pure (lookup v row_pats))
          case pat of
            IndexedVariable u -> do
              fv <- lift (freshVar "arg" (specialize (varType u)))
              go (mkTyVarTy fv : lhs_acc) ((u, mkTyVarTy fv) : env_acc) rest
            IndexedConstructor{} -> do
              (packed, p_fields) <- indexedPatternTypes specialize pat
              go (packed : lhs_acc) (p_fields ++ env_acc) rest

    compileRow header bndrs out_ty is_prenex prenex_tvs val_params mb_pack mb_cnames (row_pats, proofs, leaf_or_rc) = do
      let equalities = map CoVarCo proofs
      subst <- MaybeT $ pure $ tcUnifyTys alwaysBindTv
                 (map coercionLKind equalities) (map coercionRKind equalities)
      let specialize = substTyUnchecked subst
          res_kind = specialize out_ty
      (lhs_tys, val_env) <- buildRowLhs specialize bndrs row_pats
      (rhs_fn, mb_helper, used_self, used_callees) <- case leaf_or_rc of
        Right (IndexedResultCase call_tys operand c_pat c_body)
          | input_v : _ <- val_params -> do
              (c_name, c_ax) <- MaybeT (pure mb_cnames)
              let tys = map specialize call_tys
                  in_kind = substTyWith prenex_tvs tys (varType input_v)
                  case_kind = substTyWith prenex_tvs tys out_ty
                  locals = tyCoVarsOfTypesWellScoped [case_kind, res_kind]
                  local_tys = mkTyVarTys locals
              guard (all isTyVar locals)
              arg_ty <- MaybeT (pure (lookup operand val_env))
              guard (typeKind arg_ty `eqType` in_kind)
              (c_packed, c_fields) <- indexedPatternTypes specialize c_pat
              guard (typeKind c_packed `eqType` case_kind)
              (b_self, b_tabs, b_fn) <- compileExpr header out_ty False [] [] Nothing
                                          [] bindings c_fields specialize proofs Nothing False c_body
              guard (not b_self)
              let c_rhs = b_fn header
                  c_lhs = local_tys ++ [c_packed]
                  c_vars = tyCoVarsOfTypesWellScoped c_lhs
                  c_br = mkBranch c_vars c_lhs c_rhs
              guard (all isTyVar c_vars && typeKind c_rhs `eqType` res_kind && validBranch c_br)
              op_tv <- lift (freshVar "case" case_kind)
              let c_binders = mkNamedTyConBinders Specified locals ++ [mkAnonTyConBinder op_tv]
                  c_tc = mkFamily c_name c_ax c_binders res_kind [c_br]
                  val_fn fam = mkTyConApp c_tc (local_tys ++ [mkTyConApp fam (tys ++ [arg_ty])])
              pure (val_fn, Just (c_tc, c_br), True, b_tabs)
        Right _ -> MaybeT (pure Nothing)
        Left leaf_expr -> do
          let mb_exp_rhs = case mb_expected of
                Just [exp_br] | Just m_sub <- tcMatchTys (coAxBranchLHS exp_br) lhs_tys ->
                  Just (substTyUnchecked m_sub (coAxBranchRHS exp_br))
                _ -> Nothing
          (b_self, b_tabs, b_fn) <- compileExpr header out_ty is_prenex prenex_tvs val_params mb_pack
                                      row_pats bindings val_env specialize proofs mb_exp_rhs False leaf_expr
          pure (b_fn, Nothing, b_self, b_tabs)
      let vars = tyCoVarsOfTypesWellScoped lhs_tys
          eq = IndexedEquation vars lhs_tys rhs_fn
      guard (all isTyVar vars && typeKind (rhs_fn header) `eqType` res_kind &&
             validBranch (applyRow eq header))
      pure ((eq, mb_helper), used_self, used_callees)

    compileExpr header out_ty is_prenex prenex_tvs val_params mb_pack
                row_pats floats val_env specialize proofs mb_exp_rhs under_con = go floats under_con
      where
        validCast co =
          isSafeMultOrReflCo co ||
          (caseCoercion proofs co &&
           specialize (coercionLKind co) `eqType` specialize (coercionRKind co))

        unwrapValidCast (Core.Cast e co) | validCast co = unwrapValidCast e
        unwrapValidCast e = Just e

        go fl uc (Core.Cast e co) = do
          guard (validCast co)
          go fl uc e
        go fl uc expr
          | Just eta_v <- peelEtaTarget expr
          , Just v_ty <- lookup eta_v val_env
          = pure (False, [], const v_ty)
          | otherwise = do
              let (raw_fun, args) = Core.collectArgs expr
              Core.Var callee <- MaybeT (pure (unwrapValidCast raw_fun))
              let mb_worker = case mb_pack of { Just (_, _, w) -> w; Nothing -> Nothing }
              if callee == owner || Just callee == mb_worker
                then compileSelf uc callee expr args
                else compileVarOrCb fl callee expr args <|>
                     compileCon fl callee expr args <|>
                     compileTableCall fl callee expr args <|>
                     compileFloat fl uc callee expr

        compileVarOrCb fl callee expr args = do
          v_ty <- MaybeT (pure (lookup callee val_env))
          if null args
            then pure (False, [], const v_ty)
            else do
              arg_res <- traverse (\case
                Core.Type t -> pure (False, [], const (specialize t))
                a | not (Core.isTyCoArg a) -> go fl False (peelEtaCall a)
                _ -> MaybeT (pure Nothing)) args
              let (s_flags, t_lists, fns) = unzip3 arg_res
                  res fam = mkAppTys v_ty [f fam | f <- fns]
              guard (typeKind (res header) `eqType` specialize (exprType expr))
              pure (or s_flags, concat t_lists, res)

        compileCon fl callee expr args = do
          (con, supplied, val_args) <- MaybeT (pure (indexedConstructor specialize callee args))
          guard (lazyConstructor con && all (caseCoercion proofs) [co | Core.Coercion co <- args])
          children <- traverse (go fl True) val_args
          let (s_flags, t_lists, fns) = unzip3 children
          _ <- MaybeT (pure (checkedPromoted con (specialize (exprType expr)) supplied [f header | f <- fns]))
          pure (or s_flags, concat t_lists,
                \fam -> mkTyConApp (DC.promoteDataCon con) (supplied ++ [f fam | f <- fns]))

        compileFloat fl uc callee expr = do
          guard (isJust (lookupVarEnv fl callee))
          go emptyVarEnv uc (expandTypeHelper fl expr)

        compileTableCall fl callee expr args = do
          guard (callee /= owner)
          callee_tc <- MaybeT (pure (resolveTable callee))
          let mb_exp_args = case mb_exp_rhs of
                Just (TyConApp exp_tc exp_as) | exp_tc == callee_tc -> Just exp_as
                _ -> Nothing
              step _ _ [] (_ : _) = MaybeT (pure Nothing)
              step _ acc _ [] = pure (reverse acc)
              step idx acc (Bndr _ (NamedTCB flag) : rbs) (Core.Type t : ras)
                | isInvisiblePiFlag flag =
                    step (idx + 1) ((False, [], const (specialize t)) : acc) rbs ras
              step idx acc (Bndr _ AnonTCB : rbs) (val_a : ras)
                | not (Core.isTyCoArg val_a) = do
                    compiled_a <- go fl False (peelEtaCall val_a) <|>
                                  compileInlinedCb idx mb_exp_args fl val_a
                    step (idx + 1) (compiled_a : acc) rbs ras
              step _ _ _ _ = MaybeT (pure Nothing)
          compiled_args <- step 0 [] (tyConBinders callee_tc) args
          let (s_flags, t_lists, fns) = unzip3 compiled_args
              res fam = mkTyConApp callee_tc [f fam | f <- fns]
          guard (typeKind (res header) `eqType` specialize (exprType expr))
          pure (or s_flags, callee : concat t_lists, res)

        compileInlinedCb idx (Just exp_args) fl val_a
          | TyConApp map_tc map_prefix : _ <- drop idx exp_args
          , isJust (closedFamilyTyConCoAxiom_maybe map_tc) = do
              let (cb_raw_bs, cb_body) = collectMixedBinders (betaReduceTypeApps val_a)
              cb_mixed <- MaybeT $ pure $ traverse (\b ->
                if isTyVar b then Just (Left (Specified, b))
                else if isValueVar b then Just (Right b) else Nothing) cb_raw_bs
              let cb_vals = [v | Right v <- cb_mixed]
                  cb_pats0 = [(v, IndexedVariable v) | v <- cb_vals]
              guard (not (null cb_vals))
              cb_rows <- descendTree False [] Nothing cb_pats0 proofs cb_body
              guard (not (null cb_rows))
              for_ cb_rows $ \case
                (cb_row_pats, cb_all_proofs, Left cb_leaf) -> do
                  let cb_eqs = map CoVarCo cb_all_proofs
                  cb_sub <- MaybeT $ pure $ tcUnifyTys alwaysBindTv
                              (map coercionLKind cb_eqs) (map coercionRKind cb_eqs)
                  let cb_spec = substTyUnchecked cb_sub
                  (cb_suffix, cb_inner_env) <- buildRowLhs cb_spec cb_mixed cb_row_pats
                  let merged_env = cb_inner_env ++ [(u, cb_spec t) | (u, t) <- val_env]
                  (c_self, _, c_fn) <- compileExpr header out_ty False [] [] Nothing
                                         cb_row_pats fl merged_env cb_spec cb_all_proofs Nothing False cb_leaf
                  guard (not c_self)
                  let reduced = reductionReducedType $
                        normaliseTcApp emptyFamInstEnvs Nominal map_tc (map cb_spec map_prefix ++ cb_suffix)
                  guard (reduced `eqType` c_fn header)
                _ -> MaybeT (pure Nothing)
              pure (False, [], const (mkTyConApp map_tc map_prefix))
        compileInlinedCb _ _ _ _ = MaybeT (pure Nothing)

        compileSelf uc callee expr args = case mb_pack of
          Just (package, outer_pat, _) | input_v : _ <- val_params -> do
            guard (uc && is_prenex)
            IndexedConstructor _ _ _ original_fields <- pure outer_pat
            [(_, IndexedConstructor _ _ _ cur_pat_fields)] <- pure row_pats
            let (prefix, suffix) = splitAt (length prenex_tvs) args
            tys <- MaybeT (pure (traverse (fmap specialize . indexedTypeArg) prefix))
            guard (length tys == length prenex_tvs)
            let kind = substTyWith prenex_tvs tys (varType input_v)
                n_exs = length (DC.dataConExTyCoVars package)
            (mb_supplied, actual_fields, worker_tys) <-
              if callee == owner
                then do
                  [operand] <- pure suffix
                  guard (specialize (exprType operand) `eqType` kind)
                  (Core.Var builder, operands) <- MaybeT (pure (Just (Core.collectArgs operand)))
                  (con, supplied, values) <- MaybeT (pure (indexedConstructor specialize builder operands))
                  guard (con == package)
                  pure (Just supplied, values, [])
                else do
                  let (locals, values) = splitAt n_exs suffix
                  erased <- MaybeT (pure (traverse (fmap specialize . indexedTypeArg) locals))
                  guard (length erased == n_exs)
                  pure (Nothing, values, erased)
            actual_vs <- MaybeT (pure (traverse (\case { Core.Var v -> Just v; _ -> Nothing }) actual_fields))
            guard (length original_fields == length cur_pat_fields &&
                   oneSmaller cur_pat_fields actual_vs)
            values <- MaybeT (pure (traverse (`lookup` val_env) actual_vs))
            source <- case mb_supplied of
              Just written -> pure written
              Nothing -> do
                (_, univ_args) <- MaybeT (pure (splitTyConApp_maybe kind))
                let univs = DC.dataConUnivTyVars package
                    all_vs = univs ++ DC.dataConExTyCoVars package
                guard (length univ_args == length univs)
                MaybeT (pure (traverse (`lookup` zip all_vs (univ_args ++ worker_tys)) (DC.dataConUserTyVars package)))
            packed <- MaybeT (pure (checkedPromoted package kind source values))
            pure (True, [], \fam -> mkTyConApp fam (tys ++ [packed]))
          _ -> do
            guard is_prenex
            let (prefix, suffix) = splitAt (length prenex_tvs) args
            tys <- MaybeT (pure (traverse (fmap specialize . indexedTypeArg) prefix))
            guard (length tys == length prenex_tvs && length suffix == length val_params)
            let checkParam (param_v, arg_e) = do
                  Core.Var actual_v <- unwrapValidCast arg_e
                  pat <- lookup param_v row_pats
                  case pat of
                    IndexedVariable u -> do
                      guard (actual_v == u)
                      (False,) <$> lookup actual_v val_env
                    IndexedConstructor{} -> do
                      guard (actual_v `elem` indexedFields pat)
                      (True,) <$> lookup actual_v val_env
            checked_params <- MaybeT (pure (traverse checkParam (zip val_params suffix)))
            let (decreased_flags, arg_tys) = unzip checked_params
                expected_doms = map (substTyWith prenex_tvs tys . varType) val_params
                expected_res = substTyWith prenex_tvs tys out_ty
            guard (or decreased_flags &&
                   eqTypes (map typeKind arg_tys) expected_doms &&
                   specialize (exprType expr) `eqType` expected_res)
            pure (True, [], \fam -> mkTyConApp fam (tys ++ arg_tys))

        oneSmaller pat_fs actual_vs
          | length pat_fs == length actual_vs
          , Just changes <- zipWithM cmpField pat_fs actual_vs
          = length (filter id changes) == 1
          | otherwise = False

        cmpField (IndexedVariable old) new = False <$ guard (old == new)
        cmpField tree@IndexedConstructor{} new = True <$ guard (new `elem` indexedFields tree)

    certifiedShape has_self all_callees bndrs val_params out_ty raw_rows eq_pairs header
      | has_self = True
      | any (isIndexedCallbackTy . varType) val_params =
          case splitTyConApp_maybe out_ty of
            Just (_, TyVarTy _ : _) -> True
            _                       -> False
      | not (null all_callees) = length val_params >= 2
      | [v] <- val_params, [(eq, Nothing)] <- eq_pairs =
          let all_tvs = [tv | Left (_, tv) <- bndrs]
          in not (null all_tvs) && length all_tvs <= 3 &&
             mkVarSet all_tvs `subVarSet` tyCoVarsOfTypes [varType v, out_ty] &&
             isScopeConAlias (varType v) out_ty eq header
      | length val_params >= 2 =
          isScopeSubstMap val_params out_ty raw_rows eq_pairs header
      | otherwise = False

    isScopeConAlias dom_ty cod_ty eq header = isJust $ do
      guard (isIndexedAlgDomain dom_ty && isIndexedAlgDomain cod_ty)
      (dom_tc, dom_args) <- splitTyConApp_maybe dom_ty
      (cod_tc, cod_args) <- splitTyConApp_maybe cod_ty
      guard (length dom_args == length cod_args && length dom_args <= 2)
      let valid_weaken = dom_tc == cod_tc && case (dom_args, cod_args) of
            ([d0], [c0]) | Just (ext_tc, [inner]) <- splitTyConApp_maybe c0 ->
              isPromotedDataCon ext_tc && inner `eqType` d0
            ([d0, s0], [c0, s1]) | Just (ext_tc, [inner, _]) <- splitTyConApp_maybe c0 ->
              isPromotedDataCon ext_tc && inner `eqType` d0 && s0 `eqType` s1
            _ -> False
          valid_var = dom_tc /= cod_tc && eqTypes dom_args cod_args
      guard (valid_weaken || valid_var)
      (prom_tc, rhs_args@(_ : _)) <- splitTyConApp_maybe (coAxBranchRHS (applyRow eq header))
      dc <- isPromotedDataCon_maybe prom_tc
      (_, [last_arg]) <- pure (splitAt (length rhs_args - 1) rhs_args)
      guard (DC.dataConTyCon dc == cod_tc &&
             DC.dataConSourceArity dc == 1 &&
             isJust (getTyVar_maybe last_arg))

    isScopeSubstMap val_params out_ty raw_rows eq_pairs header = isJust $ do
      let (cap_vars, rest_params) = splitAt (length val_params - 1) val_params
          k = length cap_vars
      [idx_var] <- pure rest_params
      guard (k >= 1 && length raw_rows == k + 1 && length eq_pairs == k + 1)
      (term_tc, out_ctx : out_rest) <- splitTyConApp_maybe out_ty
      (idx_tc, idx_ctx : idx_rest) <- splitTyConApp_maybe (varType idx_var)
      guard (isAlgTyCon term_tc && isAlgTyCon idx_tc &&
             length out_rest <= 1 && eqTypes idx_rest out_rest)
      base_ctx <- peelCtxSteps k idx_ctx
      guard (base_ctx `eqType` out_ctx)
      for_ cap_vars $ \cv -> do
        (ct, c_ctx : _) <- splitTyConApp_maybe (varType cv)
        guard (ct == term_tc && c_ctx `eqType` out_ctx)
      let (hit_raw, _) = splitAt k raw_rows
      sel_vs <- traverse (\(_, pfs, l_or_r) -> case l_or_r of
        Left e | Just (Core.Var sv) <- branchBody pfs e -> Just sv
        _ -> Nothing) hit_raw
      guard (mkVarSet sel_vs == mkVarSet cap_vars)
      (_, [(miss_eq, Nothing)]) <- pure (splitAt k eq_pairs)
      (prom_tc, rhs_args@(_ : _)) <- splitTyConApp_maybe (coAxBranchRHS (applyRow miss_eq header))
      dc <- isPromotedDataCon_maybe prom_tc
      (_, [last_arg]) <- pure (splitAt (length rhs_args - 1) rhs_args)
      guard (DC.dataConTyCon dc == term_tc &&
             DC.dataConSourceArity dc == 1 &&
             isJust (getTyVar_maybe last_arg))

    peelCtxSteps 0 ctx = Just ctx
    peelCtxSteps d ctx = do
      (ext_tc, inner : _) <- splitTyConApp_maybe ctx
      guard (isPromotedDataCon ext_tc)
      peelCtxSteps (d - 1) inner

sameCompiledIndexed :: TyCon -> CompiledIndexed -> CompiledIndexed -> MaybeT TcM ()
sameCompiledIndexed header orig fin = do
    let orig_rows = ci_rows orig
        fin_rows = ci_rows fin
    guard (length orig_rows == length fin_rows)
    zipWithM_ (\(o_eq, o_c) (f_eq, f_c) -> do
      sameIndexedRow (applyRow o_eq header) (applyRow f_eq header)
      case (o_c, f_c) of
        (Nothing, Nothing) -> pure ()
        (Just (ltc, lbr), Just (rtc, rbr)) -> do
          guard (tyConKind ltc `eqType` tyConKind rtc)
          sameIndexedRow lbr rbr
        _ -> MaybeT (pure Nothing)) orig_rows fin_rows

publishCompiledIndexedM :: Bool -> (TyCon -> Bool) -> Id -> CompiledIndexed -> Maybe CompiledIndexed -> [Id]
                        -> TcM (Maybe (Id, [TyCon]))
publishCompiledIndexedM is_early public owner orig mb_fin helpers = runMaybeT $ do
    root_name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (mkClosureTableAxiom is_early root_name)
    let make = mkFamily root_name axiom (ci_binders orig) (ci_res_kind orig)
        header = make []
    for_ mb_fin (sameCompiledIndexed header orig)
    let branches = [applyRow eq table | (eq, _) <- ci_rows orig]
        table = make branches
        companions = [tc | (_, Just (tc, _)) <- ci_rows orig]
        families = table : companions
        local_helpers = nameSetElemsStable $ mkNameSet
          [ idName h
          | h <- helpers
          , idName h /= idName owner
          , nameModule_maybe (idName h) == nameModule_maybe (idName owner) ]
        details = case local_helpers of
          []  -> SourceTableId (idName owner) table NominalSourceTable
          nms -> CheckedClosureSourceTableId (idName owner) table nms
    guard (all validBranch branches &&
           all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
    pure (setIdDetails owner details, families)

-- ---------------------------------------------------------------------------
-- Unified unary & binary scope-indexed closures (SC3 + SC4)
-- ---------------------------------------------------------------------------

data CallbackLift = NamedLift Id | InlinedRename | InlinedSubst Id

-- | Certified scope-indexed closure shape parameterized by `ss_sort` (`Just sort`
-- for binary `Term g a` / `Lookup g a`, `Nothing` for unary `UTerm n` / `Fin n`).
data ScopeShape = ScopeShape
  { ss_owner     :: Id
  , ss_arity     :: Int
  , ss_context   :: Kind
  , ss_sort      :: Maybe Kind
  , ss_term      :: TyCon
  , ss_ext       :: TyCon
  , ss_index     :: TyCon
  , ss_ren_ops   :: (Id, Id)
  , ss_subst_ops :: Maybe (Id, Id, Maybe Id)
  , ss_var       :: DC.DataCon
  , ss_hit       :: DC.DataCon
  , ss_miss      :: DC.DataCon
  , ss_recipes   :: [(DC.DataCon, [Int])]
  }

sameScopeShape :: ScopeShape -> ScopeShape -> Bool
sameScopeShape a b =
    ss_arity a == ss_arity b &&
    ss_context a `eqType` ss_context b &&
    sameSort (ss_sort a) (ss_sort b) &&
    ss_term a == ss_term b &&
    ss_ext a == ss_ext b &&
    ss_index a == ss_index b &&
    ss_var a == ss_var b &&
    ss_hit a == ss_hit b &&
    ss_miss a == ss_miss b &&
    ss_recipes a == ss_recipes b
  where
    sameSort Nothing Nothing     = True
    sameSort (Just x) (Just y)   = x `eqType` y
    sameSort _ _                 = False

-- | Certify an inline-callback single or multi-binder substitution (`replace`,
-- `replaceBoth`, `substSingle`, `substSingleU`) in unary or binary scope form.
scopeFamilyInputs :: BindEnv -> Id -> CoreExpr -> Maybe ScopeShape
scopeFamilyInputs bindings owner rhs = do
    let (sig_tvs, sig_tau) = splitForAllTyVars (varType owner)
    (_, Scaled _ sig_body_ty : _ : _, _) <- pure (splitFunTys sig_tau)
    (sig_term, sig_ctx : sig_rest) <- splitTyConApp_maybe sig_body_ty
    (sig_ext, sig_ext_args) <- splitTyConApp_maybe sig_ctx
    is_typed <- case (sig_rest, length sig_ext_args) of
      ([_], 2) -> True  <$ guard (length sig_tvs >= 3)
      ([],  1) -> False <$ guard (length sig_tvs == 1 && exprType rhs `eqType` varType owner)
      _        -> Nothing
    guard (isAlgTyCon sig_term && isPromotedDataCon sig_ext)
    (context : rest_tvs, body : captures, raw_call) <- function rhs
    let call = unwrapLocalCallbackLets raw_call
    guard (not (null captures))
    (Core.Var traversal, [raw_callback, Core.Var operand]) <- application call
    guard (operand == body)
    (cb_tvs, [argument], lookup_body) <- function (betaReduceTypeApps raw_callback)
    let context_kind = varType context
    (context_tc, []) <- splitTyConApp_maybe context_kind
    guard (isAlgTyCon context_tc)
    (mb_sort, term, ext, index) <- if is_typed
      then do
        result : rev_sorts <- pure (reverse rest_tvs)
        [callback_sort] <- pure cb_tvs
        let sorts = reverse rev_sorts
            sort_kind = varType result
        (sort_tc, []) <- splitTyConApp_maybe sort_kind
        guard (isAlgTyCon sort_tc &&
               length captures == length sorts &&
               all (eqType sort_kind . varType) (callback_sort : sorts))
        (t_tc, [body_ctx, TyVarTy body_sort]) <- splitTyConApp_maybe (varType body)
        (e_tc, [_, _]) <- splitTyConApp_maybe body_ctx
        (i_tc, [index_ctx, TyVarTy index_sort]) <- splitTyConApp_maybe (varType argument)
        let ctx_ty = mkTyVarTy context
            extend k s = mkTyConApp e_tc [k, mkTyVarTy s]
            at s = mkTyConApp t_tc [ctx_ty, mkTyVarTy s]
        guard (isPromotedDataCon e_tc && all isAlgTyCon [t_tc, i_tc] &&
               foldl' extend ctx_ty sorts `eqType` body_ctx && body_sort == result &&
               and (zipWith (\cap s -> varType cap `eqType` at s) captures sorts) &&
               index_ctx `eqType` body_ctx && index_sort == callback_sort)
        pure (Just sort_kind, t_tc, e_tc, i_tc)
      else do
        [] <- pure rest_tvs
        [] <- pure cb_tvs
        (t_tc, [body_ctx]) <- splitTyConApp_maybe (varType body)
        (e_tc, [_]) <- splitTyConApp_maybe body_ctx
        (i_tc, [index_ctx]) <- splitTyConApp_maybe (varType argument)
        let ctx_ty = mkTyVarTy context
            term_at_ctx = mkTyConApp t_tc [ctx_ty]
        guard (isPromotedDataCon e_tc && all isAlgTyCon [t_tc, i_tc] &&
               foldl' (\k _ -> mkTyConApp e_tc [k]) ctx_ty captures `eqType` body_ctx &&
               all (\cap -> varType cap `eqType` term_at_ctx) captures &&
               index_ctx `eqType` body_ctx &&
               exprType call `eqType` term_at_ctx)
        pure (Nothing, t_tc, e_tc, i_tc)
    [(first, _, _), (second, _, _)] <- caseOn argument lookup_body
    let index_cons = tyConDataCons index
    guard (first /= second && length index_cons == 2 &&
           first `elem` index_cons && second `elem` index_cons &&
           all lazyConstructor index_cons)
    (hit, miss, var) <-
        outcomes first second [] argument (reverse captures) lookup_body <|>
        outcomes second first [] argument (reverse captures) lookup_body
    guard (DC.dataConTyCon var == term)
    traversal_rhs <- lookupVarEnv bindings traversal
    (trav_tvs, [sigma, scrutinee], traversal_case) <- function traversal_rhs
    guard (length trav_tvs == (if is_typed then 3 else 2))
    rows <- caseOn scrutinee traversal_case
    _ <- findTraversalVarCon True sigma rows [var]
    (ren_ops, subst_ops, recipes) <- checkedScopeTraversals is_typed bindings traversal sigma var hit miss rows
    pure ScopeShape
      { ss_owner     = owner
      , ss_arity     = length captures
      , ss_context   = context_kind
      , ss_sort      = mb_sort
      , ss_term      = term
      , ss_ext       = ext
      , ss_index     = index
      , ss_ren_ops   = ren_ops
      , ss_subst_ops = Just subst_ops
      , ss_var       = var
      , ss_hit       = hit
      , ss_miss      = miss
      , ss_recipes   = recipes
      }

outcomes :: DC.DataCon -> DC.DataCon -> [Var] -> Id -> [Id] -> CoreExpr
         -> Maybe (DC.DataCon, DC.DataCon, DC.DataCon)
outcomes hit miss scope argument captures expr = do
    body <- branchBody scope expr
    case captures of
      [] -> do
        (con, [Core.Var x]) <- constructorCall body
        guard (x == argument)
        pure (hit, miss, con)
      capture : rest -> do
        rows <- caseOn argument body
        [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
        [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
        guard (length rows == 2 && all (not . isValueVar) hit_bs)
        Core.Var selected <- branchBody (scope ++ hit_bs) hit_rhs
        guard (selected == capture)
        [older] <- pure (filter isValueVar miss_bs)
        outcomes hit miss (scope ++ miss_bs) older rest miss_rhs

findPromotedExtTyCon :: Int -> TyCon -> Maybe TyCon
findPromotedExtTyCon arity term =
    case [ ext_tc
         | con <- tyConDataCons term
         , let (_, _, _, _, fs, _) = DC.dataConFullSig con
         , f <- fs
         , Just (t, ctx : rest_t) <- [splitTyConApp_maybe (scaledThing f)]
         , t == term && length rest_t + 1 == arity
         , Just (ext_tc, ext_args) <- [splitTyConApp_maybe ctx]
         , length ext_args == arity && isPromotedDataCon ext_tc ] of
      tc : _ -> Just tc
      []     -> Nothing

findTraversalVarCon :: Bool -> Id -> [(DC.DataCon, [Var], CoreExpr)] -> [DC.DataCon] -> Maybe DC.DataCon
findTraversalVarCon is_subst cb rows =
    find $ \con -> case [(bs, e) | (dc, bs, e) <- rows, dc == con] of
      [(bs, e)]
        | [field] <- filter isValueVar bs
        , Just checked <- branchBody bs e ->
            if is_subst
              then case application checked of
                     Just (Core.Var use, [Core.Var x]) -> use == cb && x == field
                     _ -> False
              else case constructorCall checked of
                     Just (vh, [renamed])
                       | vh == con
                       , Just (Core.Var use, [Core.Var x]) <- application renamed
                       -> use == cb && x == field
                     _ -> False
      _ -> False

-- | Recover `ScopeShape`s directly from `subst`/`rename` or `substU`/`renameU`.
directScopeShapes :: BindEnv -> [Id] -> [ScopeShape]
directScopeShapes bindings candidates =
    concatMap (\is_t -> mapMaybe (fromSubst is_t) candidates ++ mapMaybe (fromRename is_t) candidates)
              [True, False]
  where
    scopeHeader is_typed op_id = do
      rhs <- lookupVarEnv bindings op_id
      (tvs, [cb, operand], body) <- function rhs
      mb_sort <- if is_typed
        then do
          [g_tv, _, a_tv] <- pure tvs
          (c_tc, []) <- splitTyConApp_maybe (varType g_tv)
          (s_tc, []) <- splitTyConApp_maybe (varType a_tv)
          guard (all isAlgTyCon [c_tc, s_tc])
          pure (Just (varType a_tv))
        else do
          [g_tv, _] <- pure tvs
          (c_tc, []) <- splitTyConApp_maybe (varType g_tv)
          guard (isAlgTyCon c_tc)
          pure Nothing
      g_tv : _ <- pure tvs
      let arity = if is_typed then 2 else 1
      (term, t_args) <- splitTyConApp_maybe (varType operand)
      let (_, cb_tau) = splitForAllTyVars (varType cb)
      (_, [Scaled _ idx_ty], _) <- pure (splitFunTys cb_tau)
      (index, i_args) <- splitTyConApp_maybe idx_ty
      guard (length t_args == arity && length i_args == arity && all isAlgTyCon [term, index])
      [c1, c2] <- pure (tyConDataCons index)
      rows <- caseOn operand body
      ext_con <- findPromotedExtTyCon arity term
      pure (varType g_tv, mb_sort, term, ext_con, index, cb, rows, c1, c2)

    fromSubst is_typed subst_id = do
      (ctx_k, mb_sort, term, ext_con, index, sigma, rows, c1, c2) <- scopeHeader is_typed subst_id
      var <- findTraversalVarCon True sigma rows (tyConDataCons term)
      let tryPair hit miss = do
            (ren_ops, subst_ops, recipes) <- checkedScopeTraversals is_typed bindings subst_id sigma var hit miss rows
            pure ScopeShape
              { ss_owner = subst_id, ss_arity = 1, ss_context = ctx_k, ss_sort = mb_sort
              , ss_term = term, ss_ext = ext_con, ss_index = index
              , ss_ren_ops = ren_ops, ss_subst_ops = Just subst_ops
              , ss_var = var, ss_hit = hit, ss_miss = miss, ss_recipes = recipes }
      tryPair c1 c2 <|> tryPair c2 c1

    fromRename is_typed rename_id = do
      (ctx_k, mb_sort, term, ext_con, index, rho, rows, c1, c2) <- scopeHeader is_typed rename_id
      let constructors = tyConDataCons term
      guard (length rows == length constructors && all lazyConstructor constructors)
      var <- findTraversalVarCon False rho rows constructors
      let tryRen hit miss = do
            (recipes, ext_id) <- checkedRenameOnlyRows is_typed bindings var hit miss rename_id rho rows
            let subst_ops = if is_typed then Just (rename_id, ext_id, Nothing) else Nothing
            pure ScopeShape
              { ss_owner = rename_id, ss_arity = 1, ss_context = ctx_k, ss_sort = mb_sort
              , ss_term = term, ss_ext = ext_con, ss_index = index
              , ss_ren_ops = (rename_id, ext_id), ss_subst_ops = subst_ops
              , ss_var = var, ss_hit = hit, ss_miss = miss, ss_recipes = recipes }
      tryRen c1 c2 <|> tryRen c2 c1

checkedRenameOnlyRows :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> DC.DataCon
                      -> Id -> Id -> [(DC.DataCon, [Var], CoreExpr)]
                      -> Maybe ([(DC.DataCon, [Int])], Id)
checkedRenameOnlyRows is_typed bindings var hit miss rename_id rho rows = do
    lifted <- traverse (checkedScopeRow is_typed bindings var hit miss rename_id rho)
                       [row | row@(con, _, _) <- rows, con /= var]
    let (recipes, exts) = unzip lifted
        steps = concatMap concat exts
        isRenLift (NamedLift _)    = True
        isRenLift InlinedRename    = True
        isRenLift (InlinedSubst _) = False
    guard (all isRenLift steps)
    ext_id <- case [v | NamedLift v <- steps] of
      v : vs | all (== v) vs -> Just v
      _ -> Nothing
    ext_rhs <- lookupVarEnv bindings ext_id
    checkScopeRenameExtension is_typed bindings hit miss ext_rhs
    pure (recipes, ext_id)

checkedScopeTraversals :: Bool -> BindEnv
                       -> Id -> Id
                       -> DC.DataCon -> DC.DataCon -> DC.DataCon
                       -> [(DC.DataCon, [Var], CoreExpr)]
                       -> Maybe ((Id, Id), (Id, Id, Maybe Id), [(DC.DataCon, [Int])])
checkedScopeTraversals is_typed bindings subst sigma var hit miss rows = do
    let constructors = tyConDataCons (DC.dataConTyCon var)
    guard (complete constructors rows)
    lifted <- traverse (checkedScopeRow is_typed bindings var hit miss subst sigma)
                       [row | row@(con, _, _) <- rows, con /= var]
    let (recipes, used_extenders) = unzip lifted
        subst_steps = concatMap concat used_extenders
    extender <- allSame [v | NamedLift v <- subst_steps]
    extender_rhs <- lookupVarEnv bindings extender
    (rename, weakening) <- checkedScopeExtension is_typed bindings var hit miss extender_rhs
    guard (all (isSubstLift rename) subst_steps)
    rename_rhs <- lookupVarEnv bindings rename
    (tvs, [rho, operand], renaming) <- function rename_rhs
    guard (length tvs == (if is_typed then 3 else 2))
    rename_rows <- caseOn operand renaming
    guard (complete constructors rename_rows)
    _ <- findTraversalVarCon False rho rename_rows [var]
    renamed_rows <- traverse (checkedScopeRow is_typed bindings var hit miss rename rho)
                             [row | row@(con, _, _) <- rename_rows, con /= var]
    let (rename_recipes, rename_extenders) = unzip renamed_rows
    guard (sortOn (nameStableString . DC.dataConName . fst) recipes ==
           sortOn (nameStableString . DC.dataConName . fst) rename_recipes)
    let rename_steps = concatMap concat rename_extenders
    guard (all isRenameLift rename_steps)
    extend_rename <- allSame [v | NamedLift v <- rename_steps]
    extend_rename_rhs <- lookupVarEnv bindings extend_rename
    checkScopeRenameExtension is_typed bindings hit miss extend_rename_rhs
    pure ((rename, extend_rename), (subst, extender, weakening), recipes)
  where
    complete constructors alts =
      let heads = [con | (con, _, _) <- alts]
      in length heads == length constructors && all (`elem` heads) constructors &&
         all lazyConstructor constructors

    isSubstLift _ (NamedLift _)      = True
    isSubstLift rename (InlinedSubst v) = rename == v
    isSubstLift _ InlinedRename      = False

    isRenameLift (NamedLift _)    = True
    isRenameLift InlinedRename    = True
    isRenameLift (InlinedSubst _) = False

    allSame []       = Nothing
    allSame (v : vs) = v <$ guard (all (== v) vs)

checkedScopeRow :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> DC.DataCon -> Id -> Id
                -> (DC.DataCon, [Var], CoreExpr)
                -> Maybe ((DC.DataCon, [Int]), [[CallbackLift]])
checkedScopeRow is_typed bindings var hit miss traversal callback (con, binders, rhs) = do
    result <- branchBody binders rhs
    (rebuilt, fields) <- constructorCall result
    let originals = filter isValueVar binders
    guard (rebuilt == con && length fields == length originals)
    owners <- zipWithM recursiveField originals fields
    pure ((con, map length owners), owners)
  where
    recursiveField original field = do
        (Core.Var owner, [captured, Core.Var subterm]) <- application field
        guard (traversal == owner && original == subterm &&
               isLiftedTypeKind (typeKind (varType original)))
        lifts captured

    lifts (Core.Var original) | callback == original = Just []
    lifts captured = named captured <|> inlined captured

    named captured
      | is_typed  = eta_lifted <|> direct_lifted
      | otherwise = case Core.collectArgs captured of
          (Core.Var extender, [Core.Type _, Core.Type _, inner]) ->
            (NamedLift extender :) <$> lifts inner
          _ -> Nothing
      where
        eta_lifted = do
          ([index], [], closure) <- function captured
          case Core.collectArgs closure of
            (Core.Var original, [Core.Type (TyVarTy rest)])
              | callback == original && index == rest -> pure []
            (Core.Var extender, [Core.Type _, Core.Type _, Core.Type _, original,
                                 Core.Type (TyVarTy rest)])
              | index == rest -> (NamedLift extender :) <$> lifts original
            (Core.Var extender, [Core.Type _, Core.Type _, Core.Type _,
                                 Core.Type (TyVarTy rest), original])
              | index == rest -> (NamedLift extender :) <$> lifts original
            _ -> Nothing
        direct_lifted = case Core.collectArgs captured of
          (Core.Var extender, [Core.Type _, Core.Type _, Core.Type _, original]) ->
            (NamedLift extender :) <$> lifts original
          _ -> Nothing

    inlined captured = do
        (tvs, [argument], body) <- function captured
        guard (length tvs == (if is_typed then 1 else 0))
        rows <- caseOn argument body
        [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
        [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
        guard (length rows == 2 && all (not . isValueVar) hit_bs)
        fresh <- expandTypeHelper bindings <$> branchBody hit_bs hit_rhs
        forwarded <- branchBody miss_bs miss_rhs
        (lift_step, see_older) <- inlineStep fresh forwarded
        [older] <- pure (filter isValueVar miss_bs)
        if is_typed
          then do
            [index] <- pure tvs
            let (head_expr, args) = Core.collectArgs see_older
            arg : preceding <- pure (reverse args)
            Core.Var input <- branchBody miss_bs arg
            guard (input == older)
            (lift_step :) <$> lifts (Core.Lam index (foldl' Core.App head_expr (reverse preceding)))
          else do
            (Core.Var fn, [arg]) <- application see_older
            Core.Var input <- branchBody miss_bs arg
            guard (input == older)
            (lift_step :) <$> lifts (Core.Var fn)

    inlineStep fresh forwarded = do
        (fresh_con, fields) <- constructorCall fresh
        case fields of
          [] -> do
            (forward_con, [lookup_older]) <- constructorCall forwarded
            guard (fresh_con == hit && forward_con == miss)
            pure (InlinedRename, lookup_older)
          [v] -> do
            (fresh_index, []) <- constructorCall v
            guard (fresh_con == var && fresh_index == hit)
            (Core.Var rename, [weakening, lookup_older]) <- application forwarded
            _ <- checkedScopeWeakening is_typed bindings miss weakening
            pure (InlinedSubst rename, lookup_older)
          _ -> Nothing

peelScopeExtensionBinders :: Bool -> CoreExpr -> Maybe (Id, Id, CoreExpr)
peelScopeExtensionBinders True rhs = do
    let (binders, body) = collect [] rhs
    case binders of
      [t1, t2, t3, capture, t4, argument]
        | all isTyVar [t1, t2, t3, t4] && all isValueVar [capture, argument]
        -> pure (capture, argument, body)
      [t1, t2, t3, t4, capture, argument]
        | all isTyVar [t1, t2, t3, t4] && all isValueVar [capture, argument]
        -> pure (capture, argument, body)
      _ -> Nothing
  where
    collect acc (Core.Lam b e) = collect (b : acc) e
    collect acc (Core.Let (Core.NonRec b _) e)
      | isLocalId b
      , isLiftedTypeKind (typeKind (varType b))
      , not (b `elemVarSet` exprFreeVars e)
      = collect acc e
    collect acc e = (reverse acc, e)
peelScopeExtensionBinders False rhs = do
    ([m, n], [capture, argument], body) <- function rhs
    guard (all isTyVar [m, n] && all isValueVar [capture, argument])
    pure (capture, argument, body)

checkScopeRenameExtension :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> CoreExpr -> Maybe ()
checkScopeRenameExtension is_typed bindings hit miss rhs = do
    (capture, argument, body) <- peelScopeExtensionBinders is_typed rhs
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs &&
           lazyConstructor hit && lazyConstructor miss)
    raw_hit <- branchBody hit_bs hit_rhs
    let hit_body = if is_typed then raw_hit else expandTypeHelper bindings raw_hit
    (fresh, []) <- constructorCall hit_body
    guard (fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (successor, [lookup_older]) <- constructorCall miss_body
    guard (successor == miss)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)

checkedScopeWeakening :: Bool -> BindEnv -> DC.DataCon -> CoreExpr -> Maybe (Maybe Id)
checkedScopeWeakening is_typed bindings miss raw = do
    unwrapped <- weakeningBody raw
    let direct = do
          (tvs, inputs, body) <- function unwrapped
          (shift, values) <- constructorCall body
          guard (length tvs <= (if is_typed then 1 else 0) &&
                 length inputs <= 1 && shift == miss &&
                 length inputs == length values)
          zipWithM_ (\i val -> case val of { Core.Var y -> guard (y == i); _ -> Nothing }) inputs values
          pure Nothing
        named = do
          (tvs, [], partial) <- function unwrapped
          let target = if null tvs && not is_typed then unwrapped else partial
          (Core.Var helper, args) <- pure (Core.collectArgs target)
          tys <- traverse indexedTypeArg args
          decl <- lookupVarEnv bindings helper
          con <- validConstructorAliasBody decl
          let (htvs, _) = splitForAllTyVars (varType helper)
          guard (length tvs <= 1 && con == miss)
          if is_typed
            then Just helper <$ guard (length tys == 2 + length tvs && length htvs == 3)
            else do
              guard (length htvs == length tys)
              pure $ if isExternalName (idName helper) && length htvs == 1
                       then Just helper
                       else Nothing
    direct <|> named

checkedScopeExtension :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> DC.DataCon
                      -> CoreExpr -> Maybe (Id, Maybe Id)
checkedScopeExtension is_typed bindings var hit miss rhs = do
    (capture, argument, body) <- peelScopeExtensionBinders is_typed rhs
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs)
    hit_body <- expandTypeHelper bindings <$> branchBody hit_bs hit_rhs
    (term, [v]) <- constructorCall hit_body
    (fresh, []) <- constructorCall v
    guard (term == var && fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (Core.Var rename, [weakening, lookup_older]) <- application miss_body
    _ <- lookupVarEnv bindings rename
    chosen <- checkedScopeWeakening is_typed bindings miss weakening
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)
    pure (rename, chosen)

-- ---------------------------------------------------------------------------
-- Closure family & traversal publication (unary & binary)
-- ---------------------------------------------------------------------------

closureCallbackKind :: TyCon -> TyCon -> Maybe TyVar -> Type -> Type -> Kind
closureCallbackKind index out (Just rank) g d =
    mkSpecForAllTys [(unmatchableDataConTy, rank)] $
      mkVisFunTyManyU (mkTyConApp index [g, mkTyVarTy rank]) (mkTyConApp out [d, mkTyVarTy rank])
closureCallbackKind index out Nothing g d =
    mkVisFunTyManyU (mkTyConApp index [g]) (mkTyConApp out [d])

publishWeakeningFamily :: Bool -> (TyCon -> Bool) -> (Id, ScopeShape) -> TcM (Maybe (Id, [TyCon]))
publishWeakeningFamily is_early public (owner, shape) = runMaybeT $ do
    g <- lift (freshVar "g" (ss_context shape))
    (b_vars, a_vars) <- case ss_sort shape of
      Just sort -> do
        b <- lift (freshVar "b" sort)
        a <- lift (freshVar "a" sort)
        pure ([b], [a])
      Nothing -> pure ([], [])
    let gk = mkTyVarTy g
        b_tys = mkTyVarTys b_vars
        a_tys = mkTyVarTys a_vars
        input = mkTyConApp (ss_index shape) (gk : a_tys)
        result = mkTyConApp (ss_index shape) (mkTyConApp (ss_ext shape) (gk : b_tys) : a_tys)
    x <- lift (freshVar "x" input)
    let named_vars = g : b_vars ++ a_vars
        binders = mkNamedTyConBinders Specified named_vars ++ [mkAnonTyConBinder x]
        branches = do
          output <- conAt (ss_miss shape) result [mkTyVarTy x]
          pure [renamingBranch (mkTyVarTys named_vars ++ [mkTyVarTy x]) output]
    publishRenamingTable is_early public owner [] binders result (const branches)

-- | Publish a rename extension (`Nothing`) or substitution extension (`Just (renaming, mb_weak)`).
publishScopeLookupExtension :: Bool -> (TyCon -> Bool) -> Id -> [Id] -> ScopeShape
                            -> Maybe (TyCon, Maybe TyCon) -> TcM (Maybe (Id, [TyCon]))
publishScopeLookupExtension is_early public owner helpers shape mb_subst = runMaybeT $ do
    let context = ss_context shape
        mb_sort = ss_sort shape
        term    = ss_term shape
        ext     = ss_ext shape
        index   = ss_index shape
        var     = ss_var shape
        hit     = ss_hit shape
        miss    = ss_miss shape
        output  = if isJust mb_subst then term else index
    (mb_ren_weak, extra_tcs) <- case mb_subst of
      Nothing -> pure (Nothing, [])
      Just (ren_tc, Just w_tc) -> pure (Just (ren_tc, w_tc), [])
      Just (ren_tc, Nothing) | isNothing mb_sort -> do
        w_name <- lift (newImplicitBinder (idName owner) (mkTcOcc . ("$weaken:" ++) . occNameString))
        w_axiom <- lift (newImplicitBinder w_name mkInstTyCoOcc)
        wg <- lift (freshVar "g" context)
        let wgk = mkTyVarTy wg
            w_in = mkTyConApp index [wgk]
            w_out = mkTyConApp index [mkTyConApp ext [wgk]]
        wx <- lift (freshVar "x" w_in)
        w_rhs <- MaybeT (pure (conAt miss w_out [mkTyVarTy wx]))
        let w_br = renamingBranch [wgk, mkTyVarTy wx] w_rhs
            w_binders = mkNamedTyConBinders Specified [wg] ++ [mkAnonTyConBinder wx]
            w_tc = mkFamily w_name w_axiom w_binders w_out [w_br]
        guard (validBranch w_br && all public (sourceEquationDatatypes [w_tc]))
        pure (Just (ren_tc, w_tc), [w_tc])
      Just _ -> MaybeT (pure Nothing)
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    (b_vars, a_vars, mb_rank, cb_occ) <- case mb_sort of
      Just sort -> do
        b <- lift (freshVar "binder" sort)
        a <- lift (freshVar "a" sort)
        rank <- lift (freshVar "sort" sort)
        pure ([b], [a], Just rank, "rho")
      Nothing -> pure ([], [], Nothing, "r")
    let gk = mkTyVarTy g
        dk = mkTyVarTy d
        b_tys = mkTyVarTys b_vars
        a_tys = mkTyVarTys a_vars
        lookupAt c ks = mkTyConApp index (c : ks)
        outputAt c ks = mkTyConApp output (c : ks)
        extend c = mkTyConApp ext (c : b_tys)
        domain = lookupAt (extend gk) a_tys
        result = outputAt (extend dk) a_tys
    rho <- lift (freshVar cb_occ (closureCallbackKind index output mb_rank gk dk))
    input <- lift (freshVar "input" domain)
    old <- lift (freshVar "index" (lookupAt gk a_tys))
    let r = mkTyVarTy rho
        x = mkTyVarTy old
        binders = mkNamedTyConBinders Specified ([g, d] ++ b_vars) ++
                  [mkAnonTyConBinder rho] ++
                  mkNamedTyConBinders Specified a_vars ++
                  [mkAnonTyConBinder input]
        lhsFor ks pat = [gk, dk] ++ b_tys ++ [r] ++ ks ++ [pat]
        branches = do
          fresh <- conAt hit (lookupAt (extend gk) b_tys) []
          new_index <- conAt hit (lookupAt (extend dk) b_tys) []
          previous <- conAt miss domain [x]
          let mapped = mkAppTys r (a_tys ++ [x])
          (fresh', previous') <- case mb_ren_weak of
            Nothing -> (new_index,) <$> conAt miss result [mapped]
            Just (rename_tc, weak_tc) -> do
              v <- conAt var (outputAt (extend dk) b_tys) [new_index]
              let shift = mkTyConApp weak_tc (dk : b_tys)
              guard (typeKind shift `eqType` closureCallbackKind index index mb_rank dk (extend dk))
              pure (v, mkTyConApp rename_tc ([dk, extend dk] ++ a_tys ++ [shift, mapped]))
          guard (typeKind fresh' `eqType` outputAt (extend dk) b_tys &&
                 typeKind previous' `eqType` result)
          pure [renamingBranch (lhsFor b_tys fresh) fresh',
                renamingBranch (lhsFor a_tys previous) previous']
    (new_owner, [table]) <- publishRenamingTable is_early public owner helpers binders result (const branches)
    pure (new_owner, table : extra_tcs)

publishTraversalFamily :: Bool -> Bool -> (TyCon -> Bool) -> [(Name, TyCon)]
                       -> (Id, (Id, ScopeShape), [Id])
                       -> TcM (Maybe (Id, [TyCon]))
publishTraversalFamily is_subst is_early public extensions (owner, (extension, shape), helpers) = runMaybeT $ do
    extend <- MaybeT (pure (lookup (idName extension) extensions))
    let context = ss_context shape
        mb_sort = ss_sort shape
        term    = ss_term shape
        ext     = ss_ext shape
        index   = ss_index shape
        var     = ss_var shape
        recipes = ss_recipes shape
        output  = if is_subst then term else index
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    (a_vars, mb_rank, cb_occ) <- case mb_sort of
      Just sort -> do
        a <- lift (freshVar "a" sort)
        rank <- lift (freshVar "sort" sort)
        pure ([a], Just rank, "rho")
      Nothing -> pure ([], Nothing, "r")
    let cbKind = closureCallbackKind index output mb_rank
        gk = mkTyVarTy g
        dk = mkTyVarTy d
        a_tys = mkTyVarTys a_vars
        termAt c ks = mkTyConApp term (c : ks)
        result = termAt dk a_tys
    rho <- lift (freshVar cb_occ (cbKind gk dk))
    input <- lift (freshVar "input" (termAt gk a_tys))
    let binders = mkNamedTyConBinders Specified ([g, d] ++ a_vars) ++ map mkAnonTyConBinder [rho, input]
        prepare (con, depths) = do
          let (con_vars, telescope) = splitForAllTyVars (tyConKind (DC.promoteDataCon con))
              (_, fields, codomain) = splitFunTys telescope
          (subst, vars) <- lift (freshenTyVarBndrs (map snd con_vars))
          let kind = substTyUnchecked subst codomain
          args <- lift (traverse (freshVar "field" . substTyUnchecked subst . scaledThing) fields)
          (tc, source : sort_ks) <- MaybeT (pure (splitTyConApp_maybe kind))
          guard (tc == term && length sort_ks == length a_vars)
          local <- lift (freshVar cb_occ (cbKind source dk))
          let r = mkTyVarTy local
              field_tys = mkTyVarTys args
              lhs = mkTyConApp (DC.promoteDataCon con) (mkTyVarTys vars ++ field_tys)
              liftField 0 ctx = (dk, r) <$ guard (ctx `eqType` source)
              liftField depth ctx = do
                (cl, outer : intro_ks) <- splitTyConApp_maybe ctx
                guard (cl == ext && length intro_ks == length a_vars)
                (target, inherited) <- liftField (depth - 1) outer
                pure (mkTyConApp ext (target : intro_ks),
                      mkTyConApp extend ([outer, target] ++ intro_ks ++ [inherited]))
              rewrite family depth arg = do
                (t, ctx : at_ks) <- splitTyConApp_maybe (typeKind arg)
                guard (t == term && length at_ks == length a_vars)
                (target, captured) <- liftField depth ctx
                guard (typeKind captured `eqType` cbKind ctx target)
                pure (mkTyConApp family ([ctx, target] ++ at_ks ++ [captured, arg]))
          pure $ \family -> do
            guard (typeKind lhs `eqType` kind)
            out <- if con == var
              then do [x] <- pure field_tys
                      guard (typeKind x `eqType` mkTyConApp index (source : sort_ks))
                      let mapped = mkAppTys r (sort_ks ++ [x])
                      if is_subst
                        then mapped <$ guard (typeKind mapped `eqType` termAt dk sort_ks)
                        else conAt con (termAt dk sort_ks) [mapped]
              else zipWithM (rewrite family) depths field_tys >>= conAt con (termAt dk sort_ks)
            pure (renamingBranch ([source, dk] ++ sort_ks ++ [r, lhs]) out)
    rows <- traverse prepare ((var, []) : recipes)
    publishRenamingTable is_early public owner helpers binders result (\family -> traverse ($ family) rows)

allocateClosureFamilies :: Bool -> ScopeShape -> TcM (Maybe (TyCon, [TyCon]))
allocateClosureFamilies is_early shape = do
    let owner        = ss_owner shape
        arity        = ss_arity shape
        context      = ss_context shape
        mb_sort      = ss_sort shape
        term         = ss_term shape
        ext          = ss_ext shape
        lookup_tycon = ss_index shape
        var          = ss_var shape
        hit          = ss_hit shape
        miss         = ss_miss shape
        recipes      = ss_recipes shape
        localName p  = newImplicitBinder (idName owner) (mkTcOcc . (p ++) . occNameString)
    (closure, constructors) <- allocateClosureData owner context mb_sort term ext
    case constructors of
      [replace, shift, lift_con, replace_next] -> do
        apply_name <- localName "$apply:"
        lookup_name <- localName "$lookup:"
        apply_axiom <- newImplicitBinder apply_name mkInstTyCoOcc
        lookup_axiom <- newImplicitBinder lookup_name mkInstTyCoOcc
        root_name <- newImplicitBinder (idName owner) mkSourceTableOcc
        root_axiom <- mkClosureTableAxiom is_early root_name
        g <- freshVar "g" context
        d <- freshVar "d" context
        (a_vars, b_vars, capture_kinds) <- case mb_sort of
          Just sort -> do
            a <- freshVar "a" sort
            b <- freshVar "b" sort
            cks <- traverse (\_ -> freshVar "captureKind" sort) [1 .. arity]
            pure ([a], [b], cks)
          Nothing -> pure ([], [], [])
        let gk = mkTyVarTy g
            dk = mkTyVarTy d
            a_tys = mkTyVarTys a_vars
            b_tys = mkTyVarTys b_vars
            termAt p ks = mkTyConApp term (p : ks)
            ctxAt p ks = mkTyConApp ext (p : ks)
            lookupAt p ks = mkTyConApp lookup_tycon (p : ks)
            closureAt p q = mkTyConApp closure [p, q]
            ctx_g = ctxAt gk a_tys
            ctx_d = ctxAt dk a_tys
            capture_steps = case mb_sort of
              Just _  -> map pure (mkTyVarTys capture_kinds)
              Nothing -> map (const []) [1 .. arity]
            body_context = foldl' ctxAt gk capture_steps
        cv <- freshVar "c" (closureAt gk dk)
        tree <- freshVar "t" (termAt gk b_tys)
        field <- freshVar "x" (lookupAt gk b_tys)
        carried <- freshVar "w" (termAt dk a_tys)
        body <- freshVar "n" (termAt body_context b_tys)
        args <- traverse (freshVar "v" . termAt gk) capture_steps
        x_d <- freshVar "older" (lookupAt dk b_tys)
        let prepare (ctor, exten) = do
              let (ctor_tvs, tel) = splitForAllTyVars (tyConKind (DC.promoteDataCon ctor))
                  (_, arg_kinds, res) = splitFunTys tel
              (renaming, tvs) <- freshenTyVarBndrs (map snd ctor_tvs)
              vars <- traverse (freshVar "arg" . substTyUnchecked renaming . scaledThing) arg_kinds
              let result = substTyUnchecked renaming res
              env <- freshVar "env" $ case splitTyConApp_maybe result of
                Just (_, src : rest_args) | length rest_args == length b_vars -> closureAt src dk
                _ -> liftedTypeKind
              pure (ctor, exten, tvs, vars, result, env)
        apply_inputs <- traverse prepare ((var, []) : recipes)
        let c = mkTyVarTy cv
            x = mkTyVarTy field
            w = mkTyVarTy carried
            outputs = termAt dk b_tys
            prefix = mkNamedTyConBinders Specified (g : d : b_vars)
            apply_apply s t ks env val = mkTyConApp apply ([s, t] ++ ks ++ [env, val])
            lookup_apply s t ks env val = mkTyConApp lookup_fam ([s, t] ++ ks ++ [env, val])
            apply = mkFamily apply_name apply_axiom (prefix ++ map mkAnonTyConBinder [cv, tree])
                        outputs (fromMaybe [] apply_branches)
            lookup_fam = mkFamily lookup_name lookup_axiom (prefix ++ map mkAnonTyConBinder [cv, field])
                        outputs (fromMaybe [] lookup_branches)
            applyBranch (ctor, exten, tvs, vars, res, env_var) = do
                (actual_term, src : sort_ks) <- splitTyConApp_maybe res
                guard (actual_term == term && length sort_ks == length b_vars &&
                       (ctor == var || length exten == length vars))
                let fields = mkTyVarTys vars
                    lhs = mkTyConApp (DC.promoteDataCon ctor) (mkTyVarTys tvs ++ fields)
                    env = mkTyVarTy env_var
                    liftField 0 source = (dk, env) <$ guard (source `eqType` src)
                    liftField depth source = do
                      (scope, outer : intro_ks) <- splitTyConApp_maybe source
                      guard (scope == ext && length intro_ks == length b_vars)
                      (target, inner) <- liftField (depth - 1) outer
                      let extended = ctxAt target intro_ks
                      lifted <- conAt lift_con (closureAt source extended) [inner]
                      pure (extended, lifted)
                    use depth value = do
                      (same, source : ks) <- splitTyConApp_maybe (typeKind value)
                      guard (same == term && length ks == length b_vars)
                      (target, closure_value) <- liftField depth source
                      pure (apply_apply source target ks closure_value value)
                out <- if ctor == var
                         then do [lookup_var] <- pure fields
                                 pure (lookup_apply src dk sort_ks env lookup_var)
                         else zipWithM use exten fields >>= conAt ctor (termAt dk sort_ks)
                guard (typeKind lhs `eqType` res && typeKind out `eqType` termAt dk sort_ks)
                let branch_lhs = [src, dk] ++ sort_ks ++ [env, lhs]
                    branch_vars = if isJust mb_sort
                                    then tvs ++ [d, env_var] ++ vars
                                    else tyCoVarsOfTypesWellScoped branch_lhs
                pure (mkBranch branch_vars branch_lhs out)
            apply_branches = traverse applyBranch apply_inputs
            lookup_branches = do
                r <- conAt replace (closureAt ctx_d dk) [w]
                z_in <- conAt hit (lookupAt ctx_d a_tys) []
                s_in <- conAt miss (lookupAt ctx_d b_tys) [mkTyVarTy x_d]
                v_d <- conAt var (termAt dk b_tys) [mkTyVarTy x_d]
                s <- conAt shift (closureAt gk ctx_g) []
                s_out <- conAt miss (lookupAt ctx_g b_tys) [x]
                v_out <- conAt var (termAt ctx_g b_tys) [s_out]
                l <- conAt lift_con (closureAt ctx_g ctx_d) [c]
                z_g <- conAt hit (lookupAt ctx_g a_tys) []
                z_d <- conAt hit (lookupAt ctx_d a_tys) []
                v0 <- conAt var (termAt ctx_d a_tys) [z_d]
                sx <- conAt miss (lookupAt ctx_g b_tys) [x]
                s_d <- conAt shift (closureAt dk ctx_d) []
                let weaken = apply_apply dk ctx_d b_tys s_d (lookup_apply gk dk b_tys c x)
                next <- conAt replace_next (closureAt ctx_g dk) [c, w]
                pure [ mkBranch ([d] ++ a_vars ++ [carried]) ([ctx_d, dk] ++ a_tys ++ [r, z_in]) w
                     , mkBranch ([d] ++ a_vars ++ b_vars ++ [carried, x_d]) ([ctx_d, dk] ++ b_tys ++ [r, s_in]) v_d
                     , mkBranch ([g] ++ a_vars ++ b_vars ++ [field]) ([gk, ctx_g] ++ b_tys ++ [s, x]) v_out
                     , mkBranch ([g, d] ++ a_vars ++ [cv]) ([ctx_g, ctx_d] ++ a_tys ++ [l, z_g]) v0
                     , mkBranch ([g, d] ++ a_vars ++ b_vars ++ [cv, field]) ([ctx_g, ctx_d] ++ b_tys ++ [l, sx]) weaken
                     , mkBranch ([g, d] ++ a_vars ++ [cv, carried]) ([ctx_g, dk] ++ a_tys ++ [next, z_g]) w
                     , mkBranch ([g, d] ++ a_vars ++ b_vars ++ [cv, carried, field]) ([ctx_g, dk] ++ b_tys ++ [next, sx])
                         (lookup_apply gk dk b_tys c x) ]
        pure $ do
            apply_equations <- apply_branches
            lookup_equations <- lookup_branches
            (first_ks, first_arg) : rest <- pure (zip capture_steps (mkTyVarTys args))
            let initial_context = ctxAt gk first_ks
                addReplacement (src, env) (ks, value) = do
                    let next_context = ctxAt src ks
                    next <- conAt replace_next (closureAt next_context gk) [env, value]
                    pure (next_context, next)
            first <- conAt replace (closureAt initial_context gk) [first_arg]
            (_, replacements) <- foldM addReplacement (initial_context, first) rest
            let result = termAt gk b_tys
                t = apply_apply body_context gk b_tys replacements (mkTyVarTy body)
                index_vars = g : capture_kinds ++ b_vars
                value_vars = body : args
                vars = index_vars ++ value_vars
                params = mkNamedTyConBinders Specified index_vars ++ map mkAnonTyConBinder value_vars
                root_equation = mkBranch vars (mkTyVarTys vars) t
                root = mkFamily root_name root_axiom params result [root_equation]
            guard (all validBranch (root_equation : apply_equations ++ lookup_equations))
            pure (root, [root, closure, apply, lookup_fam])
      _ -> pure Nothing

allocateClosureData :: Id -> Kind -> Maybe Kind -> TyCon -> TyCon -> TcM (TyCon, [DC.DataCon])
allocateClosureData owner context_kind mb_sort_kind term ext = do
    let tag prefix occ = prefix ++ occNameString occ
    closure_name <- newImplicitBinder (idName owner) (mkTcOcc . tag "$closure:")
    closure_rep <- newTyConRepName closure_name
    replace_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$replace:")
    shift_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$shift:")
    lift_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$lift:")
    next_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$replaceNext:")
    let names = [replace_name, shift_name, lift_name, next_name]
        tags = mkNameEnv (zip names [1..])
        ordinary = DC.HsSrcBang NoSourceText DC.NoSrcUnpack DC.NoSrcStrict
    source <- freshVar "source" context_kind
    target <- freshVar "target" context_kind
    incoming <- freshVar "incoming" context_kind
    outgoing <- freshVar "outgoing" context_kind
    b_vars <- case mb_sort_kind of
      Just sort_kind -> pure <$> freshVar "binder" sort_kind
      Nothing        -> pure []
    let universals = [source, target]
        source_ty = mkTyVarTy source
        target_ty = mkTyVarTy target
        incoming_ty = mkTyVarTy incoming
        outgoing_ty = mkTyVarTy outgoing
        b_tys = mkTyVarTys b_vars
        extend ctx = mkTyConApp ext (ctx : b_tys)
        termAt ctx = mkTyConApp term (ctx : b_tys)
        binders = map mkAnonTyConBinder universals
        kind = mkTyConKind binders liftedTypeKind
    closure <- mfix $ \result -> do
        let applied a b = mkTyConApp result [a, b]
            ctor name existentials user_tvs equalities fields res = do
              representative <- newTyConRepName name
              buildDataCon emptyFamInstEnvs (FixedBangOpts (map (const DC.HsLazy) fields))
                name False representative (map (const ordinary) fields) [] universals existentials
                (mkTyVarBinders Specified user_tvs) equalities []
                (map (Scaled manyDataConTy) fields) res result tags
        replace <- ctor replace_name b_vars (target : b_vars)
                     [DC.mkEqSpec source (extend target_ty)]
                     [termAt target_ty] (applied (extend target_ty) target_ty)
        shift <- ctor shift_name b_vars (source : b_vars)
                   [DC.mkEqSpec target (extend source_ty)] [] (applied source_ty (extend source_ty))
        lift_con <- ctor lift_name (incoming : outgoing : b_vars) (incoming : outgoing : b_vars)
                      [DC.mkEqSpec source (extend incoming_ty), DC.mkEqSpec target (extend outgoing_ty)]
                      [applied incoming_ty outgoing_ty] (applied (extend incoming_ty) (extend outgoing_ty))
        replace_next <- ctor next_name (incoming : b_vars) (incoming : target : b_vars)
                          [DC.mkEqSpec source (extend incoming_ty)]
                          [applied incoming_ty target_ty, termAt target_ty]
                          (applied (extend incoming_ty) target_ty)
        pure (mkAlgTyCon closure_name kind binders 0 liftedTypeKind [Nominal, Nominal] Nothing []
                (mkLevPolyDataTyConRhs True True [replace, shift, lift_con, replace_next])
                (VanillaAlgTyCon closure_rep) True)
    pure (closure, tyConDataCons closure)

allocateUntypedSubstViaSubstU :: Bool -> TyCon -> ScopeShape -> TcM (Maybe (TyCon, [TyCon]))
allocateUntypedSubstViaSubstU is_early subst_tc shape = do
    let owner        = ss_owner shape
        arity        = ss_arity shape
        context      = ss_context shape
        term         = ss_term shape
        ext          = ss_ext shape
        lookup_tycon = ss_index shape
        var          = ss_var shape
        hit          = ss_hit shape
        miss         = ss_miss shape
    map_name <- newImplicitBinder (idName owner) (mkTcOcc . ("$substMap:" ++) . occNameString)
    map_axiom <- newImplicitBinder map_name mkInstTyCoOcc
    root_name <- newImplicitBinder (idName owner) mkSourceTableOcc
    root_axiom <- mkClosureTableAxiom is_early root_name
    g <- freshVar "g" context
    let gk = mkTyVarTy g
        termAt p = mkTyConApp term [p]
        ctxAt p = mkTyConApp ext [p]
        lookupAt p = mkTyConApp lookup_tycon [p]
        body_ctx = foldl' (\k _ -> ctxAt k) gk [1 .. arity]
    body <- freshVar "n" (termAt body_ctx)
    args <- traverse (\_ -> freshVar "v" (termAt gk)) [1 .. arity]
    map_input <- freshVar "input" (lookupAt body_ctx)
    older_x <- freshVar "x" (lookupAt gk)
    let arg_tys = mkTyVarTys args
        rev_args = reverse arg_tys
        wrap_miss 0 ctx pat_fn = pat_fn ctx
        wrap_miss d ctx pat_fn = do
          (cl, [inner_ctx]) <- splitTyConApp_maybe ctx
          guard (cl == ext)
          inner_pat <- wrap_miss (d - 1) inner_ctx pat_fn
          conAt miss (lookupAt ctx) [inner_pat]
        hit_branch (depth, rhs_ty) = do
          pat <- wrap_miss depth body_ctx (\ctx -> conAt hit (lookupAt ctx) [])
          pure (renamingBranch (gk : arg_tys ++ [pat]) rhs_ty)
        miss_branch = do
          pat <- wrap_miss arity body_ctx (\_ -> Just (mkTyVarTy older_x))
          rhs_ty <- conAt var (termAt gk) [mkTyVarTy older_x]
          pure (renamingBranch (gk : arg_tys ++ [pat]) rhs_ty)
    pure $ do
      hit_brs <- traverse hit_branch (zip [0 .. arity - 1] rev_args)
      m_br <- miss_branch
      let map_brs = hit_brs ++ [m_br]
          map_binders = mkNamedTyConBinders Specified [g] ++
                        map mkAnonTyConBinder (args ++ [map_input])
          map_fam = mkFamily map_name map_axiom map_binders (termAt gk) map_brs
          partial_map = mkTyConApp map_fam (gk : arg_tys)
      guard (typeKind partial_map `eqType` closureCallbackKind lookup_tycon term Nothing body_ctx gk)
      let root_rhs = mkTyConApp subst_tc [body_ctx, gk, partial_map, mkTyVarTy body]
          value_vars = body : args
          vars = g : value_vars
          root_binders = mkNamedTyConBinders Specified [g] ++ map mkAnonTyConBinder value_vars
          root_eq = mkBranch vars (mkTyVarTys vars) root_rhs
          root = mkFamily root_name root_axiom root_binders (termAt gk) [root_eq]
      guard (typeKind root_rhs `eqType` termAt gk &&
             tyConKind root `eqType` varType owner &&
             all validBranch (root_eq : map_brs))
      pure (root, [root, map_fam])

-- ---------------------------------------------------------------------------
-- Shared early & post-tidy stage orchestrator (SC1)
-- ---------------------------------------------------------------------------

-- | Publish tables for eligible final Core bindings. The caller checks
-- the source-equation opt-in and excludes plugins and local user rules.
publishCheckedSourceClosures :: [CoreBind] -> [CoreBind] -> TypeEnv -> TcM ([Id], [TyCon])
publishCheckedSourceClosures original_binds final_binds final_types = do
    env <- getGblEnv
    rdr_env <- getGlobalRdrEnv
    opts <- initSimpleOpts <$> getDynFlags
    visible <- visibleSourceMethodRules env
    rule_heads <- visibleClosureRuleHeads env
    let here = Just (tcg_mod env)
        exported = availsToNameSet (tcg_exports env)
        imported name = isWiredInName name || ordinaryImport rdr_env name
        public = publicCaseType env exported imported
        unoptimized = stripBreakpointTicks original_binds
        original = map (safeSimpleOptBind opts) unoptimized
        final = stripBreakpointTicks final_binds
        unopt_env = mkBindEnv unoptimized
        orig_env = mkBindEnv original
        fin_env = mkBindEnv final
        floatedHelper v =
          isFloatedConstructorHelper ((`elemNameSet` exported) . idName) fin_env v ||
          isPatErrorHelper fin_env v
        lookupLocal name = case lookupTypeEnv final_types name of
          Just (AnId actual) | idName actual == name
                             , nameModule_maybe name == here -> Just actual
          _ -> Nothing
        lookupOwner old = do
          let owner = idName old
          guard (isExternalName owner && owner `elemNameSet` exported)
          actual <- lookupLocal owner
          VanillaId <- pure (idDetails actual)
          pure actual
        lookupClosureRoot old = do
          let owner = idName old
          guard (isExternalName owner && owner `elemNameSet` exported)
          actual <- lookupLocal owner
          case idDetails actual of
            VanillaId -> pure actual
            CheckedClosureSourceTableId{} -> pure actual
            _ -> Nothing
        resolveDepId v
          | nameModule_maybe (idName v) == here = lookupClosureRoot v
          | isJust (sourceTableId_maybe v)      = Just v
          | otherwise                           = Nothing
        extDeps keep old actual = do
          let names = nameSetElemsStable $ mkNameSet $
                filter (\n -> n /= idName old && keep n)
                       (reachable orig_env old ++ reachable fin_env actual)
          traverse lookupLocal names
        onlyCalls allowed old actual = do
          old_rhs <- lookupVarEnv unopt_env old
          new_rhs <- lookupVarEnv fin_env actual
          guard (hasOnlyAllowedCalls (const False) allowed old_rhs &&
                 hasOnlyAllowedCalls floatedHelper allowed new_rhs)
        safe owner helpers =
          all (\v -> not (hasSourceMethodRule visible v)
                  && not (idName v `elemNameSet` rule_heads)) (owner : helpers)
        init_tables = [(idName v, tc) | v <- typeEnvIds final_types, Just tc <- [sourceTableId_maybe v]]
    entries <- runClosureStage False here public (map fst original) orig_env (Just fin_env)
                 lookupOwner lookupClosureRoot resolveDepId extDeps onlyCalls safe init_tables
    pure ([v | (v, _, _) <- entries], [tc | (_, tcs, _) <- entries, tc <- tcs])

-- | Stage eligible local indexed lookups, GADT traversals, and substitution
-- chains before checking dependent declarations in the rest of the group.
sourceEarlyClosureTables :: [Id] -> NameEnv (Id, CoreExpr) -> TcM ([(Id, TyCon)], [TyCon], [Id])
sourceEarlyClosureTables group_owners definitions = do
    env <- getGblEnv
    dflags <- getDynFlags
    hsc <- getTopEnv
    if hasActiveLatePlugins hsc
      then pure ([], [], [])
      else do
        let here = Just (tcg_semantic_mod env)
            opts = initSimpleOpts dflags
            rdr_env = tcg_rdr_env env
            available name
              | nameModule_maybe name == here =
                  maybe False isLocalGRE (lookupGRE_Name rdr_env name)
              | otherwise = isWiredInName name || ordinaryImport rdr_env name
            public tc = all available (tyConName tc : map DC.dataConName (tyConDataCons tc))
            unoptimized = nonDetNameEnvElts definitions
            orig_env = mkBindEnv (map (safeSimpleOptBind opts) unoptimized)
            in_group_set = mkNameSet (map idName group_owners)
            lookupDef name = fst <$> lookupNameEnv definitions name
            lookupOwner v = do
              actual <- lookupDef (idName v) <|> (v <$ guard (idName v `elemNameSet` in_group_set))
              guard (idName actual `elemNameSet` in_group_set && isNothing (sourceTableId_maybe actual))
              pure actual
            lookupClosureRoot v = lookupDef (idName v) <|> (v <$ guard (idName v `elemNameSet` in_group_set))
            resolveDepId v
              | nameModule_maybe (idName v) == here =
                  lookupDef (idName v) <|>
                  (case lookupTypeEnv (tcg_type_env env) (idName v) of
                     Just (AnId actual) | isJust (sourceTableId_maybe actual) -> Just actual
                     _ -> Nothing)
              | isJust (sourceTableId_maybe v) = Just v
              | otherwise = Nothing
            extDeps keep old _ =
              let names = nameSetElemsStable $ mkNameSet $
                    filter (\n -> n /= idName old && keep n && nameModule_maybe n == here)
                           (reachable orig_env old)
              in traverse lookupDef names
            onlyCalls allowed old _ = do
              raw_rhs <- snd <$> lookupNameEnv definitions (idName old)
              opt_rhs <- lookupVarEnv orig_env old
              guard (hasOnlyAllowedCalls (const False) allowed raw_rhs &&
                     hasOnlyAllowedCalls (const False) allowed opt_rhs)
            init_tables =
              [(idName v, tc) | v <- group_owners, Just tc <- [sourceTableId_maybe v]] ++
              [(idName v, tc) | v <- typeEnvIds (tcg_type_env env), Just tc <- [sourceTableId_maybe v]]
        entries <- runClosureStage True here public group_owners orig_env Nothing
                     lookupOwner lookupClosureRoot resolveDepId extDeps onlyCalls (\_ _ -> True) init_tables
        let tables = [(v, tc) | (v, tc : _, _) <- entries]
            companions = [tc | (_, _ : rest, _) <- entries, tc <- rest]
            helpers = nubBy (\a b -> idName a == idName b)
                        [ h | (_, _, hs) <- entries, h <- hs
                        , nameModule_maybe (idName h) == here ]
        pure (tables, companions, helpers)

runClosureStage
  :: Bool
  -> Maybe a
  -> (TyCon -> Bool)
  -> [Id]
  -> BindEnv
  -> Maybe BindEnv
  -> (Id -> Maybe Id)
  -> (Id -> Maybe Id)
  -> (Id -> Maybe Id)
  -> ((Name -> Bool) -> Id -> Id -> Maybe [Id])
  -> ([Name] -> Id -> Id -> Maybe ())
  -> (Id -> [Id] -> Bool)
  -> [(Name, TyCon)]
  -> TcM [(Id, [TyCon], [Id])]
runClosureStage is_early here public candidates orig_env mb_fin_env
                lookupOwner lookupClosureRoot resolveDepId extDeps onlyCalls safe init_tables =
    let _ = here in do
    -- Phase 1: Scope-indexed closure roots and traversal chains (unary & binary)
    let sameId a b = idName a == idName b
        checkModScope shape = case mb_fin_env of
          Nothing -> True
          Just _  ->
            let req = [ss_term shape, ss_index shape] ++ [ss_ext shape | isJust (ss_sort shape)]
                owner_mod = nameModule_maybe (idName (ss_owner shape))
            in all ((== owner_mod) . nameModule_maybe . tyConName) req
        closure_cands = flip mapMaybe candidates $ \old -> do
          orig_rhs <- lookupVarEnv orig_env old
          orig_s <- scopeFamilyInputs orig_env old orig_rhs
          guard (checkModScope orig_s)
          (actual, fin_s) <- case mb_fin_env of
            Nothing -> do
              act <- lookupOwner old
              pure (act, orig_s)
            Just fin_env -> do
              act <- lookupClosureRoot old
              fin_rhs <- lookupVarEnv fin_env act
              fs <- scopeFamilyInputs fin_env act fin_rhs
              guard (sameScopeShape orig_s fs)
              pure (act, fs)
          hs <- extDeps isExternalName old actual
          guard (not (null hs) && safe actual hs)
          pure (actual, (orig_s, fin_s), hs)

        orig_direct = directScopeShapes orig_env candidates
        fin_direct = maybe [] (\fe -> directScopeShapes fe [v | (v, _) <- nonDetVarEnvPairs fe candidates]) mb_fin_env
        direct_pairs = case mb_fin_env of
          Nothing -> [(s, s) | s <- orig_direct]
          Just _  ->
            [ (o_s, f_s)
            | o_s <- orig_direct
            , checkModScope o_s
            , Just f_s <- [find (\fs -> sameId (ss_owner o_s) (ss_owner fs)) fin_direct]
            , sameScopeShape o_s f_s ]
        all_shape_pairs = [(o, f) | (_, (o, f), _) <- closure_cands] ++ direct_pairs

        ren_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b) $ flip mapMaybe all_shape_pairs $ \(o_s, f_s) -> do
          let (o_ren, o_ext) = ss_ren_ops o_s
              (f_ren, f_ext) = ss_ren_ops f_s
          guard (sameId o_ren f_ren && sameId o_ext f_ext)
          act_ren <- lookupClosureRoot f_ren
          act_ext <- lookupClosureRoot f_ext
          onlyCalls [] o_ext act_ext
          hs <- extDeps isExternalName o_ren act_ren
          guard (any (sameId act_ext) hs && safe act_ren hs)
          pure (act_ren, (act_ext, f_s), hs)

        ext_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
          [ (act_ext, f_s, [])
          | (_, (act_ext, f_s), _) <- ren_cands
          , isJust (lookupOwner act_ext)
          , safe act_ext [] ]

    pub_exts <- fmap catMaybes $ for_list ext_cands $ \(act_ext, s, _) -> do
      res <- publishScopeLookupExtension is_early public act_ext [] s Nothing
      pure ((\(v, tcs) -> (v, tcs, [])) <$> res)
    let ext_tables = [(idName v, tc) | (v, tc : _, _) <- pub_exts] ++ init_tables

    pub_rens <- fmap catMaybes $ for_list
      [ c | c@(act_ren, _, _) <- ren_cands, isJust (lookupOwner act_ren) ] $ \cand@(_, _, hs) -> do
        res <- publishTraversalFamily False is_early public ext_tables cand
        pure ((\(v, tcs) -> (v, tcs, hs)) <$> res)
    let ren_tables = [(idName v, tc) | (v, tc : _, _) <- pub_rens] ++ ext_tables

        sext_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b) $ flip mapMaybe all_shape_pairs $ \(o_s, f_s) -> do
          let (o_ren, _) = ss_ren_ops o_s
              (f_ren, _) = ss_ren_ops f_s
          (_, o_sext, mb_o_weak) <- ss_subst_ops o_s
          (_, f_sext, mb_f_weak) <- ss_subst_ops f_s
          guard (sameId o_ren f_ren && sameId o_sext f_sext &&
                 (isNothing (ss_sort o_s) || isJust mb_o_weak) &&
                 maybe True (\fw -> fmap idName mb_o_weak == Just (idName fw)) mb_f_weak)
          act_ren <- lookupClosureRoot f_ren
          act_sext <- lookupOwner f_sext
          mb_act_weak <- case mb_o_weak of
            Nothing -> pure Nothing
            Just ow -> do
              aw <- lookupClosureRoot ow
              ow_rhs <- lookupVarEnv orig_env ow
              guard (isJust (validConstructorAliasBody ow_rhs))
              for_ mb_fin_env $ \fe -> do
                fw_rhs <- lookupVarEnv fe aw
                guard (isJust (validConstructorAliasBody fw_rhs))
              onlyCalls [] ow aw
              pure (Just aw)
          let allowed = idName f_ren : maybe [] (pure . idName) mb_o_weak
          onlyCalls allowed o_sext act_sext
          raw_hs <- extDeps isExternalName o_sext act_sext
          let hs = nubBy sameId (maybeToList mb_act_weak ++ raw_hs)
          guard (any (sameId act_ren) hs && safe act_sext hs)
          pure (act_sext, (act_ren, mb_act_weak, f_s), hs)

        weak_cands = nubBy (\a b -> sameId (fst a) (fst b))
          [ (aw, f_s)
          | (_, (_, Just aw, f_s), _) <- sext_cands
          , isJust (lookupOwner aw)
          , safe aw [] ]

    pub_weaks <- fmap catMaybes $ for_list weak_cands $ \pair -> do
      res <- publishWeakeningFamily is_early public pair
      pure ((\(v, tcs) -> (v, tcs, [])) <$> res)
    let weak_tables = [(idName v, tc) | (v, tc : _, _) <- pub_weaks] ++ ext_tables

    pub_sexts <- fmap catMaybes $ for_list sext_cands $ \(act_sext, (act_ren, mb_aw, f_s), hs) ->
      case lookup (idName act_ren) ren_tables of
        Nothing -> pure Nothing
        Just ren_tc -> do
          let mb_w_tc = case mb_aw of
                Nothing -> Just Nothing
                Just aw -> Just <$> lookup (idName aw) weak_tables
          case mb_w_tc of
            Nothing -> pure Nothing
            Just w_tc_opt -> do
              res <- publishScopeLookupExtension is_early public act_sext hs f_s (Just (ren_tc, w_tc_opt))
              pure ((\(v, tcs) -> (v, tcs, hs)) <$> res)
    let sext_tables = [(idName v, tc) | (v, tc : _, _) <- pub_sexts] ++ ext_tables

        subst_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b) $ flip mapMaybe all_shape_pairs $ \(o_s, f_s) -> do
          let (o_ren, _) = ss_ren_ops o_s
              (f_ren, _) = ss_ren_ops f_s
          (o_sub, o_sext, mb_o_weak) <- ss_subst_ops o_s
          (f_sub, f_sext, mb_f_weak) <- ss_subst_ops f_s
          guard (map idName [o_sub, o_ren, o_sext] == map idName [f_sub, f_ren, f_sext] &&
                 (isNothing (ss_sort o_s) || isJust mb_o_weak) &&
                 maybe True (\fw -> fmap idName mb_o_weak == Just (idName fw)) mb_f_weak)
          act_sub <- lookupOwner f_sub
          act_sext <- lookupClosureRoot f_sext
          mb_act_weak <- case mb_o_weak of
            Nothing -> pure Nothing
            Just ow -> Just <$> lookupClosureRoot ow
          let op_names = [idName f_sub, idName f_ren, idName f_sext] ++ maybe [] (pure . idName) mb_o_weak
          onlyCalls op_names o_sub act_sub
          raw_hs <- extDeps isExternalName o_sub act_sub
          let hs = nubBy sameId (maybeToList mb_act_weak ++ raw_hs)
          guard (any (sameId act_sext) hs && safe act_sub hs)
          pure (act_sub, (act_sext, f_s), hs)

    pub_substs <- fmap catMaybes $ for_list subst_cands $ \cand@(_, _, hs) -> do
      res <- publishTraversalFamily True is_early public sext_tables cand
      pure ((\(v, tcs) -> (v, tcs, hs)) <$> res)
    let subst_tables = [(idName v, tc) | (v, tc : _, _) <- pub_substs] ++ sext_tables

    pub_closures <- fmap catMaybes $ for_list closure_cands $ \(owner, (_, shape), hs) ->
      if not (isJust (lookupOwner owner))
        then pure Nothing
        else do
          let mb_subst_u_tc
                | isNothing (ss_sort shape)
                , Just (sub_u, _, _) <- ss_subst_ops shape
                = lookup (idName sub_u) subst_tables <|> sourceTableId_maybe sub_u
                | otherwise = Nothing
          generated <- case mb_subst_u_tc of
            Just su_tc -> allocateUntypedSubstViaSubstU is_early su_tc shape
            Nothing    -> allocateClosureFamilies is_early shape
          pure $ do
            (root, families) <- generated
            guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
            let new_v = setIdDetails owner (CheckedClosureSourceTableId (idName owner) root (map idName hs))
            pure (new_v, families, hs)

    -- Phase 2: Fixed-point passes of unified indexed Core compilation (SC2 + SC5)
    let phase1 = pub_exts ++ pub_rens ++ pub_weaks ++ pub_sexts ++ pub_substs ++ pub_closures
        done1 = mkNameSet [idName v | (v, _, _) <- phase1]
        tables1 = [(idName v, tc) | (v, tc : _, _) <- phase1] ++ init_tables
    phase2 <- runIndexedPasses done1 tables1
    pure (phase1 ++ phase2)
  where
    for_list = flip traverse
    sameId a b = idName a == idName b

    nonDetVarEnvPairs fe cs =
      [(v, r) | v <- cs, Just r <- [lookupVarEnv fe v]]

    runIndexedPasses done known_tables = do
      let resolveTable v = lookup (idName v) known_tables <|> sourceTableId_maybe v
      pass_entries <- fmap catMaybes $ for_list candidates $ \old ->
        if idName old `elemNameSet` done
          then pure Nothing
          else case lookupOwner old of
            Nothing -> pure Nothing
            Just actual -> case lookupVarEnv orig_env old of
              Nothing -> pure Nothing
              Just orig_rhs -> do
                mb_orig <- runMaybeT (compileIndexedCore orig_env resolveTable Nothing Nothing old orig_rhs)
                case mb_orig of
                  Nothing -> pure Nothing
                  Just orig_ci -> do
                    case traverse resolveDepId (ci_table_callees orig_ci) of
                      Nothing -> pure Nothing
                      Just sem_callees -> do
                        mb_fin_res <- case mb_fin_env of
                          Nothing -> pure (Just (Nothing, Nothing))
                          Just fin_env -> case lookupVarEnv fin_env actual of
                            Nothing -> pure Nothing
                            Just raw_fin_rhs -> do
                              let (mb_alias, eff_fin_rhs) = resolveForwardCseAlias fin_env actual raw_fin_rhs
                                  exp_brs =
                                    let d_tc = mkFamily (idName actual) (idName actual)
                                                 (ci_binders orig_ci) (ci_res_kind orig_ci) []
                                    in [applyRow eq d_tc | (eq, _) <- ci_rows orig_ci]
                              mb_f <- runMaybeT $
                                compileIndexedCore fin_env resolveTable (ci_case_names orig_ci)
                                  (Just exp_brs) actual eff_fin_rhs
                              pure ((, mb_alias) . Just <$> mb_f)
                        case mb_fin_res of
                          Nothing -> pure Nothing
                          Just (mb_fin_ci, mb_alias) -> do
                            let fin_w = mb_fin_ci >>= ci_mb_worker
                                orig_w = ci_mb_worker orig_ci
                                w_names = map idName (maybeToList orig_w ++ maybeToList fin_w)
                                base_allowed = idName old : w_names ++ map idName sem_callees
                                full_allowed = base_allowed ++ maybe [] (pure . idName) mb_alias
                                keep_dep n
                                  | Just fw <- fin_w, n == idName fw = isExternalName n
                                  | Just ow <- orig_w, n == idName ow = isExternalName n
                                  | otherwise = isExternalName n
                                checks = do
                                  onlyCalls full_allowed old actual
                                  for_ mb_alias (onlyCalls base_allowed old)
                                  local_hs <- extDeps keep_dep old actual
                                  let hs = nubBy sameId (sem_callees ++ local_hs)
                                  guard (safe actual hs)
                                  pure hs
                            case checks of
                              Nothing -> pure Nothing
                              Just hs -> do
                                pub <- publishCompiledIndexedM is_early public actual orig_ci mb_fin_ci hs
                                pure ((\(v, tcs) -> (v, tcs, hs)) <$> pub)
      if null pass_entries
        then pure []
        else do
          let done' = done `extendNameSetList` [idName v | (v, _, _) <- pass_entries]
              tables' = [(idName v, tc) | (v, tc : _, _) <- pass_entries] ++ known_tables
          more <- runIndexedPasses done' tables'
          pure (pass_entries ++ more)

-- ---------------------------------------------------------------------------
-- Early closure pruning, export validation, Core agreement, and finalization
-- ---------------------------------------------------------------------------

earlySourceClosureEntries :: TcGblEnv -> [(Id, TyCon, [Name], [TyCon])]
earlySourceClosureEntries env =
    sortBy (\(a, _, _, _) (b, _, _, _) -> stableNameCmp (idName a) (idName b))
      [ (fun, table, helpers, companions fun)
      | fun <- typeEnvIds (tcg_type_env env)
      , Just (table, helpers) <- [classify fun] ]
  where
    companions = companionTyConLookup (tcg_tcs env)
    classify fun = case idDetails fun of
      CheckedClosureSourceTableId name table helpers
        | name == idName fun, isEarlyClosureTable table -> Just (table, helpers)
      SourceTableId name table NominalSourceTable
        | name == idName fun, isEarlyClosureTable table -> Just (table, [])
      _ -> Nothing

-- | Collect early-staged indexed lookup, traversal, and closure owners installed in the module.
earlyIndexedLookups :: TcGblEnv -> [(Id, TyCon)]
earlyIndexedLookups env = [(fun, table) | (fun, table, _, _) <- earlySourceClosureEntries env]

-- | Drop speculative early lookup, traversal, and closure tables that no dependent type referenced.
pruneUnusedIndexedLookups :: TcGblEnv -> TcM TcGblEnv
pruneUnusedIndexedLookups env = do
    direct_used <- readTcRef (tcg_source_table_uses env)
    let entries = earlySourceClosureEntries env
        table_map = mkNameEnv [(tyConName table, table) | (_, table, _, _) <- entries]
        close_uses seen [] = seen
        close_uses seen (nm : rest)
          | Just tc <- lookupNameEnv table_map nm
          , Just callees <- sourceTableCallees tc
          , let next = [tyConName c | c <- callees, not (tyConName c `elemNameSet` seen)]
          = close_uses (extendNameSetList seen next) (next ++ rest)
          | otherwise = close_uses seen rest
        used = close_uses direct_used (nameSetElemsStable direct_used)
        unused = [entry | entry@(_, table, _, _) <- entries, not (tyConName table `elemNameSet` used)]
    if null unused then pure env else do
      let owners = mkNameSet [idName fun | (fun, _, _, _) <- unused]
          removed_tcs = [tc | (_, table, _, comps) <- unused, tc <- table : comps]
          rep_names = mapMaybe tyConRepName_maybe $
            removed_tcs ++ [DC.promoteDataCon dc | tc <- removed_tcs, dc <- tyConDataCons tc]
          rep_set = mkNameSet rep_names
          removed_names = mkNameSet $
            map tyConName removed_tcs ++
            map getName (concatMap implicitTyConThings removed_tcs) ++
            rep_names
          removed_tc_names = mkNameSet (map tyConName removed_tcs)
          clearThing (AnId v) | idName v `elemNameSet` owners = AnId (setIdDetails v VanillaId)
          clearThing thing = thing
          keepBind bind =
            not (any ((`elemNameSet` rep_set) . idName)
                     (collectHsBindBinders CollNoDictBinders (unLoc bind)))
      pure env
        { tcg_tcs = filter (not . (`elemNameSet` removed_tc_names) . tyConName) (tcg_tcs env)
        , tcg_binds = filter keepBind (tcg_binds env)
        , tcg_type_env = mapNameEnv clearThing
            (filterNameEnv (not . (`elemNameSet` removed_names) . getName) (tcg_type_env env))
        }

visibleClosureRuleHeads :: TcGblEnv -> TcM NameSet
visibleClosureRuleHeads env = do
    hsc <- getTopEnv
    liftIO $ visibleSourceRuleHeads hsc (tcg_mod env) (tcg_src env) (tcVisibleOrphanMods env)

rewriteEarlyClosureApps :: TyConSet -> Type -> Type
rewriteEarlyClosureApps early_set = go
  where
    go (AppTy a b) = AppTy (go a) (go b)
    go (TyConApp tc tys)
      | let tys' = map go tys
      = if tc `elementOfUniqSet` early_set
          then reductionReducedType (normaliseTcApp emptyFamInstEnvs Nominal tc tys')
          else TyConApp tc tys'
    go (FunTy af ma mult arg res) = FunTy af (go ma) (go mult) (go arg) (go res)
    go ty = ty

data TyConScan = TyConScan !TyConSet !VarSet

survivingEarlyExportedTyCons :: [TyCon] -> [Type] -> TyConSet
survivingEarlyExportedTyCons [] tys = tyConsOfTypes tys
survivingEarlyExportedTyCons early_fams tys =
    case foldl' collect (TyConScan emptyUniqSet emptyVarSet) tys of
      TyConScan tcs _ -> tcs
  where
    !early_set = mkUniqSet early_fams
    collectVar acc@(TyConScan tcs seen_vs) v
      | v `elemVarSet` seen_vs = acc
      | otherwise = collect (TyConScan tcs (extendVarSet seen_vs v)) (varType v)
    collect (TyConScan tcs seen_vs) ty@(TyConApp tc args)
      | tc `elementOfUniqSet` early_set =
          TyConScan (tcs `unionUniqSets` tyConsOfType (rewriteEarlyClosureApps early_set ty)) seen_vs
      | otherwise =
          foldl' collect (TyConScan (addOneToUniqSet tcs tc) seen_vs) args
    collect acc (TyVarTy v)               = collectVar acc v
    collect acc (TermAppTy f a)           = collect (collectVar acc f) a
    collect acc (TermVarTy v)             = collectVar acc v
    collect acc (DepFunTy _ v b)          = collect (collectVar acc v) b
    collect acc (AppTy a b)               = collect (collect acc a) b
    collect acc (ForAllTy ma (Bndr v _) b) = collect (collectVar (collect acc ma) v) b
    collect acc (FunTy _ ma mult a r)     = foldl' collect acc [ma, mult, a, r]
    collect acc (CastTy t _)              = collect acc t
    collect acc (ExistsTy _ v b)          = collect (collectVar acc v) b
    collect acc (DepDictTy d b)           = collect (collectVar acc d) b
    collect acc (ClassAppTy _ t1 t2 t3 t4) = foldl' collect acc [t1, t2, t3, t4]
    collect acc _                         = acc

-- | Validate constructor exports, owner/helper visibility, and imported rules for used early closures.
checkLocalIndexedLookupExports :: TcGblEnv -> TcM ()
checkLocalIndexedLookupExports env = do
    let candidates = earlySourceClosureEntries env
    unless (null candidates) $ do
      visible <- visibleSourceMethodRules env
      rule_heads <- visibleClosureRuleHeads env
      let exported = availsToNameSet (tcg_exports env)
          type_env = tcg_type_env env
          all_early_fams = [tc | (_, table, _, comps) <- candidates, tc <- table : comps]
          exported_tycons =
            survivingEarlyExportedTyCons all_early_fams (exportedInterfaceTypes exported type_env)
          type_partially_exported tc =
            tyConName tc `elemNameSet` exported ||
            any ((`elemNameSet` exported) . DC.dataConName) (tyConDataCons tc)
          lookupId nm = case lookupTypeEnv type_env nm of
            Just (AnId v) -> Just v
            _             -> Nothing
      for_ candidates $ \(owner, table, helpers, companions) -> do
        let name = idName owner
            all_families = table : companions
            local_eq_types =
              [ tc
              | tc <- sourceEquationDatatypes all_families
              , tc `notElem` all_families
              , nameModule_maybe (tyConName tc) == Just (tcg_semantic_mod env) ]
            unexported_types = sortBy stableNameCmp
              [ tyConName tc
              | tc <- local_eq_types
              , not (publicCaseType env exported (const False) tc) ]
            in_exported_iface = any (`elementOfUniqSet` exported_tycons) all_families
            needs_public_types =
              in_exported_iface ||
              name `elemNameSet` exported ||
              any type_partially_exported local_eq_types
            local_helpers = filter (\h -> nameModule_maybe h == Just (tcg_semantic_mod env)) helpers
            unexported_vals = filter (not . (`elemNameSet` exported)) (name : local_helpers)
        case unexported_types of
          hidden : _ | needs_public_types -> do
            let reason = "The dependent type needs this equation. Export the constructors of "
                         ++ occNameString (nameOccName hidden) ++ " to make it available."
            setSrcSpan (nameSrcSpan name) $
              addErr (TcRnOpenTypeEval name Nothing reason)
          _ | in_exported_iface, missing : _ <- unexported_vals -> do
            let reason = "The dependent type needs this equation. Export "
                         ++ occNameString (nameOccName missing) ++ " to make it available."
            setSrcSpan (nameSrcSpan name) $
              addErr (TcRnOpenTypeEval name Nothing reason)
          _ -> pure ()
        callee_owners <- traverse sourceTableOwner (fromMaybe [] (sourceTableCallees table))
        let imported_callee_ids =
              [ v | Just v <- callee_owners
                  , nameModule_maybe (idName v) /= nameModule_maybe name ]
        helper_ids <- catMaybes <$> traverse (\nm -> case lookupId nm of
                        Just v  -> pure (Just v)
                        Nothing -> tcLookupIdMaybe nm) helpers
        let checked_ids = owner : helper_ids ++ imported_callee_ids
        case find (\v -> hasSourceMethodRule visible v || idName v `elemNameSet` rule_heads) checked_ids of
          Just bad ->
            setSrcSpan (nameSrcSpan name) $
              addErr (TcRnRetainedQuantificationError (RetainedSourceRule (idName bad)))
          Nothing -> pure ()

exportedInterfaceTypes :: NameSet -> TypeEnv -> [Type]
exportedInterfaceTypes exported type_env = concatMap entity_types (typeEnvElts type_env)
  where
    data_con_types dc = [varType (DC.dataConWrapId dc), varType (DC.dataConWorkId dc)]
    entity_types (AnId v)
      | idName v `elemNameSet` exported = [varType v]
      | otherwise = []
    entity_types (AConLike (RealDataCon dc))
      | DC.dataConName dc `elemNameSet` exported = data_con_types dc
      | otherwise = []
    entity_types (ATyCon tc)
      | tyConName tc `elemNameSet` exported =
          tyConKind tc : maybeToList (synTyConRhs_maybe tc)
          ++ concatMap data_con_types (tyConDataCons tc)
          ++ [ ty | ax <- maybeToList (closedFamilyTyConCoAxiom_maybe tc)
                  , br <- fromBranches (coAxiomBranches ax)
                  , ty <- coAxBranchRHS br : coAxBranchLHS br ++ map varType (coAxBranchTyVars br) ]
      | otherwise =
          [ ty | dc <- tyConDataCons tc
               , DC.dataConName dc `elemNameSet` exported
               , ty <- data_con_types dc ]
    entity_types _ = []

-- | Refresh exported CheckedClosureSourceTableId helper lists after Tidy externalizes
-- floated constructor helpers, or clear the certificate when an internal-only proof
-- leaves helpers unexported.
finalizeEarlyClosureTypes :: Maybe [CoreBind] -> NameSet -> TypeEnv -> TypeEnv
finalizeEarlyClosureTypes mb_tidy exported type_env =
    mapNameEnv (mark_nocode_helper . update) type_env
  where
    elts = typeEnvElts type_env
    tidy_list = maybe [] stripBreakpointTicks mb_tidy
    tidy_env = mkBindEnv tidy_list
    tidy_by_name = mkNameEnv [(idName v, v) | (v, _) <- tidy_list]
    companions_of = companionTyConLookup [tc | ATyCon tc <- elts]
    early_fams =
      [ tc
      | AnId v <- elts
      , CheckedClosureSourceTableId owner table _ <- [idDetails v]
      , owner == idName v, isEarlyClosureTable table
      , tc <- table : companions_of v ]
    iface_tys = exportedInterfaceTypes exported type_env
    iface_tycons = tyConsOfTypes iface_tys
    exported_tycons
      | null early_fams = iface_tycons
      | otherwise       = survivingEarlyExportedTyCons early_fams iface_tys
    root_tables =
      [ table
      | AnId v <- elts
      , idName v `elemNameSet` exported
      , Just table <- [sourceTableId_maybe v] ] ++
      [ tc
      | ATyCon tc <- elts
      , tc `elementOfUniqSet` iface_tycons ]
    close_callees seen [] = seen
    close_callees seen (tc : rest)
      | tyConName tc `elemNameSet` seen = close_callees seen rest
      | otherwise =
          let kids = fromMaybe [] (sourceTableCallees tc)
          in close_callees (extendNameSetList seen [tyConName tc]) (kids ++ rest)
    reachable_tables = close_callees (mkNameSet []) root_tables
    closure_helper_ids = mkNameSet
      [ h
      | AnId v <- elts
      , CheckedClosureSourceTableId owner table helpers <- [idDetails v]
      , owner == idName v
      , idName v `elemNameSet` exported || any (`elementOfUniqSet` exported_tycons) (table : companions_of v)
      , h <- helpers ]
    mark_nocode_helper (AnId v)
      | isNothing mb_tidy
      , idName v `elemNameSet` closure_helper_ids ||
        maybe False ((`elemNameSet` reachable_tables) . tyConName) (sourceTableId_maybe v)
      = AnId (setIdExported v)
    mark_nocode_helper thing = thing
    update (AnId v)
      | CheckedClosureSourceTableId owner table helpers <- idDetails v
      , owner == idName v
      , isEarlyClosureTable table =
          let local_helpers = filter (\h -> nameModule_maybe h == nameModule_maybe owner) helpers
          in if not (any (`elementOfUniqSet` exported_tycons) (table : companions_of v)) &&
                any (not . (`elemNameSet` exported)) (owner : local_helpers)
               then AnId (setIdDetails v VanillaId)
               else
                 let floated = case lookupNameEnv tidy_by_name owner of
                       Just actual ->
                         [ nm | nm <- reachable tidy_env actual
                              , nm /= owner, isExternalName nm
                              , nameModule_maybe nm == nameModule_maybe owner
                              , isJust (lookupTypeEnv type_env nm) ]
                       Nothing -> []
                     merged = nameSetElemsStable $ mkNameSet $
                       filter (\nm -> nm /= owner && nameModule_maybe nm == nameModule_maybe owner)
                              (helpers ++ floated)
                     details' = case merged of
                       [] -> SourceTableId owner table NominalSourceTable
                       _  -> CheckedClosureSourceTableId owner table merged
                 in AnId (setIdDetails v details')
    update thing = thing

data StageBinds = StageBinds
  { sb_env       :: BindEnv
  , sb_by_name   :: NameEnv (Id, CoreExpr)
  , sb_scope_ops :: NameSet
  }

mkStageBinds :: [(Id, CoreExpr)] -> StageBinds
mkStageBinds pairs = StageBinds
  { sb_env       = bind_env
  , sb_by_name   = mkNameEnv [(idName v, p) | p@(v, _) <- reverse pairs]
  , sb_scope_ops = mkNameSet
      [ idName op
      | s <- directScopeShapes bind_env (map fst pairs)
      , let (ren, ext) = ss_ren_ops s
            sub_ops = case ss_subst_ops s of
              Just (sub, sext, mb_w) -> [sub, sext] ++ maybeToList mb_w
              Nothing -> []
      , op <- [ren, ext] ++ sub_ops ]
  }
  where
    bind_env = mkBindEnv pairs

-- | Require every early-staged lookup, traversal, and closure table to agree with desugared and final Core.
validateEarlyIndexedLookups :: Bool -> [CoreRule] -> [CoreBind]
                            -> Maybe [CoreBind] -> Maybe [CoreBind] -> TcM ()
validateEarlyIndexedLookups plugins_free rules desugared_binds mb_simplified mb_tidy = do
    env <- getGblEnv
    let candidates = earlySourceClosureEntries env
    unless (null candidates) $ do
      opts <- initSimpleOpts <$> getDynFlags
      hpc <- goptM Opt_Hpc
      let exported = availsToNameSet (tcg_exports env)
          known_tables = [(idName v, tc) | v <- typeEnvIds (tcg_type_env env), Just tc <- [sourceTableId_maybe v]]
          resolveTable v = lookup (idName v) known_tables <|> sourceTableId_maybe v
          raw_desugared = stripBreakpointTicks desugared_binds
          desugared = map (safeSimpleOptBind opts) raw_desugared
          raw_desug_sb = mkStageBinds raw_desugared
          desug_sb = mkStageBinds desugared
          mb_simp_sb = mkStageBinds . stripBreakpointTicks <$> mb_simplified
          mb_tidy_sb = mkStageBinds . stripBreakpointTicks <$> mb_tidy
          user_rule_owner Core.Rule{Core.ru_fn = fn, Core.ru_auto = auto, Core.ru_act = act}
            | not (auto || isNeverActive act) = Just fn
          user_rule_owner _ = Nothing
          active_rule_owners = mkNameSet (mapMaybe user_rule_owner rules)
      for_ candidates $ \(owner, table, helpers, companions) -> do
        callee_owners <- traverse sourceTableOwner (fromMaybe [] (sourceTableCallees table))
        let name = idName owner
            imported_callees =
              [ idName v | Just v <- callee_owners
                         , nameModule_maybe (idName v) /= nameModule_maybe name ]
            all_helpers = nameSetElemsStable (mkNameSet (helpers ++ imported_callees))
            watched = name : all_helpers
        case find (`elemNameSet` active_rule_owners) watched of
          Just ruled ->
            setSrcSpan (nameSrcSpan name) $
              addErr (TcRnRetainedQuantificationError (RetainedSourceRule ruled))
          Nothing -> do
            ok <- fmap isJust $ runMaybeT $ do
              guard (plugins_free && not hpc)
              axiom <- MaybeT (pure (closedFamilyTyConCoAxiom_maybe table))
              let expected = fromBranches (coAxiomBranches axiom)
              guard (not (null expected))
              simp_sb <- MaybeT (pure mb_simp_sb)
              checkEntry exported resolveTable expected table owner all_helpers companions
                         raw_desug_sb desug_sb simp_sb mb_tidy_sb
            unless ok $
              setSrcSpan (nameSrcSpan name) $
                addErr (TcRnNoSourceTable name)
      failIfErrsM
  where
    noUserIdRules v =
      all (\r -> case r of
                   Core.BuiltinRule{} -> True
                   _ -> Core.isAutoRule r || isNeverActive (Core.ruleActivation r))
          (idCoreRules v)

    lookupStage owner check_rules helpers sb = do
      (actual, rhs) <- MaybeT (pure (lookupNameEnv (sb_by_name sb) (idName owner)))
      guard (varType actual `eqType` varType owner)
      when check_rules $ do
        guard (noUserIdRules actual)
        for_ helpers $ \h ->
          when (nameModule_maybe h == nameModule_maybe (idName owner)) $ do
            (hv, _) <- MaybeT (pure (lookupNameEnv (sb_by_name sb) h))
            guard (noUserIdRules hv)
      pure (actual, rhs)

    checkOnlyCalls exported allow_floated allowed sb rhs =
      let extra v = isPatErrorHelper (sb_env sb) v ||
                    (allow_floated &&
                     isFloatedConstructorHelper ((`elemNameSet` exported) . idName) (sb_env sb) v)
      in guard (hasOnlyAllowedCalls extra allowed rhs)

    hasCompanionPrefix p = any (isPrefixOf p . occNameString . nameOccName . tyConName)

    companionCaseNames companions =
      case [tc | tc <- companions, "$case:" `isPrefixOf` occNameString (nameOccName (tyConName tc))] of
        [tc] | Just ax <- closedFamilyTyConCoAxiom_maybe tc -> Just (tyConName tc, coAxiomName ax)
        _ -> Nothing

    checkEntry exported resolveTable expected table owner helpers companions raw_desug desug simplified mb_tidy
      | hasCompanionPrefix "$closure:" companions || hasCompanionPrefix "$substMap:" companions = do
          let is_subst_map = hasCompanionPrefix "$substMap:" companions
          guard (length expected == 1 && length companions == (if is_subst_map then 1 else 3))
          (d_actual, d_rhs) <- lookupStage owner False helpers desug
          early_s <- MaybeT (pure (scopeFamilyInputs (sb_env desug) d_actual d_rhs))
          let exp_arity = if isJust (ss_sort early_s)
                            then 2 * ss_arity early_s + 3
                            else ss_arity early_s + 2
          guard ((not is_subst_map || isNothing (ss_sort early_s)) && tyConArity table == exp_arity)
          let checkClosure sb = do
                (actual, rhs) <- lookupStage owner True helpers sb
                shape <- MaybeT (pure (scopeFamilyInputs (sb_env sb) actual rhs))
                guard (sameScopeShape early_s shape)
          checkClosure simplified
          for_ mb_tidy $ \tidy ->
            when (idName owner `elemNameSet` exported) $
              checkClosure tidy
      | otherwise = do
          (d_actual, d_rhs) <- lookupStage owner False helpers desug
          (_, raw_rhs) <- lookupStage owner False helpers raw_desug
          let mb_cnames = companionCaseNames companions
              checkIndexedStage check_rules allow_floated sb = do
                (actual, raw_stage_rhs) <- lookupStage owner check_rules helpers sb
                let (mb_alias, rhs) = if allow_floated
                                        then resolveForwardCseAlias (sb_env sb) actual raw_stage_rhs
                                        else (Nothing, raw_stage_rhs)
                for_ mb_alias (guard . noUserIdRules)
                ci <- compileIndexedCore (sb_env sb) resolveTable mb_cnames (Just expected) actual rhs
                let w_names = maybe [] (pure . idName) (ci_mb_worker ci)
                    alias_names = maybe [] (pure . idName) mb_alias
                checkOnlyCalls exported allow_floated (idName actual : w_names ++ alias_names ++ helpers) sb rhs
                guard (length (ci_rows ci) == length expected)
                zipWithM_ sameIndexedRow expected [applyRow eq table | (eq, _) <- ci_rows ci]
                pure (ci_mb_worker ci)
          (do guard (null companions || (length companions == 1 && hasCompanionPrefix "$case:" companions))
              d_w <- checkIndexedStage False False desug
              checkOnlyCalls exported False (idName owner : maybeToList (idName <$> d_w) ++ helpers) raw_desug raw_rhs
              _ <- checkIndexedStage True True simplified
              for_ mb_tidy $ \tidy ->
                when (idName owner `elemNameSet` exported) $
                  () <$ checkIndexedStage True True tidy) <|>
            (do guard (null companions || (length companions == 1 && hasCompanionPrefix "$weaken:" companions))
                checkOnlyCalls exported False (idName owner : helpers) raw_desug raw_rhs
                checkScopeOp exported False d_actual helpers desug d_rhs
                (s_actual, s_rhs) <- lookupStage owner True helpers simplified
                checkScopeOp exported True s_actual helpers simplified s_rhs
                for_ mb_tidy $ \tidy ->
                  when (idName owner `elemNameSet` exported) $ do
                    (t_actual, t_rhs) <- lookupStage owner True helpers tidy
                    checkScopeOp exported True t_actual helpers tidy t_rhs)

    checkScopeOp exported allow_floated actual helpers sb rhs = do
      checkOnlyCalls exported allow_floated (idName actual : helpers) sb rhs
      guard (idName actual `elemNameSet` sb_scope_ops sb ||
             isJust (validStandaloneExt True sb actual rhs <|> validStandaloneExt False sb actual rhs))

    validStandaloneExt is_typed sb actual rhs = do
      (_, arg, _) <- peelScopeExtensionBinders is_typed rhs
      (idx, i_args) <- splitTyConApp_maybe (varType arg)
      let arity = if is_typed then 2 else 1
      guard (length i_args == arity)
      [c1, c2] <- pure (tyConDataCons idx)
      let (_, tau) = splitForAllTyVars (varType actual)
          (_, _, cod_raw) = splitFunTys tau
          (_, cod) = splitForAllTyVars cod_raw
          (_, _, out_ty) = splitFunTys cod
      (res_tc, r_args) <- splitTyConApp_maybe out_ty
      guard (length r_args == arity)
      let env = sb_env sb
          check_sext vc = () <$ (checkedScopeExtension is_typed env vc c1 c2 rhs <|>
                                 checkedScopeExtension is_typed env vc c2 c1 rhs)
      checkScopeRenameExtension is_typed env c1 c2 rhs <|>
        checkScopeRenameExtension is_typed env c2 c1 rhs <|>
        foldr (<|>) Nothing (map check_sext (tyConDataCons res_tc))

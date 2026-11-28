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
import Data.Foldable (asum, for_)
import Data.List (find, isPrefixOf, nubBy, sortBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)

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
import GHC.Core.FamInstEnv (emptyFamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch, normaliseTcAppWithFuel)
import GHC.Core.FVs (exprFreeVars, exprSomeFreeVarsList)
import GHC.Core.OpenWhnf (checkedMultiplicityCast, openDataConFieldTypes, openIndexedDataCon)
import GHC.Core.Predicate (mkNomEqPred, tyCoVarsOfTypesWellScoped)
import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.SimpleOpt (SimpleOpts, simpleOptExpr)
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, mkOpenSubst, substExpr, substTy)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCo.FVs (tyConsOfType, tyConsOfTypes)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(MRefl), Scaled(..), Type(..), scaledThing)
import GHC.Core.TyCon
  ( AlgTyConFlav(VanillaAlgTyCon), FamTyConFlav(ClosedTypeFamilyTyCon), Injectivity(NotInjective)
  , TyCon, TyConBinder, TyConBndrVis(AnonTCB, NamedTCB), closedFamilyTyConCoAxiom_maybe
  , isAlgTyCon, isNewTyCon, isPromotedDataCon, isPromotedDataCon_maybe, isTypeDataTyCon
  , mkAlgTyCon, mkAnonTyConBinder, mkFamilyTyCon, mkLevPolyDataTyConRhs, mkNamedTyConBinder
  , mkNamedTyConBinders, mkTyConKind, mkTyConKindWithMatchability, synTyConRhs_maybe, tyConArity
  , tyConBinders, tyConDataCons, tyConKind, tyConName, tyConRepName_maybe, unwrapNewTyConEtad_maybe )
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
  ( hasSourceMethodRule, isNominalStructuralSource, ordinaryImport, publicCaseType
  , sourceEquationDatatypes, sourceTableCallees, sourceTableOwner, tidySourceAxBranch, visibleSourceMethodRules )
import GHC.Tc.TyCl.Build (buildDataCon, newImplicitBinder, newTyConRepName)
import GHC.Tc.Types (tcVisibleOrphanMods)
import GHC.Tc.Utils.Env (tcLookupIdMaybe)
import GHC.Tc.Utils.Instantiate (freshenTyVarBndrs)
import GHC.Tc.Utils.Monad
  ( SourceCheckState(..), TcGblEnv(..), TcM, addErr, failIfErrsM, getGblEnv, getGlobalRdrEnv, getTopEnv, goptM, liftIO
  , newSysName, readTcRef, setSrcSpan )
import GHC.Tc.Utils.TcType (isTyFamFree)
import GHC.Types.Avail (availsToNameSet)
import GHC.Types.Basic (SourceTableOrigin(NominalSourceTable))
import GHC.Types.Id
  ( Id, idCoreRules, idDetails, idName, isDataConWorkId_maybe, isDataConWrapId_maybe, isJoinId
  , mkLocalCoVar, mkLocalId, setIdExported, sourceTableId_maybe )
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
import GHC.Types.Name.Set (NameSet, elemNameSet, emptyNameSet, extendNameSetList, mkNameSet, nameSetElemsStable)
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
RS-01-23), `GHC.Tc.Gen.SourceClosure` certifies unary (`UTerm n`, `Fin n`) and
binary (`Term g a`, `Lookup g a`) renaming and substitution chains by
defunctionalizing inline replacement callbacks into a private closure GADT
(`$closure:`) interpreted by nominal closed type families (`$apply:`, `$lookup:`).

When a provider exports the underlying operations (`ext`, `rename`, `exts`,
`subst` or their unary counterparts), each operation receives its own nominal
closed type family over unmatchable (`%U`, GHC Proposal #242; Kiss et al.,
ICFP 2019) callback kinds. Every generated equation preserves call-by-need
definedness (Johann and Voigtländer, Fundam. Inform. 2006; Schmidt-Schauß,
Sabel, and Machkasova, LMCS 2015):
  1. Desugared Core and post-Simplifier Core must produce alpha-identical
     `CoAxBranch` equations (`sameIndexedRow`) or identical traversal recipes.
  2. Constructor fields and scrutinized branches must stay lazy.
  3. Referenced source datatypes and constructors must be exported or ordinarily
     imported, with no active user `RULES` on the owner or reachable helpers.
  4. GHCi `Breakpoint` ticks and dead lifted local bindings are ignored during
     inspection while leaving the executable Core untouched.
-}

{- Note [Indexed GADT decision-tree and continuation compilation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Scope-indexed functions outside inline-closure chains share `compileIndexedCore`:
  1. Mixed `forall`/value telescopes (`splitMixedPiTys`) preserve `Inferred` and
     `Specified` binders, rank-2 callback domains, and point-free unary lazy
     constructors (`ids = UVar`, `shift = FS`).
  2. Single-constructor existential input packages (`Marked g -> Found g`)
     unwrap a local worker (`$wfindMarked`) when all fields forward in order.
  3. Exhaustive `Case (Var v)` trees refine nominal GADT `EqSpec` equalities
     (`tcUnifyTys alwaysBindTv`; Cockx and Abel, ICFP 2018; Maranget, ML 2008).
     Missing or `patError` branches are refuted when constructor unification
     leaves another value parameter uninhabited (`dataConCannotMatch`).
  4. A direct single-constructor existential `Case` on a structurally smaller
     recursive call compiles to a private `$case:` continuation family.
  5. Leaf expressions support bound variables, neutral callback applications,
     lazy promoted constructors, floated constructor helpers, certified
     `SourceTable` calls (including `-O2` inlined callback verification against
     `normaliseTcApp`), and structurally decreasing recursive calls.
-}

type BindEnv = VarEnv CoreExpr
type MixedPiSpec = Either (ForAllTyFlag, TyVar) Type

mkBindEnv :: [(Id, CoreExpr)] -> BindEnv
mkBindEnv = mkVarEnv . reverse

isSourceBreakpoint :: GenTickish pass -> Bool
isSourceBreakpoint Breakpoint{} = True
isSourceBreakpoint _            = False

stripBreakpointTicks :: [CoreBind] -> [(Id, CoreExpr)]
stripBreakpointTicks binds = [(v, stripTicksE isSourceBreakpoint rhs) | (v, rhs) <- Core.flattenBinds binds]

safeSimpleOptBind :: SimpleOpts -> (Id, CoreExpr) -> (Id, CoreExpr)
safeSimpleOptBind opts (v, rhs)
  | hasNativeDependentExpr (Core.Type (varType v)) || hasExistentialExpr rhs || hasNativeDependentExpr rhs = (v, rhs)
  | otherwise = (v, simpleOptExpr opts rhs)

isValueVar :: Id -> Bool
isValueVar v = isId v && not (isCoVar v)

isConstructorId :: Id -> Bool
isConstructorId v = isJust (isDataConWorkId_maybe v <|> isDataConWrapId_maybe v)

lazyConstructor :: DC.DataCon -> Bool
lazyConstructor con =
    not (any DC.isBanged (DC.dataConImplBangs con)) &&
    null (DC.dataConRepArgBinders con) && null (DC.dataConStupidTheta con)

constructorOnlyExpr :: CoreExpr -> Bool
constructorOnlyExpr expr = maybe False (all constructorOnlyExpr . snd) (constructorCall expr)

isPatErrorExpr :: BindEnv -> CoreExpr -> Bool
isPatErrorExpr bindings = check (2 :: Int)
  where
    check 0 _ = False
    check d expr = case snd (collectMixedBinders expr) of
      Core.Case scrut _ _ [] -> check d scrut
      body | (Core.Var v, _) <- Core.collectArgs body ->
               v `hasKey` patErrorIdKey || maybe False (check (d - 1)) (lookupVarEnv bindings v)
           | otherwise -> False

isPatErrorHelper :: BindEnv -> Id -> Bool
isPatErrorHelper bind_env v = maybe False (isPatErrorExpr bind_env) (lookupVarEnv bind_env v)

isFloatedConstructorHelper :: (Id -> Bool) -> BindEnv -> Id -> Bool
isFloatedConstructorHelper is_public bind_env v
  | not (is_public v), Just decl <- lookupVarEnv bind_env v =
      (case function decl of { Just (_ : _, [], body) -> constructorOnlyExpr body; _ -> False }) ||
      isJust (validConstructorAliasBody decl)
  | otherwise = False

hasOnlyAllowedCalls :: (Id -> Bool) -> [Name] -> CoreExpr -> Bool
hasOnlyAllowedCalls allow_extra allowed =
    null . exprSomeFreeVarsList
      (\v -> isValueVar v && not (v `hasKey` patErrorIdKey) && idName v `notElem` allowed &&
             not (isConstructorId v) && not (allow_extra v))

reachable :: BindEnv -> Id -> [Name]
reachable bind_env root = walk emptyVarSet [] [root]
  where
    walk !_ acc [] = acc
    walk !seen acc (v : rest)
      | v `elemVarSet` seen = walk seen acc rest
      | Just body <- lookupVarEnv bind_env v =
          let fvs = exprSomeFreeVarsList (\u -> u `elemVarEnv` bind_env && not (isPatErrorHelper bind_env u)) body
          in walk (extendVarSet seen v) (idName v : acc) (fvs ++ rest)
      | otherwise = walk seen acc rest

expandTypeHelper :: BindEnv -> CoreExpr -> CoreExpr
expandTypeHelper bindings rhs
  | (Core.Var helper, args) <- Core.collectArgs rhs
  , Just decl <- lookupVarEnv bindings helper
  , Just (tvs, [], body) <- function decl
  , Just tys <- traverse indexedTypeArg args, length tvs == length tys
  , let in_scope = mkInScopeSet (foldl' unionVarSet (exprFreeVars body) (map (exprFreeVars . Core.Type) tys))
        sub = foldl' (\s (tv, ty) -> extendTvSubst s tv ty) (mkEmptySubst in_scope) (zip tvs tys)
        expanded = substExpr sub body
  , exprType expanded `eqType` exprType rhs = expanded
  | otherwise = rhs

indexedTypeArg :: CoreExpr -> Maybe Type
indexedTypeArg (Core.Type ty) = Just ty
indexedTypeArg _              = Nothing

isSafeMultOrReflCo :: Coercion -> Bool
isSafeMultOrReflCo co
  | coercionLKind co `eqType` coercionRKind co || checkedMultiplicityCast co = True
isSafeMultOrReflCo (UnivCo _ _ lhs rhs _)
  | Just (FTF_T_T, ma, mult, arg, res) <- splitFunTy_maybe lhs
  , Just (FTF_T_T, ma', mult', arg', res') <- splitFunTy_maybe rhs =
      okMa ma ma' && okMult mult mult' && arg `eqType` arg' && res `eqType` res'
  where
    okMa a b = a `eqType` b || (a `eqType` matchableDataConTy && b `eqType` unmatchableDataConTy)
    okMult a b = a `eqType` b || (isOneTy a && isManyTy b)
isSafeMultOrReflCo (SubCo c) = isSafeMultOrReflCo c
isSafeMultOrReflCo (TransCo c1 c2) = isSafeMultOrReflCo c1 && isSafeMultOrReflCo c2
isSafeMultOrReflCo ForAllCo{ fco_tcv = tv, fco_visL = l, fco_visR = r, fco_ma = ma, fco_kind = MRefl, fco_body = b } =
    isTyVar tv && (eqForAllVis l r || (isInvisiblePiFlag l && isInvisiblePiFlag r)) &&
    (coercionLKind ma `eqType` coercionRKind ma ||
     (coercionLKind ma `eqType` matchableDataConTy && coercionRKind ma `eqType` unmatchableDataConTy)) &&
    isSafeMultOrReflCo b
isSafeMultOrReflCo FunCo{ fco_afl = FTF_T_T, fco_afr = FTF_T_T, fco_ma = cm, fco_mult = cw, fco_arg = c1, fco_res = c2 } =
    (coercionLKind cm `eqType` coercionRKind cm ||
     (coercionLKind cm `eqType` matchableDataConTy && coercionRKind cm `eqType` unmatchableDataConTy)) &&
    (coercionLKind cw `eqType` coercionRKind cw || (isOneTy (coercionLKind cw) && isManyTy (coercionRKind cw))) &&
    coercionLKind c1 `eqType` coercionRKind c1 && coercionLKind c2 `eqType` coercionRKind c2
isSafeMultOrReflCo _ = False

weakeningBody :: CoreExpr -> Maybe CoreExpr
weakeningBody (Core.Cast body co) = guard (isSafeMultOrReflCo co) *> weakeningBody body
weakeningBody body                = Just body

caseCoercion :: [Var] -> Coercion -> Bool
caseCoercion scope = go
  where
    go (Refl _)            = True
    go (GRefl _ _ MRefl)   = True
    go (CoVarCo cv)        = cv `elem` scope
    go (TyConAppCo _ _ cs) = all go cs
    go (AppCo a b)         = go a && go b
    go (TransCo a b)       = go a && go b
    go (SubCo c)           = go c
    go (SymCo c)           = go c
    go (SelCo _ c)         = go c
    go _                   = False

branchBody :: [Var] -> CoreExpr -> Maybe CoreExpr
branchBody binders (Core.Cast body co) = guard (caseCoercion (filter isCoVar binders) co) *> branchBody binders body
branchBody _ body                      = Just body

isDeadLocalLet :: Var -> CoreExpr -> Bool
isDeadLocalLet b e = isLocalId b && isLiftedTypeKind (typeKind (varType b)) && not (b `elemVarSet` exprFreeVars e)

function :: CoreExpr -> Maybe ([TyVar], [Id], CoreExpr)
function expr = do
    let (binders, body) = collect [] expr
        (ty_binders, tm_binders) = span isTyVar binders
    guard (all isValueVar tm_binders)
    pure (ty_binders, tm_binders, body)
  where
    collect bs (Core.Lam b body) = collect (b : bs) body
    collect bs (Core.Let (Core.NonRec b _) body) | isDeadLocalLet b body = collect bs body
    collect bs body = (reverse bs, body)

collectMixedBinders :: CoreExpr -> ([Var], CoreExpr)
collectMixedBinders = go []
  where
    go acc (Core.Lam b e) | isTyVar b || isValueVar b = go (b : acc) e
    go acc (Core.Let (Core.NonRec b _) e) | isDeadLocalLet b e = go acc e
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
    (head_expr, fields) <- weakeningBody rhs >>= application
    Core.Var make <- weakeningBody head_expr
    con <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
    pure (con, fields)

caseOn :: Id -> CoreExpr -> Maybe [(DC.DataCon, [Var], CoreExpr)]
caseOn target (Core.Case (Core.Var scrutinee) _ _ alts) | target == scrutinee =
    traverse (\case Core.Alt (Core.DataAlt con) bs rhs -> Just (con, bs, rhs); _ -> Nothing) alts
caseOn _ _ = Nothing

unwrapLocalCallbackLets :: CoreExpr -> CoreExpr
unwrapLocalCallbackLets expr
  | hasExistentialExpr expr || hasNativeDependentExpr expr = expr
  | otherwise = unwrapCallbackLetsGo expr

unwrapCallbackLetsGo :: CoreExpr -> CoreExpr
unwrapCallbackLetsGo (Core.Let (Core.NonRec b b_rhs) inner)
  | isLocalId b, isValueVar b, VanillaId <- idDetails b, null (idCoreRules b)
  , not (b `elemVarSet` exprFreeVars b_rhs)
  = let sub = mkOpenSubst (mkInScopeSet (exprFreeVars inner `unionVarSet` exprFreeVars b_rhs)) [(b, b_rhs)]
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
      | (Core.Lam b body, Core.Type ty : rest) <- collect [] unwrapped, isTyVar b
      = let sub = extendTvSubst (mkEmptySubst (mkInScopeSet (exprFreeVars body `unionVarSet` exprFreeVars (Core.Type ty)))) b ty
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
        prefix_expr = foldl' Core.App fn prefix_args
  , and (zipWith matchesEta ebndrs suffix_args)
  , all (not . (`elemVarSet` exprFreeVars prefix_expr)) ebndrs
  = peelEtaCall prefix_expr
  | otherwise = raw_e
  where
    matchesEta b (Core.Type (TyVarTy tv)) = isTyVar b && tv == b
    matchesEta b arg | isValueVar b, Just (Core.Var v) <- weakeningBody arg = v == b
    matchesEta _ _ = False

peelEtaTarget :: CoreExpr -> Maybe Id
peelEtaTarget raw_e = case weakeningBody (peelEtaCall raw_e) of
  Just (Core.Var fn) -> Just fn
  _                  -> Nothing

resolveForwardCseAlias :: BindEnv -> Id -> CoreExpr -> (Maybe Id, CoreExpr)
resolveForwardCseAlias bind_env actual rhs
  | Just av <- peelEtaTarget rhs, idName av /= idName actual
  , varType av `eqType` varType actual, Just ar <- lookupVarEnv bind_env av = (Just av, ar)
  | otherwise = (Nothing, rhs)

validConstructorAliasBody :: CoreExpr -> Maybe DC.DataCon
validConstructorAliasBody raw = do
    expr <- weakeningBody raw
    let (bndrs, raw_body) = collectMixedBinders (betaReduceTypeApps expr)
        val_bndrs = filter isValueVar bndrs
    body <- weakeningBody raw_body
    guard (length bndrs == length (filter isTyVar bndrs) + length val_bndrs && length val_bndrs <= 1)
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
          (more, res) <- splitMixedPiTys (foldr (\(m, b) acc -> ForAllTy m b acc) inner rest_bndrs)
          pure (Left (flag, tv) : more, res)
    (_ : _, _) -> Nothing
    ([], tau)
      | Just (FTF_T_T, match, mult, dom, cod) <- splitFunTy_maybe tau
      , match `eqType` unmatchableDataConTy, isManyTy mult -> do
          (more, res) <- splitMixedPiTys cod
          pure (Right dom : more, res)
      | Just _ <- splitFunTy_maybe tau -> Nothing
      | otherwise -> Just ([], coreFullView tau)

hasTypeDataTyCon :: TyCon -> Bool
hasTypeDataTyCon tc
  | isTypeDataTyCon tc = True
  | Just (_, rhs, _) <- unwrapNewTyConEtad_maybe tc
  , Just (tc', _) <- splitTyConApp_maybe rhs
  = hasTypeDataTyCon tc'
  | otherwise = False

isRuntimeAlgTyCon :: TyCon -> Bool
isRuntimeAlgTyCon tc = isAlgTyCon tc && not (isNewTyCon tc) && not (hasTypeDataTyCon tc)

isIndexedAlgDomain :: Type -> Bool
isIndexedAlgDomain ty
  | Just (tc, args@(_ : _)) <- splitTyConApp_maybe ty
  = isRuntimeAlgTyCon tc && (any (not . DC.isVanillaDataCon) (tyConDataCons tc) || any (not . isLiftedTypeKind . typeKind) args)
  | otherwise = False

isIndexedCallbackTy :: Type -> Bool
isIndexedCallbackTy ty
  | Just (pis, cod) <- splitMixedPiTys ty, isIndexedAlgDomain cod = case pis of
      [Right dom]         -> isIndexedAlgDomain dom
      [Left _, Right dom] -> isIndexedAlgDomain dom
      _                   -> False
  | otherwise = False

-- | Select functions whose signatures can head an early indexed lookup,
-- GADT structural traversal, or scope-indexed closure chain.
earlyClosureCandidateType :: Type -> Bool
earlyClosureCandidateType ty
  | Just (pis, out_ty) <- splitMixedPiTys ty
  , let val_doms = [d | Right d <- pis]
  , Right first_dom : _ <- [Right d | Right d <- pis]
  , isLiftedTypeKind (typeKind out_ty)
  = any isIndexedAlgDomain val_doms || isIndexedCallbackTy first_dom
  | otherwise = False

companionTyConLookup :: [TyCon] -> Id -> [TyCon]
companionTyConLookup tcs =
    let !by_occ = mkOccEnv_C (++) [(nameOccName (tyConName tc), [(nameModule_maybe (tyConName tc), tc)]) | tc <- tcs]
    in \fun ->
      let mod_mb = nameModule_maybe (idName fun)
          occ_str = occNameString (nameOccName (idName fun))
          pick p = lookupOccEnv by_occ (mkTcOcc (p ++ occ_str)) >>= lookup mod_mb
      in mapMaybe pick ["$closure:", "$apply:", "$lookup:", "$weaken:", "$substMap:", "$case:"]

mkClosureTableAxiom :: Bool -> Name -> TcM Name
mkClosureTableAxiom False root = newImplicitBinder root mkInstTyCoOcc
mkClosureTableAxiom True root  = newImplicitBinder root (mkInstTyCoOcc . mkTcOcc . ("early:" ++) . occNameString)

isEarlyClosureTable :: TyCon -> Bool
isEarlyClosureTable = maybe False (("D:early:" `isPrefixOf`) . occNameString . nameOccName . coAxiomName)
                    . closedFamilyTyConCoAxiom_maybe

freshVar :: String -> Kind -> TcM TyVar
freshVar str kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc str)

freshLocalId :: String -> Type -> TcM Id
freshLocalId str ty = (\nm -> mkLocalId nm manyDataConTy ty) <$> newSysName (mkVarOcc str)

mkFamily :: Name -> Name -> [TyConBinder] -> Kind -> [CoAxBranch] -> TyCon
mkFamily name axname binders result branches =
    let family = mkFamilyTyCon name (mkTyConKindWithMatchability unmatchableDataConTy binders result)
                    binders 0 result Nothing (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal axname family (map tidySourceAxBranch branches)
    in family

mkBranch :: [TyVar] -> [Type] -> Type -> CoAxBranch
mkBranch vars lhs rhs = mkCoAxBranch vars [] [] lhs rhs (map (const Nominal) vars) noSrcSpan

renamingBranch :: [Type] -> Type -> CoAxBranch
renamingBranch lhs = mkBranch (tyCoVarsOfTypesWellScoped lhs) lhs

validBranch :: CoAxBranch -> Bool
validBranch branch =
    all isTyFamFree lhs &&
    tyCoVarsOfTypes (coAxBranchRHS branch : lhs ++ map varType tvs) `subVarSet` mkVarSet tvs
  where
    lhs = coAxBranchLHS branch
    tvs = coAxBranchTyVars branch

normaliseClosedTables :: Type -> Type
normaliseClosedTables = go . coreFullView
  where
    go (AppTy a b) = AppTy (normaliseClosedTables a) (normaliseClosedTables b)
    go (TyConApp tc tys) =
      let tys' = map normaliseClosedTables tys
      in if isJust (closedFamilyTyConCoAxiom_maybe tc)
           then let red = reductionReducedType (normaliseTcAppWithFuel 2000 emptyFamInstEnvs Nominal tc tys')
                in if red `eqType` TyConApp tc tys' then TyConApp tc tys' else normaliseClosedTables red
           else TyConApp tc tys'
    go (FunTy af ma mult arg res) =
      FunTy af (normaliseClosedTables ma) (normaliseClosedTables mult)
        (normaliseClosedTables arg) (normaliseClosedTables res)
    go (ForAllTy ma bndr body) = ForAllTy (normaliseClosedTables ma) bndr (normaliseClosedTables body)
    go ty = ty

sameIndexedRow :: CoAxBranch -> CoAxBranch -> MaybeT TcM ()
sameIndexedRow left right = do
    subst <- MaybeT (pure (tcMatchTys (coAxBranchLHS left) (coAxBranchLHS right)))
    let extractVars v = do
          img <- lookupTyVar subst v
          case getTyVar_maybe img of
            Just tv -> Just [tv]
            Nothing -> do
              (tc, tc_args) <- splitTyConApp_maybe (varType v)
              guard (isRuntimeAlgTyCon tc)
              [only_dc] <- Just [c | c <- tyConDataCons tc, not (DC.dataConCannotMatch tc_args c)]
              (prom_tc, p_args) <- splitTyConApp_maybe img
              guard (prom_tc == DC.promoteDataCon only_dc && length p_args >= DC.dataConSourceArity only_dc)
              let fld_tys = drop (length p_args - DC.dataConSourceArity only_dc) p_args
              traverse getTyVar_maybe fld_tys
    renamed_lists <- MaybeT (pure (traverse extractVars (coAxBranchTyVars left)))
    let renamed = concat renamed_lists
        lhs_rhs = normaliseClosedTables (substTyUnchecked subst (coAxBranchRHS left))
        rhs_rhs = normaliseClosedTables (coAxBranchRHS right)
    guard (length renamed == length (coAxBranchTyVars right) &&
           mkVarSet renamed == mkVarSet (coAxBranchTyVars right) &&
           lhs_rhs `eqType` rhs_rhs)

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
    let ma_args = replicate (DC.promotedDataConMatchabilityArity con) unmatchableDataConTy
        value   = mkTyConApp (DC.promoteDataCon con) (ma_args ++ source ++ fields)
        vanilla = do
          expected <- traverse (`lookup` zip (DC.dataConUnivTyVars con) parameters) (DC.dataConUserTyVars con)
          guard (eqTypes expected source)
          openDataConFieldTypes con parameters
    expected <- vanilla <|> (\(_, _, tys) -> tys) <$> openIndexedDataCon con kind source
    value <$ guard (eqTypes (map typeKind fields) expected && typeKind value `eqType` kind)

mkClosureOwnerDetails :: Id -> TyCon -> [Id] -> IdDetails
mkClosureOwnerDetails owner table helpers =
    case nameSetElemsStable $ mkNameSet
           [idName h | h <- helpers, idName h /= idName owner, nameModule_maybe (idName h) == nameModule_maybe (idName owner)] of
      []  -> SourceTableId (idName owner) table NominalSourceTable
      nms -> CheckedClosureSourceTableId (idName owner) table nms

publishRenamingTable :: Bool -> (TyCon -> Bool) -> Id -> [Id] -> [TyConBinder] -> Kind
                     -> (TyCon -> Maybe [CoAxBranch]) -> MaybeT TcM (Id, [TyCon])
publishRenamingTable is_early public owner helpers binders result rows = do
    root <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (mkClosureTableAxiom is_early root)
    let equations = rows table
        table = mkFamily root axiom binders result (fromMaybe [] equations)
    guard (tyConKind table `eqType` varType owner)
    branches <- MaybeT (pure equations)
    guard (all validBranch branches && all public (sourceEquationDatatypes [table]))
    pure (setIdDetails owner (mkClosureOwnerDetails owner table helpers), [table])

-- ---------------------------------------------------------------------------
-- Unified indexed GADT decision-tree & continuation compiler (SC2 + SC5)
-- ---------------------------------------------------------------------------

data IndexedPattern
  = IndexedVariable Id
  | IndexedConstructor Type DC.DataCon [Type] [IndexedPattern]

data IndexedResultCase = IndexedResultCase [(CoreExpr, Id, IndexedPattern, [Var])] CoreExpr
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
    zipWithM_ (\spec b -> let (tv, ty) = DC.eqSpecPair spec; co = CoVarCo b in
      guard (coercionRole co == Nominal && coercionLKind co `eqType` instantiate (mkTyVarTy tv) &&
             coercionRKind co `eqType` instantiate ty)) specs proofs
    pure (IndexedConstructor kind con (map (instantiate . mkTyVarTy) (DC.dataConUserTyVars con))
            (map IndexedVariable values), proofs)

indexedPatternTypes :: (Type -> Type) -> IndexedPattern -> MaybeT TcM (Type, [(Id, Type)])
indexedPatternTypes specialize pat = do
    let ids = indexedFields pat
    variables <- lift $ traverse (freshVar "field" . specialize . varType) ids
    let fields = zip ids (mkTyVarTys variables)
        write_pat (IndexedVariable v) = lookup v fields
        write_pat (IndexedConstructor kind con source cs) =
          checkedPromoted con (specialize kind) (map specialize source) =<< traverse write_pat cs
    packed <- MaybeT (pure (write_pat pat))
    pure (packed, fields)

indexedConstructor :: (Type -> Type) -> Id -> [CoreExpr] -> Maybe (DC.DataCon, [Type], [CoreExpr])
indexedConstructor specialize callee args = do
    let (erased, values) = span Core.isTyCoArg args
        (tys, proofs) = span (isJust . indexedTypeArg) erased
    types <- traverse (fmap specialize . indexedTypeArg) tys
    let okProof (Core.Coercion co) = coercionRole co == Nominal &&
          specialize (coercionLKind co) `eqType` specialize (coercionRKind co)
        okProof _ = False
    (con, supplied) <- case isDataConWrapId_maybe callee of
      Just con -> (con, types) <$ guard (null proofs)
      Nothing -> do
        con <- isDataConWorkId_maybe callee
        let variables = DC.dataConUnivTyVars con ++ DC.dataConExTyCoVars con
        (_, _, specs, [], _, _) <- pure (DC.dataConFullSig con)
        guard (length variables == length types && length specs == length proofs && all okProof proofs)
        (con,) <$> traverse (`lookup` zip variables types) (DC.dataConUserTyVars con)
    guard (length supplied == length (DC.dataConUserTyVars con))
    pure (con, supplied, values)

indexedResultCase :: CoreExpr -> Maybe IndexedResultCase
indexedResultCase expr = go [] expr
  where
    go acc (Core.Case call binder kind [Core.Alt (Core.DataAlt con) binders body])
      | Just (tc, args) <- splitTyConApp_maybe (varType binder)
      , isRuntimeAlgTyCon tc
      , [only_con] <- [c | c <- tyConDataCons tc, not (DC.dataConCannotMatch args c)]
      , only_con == con
      , varType binder `eqType` exprType call
      , kind `eqType` exprType body
      , Just (res_pat, eq_vars) <- indexedAlt binder con binders
      = let acc' = acc ++ [(call, binder, res_pat, eq_vars)]
        in go acc' body <|> Just (IndexedResultCase acc' body)
    go acc@(_ : _) body = Just (IndexedResultCase acc body)
    go [] _ = Nothing

-- | Compile a mixed-telescope indexed Core definition into closed-family equations.
-- See Note [Indexed GADT decision-tree and continuation compilation].
compileIndexedCore :: BindEnv -> (Id -> Maybe TyCon) -> Maybe (Name, Name) -> Maybe [CoAxBranch]
                   -> Id -> CoreExpr -> MaybeT TcM CompiledIndexed
compileIndexedCore bindings resolveTable mb_case_names mb_expected owner rhs = do
    (pis, sig_out_ty) <- MaybeT (pure (splitMixedPiTys (varType owner)))
    let val_sig_doms = [d | Right d <- pis]
    guard (not (null val_sig_doms) &&
           all (isLiftedTypeKind . typeKind) (sig_out_ty : val_sig_doms) &&
           any (\d -> isIndexedAlgDomain d || isIndexedCallbackTy d) val_sig_doms)
    (hdr_binders, hdr_res) <- lift (buildHeaderTelescope pis sig_out_ty)
    dummy_name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    dummy_ax <- lift (newImplicitBinder dummy_name mkInstTyCoOcc)
    let header = mkFamily dummy_name dummy_ax hdr_binders hdr_res []
        init_in_scope = mkInScopeSet (tyCoVarsOfTypes [varType owner, exprType rhs])
    compileConAlias pis hdr_binders hdr_res header <|>
      compileFullTree pis sig_out_ty init_in_scope hdr_binders hdr_res header
  where
    compileConAlias pis hdr_binders hdr_res header = do
      let (left_pis, rest_pis) = span (\case { Left _ -> True; _ -> False }) pis
      [Right _] <- pure rest_pis
      dc <- MaybeT (pure (validConstructorAliasBody rhs))
      let hdr_tvs = [tv | Bndr tv (NamedTCB _) <- hdr_binders]
      [Bndr arg_tv AnonTCB] <- pure [b | b@(Bndr _ AnonTCB) <- hdr_binders]
      guard (length hdr_tvs == length left_pis && not (null hdr_tvs) && length hdr_tvs <= 3 &&
             mkVarSet hdr_tvs `subVarSet` tyCoVarsOfTypes [varType arg_tv, hdr_res])
      out <- MaybeT (pure (conAt dc hdr_res [mkTyVarTy arg_tv]))
      let lhs_tys = mkTyVarTys (hdr_tvs ++ [arg_tv])
          eq = IndexedEquation (tyCoVarsOfTypesWellScoped lhs_tys) lhs_tys (const out)
      guard (isScopeConAlias (varType arg_tv) hdr_res eq header && validBranch (applyRow eq header))
      pure (CompiledIndexed hdr_binders hdr_res [(eq, Nothing)] Nothing Nothing [])

    compileFullTree pis sig_out_ty init_in_scope hdr_binders hdr_res header = do
      guard (exprType rhs `eqType` varType owner)
      (bndrs, inst_out_ty, body0) <- peelMixedBinders pis sig_out_ty (mkEmptySubst init_in_scope) (betaReduceTypeApps rhs)
      guard (exprType body0 `eqType` inst_out_ty)
      let val_params = [v | Right v <- bndrs]
          prenex_tvs = [tv | Left (_, tv) <- takeWhile (\case { Left _ -> True; _ -> False }) bndrs]
          is_prenex = length prenex_tvs + length val_params == length bndrs
      (mb_pack, mb_worker, pats0, tree0) <- unwrapPackWorker is_prenex prenex_tvs val_params body0
      let unwrapScrutCast eq_scope = go_scrut
            where
              go_scrut (Core.Var v) = Just v
              go_scrut (Core.Cast e co)
                | isSafeMultOrReflCo co || caseCoercion eq_scope co = go_scrut e
              go_scrut _ = Nothing

          freshAltBinders scrut_ty dc = do
            (tc, args) <- MaybeT (pure (splitTyConApp_maybe scrut_ty))
            let (univs, exs, specs, thetas, fields, _) = DC.dataConFullSig dc
            guard (tc == DC.dataConTyCon dc && null thetas && length args == length univs &&
                   all isTyVar exs && lazyConstructor dc)
            let base_sub = zip univs args
            locals <- lift $ foldM (\acc tv -> do
              let k = substTyWith (map fst (base_sub ++ acc)) (map snd (base_sub ++ acc)) (varType tv)
              tv' <- freshVar "ex" k
              pure (acc ++ [(tv, mkTyVarTy tv')])) [] exs
            let inst = substTyWith (univs ++ exs) (args ++ map snd locals)
            eq_vars <- lift $ traverse (\spec -> do
              let (tv, rty) = DC.eqSpecPair spec
              nm <- newSysName (mkVarOcc "co")
              pure (mkLocalCoVar nm (mkNomEqPred (inst (mkTyVarTy tv)) (inst rty)))) specs
            val_vars <- lift $ traverse (\(Scaled _ fty) -> freshLocalId "fld" (inst fty)) fields
            pure ([tv | Just tv <- map (getTyVar_maybe . snd) locals] ++ eq_vars ++ val_vars)

          simplifyTreeExpr known_alts eq_scope raw_e
            | hasExistentialExpr raw_e || hasNativeDependentExpr raw_e = raw_e
            | otherwise = go_simp raw_e
            where
              substPairs prs target
                | null prs  = target
                | otherwise =
                    let free = foldl' unionVarSet (exprFreeVars target) [exprFreeVars r | (_, r) <- prs]
                    in substExpr (mkOpenSubst (mkInScopeSet free) prs) target

              go_simp (Core.Let (Core.NonRec cv (Core.Coercion co)) inner)
                | isCoVar cv
                = go_simp (substPairs [(cv, Core.Coercion co)] inner)
              go_simp (Core.Let (Core.NonRec b b_rhs) inner)
                | isLocalId b, isValueVar b
                , (case idDetails b of { VanillaId -> True; _ -> isJoinId b })
                , null (idCoreRules b), not (b `elemVarSet` exprFreeVars b_rhs)
                = go_simp (substPairs [(b, b_rhs)] inner)
              go_simp (Core.App f a) =
                case go_simp f of
                  Core.Lam p body
                    | isValueVar p, not (Core.isTyCoArg a)
                    , not (p `elemVarSet` exprFreeVars body) || (case a of { Core.Var _ -> True; _ -> False })
                    -> go_simp (substPairs [(p, go_simp a)] body)
                  Core.Lam tv body
                    | isTyVar tv, Core.Type ty <- a
                    -> go_simp (substPairs [(tv, Core.Type ty)] body)
                  f' -> Core.App f' (go_simp a)
              go_simp (Core.Cast inner co) = Core.Cast (go_simp inner) co
              go_simp (Core.Case s_e inner_bndr r_ty inner_alts)
                | Just s_v <- unwrapScrutCast eq_scope (go_simp s_e)
                , Just (known_dc, outer_bs) <- lookup s_v known_alts =
                    case find (\case { Core.Alt (Core.DataAlt dc) bs _ -> dc == known_dc && length bs == length outer_bs; _ -> False }) inner_alts of
                      Just (Core.Alt _ inner_bs inner_rhs) ->
                        let prs = [(inner_bndr, Core.Var s_v) | inner_bndr /= s_v] ++
                                  zip inner_bs (map Core.varToCoreExpr outer_bs)
                        in go_simp (substPairs prs inner_rhs)
                      Nothing
                        | not (any (\case { Core.Alt (Core.DataAlt dc) _ _ -> dc == known_dc; _ -> False }) inner_alts)
                        , Just (Core.Alt Core.DEFAULT [] def_rhs) <- find (\case { Core.Alt Core.DEFAULT [] _ -> True; _ -> False }) inner_alts ->
                            go_simp (substPairs [(inner_bndr, Core.Var s_v) | inner_bndr /= s_v] def_rhs)
                      _ -> Core.Case (go_simp s_e) inner_bndr r_ty inner_alts
              go_simp other = other

          descendTree allow_rc known_alts pats proofs raw_expr =
            let expr = simplifyTreeExpr known_alts proofs raw_expr
                leaf_vars = concatMap (indexedFields . snd) pats
            in case expr of
              Core.Case scrut_e bndr res_ty alts
                | Just scrut <- unwrapScrutCast proofs scrut_e
                , scrut `elem` leaf_vars -> do
                    guard (varType bndr `eqType` exprType scrut_e && res_ty `eqType` exprType expr)
                    (tc, _) <- MaybeT (pure (splitTyConApp_maybe (varType bndr)))
                    guard (isRuntimeAlgTyCon tc)
                    let cons = tyConDataCons tc
                        mb_def = listToMaybe [def_e | Core.Alt Core.DEFAULT [] def_e <- alts]
                        substBndr e
                          | bndr == scrut || not (bndr `elemVarSet` exprFreeVars e) = e
                          | otherwise = substExpr (mkOpenSubst (mkInScopeSet (exprFreeVars e `extendVarSet` scrut)) [(bndr, Core.Var scrut)]) e
                    parsed <- fmap catMaybes $ MaybeT $ pure $ traverse (\case
                      Core.Alt (Core.DataAlt dc) bs rhs_e | dc `elem` cons -> Just (Just (dc, (bs, substBndr rhs_e)))
                      Core.Alt Core.DEFAULT [] _ -> Just Nothing
                      _ -> Nothing) alts
                    guard (length parsed == length (nubBy (\a b -> fst a == fst b) parsed))
                    fmap concat $ traverse (\dc -> case lookup dc parsed of
                      Just (bs, rhs_e) -> do
                        let known' = (scrut, (dc, bs)) : known_alts
                            rhs_s  = simplifyTreeExpr known' proofs rhs_e
                        if isPatErrorExpr bindings rhs_s
                          then do
                            refuted <- lift (isRefutedBranch scrut (varType bndr) dc leaf_vars proofs)
                            [] <$ guard refuted
                          else do
                            (node, eqs) <- MaybeT (pure (indexedAlt bndr dc bs))
                            descendTree allow_rc known' [(p, replacePat scrut node pat) | (p, pat) <- pats] (proofs ++ eqs) rhs_s
                      Nothing -> do
                        refuted <- lift (isRefutedBranch scrut (varType bndr) dc leaf_vars proofs)
                        if refuted
                          then pure []
                          else case mb_def of
                            Just def_raw | not (isPatErrorExpr bindings def_raw) -> do
                              bs <- freshAltBinders (varType bndr) dc
                              (node, eqs) <- MaybeT (pure (indexedAlt bndr dc bs))
                              let known' = (scrut, (dc, bs)) : known_alts
                                  rhs_s  = simplifyTreeExpr known' (proofs ++ eqs) (substBndr def_raw)
                              guard (not (isPatErrorExpr bindings rhs_s))
                              descendTree allow_rc known' [(p, replacePat scrut node pat) | (p, pat) <- pats] (proofs ++ eqs) rhs_s
                            _ -> MaybeT (pure Nothing)) cons
              _ | allow_rc, is_prenex, isNothing mb_pack
                , any (\(_, pat) -> case pat of { IndexedConstructor{} -> True; _ -> False }) pats
                , Just rc <- indexedResultCase (unwrapLocalCallbackLets expr)
                -> pure [(pats, proofs, Right rc)]
              Core.Case{} -> MaybeT (pure Nothing)
              _ -> pure [(pats, proofs, Left (unwrapLocalCallbackLets expr))]

          compileExpr allow_self row_pats param_lhs_env floats val_env specialize proofs mb_exp_rhs = go floats
            where
              validCast co = isSafeMultOrReflCo co ||
                (caseCoercion proofs co && specialize (coercionLKind co) `eqType` specialize (coercionRKind co))
              unwrapCast (Core.Cast e co) | validCast co = unwrapCast e
              unwrapCast e = Just e

              go fl uc (Core.Cast e co) = guard (validCast co) *> go fl uc e
              go fl uc expr
                | Just eta_v <- peelEtaTarget expr, Just v_ty <- lookup eta_v val_env = pure (False, [], const v_ty)
                | otherwise = do
                    let (raw_fun, args) = Core.collectArgs expr
                    Core.Var callee <- MaybeT (pure (unwrapCast raw_fun))
                    if callee == owner || (isJust mb_pack && Just callee == mb_worker)
                      then guard allow_self *> compileSelf uc callee expr args
                      else compileVarOrCb fl callee expr args <|>
                           compileCon fl callee expr args <|>
                           compileTableCall fl callee expr args <|>
                           (guard (isJust (lookupVarEnv fl callee)) *> go emptyVarEnv uc (expandTypeHelper fl expr))

              compileVarOrCb fl callee expr args = do
                v_ty <- MaybeT (pure (lookup callee val_env))
                if null args then pure (False, [], const v_ty) else do
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
                let ma_args = replicate (DC.promotedDataConMatchabilityArity con) unmatchableDataConTy
                pure (or s_flags, concat t_lists, \fam -> mkTyConApp (DC.promoteDataCon con) (ma_args ++ supplied ++ [f fam | f <- fns]))

              compileTableCall fl callee expr args = do
                guard (callee /= owner)
                callee_tc <- MaybeT (pure (resolveTable callee))
                let mb_exp_args = case mb_exp_rhs of
                      Just (TyConApp exp_tc exp_as) | exp_tc == callee_tc -> Just exp_as
                      _ -> Nothing
                    step _ _ [] (_ : _) = MaybeT (pure Nothing)
                    step _ acc _ [] = pure (reverse acc)
                    step idx acc (Bndr _ (NamedTCB flag) : rbs) (Core.Type t : ras)
                      | isInvisiblePiFlag flag = step (idx + 1) ((False, [], const (specialize t)) : acc) rbs ras
                    step idx acc (Bndr _ AnonTCB : rbs) (val_a : ras) | not (Core.isTyCoArg val_a) = do
                      ca <- go fl False (peelEtaCall val_a) <|> compileInlinedCb idx mb_exp_args fl val_a
                      step (idx + 1) (ca : acc) rbs ras
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
                    guard (not (null cb_vals))
                    cb_rows <- descendTree False [] [(v, IndexedVariable v) | v <- cb_vals] proofs cb_body
                    guard (not (null cb_rows))
                    for_ cb_rows $ \case
                      (cb_row_pats, cb_all_proofs, Left cb_leaf) -> do
                        let cb_eqs = map CoVarCo cb_all_proofs
                        cb_sub <- MaybeT (pure (tcUnifyTys alwaysBindTv (map coercionLKind cb_eqs) (map coercionRKind cb_eqs)))
                        let cb_spec = substTyUnchecked cb_sub
                        (cb_suffix, cb_inner_env) <- buildRowLhs cb_spec cb_mixed cb_row_pats
                        let merged_env = cb_inner_env ++ [(u, cb_spec t) | (u, t) <- val_env]
                        (_, _, c_fn) <- compileExpr False cb_row_pats [] fl merged_env cb_spec cb_all_proofs Nothing False cb_leaf
                        let reduced = reductionReducedType $
                              normaliseTcAppWithFuel 2000 emptyFamInstEnvs Nominal map_tc (map cb_spec map_prefix ++ cb_suffix)
                        guard (reduced `eqType` c_fn header)
                      _ -> MaybeT (pure Nothing)
                    pure (False, [], const (mkTyConApp map_tc map_prefix))
              compileInlinedCb _ _ _ _ = MaybeT (pure Nothing)

              compileSelf uc callee expr args = case mb_pack of
                Just (package, outer_pat) | input_v : _ <- val_params -> do
                  guard (uc && is_prenex)
                  IndexedConstructor _ _ _ original_fields <- pure outer_pat
                  [(_, IndexedConstructor _ _ _ cur_pat_fields)] <- pure row_pats
                  let (prefix, suffix) = splitAt (length prenex_tvs) args
                  tys <- MaybeT (pure (traverse (fmap specialize . indexedTypeArg) prefix))
                  guard (length tys == length prenex_tvs)
                  let kind = substTyWith prenex_tvs tys (varType input_v)
                      n_exs = length (DC.dataConExTyCoVars package)
                  (mb_supplied, actual_fields, worker_tys) <- if callee == owner
                    then do
                      [operand] <- pure suffix
                      guard (specialize (exprType operand) `eqType` kind)
                      (Core.Var builder, operands) <- pure (Core.collectArgs operand)
                      (con, supplied, values) <- MaybeT (pure (indexedConstructor specialize builder operands))
                      guard (con == package)
                      pure (Just supplied, values, [])
                    else do
                      let (locals, values) = splitAt n_exs suffix
                      erased <- MaybeT (pure (traverse (fmap specialize . indexedTypeArg) locals))
                      guard (length erased == n_exs)
                      pure (Nothing, values, erased)
                  actual_vs <- MaybeT (pure (traverse (\case { Core.Var v -> Just v; _ -> Nothing }) actual_fields))
                  guard (length original_fields == length cur_pat_fields && oneSmaller cur_pat_fields actual_vs)
                  values <- MaybeT (pure (traverse (`lookup` val_env) actual_vs))
                  source <- case mb_supplied of
                    Just written -> pure written
                    Nothing -> do
                      (_, univ_args) <- MaybeT (pure (splitTyConApp_maybe kind))
                      let univs = DC.dataConUnivTyVars package
                      guard (length univ_args == length univs)
                      MaybeT (pure (traverse (`lookup` zip (univs ++ DC.dataConExTyCoVars package) (univ_args ++ worker_tys))
                                             (DC.dataConUserTyVars package)))
                  packed <- MaybeT (pure (checkedPromoted package kind source values))
                  pure (True, [], \fam -> mkTyConApp fam (tys ++ [packed]))
                _ -> do
                  guard is_prenex
                  let (prefix, suffix) = splitAt (length prenex_tvs) args
                  tys <- MaybeT (pure (traverse (fmap specialize . indexedTypeArg) prefix))
                  guard (length tys == length prenex_tvs && length suffix == length val_params)
                  let checkSubterm pat arg_e
                        | Just (Core.Var actual_v) <- unwrapCast arg_e =
                            case pat of
                              IndexedVariable u -> (False,) <$> (guard (actual_v == u) *> lookup actual_v val_env)
                              IndexedConstructor{} -> (True,) <$> (guard (actual_v `elem` indexedFields pat) *> lookup actual_v val_env)
                        | IndexedConstructor _ pat_con pat_src pat_fs <- pat = do
                            unwrapped <- unwrapCast arg_e
                            let (raw_b, ops) = Core.collectArgs unwrapped
                            Core.Var builder <- unwrapCast raw_b
                            (con, supplied, sub_es) <- indexedConstructor specialize builder ops
                            guard (con == pat_con && lazyConstructor con &&
                                   eqTypes supplied (map specialize pat_src) &&
                                   length sub_es == length pat_fs &&
                                   all (caseCoercion proofs) [co | Core.Coercion co <- ops])
                            sub_res <- zipWithM checkSubterm pat_fs sub_es
                            let (sub_dec, sub_tys) = unzip sub_res
                            packed <- checkedPromoted con (specialize (exprType unwrapped)) supplied sub_tys
                            pure (or sub_dec, packed)
                        | otherwise = Nothing
                      checkParam (param_v, arg_e)
                        | Just (Core.Var actual_v) <- unwrapCast arg_e
                        , actual_v == param_v
                        , Just param_ty <- lookup param_v param_lhs_env
                        = Just (False, param_ty)
                        | otherwise = do
                            pat <- lookup param_v row_pats
                            checkSubterm pat arg_e
                  checked_params <- MaybeT (pure (traverse checkParam (zip val_params suffix)))
                  let (decreased_flags, arg_tys) = unzip checked_params
                  guard (or decreased_flags &&
                         eqTypes (map typeKind arg_tys) (map (substTyWith prenex_tvs tys . varType) val_params) &&
                         specialize (exprType expr) `eqType` substTyWith prenex_tvs tys inst_out_ty)
                  pure (True, [], \fam -> mkTyConApp fam (tys ++ arg_tys))

          compileRow mb_cnames (row_pats, proofs, leaf_or_rc) = do
            let equalities = map CoVarCo proofs
            subst <- MaybeT (pure (tcUnifyTys alwaysBindTv (map coercionLKind equalities) (map coercionRKind equalities)))
            let specialize = substTyUnchecked subst
                res_kind = specialize inst_out_ty
            (lhs_tys, val_env0) <- buildRowLhs specialize bndrs row_pats
            let param_lhs_env = [(p, t) | (Right p, t) <- zip bndrs lhs_tys]
                val_env       = val_env0 ++ [(p, t) | (p, t) <- param_lhs_env, isNothing (lookup p val_env0)]
            (rhs_fn, mb_helper, used_self, used_callees) <- case leaf_or_rc of
              Right (IndexedResultCase steps c_body) -> do
                (c_name, c_ax) <- MaybeT (pure mb_cnames)
                let all_rc_proofs = proofs ++ concat [eqs | (_, _, _, eqs) <- steps]
                    rc_eqs = map CoVarCo all_rc_proofs
                rc_subst <- MaybeT (pure (tcUnifyTys alwaysBindTv (map coercionLKind rc_eqs) (map coercionRKind rc_eqs)))
                let rc_spec = substTyUnchecked rc_subst
                    rc_res_kind = rc_spec inst_out_ty
                step_compiled <- traverse (\(call_e, bndr, c_pat, _) -> do
                  (s_self, s_tabs, s_fn) <- compileExpr True row_pats param_lhs_env bindings val_env specialize proofs Nothing False (unwrapLocalCallbackLets call_e)
                  let case_kind = rc_spec (varType bndr)
                  (c_packed, c_fields) <- indexedPatternTypes rc_spec c_pat
                  guard (typeKind (s_fn header) `eqType` case_kind && typeKind c_packed `eqType` case_kind)
                  op_tv <- lift (freshVar "case" case_kind)
                  pure (s_self, s_tabs, s_fn, op_tv, c_packed, c_fields)) steps
                let (s_selfs, s_tab_lists, s_fns, op_tvs, c_packeds, c_field_lists) = unzip6 step_compiled
                    case_kinds = map varType op_tvs
                    locals = tyCoVarsOfTypesWellScoped (case_kinds ++ [rc_res_kind])
                    local_tys = mkTyVarTys locals
                    merged_fields = concat c_field_lists ++ [(u, rc_spec t) | (u, t) <- val_env]
                guard (or s_selfs && all isTyVar locals)
                (b_self, b_tabs, b_fn) <- compileExpr False [] [] bindings merged_fields rc_spec all_rc_proofs Nothing False (unwrapLocalCallbackLets c_body)
                guard (not b_self)
                let c_rhs = b_fn header
                    c_lhs = local_tys ++ c_packeds
                    c_vars = tyCoVarsOfTypesWellScoped c_lhs
                    c_br = mkBranch c_vars c_lhs c_rhs
                guard (all isTyVar c_vars && typeKind c_rhs `eqType` rc_res_kind && validBranch c_br && rc_res_kind `eqType` res_kind)
                let c_tc = mkFamily c_name c_ax (mkNamedTyConBinders Specified locals ++ map mkAnonTyConBinder op_tvs) rc_res_kind [c_br]
                pure (\fam -> mkTyConApp c_tc (local_tys ++ [fn fam | fn <- s_fns]), Just (c_tc, c_br), True, concat s_tab_lists ++ b_tabs)
                where
                  unzip6 xs = ( [a | (a, _, _, _, _, _) <- xs]
                              , [b | (_, b, _, _, _, _) <- xs]
                              , [c | (_, _, c, _, _, _) <- xs]
                              , [d | (_, _, _, d, _, _) <- xs]
                              , [e | (_, _, _, _, e, _) <- xs]
                              , [f | (_, _, _, _, _, f) <- xs] )
              Left leaf_expr -> do
                let mb_exp_rhs = case mb_expected of
                      Just exp_brs -> asum
                        [ Just (substTyUnchecked m_sub (coAxBranchRHS exp_br))
                        | exp_br <- exp_brs
                        , Just m_sub <- [tcMatchTys (coAxBranchLHS exp_br) lhs_tys] ]
                      Nothing -> Nothing
                (b_self, b_tabs, b_fn) <- compileExpr True row_pats param_lhs_env bindings val_env specialize proofs mb_exp_rhs False leaf_expr
                pure (b_fn, Nothing, b_self, b_tabs)
            let vars = tyCoVarsOfTypesWellScoped lhs_tys
                eq = IndexedEquation vars lhs_tys rhs_fn
            guard (all isTyVar vars && typeKind (rhs_fn header) `eqType` res_kind && validBranch (applyRow eq header))
            pure ((eq, mb_helper), used_self, used_callees)

      raw_rows <- descendTree True [] pats0 [] tree0
      guard (not (null raw_rows) && (isNothing mb_pack || length raw_rows > 1))
      case_names <- case [() | (_, _, Right _) <- raw_rows] of
        [] -> pure Nothing
        [_] | isNothing mb_pack -> maybe (Just <$> lift (allocateCaseNames owner)) (pure . Just) mb_case_names
        _ -> MaybeT (pure Nothing)
      compiled <- traverse (compileRow case_names) raw_rows
      let (eq_pairs, self_flags, callee_lists) = unzip3 compiled
          all_callees = nubBy (\a b -> idName a == idName b) (concat callee_lists)
      guard (certifiedShape (or self_flags) all_callees bndrs val_params inst_out_ty raw_rows eq_pairs header)
      pure (CompiledIndexed hdr_binders hdr_res eq_pairs case_names mb_worker all_callees)

    allocateCaseNames fn = do
      nm <- newImplicitBinder (idName fn) (mkTcOcc . ("$case:" ++) . occNameString)
      (nm,) <$> newImplicitBinder nm mkInstTyCoOcc

    buildHeaderTelescope pis out_ty = go (mkEmptySubst (mkInScopeSet (tyCoVarsOfTypes [varType owner]))) [] pis
      where
        go sub acc [] = pure (reverse acc, substTy sub out_ty)
        go sub acc (Left (flag, tv) : rest) = do
          tv' <- freshVar (occNameString (nameOccName (getName tv))) (substTy sub (varType tv))
          go (extendTvSubst sub tv (mkTyVarTy tv')) (mkNamedTyConBinder flag tv' : acc) rest
        go sub acc (Right dom : rest) = do
          v' <- freshVar "arg" (substTy sub dom)
          go sub (mkAnonTyConBinder v' : acc) rest

    stripNoise (Core.Let (Core.NonRec b _) e) | isDeadLocalLet b e = stripNoise e
    stripNoise (Core.Cast e co) | isSafeMultOrReflCo co = stripNoise e
    stripNoise e = e

    pointFreeCon e = do
      inner <- weakeningBody e
      let (raw_hd, args) = Core.collectArgs inner
      Core.Var make <- weakeningBody raw_hd
      dc <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
      inner <$ guard (lazyConstructor dc && all Core.isTyCoArg args)

    peelMixedBinders [] sig_out sub expr = pure ([], substTy sub sig_out, stripNoise expr)
    peelMixedBinders (Left (flag, sig_tv) : rest) sig_out sub expr = do
      let exp_k = substTy sub (varType sig_tv)
          e' = stripNoise expr
      case e' of
        Core.Lam b body | isTyVar b, varType b `eqType` exp_k -> do
          (more, out, final_b) <- peelMixedBinders rest sig_out (extendTvSubst sub sig_tv (mkTyVarTy b)) body
          pure (Left (flag, b) : more, out, final_b)
        _ | Just con_fn <- pointFreeCon e' -> do
          b <- lift (freshVar "tv" exp_k)
          (more, out, final_b) <- peelMixedBinders rest sig_out (extendTvSubst sub sig_tv (mkTyVarTy b))
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
      , Just [(pack', alt_bs, nested)] <- caseOn input body, pack' == pack = do
          (outer_pat, []) <- MaybeT (pure (indexedAlt input pack alt_bs))
          let (existentials, fields) = splitAt (length exs) alt_bs
              tys = prenex_tvs ++ existentials
          (mb_w, tree) <- case nested of
            Core.Case{} -> pure (Nothing, nested)
            _ -> do
              let (raw_fn, args) = Core.collectArgs nested
              Core.Var worker <- pure raw_fn
              guard (worker /= owner && (not (isExternalName (idName worker)) ||
                                         nameModule_maybe (idName worker) == nameModule_maybe (idName owner)))
              let (passed_tys, passed_fs) = splitAt (length tys) args
              supplied <- MaybeT (pure (traverse indexedTypeArg passed_tys))
              guard (eqTypes supplied (mkTyVarTys tys) && length passed_fs == length fields &&
                     and (zipWith (\a e -> case a of { Core.Var u -> u == e; _ -> False }) passed_fs fields))
              decl <- MaybeT (pure (lookupVarEnv bindings worker))
              (w_tys, w_fs, w_tree) <- MaybeT (pure (function decl))
              let wbndrs = w_tys ++ w_fs
                  free = foldl' unionVarSet (exprFreeVars w_tree) (map exprFreeVars args)
                  expanded = substExpr (mkOpenSubst (mkInScopeSet free) (zip wbndrs args)) w_tree
              guard (length w_tys == length tys && length w_fs == length fields &&
                     exprType decl `eqType` varType worker && exprType expanded `eqType` exprType nested)
              pure (Just worker, expanded)
          pure (Just (pack, outer_pat), mb_w, [(input, outer_pat)], tree)
    unwrapPackWorker True prenex_tvs val_params body
      | Just (worker, expanded) <- unwrapStrictGadtWorker prenex_tvs val_params body
      = pure (Nothing, Just worker, [(v, IndexedVariable v) | v <- val_params], expanded)
    unwrapPackWorker _ _ val_params body = pure (Nothing, Nothing, [(v, IndexedVariable v) | v <- val_params], body)

    unwrapStrictGadtWorker prenex_tvs val_params body = do
      let peelCases acc (Core.Case (Core.Var v) bndr res_ty [Core.Alt (Core.DataAlt dc) bs inner])
            | v `elem` val_params, not (any (\(u, _, _, _, _) -> u == v) acc)
            , lazyConstructor dc
            = peelCases (acc ++ [(v, bndr, res_ty, dc, bs)]) inner
          peelCases acc inner = (acc, inner)
          (unpacked, call_e) = peelCases [] body
      guard (not (null unpacked))
      (Core.Var worker, call_args) <- pure (Core.collectArgs call_e)
      guard (worker /= owner && (not (isExternalName (idName worker)) ||
                                 nameModule_maybe (idName worker) == nameModule_maybe (idName owner)))
      w_decl <- lookupVarEnv bindings worker
      let collectAllLams acc (Core.Lam b e) = collectAllLams (b : acc) e
          collectAllLams acc (Core.Let (Core.NonRec b _) e) | isDeadLocalLet b e = collectAllLams acc e
          collectAllLams acc e = (reverse acc, e)
          (w_bndrs, w_body) = collectAllLams [] w_decl
      guard (length w_bndrs == length call_args && exprType w_decl `eqType` varType worker)
      let argVar (Core.Type (TyVarTy tv)) = Just tv
          argVar (Core.Coercion (CoVarCo cv)) = Just cv
          argVar (Core.Var v) = Just v
          argVar _ = Nothing
      call_vars <- traverse argVar call_args
      let pos_map = zip call_vars [0 :: Int ..]
          rebuildParam p = case find (\(u, _, _, _, _) -> u == p) unpacked of
            Nothing -> do
              idx <- lookup p pos_map
              pure (\rargs -> rargs !! idx)
            Just (_, bndr, _, dc, bs) -> do
              (tc, tc_args) <- splitTyConApp_maybe (varType bndr)
              guard (tc == DC.dataConTyCon dc)
              bs_idxs <- traverse (`lookup` pos_map) bs
              pure (\rargs ->
                let supplied_bs = [rargs !! i | i <- bs_idxs]
                in foldl' Core.App (Core.Var (DC.dataConWorkId dc)) (map Core.Type tc_args ++ supplied_bs))
      param_rebuilders <- traverse rebuildParam val_params
      tys_rebuilders <- traverse (\tv -> do
        idx <- lookup tv pos_map
        pure (\rargs -> rargs !! idx)) prenex_tvs
      let rewriteRec (Core.App f a)
            | (Core.Var fn, rargs) <- Core.collectArgs (Core.App f a)
            , fn == worker, length rargs == length w_bndrs =
                let rargs' = map rewriteRec rargs
                    ty_es  = [rb rargs' | rb <- tys_rebuilders]
                    val_es = [rb rargs' | rb <- param_rebuilders]
                in foldl' Core.App (Core.Var owner) (ty_es ++ val_es)
            | otherwise = Core.App (rewriteRec f) (rewriteRec a)
          rewriteRec (Core.Lam b e) = Core.Lam b (rewriteRec e)
          rewriteRec (Core.Let b e) = Core.Let (rewriteBind b) (rewriteRec e)
          rewriteRec (Core.Case s b t alts) =
            Core.Case (rewriteRec s) b t [Core.Alt c bs (rewriteRec r) | Core.Alt c bs r <- alts]
          rewriteRec (Core.Cast e co) = Core.Cast (rewriteRec e) co
          rewriteRec (Core.Tick tk e) = Core.Tick tk (rewriteRec e)
          rewriteRec e = e
          rewriteBind (Core.NonRec b r) = Core.NonRec b (rewriteRec r)
          rewriteBind (Core.Rec prs) = Core.Rec [(b, rewriteRec r) | (b, r) <- prs]
          w_body' = rewriteRec w_body
          free = foldl' unionVarSet (exprFreeVars w_body') (map exprFreeVars call_args)
          inlined = substExpr (mkOpenSubst (mkInScopeSet free) (zip w_bndrs call_args)) w_body'
          rebuilt = foldr (\(v, bndr, res_ty, dc, bs) inner ->
                      Core.Case (Core.Var v) bndr res_ty [Core.Alt (Core.DataAlt dc) bs inner]) inlined unpacked
      guard (exprType rebuilt `eqType` exprType body)
      pure (worker, rebuilt)

    replacePat v node (IndexedVariable u) | v == u = node
    replacePat v node (IndexedConstructor ty con src fs) = IndexedConstructor ty con src (map (replacePat v node) fs)
    replacePat _ _ pat = pat

    isRefutedBranch scrut scrut_ty dc leaf_vars proofs = do
      let (univs, exs, specs, _, fields, orig_res) = DC.dataConFullSig dc
      (ren, _) <- freshenTyVarBndrs (univs ++ exs)
      let inst = substTyUnchecked ren
          proof_cos = map CoVarCo proofs
          lhs_tys = map coercionLKind proof_cos ++ [inst (mkTyVarTy tv) | s <- specs, let (tv, _) = DC.eqSpecPair s] ++ [scrut_ty]
          rhs_tys = map coercionRKind proof_cos ++ [inst rhs_t | s <- specs, let (_, rhs_t) = DC.eqSpecPair s] ++ [inst orig_res]
      pure $ case tcUnifyTys alwaysBindTv lhs_tys rhs_tys of
        Nothing -> True
        Just subst ->
          let isUninhabited ty = case splitTyConApp_maybe (coreFullView ty) of
                Just (otc, oargs) -> isRuntimeAlgTyCon otc && all (DC.dataConCannotMatch oargs) (tyConDataCons otc)
                Nothing -> False
              field_tys = [substTyUnchecked subst (inst (scaledThing f)) | f <- fields]
              other_tys = [substTyUnchecked subst (varType v) | v <- leaf_vars, v /= scrut]
          in any isUninhabited (field_tys ++ other_tys)

    buildRowLhs specialize mixed_bndrs row_pats = go [] [] mixed_bndrs
      where
        go lhs_acc env_acc [] = pure (reverse lhs_acc, env_acc)
        go lhs_acc env_acc (Left (_, tv) : rest) = go (specialize (mkTyVarTy tv) : lhs_acc) env_acc rest
        go lhs_acc env_acc (Right v : rest) = do
          pat <- MaybeT (pure (lookup v row_pats))
          case pat of
            IndexedVariable u -> do
              fv <- lift (freshVar "arg" (specialize (varType u)))
              go (mkTyVarTy fv : lhs_acc) ((u, mkTyVarTy fv) : env_acc) rest
            IndexedConstructor{} -> do
              (packed, p_fields) <- indexedPatternTypes specialize pat
              go (packed : lhs_acc) (p_fields ++ env_acc) rest

    oneSmaller pat_fs actual_vs =
      length pat_fs == length actual_vs && maybe False ((== 1) . length . filter id) (zipWithM cmpField pat_fs actual_vs)
    cmpField (IndexedVariable old) new = False <$ guard (old == new)
    cmpField tree@IndexedConstructor{} new = True <$ guard (new `elem` indexedFields tree)

    certifiedShape has_self all_callees bndrs val_params out_ty _raw_rows eq_pairs header
      | has_self = True
      | any (isIndexedCallbackTy . varType) val_params =
          maybe False (\(_, args) -> case args of { TyVarTy _ : _ -> True; _ -> False }) (splitTyConApp_maybe out_ty)
      | not (null all_callees) = length val_params >= 2
      | [v] <- val_params, [(eq, Nothing)] <- eq_pairs =
          let all_tvs = [tv | Left (_, tv) <- bndrs]
          in not (null all_tvs) && length all_tvs <= 3 &&
             mkVarSet all_tvs `subVarSet` tyCoVarsOfTypes [varType v, out_ty] &&
             isScopeConAlias (varType v) out_ty eq header
      | length val_params >= 2 = True
      | otherwise = False

    hasSingleVarConstructorRhs cod_tc eq header = isJust $ do
      (prom_tc, rhs_args@(_ : _)) <- splitTyConApp_maybe (coAxBranchRHS (applyRow eq header))
      dc <- isPromotedDataCon_maybe prom_tc
      guard (DC.dataConTyCon dc == cod_tc && DC.dataConSourceArity dc == 1 &&
             isJust (getTyVar_maybe (last rhs_args)))

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
      guard ((valid_weaken || (dom_tc /= cod_tc && eqTypes dom_args cod_args)) &&
             hasSingleVarConstructorRhs cod_tc eq header)

    isScopeSubstMap val_params out_ty raw_rows eq_pairs header = isJust $ do
      let (cap_vars, rest_params) = splitAt (length val_params - 1) val_params
          k = length cap_vars
      [idx_var] <- pure rest_params
      guard (k >= 1 && length raw_rows == k + 1 && length eq_pairs == k + 1)
      (term_tc, out_ctx : out_rest) <- splitTyConApp_maybe out_ty
      (idx_tc, idx_ctx : idx_rest) <- splitTyConApp_maybe (varType idx_var)
      guard (isAlgTyCon term_tc && isAlgTyCon idx_tc && length out_rest <= 1 && eqTypes idx_rest out_rest)
      base_ctx <- peelCtxSteps k idx_ctx
      guard (base_ctx `eqType` out_ctx)
      for_ cap_vars $ \cv -> do
        (ct, c_ctx : _) <- splitTyConApp_maybe (varType cv)
        guard (ct == term_tc && c_ctx `eqType` out_ctx)
      let (hit_raw, _) = splitAt k raw_rows
      sel_vs <- traverse (\(_, pfs, l_or_r) -> case l_or_r of
        Left e | Just (Core.Var sv) <- branchBody pfs e -> Just sv
        _ -> Nothing) hit_raw
      (_, [(miss_eq, Nothing)]) <- pure (splitAt k eq_pairs)
      guard (mkVarSet sel_vs == mkVarSet cap_vars && hasSingleVarConstructorRhs term_tc miss_eq header)

    peelCtxSteps 0 ctx = Just ctx
    peelCtxSteps d ctx = do
      (ext_tc, inner : _) <- splitTyConApp_maybe ctx
      guard (isPromotedDataCon ext_tc)
      peelCtxSteps (d - 1) inner

sameCompiledIndexed :: TyCon -> CompiledIndexed -> CompiledIndexed -> MaybeT TcM ()
sameCompiledIndexed header orig fin
  | length (ci_rows orig) == length (ci_rows fin) =
      zipWithM_ (\(o_eq, o_c) (f_eq, f_c) -> do
        sameIndexedRow (applyRow o_eq header) (applyRow f_eq header)
        case (o_c, f_c) of
          (Nothing, Nothing) -> pure ()
          (Just (ltc, lbr), Just (rtc, rbr)) -> guard (tyConKind ltc `eqType` tyConKind rtc) *> sameIndexedRow lbr rbr
          _ -> MaybeT (pure Nothing)) (ci_rows orig) (ci_rows fin)
  | all (isNothing . snd) (ci_rows orig ++ ci_rows fin) = do
      let mkTemp rows =
            let fam = mkFamily (tyConName header) (tyConName header) (ci_binders orig) (ci_res_kind orig)
                        [applyRow eq fam | (eq, _) <- rows]
            in fam
          orig_fam = mkTemp (ci_rows orig)
          fin_fam  = mkTemp (ci_rows fin)
          agreesOn fam brs = all (\br ->
            normaliseClosedTables (TyConApp fam (coAxBranchLHS br))
              `eqType` normaliseClosedTables (coAxBranchRHS br)) brs
          orig_brs = [applyRow eq orig_fam | (eq, _) <- ci_rows orig]
          fin_brs  = [applyRow eq fin_fam  | (eq, _) <- ci_rows fin]
      guard (not (null orig_brs) && not (null fin_brs) &&
             (agreesOn orig_fam fin_brs || agreesOn fin_fam orig_brs))
  | otherwise = MaybeT (pure Nothing)

publishCompiledIndexedM :: Bool -> (TyCon -> Bool) -> Id -> CompiledIndexed -> Maybe CompiledIndexed -> [Id]
                        -> TcM (Maybe (Id, [TyCon]))
publishCompiledIndexedM is_early public owner orig mb_fin helpers = runMaybeT $ do
    root_name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (mkClosureTableAxiom is_early root_name)
    let make = mkFamily root_name axiom (ci_binders orig) (ci_res_kind orig)
    for_ mb_fin (sameCompiledIndexed (make []) orig)
    let (chosen, chosen_helpers) = case mb_fin of
          Just fin | null (ci_table_callees fin), not (null (ci_table_callees orig))
                   , length (ci_rows fin) == length (ci_rows orig)
                   , all (isNothing . snd) (ci_rows fin)
                   -> (fin, [h | h <- helpers, any (\c -> idName c == idName h) (ci_table_callees fin)])
          _        -> (orig, helpers)
        branches = [applyRow eq table | (eq, _) <- ci_rows chosen]
        table = make branches
        families = table : [tc | (_, Just (tc, _)) <- ci_rows chosen]
    guard (all validBranch branches &&
           all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
    pure (setIdDetails owner (mkClosureOwnerDetails owner table chosen_helpers), families)

-- ---------------------------------------------------------------------------
-- Unified unary & binary scope-indexed closures (SC3 + SC4)
-- ---------------------------------------------------------------------------

data CallbackLift = NamedLift Id | InlinedRename | InlinedSubst Id

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
    ss_arity a == ss_arity b && ss_context a `eqType` ss_context b &&
    maybe (isNothing (ss_sort b)) (\x -> maybe False (eqType x) (ss_sort b)) (ss_sort a) &&
    ss_term a == ss_term b && ss_ext a == ss_ext b && ss_index a == ss_index b &&
    ss_var a == ss_var b && ss_hit a == ss_hit b && ss_miss a == ss_miss b &&
    ss_recipes a == ss_recipes b

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
        ctx_ty = mkTyVarTy context
    (context_tc, []) <- splitTyConApp_maybe context_kind
    guard (isAlgTyCon context_tc)
    (t_tc, body_ctx : body_sorts) <- splitTyConApp_maybe (varType body)
    (e_tc, ext_args) <- splitTyConApp_maybe body_ctx
    (i_tc, index_ctx : index_sorts) <- splitTyConApp_maybe (varType argument)
    guard (isPromotedDataCon e_tc && all isAlgTyCon [t_tc, i_tc] && index_ctx `eqType` body_ctx)
    mb_sort <- if is_typed
      then do
        result : rev_sorts <- pure (reverse rest_tvs)
        [callback_sort] <- pure cb_tvs
        let sorts = reverse rev_sorts
            sort_kind = varType result
        (sort_tc, []) <- splitTyConApp_maybe sort_kind
        guard (isAlgTyCon sort_tc && length ext_args == 2 && length captures == length sorts &&
               all (eqType sort_kind . varType) (callback_sort : sorts) &&
               eqTypes body_sorts [mkTyVarTy result] && eqTypes index_sorts [mkTyVarTy callback_sort] &&
               foldl' (\k s -> mkTyConApp e_tc [k, mkTyVarTy s]) ctx_ty sorts `eqType` body_ctx &&
               and (zipWith (\cap s -> varType cap `eqType` mkTyConApp t_tc [ctx_ty, mkTyVarTy s]) captures sorts))
        pure (Just sort_kind)
      else do
        guard (null rest_tvs && null cb_tvs && null body_sorts && null index_sorts && length ext_args == 1 &&
               foldl' (\k _ -> mkTyConApp e_tc [k]) ctx_ty captures `eqType` body_ctx &&
               all (\cap -> varType cap `eqType` mkTyConApp t_tc [ctx_ty]) captures &&
               exprType call `eqType` mkTyConApp t_tc [ctx_ty])
        pure Nothing
    [(first, _, _), (second, _, _)] <- caseOn argument lookup_body
    let index_cons = tyConDataCons i_tc
    guard (first /= second && length index_cons == 2 && first `elem` index_cons && second `elem` index_cons &&
           all lazyConstructor index_cons)
    (hit, miss, var) <- outcomes first second [] argument (reverse captures) lookup_body <|>
                        outcomes second first [] argument (reverse captures) lookup_body
    guard (DC.dataConTyCon var == t_tc)
    traversal_rhs <- lookupVarEnv bindings traversal
    (trav_tvs, [sigma, scrutinee], traversal_case) <- function traversal_rhs
    guard (length trav_tvs == (if is_typed then 3 else 2))
    rows <- caseOn scrutinee traversal_case
    _ <- findTraversalVarCon True sigma rows [var]
    (ren_ops, subst_ops, recipes) <- checkedScopeTraversals is_typed bindings traversal sigma var hit miss rows
    pure (ScopeShape owner (length captures) context_kind mb_sort t_tc e_tc i_tc ren_ops (Just subst_ops) var hit miss recipes)

outcomes :: DC.DataCon -> DC.DataCon -> [Var] -> Id -> [Id] -> CoreExpr -> Maybe (DC.DataCon, DC.DataCon, DC.DataCon)
outcomes hit miss scope argument captures expr = do
    body <- branchBody scope expr
    case captures of
      [] -> do
        (con, [field]) <- constructorCall body
        Core.Var x <- branchBody scope field
        (hit, miss, con) <$ guard (x == argument)
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
    listToMaybe [ ext_tc | con <- tyConDataCons term, let (_, _, _, _, fs, _) = DC.dataConFullSig con, f <- fs
                         , Just (t, ctx : rest_t) <- [splitTyConApp_maybe (scaledThing f)]
                         , t == term && length rest_t + 1 == arity
                         , Just (ext_tc, ext_args) <- [splitTyConApp_maybe ctx]
                         , length ext_args == arity && isPromotedDataCon ext_tc ]
  where
    listToMaybe (x : _) = Just x
    listToMaybe []      = Nothing

findTraversalVarCon :: Bool -> Id -> [(DC.DataCon, [Var], CoreExpr)] -> [DC.DataCon] -> Maybe DC.DataCon
findTraversalVarCon is_subst cb rows =
    find $ \con -> case [(bs, e) | (dc, bs, e) <- rows, dc == con] of
      [(bs, e)] | [field] <- filter isValueVar bs, Just checked <- branchBody bs e ->
        if is_subst
          then case application checked of
                 Just (Core.Var use, [arg]) | Just (Core.Var x) <- branchBody bs arg -> use == cb && x == field
                 _ -> False
          else case constructorCall checked of
                 Just (vh, [renamed0])
                   | vh == con
                   , Just renamed <- branchBody bs renamed0
                   , Just (Core.Var use, [arg]) <- application renamed
                   , Just (Core.Var x) <- branchBody bs arg ->
                     use == cb && x == field
                 _ -> False
      _ -> False

directScopeShapes :: BindEnv -> [Id] -> [ScopeShape]
directScopeShapes bindings candidates =
    concatMap (\is_t -> mapMaybe (fromSubst is_t) candidates ++ mapMaybe (fromRename is_t) candidates) [True, False]
  where
    scopeHeader is_typed op_id = do
      rhs <- lookupVarEnv bindings op_id
      (tvs, [cb, operand], body) <- function rhs
      mb_sort <- if is_typed
        then do [g_tv, _, a_tv] <- pure tvs
                (c_tc, []) <- splitTyConApp_maybe (varType g_tv)
                (s_tc, []) <- splitTyConApp_maybe (varType a_tv)
                Just (varType a_tv) <$ guard (all isAlgTyCon [c_tc, s_tc])
        else do [g_tv, _] <- pure tvs
                (c_tc, []) <- splitTyConApp_maybe (varType g_tv)
                Nothing <$ guard (isAlgTyCon c_tc)
      g_tv : _ <- pure tvs
      let arity = if is_typed then 2 else 1
      (term, t_args) <- splitTyConApp_maybe (varType operand)
      (_, [Scaled _ idx_ty], _) <- pure (splitFunTys (snd (splitForAllTyVars (varType cb))))
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
            pure (ScopeShape subst_id 1 ctx_k mb_sort term ext_con index ren_ops (Just subst_ops) var hit miss recipes)
      tryPair c1 c2 <|> tryPair c2 c1

    fromRename is_typed rename_id = do
      (ctx_k, mb_sort, term, ext_con, index, rho, rows, c1, c2) <- scopeHeader is_typed rename_id
      let constructors = tyConDataCons term
      guard (length rows == length constructors && all lazyConstructor constructors)
      var <- findTraversalVarCon False rho rows constructors
      let tryRen hit miss = do
            (recipes, ext_id) <- checkedRenameOnlyRows is_typed bindings var hit miss rename_id rho rows
            let subst_ops = if is_typed then Just (rename_id, ext_id, Nothing) else Nothing
            pure (ScopeShape rename_id 1 ctx_k mb_sort term ext_con index (rename_id, ext_id) subst_ops var hit miss recipes)
      tryRen c1 c2 <|> tryRen c2 c1

checkedRenameOnlyRows :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> DC.DataCon
                      -> Id -> Id -> [(DC.DataCon, [Var], CoreExpr)] -> Maybe ([(DC.DataCon, [Int])], Id)
checkedRenameOnlyRows is_typed bindings var hit miss rename_id rho rows = do
    lifted <- traverse (checkedScopeRow is_typed bindings var hit miss rename_id rho) [r | r@(con, _, _) <- rows, con /= var]
    let (recipes, exts) = unzip lifted
        steps = concatMap concat exts
    guard (all (\case { InlinedSubst{} -> False; _ -> True }) steps)
    ext_id <- case [v | NamedLift v <- steps] of { v : vs | all (== v) vs -> Just v; _ -> Nothing }
    ext_rhs <- lookupVarEnv bindings ext_id
    checkScopeRenameExtension is_typed bindings hit miss ext_rhs
    pure (recipes, ext_id)

checkedScopeTraversals :: Bool -> BindEnv -> Id -> Id -> DC.DataCon -> DC.DataCon -> DC.DataCon
                       -> [(DC.DataCon, [Var], CoreExpr)]
                       -> Maybe ((Id, Id), (Id, Id, Maybe Id), [(DC.DataCon, [Int])])
checkedScopeTraversals is_typed bindings subst sigma var hit miss rows = do
    let constructors = tyConDataCons (DC.dataConTyCon var)
        complete alts = let hs = [c | (c, _, _) <- alts]
                        in length hs == length constructors && all (`elem` hs) constructors && all lazyConstructor constructors
    guard (complete rows)
    lifted <- traverse (checkedScopeRow is_typed bindings var hit miss subst sigma) [r | r@(con, _, _) <- rows, con /= var]
    let (recipes, used_extenders) = unzip lifted
        subst_steps = concatMap concat used_extenders
    extender <- case [v | NamedLift v <- subst_steps] of { v : vs | all (== v) vs -> Just v; _ -> Nothing }
    extender_rhs <- lookupVarEnv bindings extender
    (rename, weakening) <- checkedScopeExtension is_typed bindings var hit miss extender_rhs
    guard (all (\case { NamedLift _ -> True; InlinedSubst v -> rename == v; InlinedRename -> False }) subst_steps)
    rename_rhs <- lookupVarEnv bindings rename
    (tvs, [rho, operand], renaming) <- function rename_rhs
    guard (length tvs == (if is_typed then 3 else 2))
    rename_rows <- caseOn operand renaming
    guard (complete rename_rows)
    _ <- findTraversalVarCon False rho rename_rows [var]
    (rename_recipes, extend_rename) <- checkedRenameOnlyRows is_typed bindings var hit miss rename rho rename_rows
    let canonical = sortOn (nameStableString . DC.dataConName . fst)
    guard (canonical recipes == canonical rename_recipes)
    pure ((rename, extend_rename), (subst, extender, weakening), recipes)

checkedScopeRow :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> DC.DataCon -> Id -> Id
                -> (DC.DataCon, [Var], CoreExpr) -> Maybe ((DC.DataCon, [Int]), [[CallbackLift]])
checkedScopeRow is_typed bindings var hit miss traversal callback (con, binders, rhs) = do
    (rebuilt, fields) <- branchBody binders rhs >>= constructorCall
    let originals = filter isValueVar binders
    guard (rebuilt == con && length fields == length originals)
    owners <- zipWithM recursiveField originals fields
    pure ((con, map length owners), owners)
  where
    recursiveField original field = do
      field' <- branchBody binders field
      (Core.Var owner, [captured, sub_expr]) <- application field'
      Core.Var subterm <- branchBody binders sub_expr
      guard (traversal == owner && original == subterm && isLiftedTypeKind (typeKind (varType original)))
      lifts captured

    lifts (Core.Var original) | callback == original = Just []
    lifts captured = named captured <|> inlined captured

    named captured
      | is_typed = eta_lifted <|> case Core.collectArgs captured of
          (Core.Var ext, [Core.Type _, Core.Type _, Core.Type _, orig]) -> (NamedLift ext :) <$> lifts orig
          _ -> Nothing
      | otherwise = case Core.collectArgs captured of
          (Core.Var ext, [Core.Type _, Core.Type _, inner]) -> (NamedLift ext :) <$> lifts inner
          _ -> Nothing
      where
        eta_lifted = do
          ([index], [], closure) <- function captured
          case Core.collectArgs closure of
            (Core.Var orig, [Core.Type (TyVarTy rest)]) | callback == orig && index == rest -> pure []
            (Core.Var ext, [Core.Type _, Core.Type _, Core.Type _, orig, Core.Type (TyVarTy rest)])
              | index == rest -> (NamedLift ext :) <$> lifts orig
            (Core.Var ext, [Core.Type _, Core.Type _, Core.Type _, Core.Type (TyVarTy rest), orig])
              | index == rest -> (NamedLift ext :) <$> lifts orig
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
      (lift_step, see_older) <- inlineStep hit_bs miss_bs fresh forwarded
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

    inlineStep hit_bs miss_bs fresh forwarded = do
      (fresh_con, fields) <- constructorCall fresh
      case fields of
        [] -> do
          (forward_con, [lo0]) <- constructorCall forwarded
          lookup_older <- branchBody miss_bs lo0
          (InlinedRename, lookup_older) <$ guard (fresh_con == hit && forward_con == miss)
        [v] -> do
          v' <- branchBody hit_bs v
          (fresh_index, []) <- constructorCall v'
          guard (fresh_con == var && fresh_index == hit)
          (Core.Var rename, [weakening, lookup_older]) <- application forwarded
          _ <- checkedScopeWeakening is_typed bindings miss weakening
          pure (InlinedSubst rename, lookup_older)
        _ -> Nothing

peelScopeExtensionBinders :: Bool -> CoreExpr -> Maybe (Id, Id, CoreExpr)
peelScopeExtensionBinders True rhs = do
    let collect acc (Core.Lam b e) = collect (b : acc) e
        collect acc (Core.Let (Core.NonRec b _) e) | isDeadLocalLet b e = collect acc e
        collect acc e = (reverse acc, e)
        (binders, body) = collect [] rhs
    case binders of
      [t1, t2, t3, capture, t4, argument] | all isTyVar [t1, t2, t3, t4] && all isValueVar [capture, argument] ->
        pure (capture, argument, body)
      [t1, t2, t3, t4, capture, argument] | all isTyVar [t1, t2, t3, t4] && all isValueVar [capture, argument] ->
        pure (capture, argument, body)
      _ -> Nothing
peelScopeExtensionBinders False rhs = do
    ([m, n], [capture, argument], body) <- function rhs
    (capture, argument, body) <$ guard (all isTyVar [m, n] && all isValueVar [capture, argument])

checkScopeExt :: Bool -> BindEnv -> Maybe DC.DataCon -> DC.DataCon -> DC.DataCon -> CoreExpr
              -> Maybe (Maybe (Id, Maybe Id))
checkScopeExt is_typed bindings mb_var hit miss rhs = do
    (capture, argument, body) <- peelScopeExtensionBinders is_typed rhs
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs &&
           (isJust mb_var || (lazyConstructor hit && lazyConstructor miss)))
    raw_hit <- branchBody hit_bs hit_rhs
    let hit_body = if is_typed && isNothing mb_var then raw_hit else expandTypeHelper bindings raw_hit
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (res, lookup_older) <- case mb_var of
      Nothing -> do
        (fresh, []) <- constructorCall hit_body
        (successor, [lo0]) <- constructorCall miss_body
        lo <- branchBody miss_bs lo0
        guard (fresh == hit && successor == miss)
        pure (Nothing, lo)
      Just var -> do
        (term, [v]) <- constructorCall hit_body
        v' <- branchBody hit_bs v
        (fresh, []) <- constructorCall v'
        guard (term == var && fresh == hit)
        (Core.Var rename, [weakening, lo]) <- application miss_body
        _ <- lookupVarEnv bindings rename
        chosen <- checkedScopeWeakening is_typed bindings miss weakening
        pure (Just (rename, chosen), lo)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    res <$ guard (lookup_capture == capture && input == older)

checkScopeRenameExtension :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> CoreExpr -> Maybe ()
checkScopeRenameExtension is_typed bindings hit miss rhs = () <$ checkScopeExt is_typed bindings Nothing hit miss rhs

checkedScopeExtension :: Bool -> BindEnv -> DC.DataCon -> DC.DataCon -> DC.DataCon -> CoreExpr -> Maybe (Id, Maybe Id)
checkedScopeExtension is_typed bindings var hit miss rhs = checkScopeExt is_typed bindings (Just var) hit miss rhs >>= id

checkedScopeWeakening :: Bool -> BindEnv -> DC.DataCon -> CoreExpr -> Maybe (Maybe Id)
checkedScopeWeakening is_typed bindings miss raw = do
    unwrapped <- weakeningBody raw
    let direct = do
          (tvs, inputs, body) <- function unwrapped
          (shift, values) <- constructorCall body
          guard (length tvs <= (if is_typed then 1 else 0) && length inputs <= 1 &&
                 shift == miss && length inputs == length values)
          zipWithM_ (\i val -> case val of { Core.Var y -> guard (y == i); _ -> Nothing }) inputs values
          pure Nothing
        named = do
          (tvs, [], partial) <- function unwrapped
          (Core.Var helper, args) <- pure (Core.collectArgs (if null tvs && not is_typed then unwrapped else partial))
          tys <- traverse indexedTypeArg args
          con <- lookupVarEnv bindings helper >>= validConstructorAliasBody
          let (htvs, _) = splitForAllTyVars (varType helper)
          guard (length tvs <= 1 && con == miss)
          if is_typed
            then Just helper <$ guard (length tys == 2 + length tvs && length htvs == 3)
            else (if isExternalName (idName helper) && length htvs == 1 then Just helper else Nothing)
                   <$ guard (length htvs == length tys)
    direct <|> named

-- ---------------------------------------------------------------------------
-- Closure family & traversal publication (unary & binary)
-- ---------------------------------------------------------------------------

closureCallbackKind :: TyCon -> TyCon -> Maybe TyVar -> Type -> Type -> Kind
closureCallbackKind index out (Just rank) g d =
    mkSpecForAllTys [(unmatchableDataConTy, rank)] $
      mkVisFunTyManyU (mkTyConApp index [g, mkTyVarTy rank]) (mkTyConApp out [d, mkTyVarTy rank])
closureCallbackKind index out Nothing g d =
    mkVisFunTyManyU (mkTyConApp index [g]) (mkTyConApp out [d])

buildWeakeningParts :: Kind -> Maybe Kind -> TyCon -> TyCon -> DC.DataCon
                    -> TcM ([TyConBinder], Kind, Maybe [CoAxBranch])
buildWeakeningParts context mb_sort index ext miss = do
    g <- freshVar "g" context
    (b_vars, a_vars) <- maybe (pure ([], [])) (\s -> (,) <$> (pure <$> freshVar "b" s) <*> (pure <$> freshVar "a" s)) mb_sort
    let gk = mkTyVarTy g
        b_tys = mkTyVarTys b_vars
        a_tys = mkTyVarTys a_vars
        result = mkTyConApp index (mkTyConApp ext (gk : b_tys) : a_tys)
        named_vars = g : b_vars ++ a_vars
    x <- freshVar "x" (mkTyConApp index (gk : a_tys))
    let binders = mkNamedTyConBinders Specified named_vars ++ [mkAnonTyConBinder x]
        branches = pure . renamingBranch (mkTyVarTys named_vars ++ [mkTyVarTy x]) <$> conAt miss result [mkTyVarTy x]
    pure (binders, result, branches)

publishWeakeningFamily :: Bool -> (TyCon -> Bool) -> (Id, ScopeShape) -> TcM (Maybe (Id, [TyCon]))
publishWeakeningFamily is_early public (owner, shape) = runMaybeT $ do
    (binders, result, branches) <- lift $
      buildWeakeningParts (ss_context shape) (ss_sort shape) (ss_index shape) (ss_ext shape) (ss_miss shape)
    publishRenamingTable is_early public owner [] binders result (const branches)

publishScopeLookupExtension :: Bool -> (TyCon -> Bool) -> Id -> [Id] -> ScopeShape
                            -> Maybe (TyCon, Maybe TyCon) -> TcM (Maybe (Id, [TyCon]))
publishScopeLookupExtension is_early public owner helpers shape mb_subst = runMaybeT $ do
    let context = ss_context shape; mb_sort = ss_sort shape; term = ss_term shape
        ext = ss_ext shape; index = ss_index shape; var = ss_var shape
        hit = ss_hit shape; miss = ss_miss shape; output = if isJust mb_subst then term else index
    (mb_ren_weak, extra_tcs) <- case mb_subst of
      Nothing -> pure (Nothing, [])
      Just (ren_tc, Just w_tc) -> pure (Just (ren_tc, w_tc), [])
      Just (ren_tc, Nothing) | isNothing mb_sort -> do
        w_name <- lift (newImplicitBinder (idName owner) (mkTcOcc . ("$weaken:" ++) . occNameString))
        w_axiom <- lift (newImplicitBinder w_name mkInstTyCoOcc)
        (w_binders, w_out, mb_w_brs) <- lift (buildWeakeningParts context Nothing index ext miss)
        w_brs <- MaybeT (pure mb_w_brs)
        let w_tc = mkFamily w_name w_axiom w_binders w_out w_brs
        guard (all validBranch w_brs && all public (sourceEquationDatatypes [w_tc]))
        pure (Just (ren_tc, w_tc), [w_tc])
      Just _ -> MaybeT (pure Nothing)
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    (b_vars, a_vars, mb_rank, cb_occ) <- case mb_sort of
      Just sort -> (\b a rk -> ([b], [a], Just rk, "rho"))
                     <$> lift (freshVar "binder" sort) <*> lift (freshVar "a" sort) <*> lift (freshVar "sort" sort)
      Nothing -> pure ([], [], Nothing, "r")
    let gk = mkTyVarTy g; dk = mkTyVarTy d
        b_tys = mkTyVarTys b_vars; a_tys = mkTyVarTys a_vars
        lookupAt c ks = mkTyConApp index (c : ks)
        outputAt c ks = mkTyConApp output (c : ks)
        extend c = mkTyConApp ext (c : b_tys)
        domain = lookupAt (extend gk) a_tys
        result = outputAt (extend dk) a_tys
    rho <- lift (freshVar cb_occ (closureCallbackKind index output mb_rank gk dk))
    input <- lift (freshVar "input" domain)
    old <- lift (freshVar "index" (lookupAt gk a_tys))
    let r = mkTyVarTy rho; x = mkTyVarTy old
        binders = mkNamedTyConBinders Specified ([g, d] ++ b_vars) ++ [mkAnonTyConBinder rho] ++
                  mkNamedTyConBinders Specified a_vars ++ [mkAnonTyConBinder input]
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
          guard (typeKind fresh' `eqType` outputAt (extend dk) b_tys && typeKind previous' `eqType` result)
          pure [renamingBranch (lhsFor b_tys fresh) fresh', renamingBranch (lhsFor a_tys previous) previous']
    (new_owner, [table]) <- publishRenamingTable is_early public owner helpers binders result (const branches)
    pure (new_owner, table : extra_tcs)

publishTraversalFamily :: Bool -> Bool -> (TyCon -> Bool) -> [(Name, TyCon)]
                       -> (Id, (Id, ScopeShape), [Id]) -> TcM (Maybe (Id, [TyCon]))
publishTraversalFamily is_subst is_early public extensions (owner, (extension, shape), helpers) = runMaybeT $ do
    extend <- MaybeT (pure (lookup (idName extension) extensions))
    let context = ss_context shape; mb_sort = ss_sort shape; term = ss_term shape
        ext = ss_ext shape; index = ss_index shape; var = ss_var shape
        output = if is_subst then term else index
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    (a_vars, mb_rank, cb_occ) <- case mb_sort of
      Just sort -> (\a rk -> ([a], Just rk, "rho")) <$> lift (freshVar "a" sort) <*> lift (freshVar "sort" sort)
      Nothing   -> pure ([], Nothing, "r")
    let cbKind = closureCallbackKind index output mb_rank
        gk = mkTyVarTy g; dk = mkTyVarTy d; a_tys = mkTyVarTys a_vars
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
          let r = mkTyVarTy local; field_tys = mkTyVarTys args
              lhs = mkTyConApp (DC.promoteDataCon con) (mkTyVarTys vars ++ field_tys)
              liftField 0 ctx = (dk, r) <$ guard (ctx `eqType` source)
              liftField depth ctx = do
                (cl, outer : intro_ks) <- splitTyConApp_maybe ctx
                guard (cl == ext && length intro_ks == length a_vars)
                (target, inherited) <- liftField (depth - 1) outer
                pure (mkTyConApp ext (target : intro_ks), mkTyConApp extend ([outer, target] ++ intro_ks ++ [inherited]))
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
                      if is_subst then mapped <$ guard (typeKind mapped `eqType` termAt dk sort_ks)
                                  else conAt con (termAt dk sort_ks) [mapped]
              else zipWithM (rewrite family) depths field_tys >>= conAt con (termAt dk sort_ks)
            pure (renamingBranch ([source, dk] ++ sort_ks ++ [r, lhs]) out)
    rows <- traverse prepare ((var, []) : ss_recipes shape)
    publishRenamingTable is_early public owner helpers binders result (\family -> traverse ($ family) rows)

allocateClosureFamilies :: Bool -> ScopeShape -> TcM (Maybe (TyCon, [TyCon]))
allocateClosureFamilies is_early shape = do
    let owner = ss_owner shape; arity = ss_arity shape; context = ss_context shape
        mb_sort = ss_sort shape; term = ss_term shape; ext = ss_ext shape
        lookup_tycon = ss_index shape; var = ss_var shape; hit = ss_hit shape
        miss = ss_miss shape; recipes = ss_recipes shape
        localName p = newImplicitBinder (idName owner) (mkTcOcc . (p ++) . occNameString)
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
          Just sort -> (\a b cks -> ([a], [b], cks))
                         <$> freshVar "a" sort <*> freshVar "b" sort
                         <*> traverse (\_ -> freshVar "captureKind" sort) [1 .. arity]
          Nothing -> pure ([], [], [])
        let gk = mkTyVarTy g; dk = mkTyVarTy d
            a_tys = mkTyVarTys a_vars; b_tys = mkTyVarTys b_vars
            termAt p ks = mkTyConApp term (p : ks)
            ctxAt p ks = mkTyConApp ext (p : ks)
            lookupAt p ks = mkTyConApp lookup_tycon (p : ks)
            closureAt p q = mkTyConApp closure [p, q]
            ctx_g = ctxAt gk a_tys; ctx_d = ctxAt dk a_tys
            capture_steps = maybe (map (const []) [1 .. arity]) (const (map pure (mkTyVarTys capture_kinds))) mb_sort
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
        let c = mkTyVarTy cv; x = mkTyVarTy field; w = mkTyVarTy carried
            outputs = termAt dk b_tys
            prefix = mkNamedTyConBinders Specified (g : d : b_vars)
            applyAt s t ks env val = mkTyConApp apply ([s, t] ++ ks ++ [env, val])
            lookupFn s t ks env val = mkTyConApp lookup_fam ([s, t] ++ ks ++ [env, val])
            apply = mkFamily apply_name apply_axiom (prefix ++ map mkAnonTyConBinder [cv, tree]) outputs (fromMaybe [] apply_branches)
            lookup_fam = mkFamily lookup_name lookup_axiom (prefix ++ map mkAnonTyConBinder [cv, field]) outputs (fromMaybe [] lookup_branches)
            applyBranch (ctor, exten, tvs, vars, res, env_var) = do
              (actual_term, src : sort_ks) <- splitTyConApp_maybe res
              guard (actual_term == term && length sort_ks == length b_vars && (ctor == var || length exten == length vars))
              let fields = mkTyVarTys vars; env = mkTyVarTy env_var
                  lhs = mkTyConApp (DC.promoteDataCon ctor) (mkTyVarTys tvs ++ fields)
                  liftField 0 source = (dk, env) <$ guard (source `eqType` src)
                  liftField depth source = do
                    (scope, outer : intro_ks) <- splitTyConApp_maybe source
                    guard (scope == ext && length intro_ks == length b_vars)
                    (target, inner) <- liftField (depth - 1) outer
                    let extended = ctxAt target intro_ks
                    (extended,) <$> conAt lift_con (closureAt source extended) [inner]
                  use depth value = do
                    (same, source : ks) <- splitTyConApp_maybe (typeKind value)
                    guard (same == term && length ks == length b_vars)
                    (target, closure_val) <- liftField depth source
                    pure (applyAt source target ks closure_val value)
              out <- if ctor == var
                then do [lookup_var] <- pure fields; pure (lookupFn src dk sort_ks env lookup_var)
                else zipWithM use exten fields >>= conAt ctor (termAt dk sort_ks)
              guard (typeKind lhs `eqType` res && typeKind out `eqType` termAt dk sort_ks)
              let branch_lhs = [src, dk] ++ sort_ks ++ [env, lhs]
                  branch_vars = if isJust mb_sort then tvs ++ [d, env_var] ++ vars else tyCoVarsOfTypesWellScoped branch_lhs
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
              next <- conAt replace_next (closureAt ctx_g dk) [c, w]
              pure [ mkBranch ([d] ++ a_vars ++ [carried]) ([ctx_d, dk] ++ a_tys ++ [r, z_in]) w
                   , mkBranch ([d] ++ a_vars ++ b_vars ++ [carried, x_d]) ([ctx_d, dk] ++ b_tys ++ [r, s_in]) v_d
                   , mkBranch ([g] ++ a_vars ++ b_vars ++ [field]) ([gk, ctx_g] ++ b_tys ++ [s, x]) v_out
                   , mkBranch ([g, d] ++ a_vars ++ [cv]) ([ctx_g, ctx_d] ++ a_tys ++ [l, z_g]) v0
                   , mkBranch ([g, d] ++ a_vars ++ b_vars ++ [cv, field]) ([ctx_g, ctx_d] ++ b_tys ++ [l, sx])
                       (applyAt dk ctx_d b_tys s_d (lookupFn gk dk b_tys c x))
                   , mkBranch ([g, d] ++ a_vars ++ [cv, carried]) ([ctx_g, dk] ++ a_tys ++ [next, z_g]) w
                   , mkBranch ([g, d] ++ a_vars ++ b_vars ++ [cv, carried, field]) ([ctx_g, dk] ++ b_tys ++ [next, sx])
                       (lookupFn gk dk b_tys c x) ]
        pure $ do
          apply_eqs <- apply_branches
          lookup_eqs <- lookup_branches
          (first_ks, first_arg) : rest <- pure (zip capture_steps (mkTyVarTys args))
          let initial_ctx = ctxAt gk first_ks
              addRep (src, env) (ks, val) = let nc = ctxAt src ks in (nc,) <$> conAt replace_next (closureAt nc gk) [env, val]
          first <- conAt replace (closureAt initial_ctx gk) [first_arg]
          (_, replacements) <- foldM addRep (initial_ctx, first) rest
          let index_vars = g : capture_kinds ++ b_vars; value_vars = body : args; vars = index_vars ++ value_vars
              params = mkNamedTyConBinders Specified index_vars ++ map mkAnonTyConBinder value_vars
              root_eq = mkBranch vars (mkTyVarTys vars) (applyAt body_context gk b_tys replacements (mkTyVarTy body))
              root = mkFamily root_name root_axiom params (termAt gk b_tys) [root_eq]
          guard (all validBranch (root_eq : apply_eqs ++ lookup_eqs))
          pure (root, [root, closure, apply, lookup_fam])
      _ -> pure Nothing

allocateClosureData :: Id -> Kind -> Maybe Kind -> TyCon -> TyCon -> TcM (TyCon, [DC.DataCon])
allocateClosureData owner context_kind mb_sort_kind term ext = do
    let tag p occ = p ++ occNameString occ
        mkData p = newImplicitBinder (idName owner) (mkDataOcc . tag p)
    closure_name <- newImplicitBinder (idName owner) (mkTcOcc . tag "$closure:")
    closure_rep <- newTyConRepName closure_name
    replace_n <- mkData "$replace:"
    shift_n <- mkData "$shift:"
    lift_n <- mkData "$lift:"
    next_n <- mkData "$replaceNext:"
    let names = [replace_n, shift_n, lift_n, next_n]
        tags = mkNameEnv (zip names [1..])
        ordinary = DC.HsSrcBang NoSourceText DC.NoSrcUnpack DC.NoSrcStrict
    source <- freshVar "source" context_kind
    target <- freshVar "target" context_kind
    incoming <- freshVar "incoming" context_kind
    outgoing <- freshVar "outgoing" context_kind
    b_vars <- maybe (pure []) (fmap pure . freshVar "binder") mb_sort_kind
    let universals = [source, target]
        source_ty = mkTyVarTy source; target_ty = mkTyVarTy target
        incoming_ty = mkTyVarTy incoming; outgoing_ty = mkTyVarTy outgoing
        b_tys = mkTyVarTys b_vars
        extend ctx = mkTyConApp ext (ctx : b_tys)
        termAt ctx = mkTyConApp term (ctx : b_tys)
        binders = map mkAnonTyConBinder universals
    closure <- mfix $ \result -> do
      let applied a b = mkTyConApp result [a, b]
          ctor nm exs user_tvs eqs fields res = do
            rep <- newTyConRepName nm
            buildDataCon emptyFamInstEnvs (FixedBangOpts (map (const DC.HsLazy) fields))
              nm False rep (map (const ordinary) fields) [] universals exs
              (mkTyVarBinders Specified user_tvs) eqs [] (map (Scaled manyDataConTy) fields) res result tags
      replace <- ctor replace_n b_vars (target : b_vars) [DC.mkEqSpec source (extend target_ty)]
                   [termAt target_ty] (applied (extend target_ty) target_ty)
      shift <- ctor shift_n b_vars (source : b_vars) [DC.mkEqSpec target (extend source_ty)]
                 [] (applied source_ty (extend source_ty))
      lift_con <- ctor lift_n (incoming : outgoing : b_vars) (incoming : outgoing : b_vars)
                    [DC.mkEqSpec source (extend incoming_ty), DC.mkEqSpec target (extend outgoing_ty)]
                    [applied incoming_ty outgoing_ty] (applied (extend incoming_ty) (extend outgoing_ty))
      replace_next <- ctor next_n (incoming : b_vars) (incoming : target : b_vars)
                        [DC.mkEqSpec source (extend incoming_ty)]
                        [applied incoming_ty target_ty, termAt target_ty] (applied (extend incoming_ty) target_ty)
      pure (mkAlgTyCon closure_name (mkTyConKind binders liftedTypeKind) binders 0 liftedTypeKind [Nominal, Nominal]
              Nothing [] (mkLevPolyDataTyConRhs True True [replace, shift, lift_con, replace_next])
              (VanillaAlgTyCon closure_rep) True)
    pure (closure, tyConDataCons closure)

allocateUntypedSubstViaSubstU :: Bool -> TyCon -> ScopeShape -> TcM (Maybe (TyCon, [TyCon]))
allocateUntypedSubstViaSubstU is_early subst_tc shape = do
    let owner = ss_owner shape; arity = ss_arity shape; context = ss_context shape
        term = ss_term shape; ext = ss_ext shape; lookup_tycon = ss_index shape
        var = ss_var shape; hit = ss_hit shape; miss = ss_miss shape
    map_name <- newImplicitBinder (idName owner) (mkTcOcc . ("$substMap:" ++) . occNameString)
    map_axiom <- newImplicitBinder map_name mkInstTyCoOcc
    root_name <- newImplicitBinder (idName owner) mkSourceTableOcc
    root_axiom <- mkClosureTableAxiom is_early root_name
    g <- freshVar "g" context
    let gk = mkTyVarTy g
        termAt p = mkTyConApp term [p]; ctxAt p = mkTyConApp ext [p]; lookupAt p = mkTyConApp lookup_tycon [p]
        body_ctx = foldl' (\k _ -> ctxAt k) gk [1 .. arity]
    body <- freshVar "n" (termAt body_ctx)
    args <- traverse (\_ -> freshVar "v" (termAt gk)) [1 .. arity]
    map_input <- freshVar "input" (lookupAt body_ctx)
    older_x <- freshVar "x" (lookupAt gk)
    let arg_tys = mkTyVarTys args
        wrap_miss 0 ctx pat_fn = pat_fn ctx
        wrap_miss d ctx pat_fn = do
          (cl, [inner_ctx]) <- splitTyConApp_maybe ctx
          guard (cl == ext)
          wrap_miss (d - 1) inner_ctx pat_fn >>= conAt miss (lookupAt ctx) . pure
    pure $ do
      hit_brs <- traverse (\(d, r_ty) -> (\p -> renamingBranch (gk : arg_tys ++ [p]) r_ty)
                                           <$> wrap_miss d body_ctx (\ctx -> conAt hit (lookupAt ctx) []))
                          (zip [0 .. arity - 1] (reverse arg_tys))
      m_pat <- wrap_miss arity body_ctx (\_ -> Just (mkTyVarTy older_x))
      m_rhs <- conAt var (termAt gk) [mkTyVarTy older_x]
      let map_brs = hit_brs ++ [renamingBranch (gk : arg_tys ++ [m_pat]) m_rhs]
          map_fam = mkFamily map_name map_axiom (mkNamedTyConBinders Specified [g] ++ map mkAnonTyConBinder (args ++ [map_input]))
                      (termAt gk) map_brs
          partial_map = mkTyConApp map_fam (gk : arg_tys)
      guard (typeKind partial_map `eqType` closureCallbackKind lookup_tycon term Nothing body_ctx gk)
      let root_rhs = mkTyConApp subst_tc [body_ctx, gk, partial_map, mkTyVarTy body]
          value_vars = body : args; vars = g : value_vars
          root_eq = mkBranch vars (mkTyVarTys vars) root_rhs
          root = mkFamily root_name root_axiom (mkNamedTyConBinders Specified [g] ++ map mkAnonTyConBinder value_vars)
                   (termAt gk) [root_eq]
      guard (typeKind root_rhs `eqType` termAt gk && tyConKind root `eqType` varType owner &&
             all validBranch (root_eq : map_brs))
      pure (root, [root, map_fam])

-- ---------------------------------------------------------------------------
-- Shared early & post-tidy stage orchestrator (SC1)
-- ---------------------------------------------------------------------------

publishCheckedSourceClosures :: [CoreBind] -> [CoreBind] -> TypeEnv -> TcM ([Id], [TyCon])
publishCheckedSourceClosures original_binds final_binds final_types = do
    env <- getGblEnv
    rdr_env <- getGlobalRdrEnv
    opts <- initSimpleOpts <$> getDynFlags
    visible <- visibleSourceMethodRules env
    rule_heads <- visibleClosureRuleHeads env
    let here = Just (tcg_mod env)
        exported = availsToNameSet (tcg_exports env)
        public = publicCaseType env exported (\n -> isWiredInName n || ordinaryImport rdr_env n)
        unoptimized = stripBreakpointTicks original_binds
        original = map (safeSimpleOptBind opts) unoptimized
        final = stripBreakpointTicks final_binds
        unopt_env = mkBindEnv unoptimized; orig_env = mkBindEnv original; fin_env = mkBindEnv final
        floated v = isFloatedConstructorHelper ((`elemNameSet` exported) . idName) fin_env v || isPatErrorHelper fin_env v
        lookupLocal name = case lookupTypeEnv final_types name of
          Just (AnId actual) | idName actual == name, nameModule_maybe name == here -> Just actual
          _ -> Nothing
        lookupOwner old = do
          let owner = idName old
          guard (isExternalName owner && owner `elemNameSet` exported)
          actual <- lookupLocal owner
          actual <$ case idDetails actual of { VanillaId -> Just (); _ -> Nothing }
        lookupClosureRoot old = do
          let owner = idName old
          guard (isExternalName owner && owner `elemNameSet` exported)
          actual <- lookupLocal owner
          actual <$ case idDetails actual of { VanillaId -> Just (); CheckedClosureSourceTableId{} -> Just (); _ -> Nothing }
        resolveDepId v | nameModule_maybe (idName v) == here = lookupClosureRoot v
                       | isJust (sourceTableId_maybe v) = Just v
                       | otherwise = Nothing
        extDeps keep old actual =
          traverse lookupLocal $ nameSetElemsStable $ mkNameSet $
            filter (\n -> n /= idName old && keep n) (reachable orig_env old ++ reachable fin_env actual)
        onlyCalls allowed old actual = do
          old_rhs <- lookupVarEnv unopt_env old
          new_rhs <- lookupVarEnv fin_env actual
          guard (hasOnlyAllowedCalls (const False) allowed old_rhs && hasOnlyAllowedCalls floated allowed new_rhs)
        safe owner hs = all (\v -> (isNominalStructuralSource v || not (hasSourceMethodRule visible v)) && not (idName v `elemNameSet` rule_heads)) (owner : hs)
        init_tables = [(idName v, tc) | v <- typeEnvIds final_types, Just tc <- [sourceTableId_maybe v]]
    entries <- runClosureStage False public (map fst original) orig_env (Just fin_env)
                 lookupOwner lookupClosureRoot resolveDepId extDeps onlyCalls safe init_tables
    pure ([v | (v, _, _) <- entries], [tc | (_, tcs, _) <- entries, tc <- tcs])

sourceEarlyClosureTables :: [Id] -> NameEnv (Id, CoreExpr) -> TcM ([(Id, TyCon)], [TyCon], [Id])
sourceEarlyClosureTables group_owners definitions = do
    env <- getGblEnv
    dflags <- getDynFlags
    hsc <- getTopEnv
    if hasActiveLatePlugins hsc then pure ([], [], []) else do
      let here = Just (tcg_semantic_mod env)
          rdr_env = tcg_rdr_env env
          available name
            | nameModule_maybe name == here = maybe False isLocalGRE (lookupGRE_Name rdr_env name)
            | otherwise = isWiredInName name || ordinaryImport rdr_env name
          public tc = all available (tyConName tc : map DC.dataConName (tyConDataCons tc))
          orig_env = mkBindEnv (map (safeSimpleOptBind (initSimpleOpts dflags)) (nonDetNameEnvElts definitions))
          in_group_set = mkNameSet (map idName group_owners)
          lookupDef name = fst <$> lookupNameEnv definitions name
          lookupOwner v = do
            actual <- lookupDef (idName v) <|> (v <$ guard (idName v `elemNameSet` in_group_set))
            actual <$ guard (idName actual `elemNameSet` in_group_set && isNothing (sourceTableId_maybe actual))
          lookupClosureRoot v = lookupDef (idName v) <|> (v <$ guard (idName v `elemNameSet` in_group_set))
          resolveDepId v
            | nameModule_maybe (idName v) == here =
                lookupDef (idName v) <|> case lookupTypeEnv (tcg_type_env env) (idName v) of
                  Just (AnId actual) | isJust (sourceTableId_maybe actual) -> Just actual
                  _ -> Nothing
            | isJust (sourceTableId_maybe v) = Just v
            | otherwise = Nothing
          extDeps keep old _ =
            traverse lookupDef $ nameSetElemsStable $ mkNameSet $
              filter (\n -> n /= idName old && keep n && nameModule_maybe n == here) (reachable orig_env old)
          onlyCalls allowed old _ = do
            raw_rhs <- snd <$> lookupNameEnv definitions (idName old)
            opt_rhs <- lookupVarEnv orig_env old
            guard (hasOnlyAllowedCalls (const False) allowed raw_rhs && hasOnlyAllowedCalls (const False) allowed opt_rhs)
          init_tables = [(idName v, tc) | v <- group_owners ++ typeEnvIds (tcg_type_env env), Just tc <- [sourceTableId_maybe v]]
      entries <- runClosureStage True public group_owners orig_env Nothing
                   lookupOwner lookupClosureRoot resolveDepId extDeps onlyCalls (\_ _ -> True) init_tables
      pure ( [(v, tc) | (v, tc : _, _) <- entries]
           , [tc | (_, _ : rest, _) <- entries, tc <- rest]
           , nubBy (\a b -> idName a == idName b) [h | (_, _, hs) <- entries, h <- hs, nameModule_maybe (idName h) == here] )

runClosureStage
  :: Bool -> (TyCon -> Bool) -> [Id] -> BindEnv -> Maybe BindEnv
  -> (Id -> Maybe Id) -> (Id -> Maybe Id) -> (Id -> Maybe Id)
  -> ((Name -> Bool) -> Id -> Id -> Maybe [Id]) -> ([Name] -> Id -> Id -> Maybe ())
  -> (Id -> [Id] -> Bool) -> [(Name, TyCon)] -> TcM [(Id, [TyCon], [Id])]
runClosureStage is_early public candidates orig_env mb_fin_env
                lookupOwner lookupClosureRoot resolveDepId extDeps onlyCalls safe init_tables = do
    let sameId a b = idName a == idName b
        checkModScope shape = isNothing mb_fin_env ||
          let req = [ss_term shape, ss_index shape] ++ [ss_ext shape | isJust (ss_sort shape)]
              owner_mod = nameModule_maybe (idName (ss_owner shape))
          in all ((== owner_mod) . nameModule_maybe . tyConName) req
        closure_cands = flip mapMaybe candidates $ \old -> do
          orig_s <- lookupVarEnv orig_env old >>= scopeFamilyInputs orig_env old
          guard (checkModScope orig_s)
          (actual, fin_s) <- case mb_fin_env of
            Nothing -> (, orig_s) <$> lookupOwner old
            Just fin_env -> do
              act <- lookupClosureRoot old
              raw_fin_rhs <- lookupVarEnv fin_env act
              let (_, eff_fin_rhs) = resolveForwardCseAlias fin_env act raw_fin_rhs
              fs <- scopeFamilyInputs fin_env act eff_fin_rhs
              (act, fs) <$ guard (sameScopeShape orig_s fs)
          hs <- extDeps isExternalName old actual
          (actual, (orig_s, fin_s), hs) <$ guard (not (null hs) && safe actual hs)

        orig_direct = directScopeShapes orig_env candidates
        fin_direct = maybe [] (\fe -> directScopeShapes fe [v | v <- candidates, isJust (lookupVarEnv fe v)]) mb_fin_env
        direct_pairs = maybe [(s, s) | s <- orig_direct] (const
          [ (o_s, f_s) | o_s <- orig_direct, checkModScope o_s
                       , Just f_s <- [find (\fs -> sameId (ss_owner o_s) (ss_owner fs)) fin_direct]
                       , sameScopeShape o_s f_s ]) mb_fin_env
        all_shape_pairs = [(o, f) | (_, (o, f), _) <- closure_cands] ++ direct_pairs

        ren_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b) $ flip mapMaybe all_shape_pairs $ \(o_s, f_s) -> do
          let (o_ren, o_ext) = ss_ren_ops o_s; (f_ren, f_ext) = ss_ren_ops f_s
          guard (sameId o_ren f_ren && sameId o_ext f_ext)
          act_ren <- lookupClosureRoot f_ren
          act_ext <- lookupClosureRoot f_ext
          onlyCalls [] o_ext act_ext
          hs <- extDeps isExternalName o_ren act_ren
          (act_ren, (act_ext, f_s), hs) <$ guard (any (sameId act_ext) hs && safe act_ren hs)

        ext_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
          [(act_ext, f_s, []) | (_, (act_ext, f_s), _) <- ren_cands, isJust (lookupOwner act_ext), safe act_ext []]

    pub_exts <- fmap catMaybes $ for_list ext_cands $ \(act_ext, s, _) ->
      fmap (\(v, tcs) -> (v, tcs, [])) <$> publishScopeLookupExtension is_early public act_ext [] s Nothing
    let ext_tables = [(idName v, tc) | (v, tc : _, _) <- pub_exts] ++ init_tables

    pub_rens <- fmap catMaybes $ for_list [c | c@(act_ren, _, _) <- ren_cands, isJust (lookupOwner act_ren)] $
      \cand@(_, _, hs) -> fmap (\(v, tcs) -> (v, tcs, hs)) <$> publishTraversalFamily False is_early public ext_tables cand
    let ren_tables = [(idName v, tc) | (v, tc : _, _) <- pub_rens] ++ ext_tables

        sext_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b) $ flip mapMaybe all_shape_pairs $ \(o_s, f_s) -> do
          let (o_ren, _) = ss_ren_ops o_s; (f_ren, _) = ss_ren_ops f_s
          (_, o_sext, mb_o_weak) <- ss_subst_ops o_s
          (_, f_sext, mb_f_weak) <- ss_subst_ops f_s
          guard (sameId o_ren f_ren && sameId o_sext f_sext && (isNothing (ss_sort o_s) || isJust mb_o_weak) &&
                 maybe True (\fw -> fmap idName mb_o_weak == Just (idName fw)) mb_f_weak)
          act_ren <- lookupClosureRoot f_ren
          act_sext <- lookupOwner f_sext
          mb_act_weak <- case mb_o_weak of
            Nothing -> pure Nothing
            Just ow -> do
              aw <- lookupClosureRoot ow
              guard (maybe False (isJust . validConstructorAliasBody) (lookupVarEnv orig_env ow))
              for_ mb_fin_env (\fe -> guard (maybe False (isJust . validConstructorAliasBody) (lookupVarEnv fe aw)))
              Just aw <$ onlyCalls [] ow aw
          onlyCalls (idName f_ren : maybe [] (pure . idName) mb_o_weak) o_sext act_sext
          hs <- nubBy sameId . (maybeToList mb_act_weak ++) <$> extDeps isExternalName o_sext act_sext
          (act_sext, (act_ren, mb_act_weak, f_s), hs) <$ guard (any (sameId act_ren) hs && safe act_sext hs)

        weak_cands = nubBy (\a b -> sameId (fst a) (fst b))
          [(aw, f_s) | (_, (_, Just aw, f_s), _) <- sext_cands, isJust (lookupOwner aw), safe aw []]

    pub_weaks <- fmap catMaybes $ for_list weak_cands $ \pair ->
      fmap (\(v, tcs) -> (v, tcs, [])) <$> publishWeakeningFamily is_early public pair
    let weak_tables = [(idName v, tc) | (v, tc : _, _) <- pub_weaks] ++ ext_tables

    pub_sexts <- fmap catMaybes $ for_list sext_cands $ \(act_sext, (act_ren, mb_aw, f_s), hs) -> runMaybeT $ do
      ren_tc <- MaybeT (pure (lookup (idName act_ren) ren_tables))
      w_tc_opt <- MaybeT (pure (maybe (Just Nothing) (\aw -> Just <$> lookup (idName aw) weak_tables) mb_aw))
      (v, tcs) <- MaybeT (publishScopeLookupExtension is_early public act_sext hs f_s (Just (ren_tc, w_tc_opt)))
      pure (v, tcs, hs)
    let sext_tables = [(idName v, tc) | (v, tc : _, _) <- pub_sexts] ++ ext_tables

        subst_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b) $ flip mapMaybe all_shape_pairs $ \(o_s, f_s) -> do
          let (o_ren, _) = ss_ren_ops o_s; (f_ren, _) = ss_ren_ops f_s
          (o_sub, o_sext, mb_o_weak) <- ss_subst_ops o_s
          (f_sub, f_sext, mb_f_weak) <- ss_subst_ops f_s
          guard (map idName [o_sub, o_ren, o_sext] == map idName [f_sub, f_ren, f_sext] &&
                 (isNothing (ss_sort o_s) || isJust mb_o_weak) &&
                 maybe True (\fw -> fmap idName mb_o_weak == Just (idName fw)) mb_f_weak)
          act_sub <- lookupOwner f_sub
          act_sext <- lookupClosureRoot f_sext
          mb_act_weak <- traverse lookupClosureRoot mb_o_weak
          onlyCalls ([idName f_sub, idName f_ren, idName f_sext] ++ maybe [] (pure . idName) mb_o_weak) o_sub act_sub
          hs <- nubBy sameId . (maybeToList mb_act_weak ++) <$> extDeps isExternalName o_sub act_sub
          (act_sub, (act_sext, f_s), hs) <$ guard (any (sameId act_sext) hs && safe act_sub hs)

    pub_substs <- fmap catMaybes $ for_list subst_cands $ \cand@(_, _, hs) ->
      fmap (\(v, tcs) -> (v, tcs, hs)) <$> publishTraversalFamily True is_early public sext_tables cand
    let subst_tables = [(idName v, tc) | (v, tc : _, _) <- pub_substs] ++ sext_tables

    pub_closures <- fmap catMaybes $ for_list closure_cands $ \(owner, (_, shape), hs) -> runMaybeT $ do
      _ <- MaybeT (pure (lookupOwner owner))
      let mb_subst_u_tc = do
            guard (isNothing (ss_sort shape))
            (sub_u, _, _) <- ss_subst_ops shape
            lookup (idName sub_u) subst_tables <|> sourceTableId_maybe sub_u
      (root, families) <- MaybeT $ maybe (allocateClosureFamilies is_early shape)
                                         (\su_tc -> allocateUntypedSubstViaSubstU is_early su_tc shape) mb_subst_u_tc
      guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
      pure (setIdDetails owner (CheckedClosureSourceTableId (idName owner) root (map idName hs)), families, hs)

    let phase1 = pub_exts ++ pub_rens ++ pub_weaks ++ pub_sexts ++ pub_substs ++ pub_closures
        done1 = mkNameSet [idName v | (v, _, _) <- phase1]
        tables1 = [(idName v, tc) | (v, tc : _, _) <- phase1] ++ init_tables
    (phase1 ++) <$> runIndexedPasses done1 tables1
  where
    for_list = flip traverse
    sameId a b = idName a == idName b

    runIndexedPasses done known_tables = do
      let resolveTable v = lookup (idName v) known_tables <|> sourceTableId_maybe v
      pass_entries <- fmap catMaybes $ for_list candidates $ \old -> runMaybeT $ do
        guard (not (idName old `elemNameSet` done))
        actual <- MaybeT (pure (lookupOwner old))
        orig_rhs <- MaybeT (pure (lookupVarEnv orig_env old))
        orig_ci <- compileIndexedCore orig_env resolveTable Nothing Nothing old orig_rhs
        sem_callees <- MaybeT (pure (traverse resolveDepId (ci_table_callees orig_ci)))
        (mb_fin_ci, mb_alias) <- case mb_fin_env of
          Nothing -> pure (Nothing, Nothing)
          Just fin_env -> do
            raw_fin_rhs <- MaybeT (pure (lookupVarEnv fin_env actual))
            let (mb_alias, eff_fin_rhs) = resolveForwardCseAlias fin_env actual raw_fin_rhs
                d_tc = mkFamily (idName actual) (idName actual) (ci_binders orig_ci) (ci_res_kind orig_ci) []
                exp_brs = [applyRow eq d_tc | (eq, _) <- ci_rows orig_ci]
            f_ci <- compileIndexedCore fin_env resolveTable (ci_case_names orig_ci) (Just exp_brs) actual eff_fin_rhs
            pure (Just f_ci, mb_alias)
        let fin_w = mb_fin_ci >>= ci_mb_worker; orig_w = ci_mb_worker orig_ci
            workers = maybeToList orig_w ++ maybeToList fin_w
            base_allowed = idName old : map idName (workers ++ sem_callees)
            full_allowed = base_allowed ++ maybe [] (pure . idName) mb_alias
        hs <- MaybeT $ pure $ do
          onlyCalls full_allowed old actual
          for_ mb_alias (onlyCalls base_allowed old)
          local_hs <- extDeps isExternalName old actual
          let hs = [h | h <- nubBy sameId (sem_callees ++ local_hs), not (any (sameId h) workers)]
          hs <$ guard (safe actual hs)
        (v, tcs) <- MaybeT (publishCompiledIndexedM is_early public actual orig_ci mb_fin_ci hs)
        pure (v, tcs, hs)
      if null pass_entries then pure [] else do
        let done' = done `extendNameSetList` [idName v | (v, _, _) <- pass_entries]
            tables' = [(idName v, tc) | (v, tc : _, _) <- pass_entries] ++ known_tables
        (pass_entries ++) <$> runIndexedPasses done' tables'

-- ---------------------------------------------------------------------------
-- Early closure pruning, export validation, Core agreement, and finalization
-- ---------------------------------------------------------------------------

earlySourceClosureEntries :: TcGblEnv -> [(Id, TyCon, [Name], [TyCon])]
earlySourceClosureEntries env =
    sortBy (\(a, _, _, _) (b, _, _, _) -> stableNameCmp (idName a) (idName b))
      [(fun, table, helpers, companions fun) | fun <- typeEnvIds (tcg_type_env env), Just (table, helpers) <- [classify fun]]
  where
    companions = companionTyConLookup (tcg_tcs env)
    classify fun = case idDetails fun of
      CheckedClosureSourceTableId name table hs | name == idName fun, isEarlyClosureTable table -> Just (table, hs)
      SourceTableId name table NominalSourceTable | name == idName fun, isEarlyClosureTable table -> Just (table, [])
      _ -> Nothing

earlyIndexedLookups :: TcGblEnv -> [(Id, TyCon)]
earlyIndexedLookups env = [(fun, table) | (fun, table, _, _) <- earlySourceClosureEntries env]

pruneUnusedIndexedLookups :: TcGblEnv -> TcM TcGblEnv
pruneUnusedIndexedLookups env = do
    direct_used <- scs_table_uses <$> readTcRef (tcg_source_check env)
    let entries = earlySourceClosureEntries env
        table_map = mkNameEnv [(tyConName table, table) | (_, table, _, _) <- entries]
        close_uses seen [] = seen
        close_uses seen (nm : rest)
          | Just tc <- lookupNameEnv table_map nm, Just callees <- sourceTableCallees tc
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
            map tyConName removed_tcs ++ map getName (concatMap implicitTyConThings removed_tcs) ++ rep_names
          removed_tc_names = mkNameSet (map tyConName removed_tcs)
          clearThing (AnId v) | idName v `elemNameSet` owners = AnId (setIdDetails v VanillaId)
          clearThing thing = thing
          keepBind bind = not (any ((`elemNameSet` rep_set) . idName) (collectHsBindBinders CollNoDictBinders (unLoc bind)))
      pure env
        { tcg_tcs = filter (not . (`elemNameSet` removed_tc_names) . tyConName) (tcg_tcs env)
        , tcg_binds = filter keepBind (tcg_binds env)
        , tcg_type_env = mapNameEnv clearThing (filterNameEnv (not . (`elemNameSet` removed_names) . getName) (tcg_type_env env)) }

visibleClosureRuleHeads :: TcGblEnv -> TcM NameSet
visibleClosureRuleHeads env = do
    hsc <- getTopEnv
    liftIO $ visibleSourceRuleHeads hsc (tcg_mod env) (tcg_src env) (tcVisibleOrphanMods env)

rewriteEarlyClosureApps :: TyConSet -> Type -> Type
rewriteEarlyClosureApps early_set = go
  where
    go (AppTy a b) = AppTy (go a) (go b)
    go (TyConApp tc tys) =
      let tys' = map go tys
      in if tc `elementOfUniqSet` early_set
           then reductionReducedType (normaliseTcAppWithFuel 2000 emptyFamInstEnvs Nominal tc tys')
           else TyConApp tc tys'
    go (FunTy af ma mult arg res) = FunTy af (go ma) (go mult) (go arg) (go res)
    go ty = ty

data TyConScan = TyConScan !TyConSet !VarSet

survivingEarlyExportedTyCons :: [TyCon] -> [Type] -> TyConSet
survivingEarlyExportedTyCons [] tys = tyConsOfTypes tys
survivingEarlyExportedTyCons early_fams tys =
    case foldl' collect (TyConScan emptyUniqSet emptyVarSet) tys of { TyConScan tcs _ -> tcs }
  where
    !early_set = mkUniqSet early_fams
    collectVar acc@(TyConScan tcs seen_vs) v
      | v `elemVarSet` seen_vs = acc
      | otherwise = collect (TyConScan tcs (extendVarSet seen_vs v)) (varType v)
    collect (TyConScan tcs seen_vs) ty@(TyConApp tc args)
      | tc `elementOfUniqSet` early_set =
          TyConScan (tcs `unionUniqSets` tyConsOfType (rewriteEarlyClosureApps early_set ty)) seen_vs
      | otherwise = foldl' collect (TyConScan (addOneToUniqSet tcs tc) seen_vs) args
    collect acc (TyVarTy v)                = collectVar acc v
    collect acc (TermAppTy f a)            = collect (collectVar acc f) a
    collect acc (TermVarTy v)              = collectVar acc v
    collect acc (DepFunTy _ v b)           = collect (collectVar acc v) b
    collect acc (AppTy a b)                = collect (collect acc a) b
    collect acc (ForAllTy ma (Bndr v _) b) = collect (collectVar (collect acc ma) v) b
    collect acc (FunTy _ ma mult a r)      = foldl' collect acc [ma, mult, a, r]
    collect acc (CastTy t _)               = collect acc t
    collect acc (ExistsTy _ v b)           = collect (collectVar acc v) b
    collect acc (DepDictTy d b)            = collect (collectVar acc d) b
    collect acc (ClassAppTy _ t1 t2 t3 t4) = foldl' collect acc [t1, t2, t3, t4]
    collect acc _                          = acc

checkLocalIndexedLookupExports :: TcGblEnv -> TcM ()
checkLocalIndexedLookupExports env = do
    let candidates = earlySourceClosureEntries env
    unless (null candidates) $ do
      visible <- visibleSourceMethodRules env
      rule_heads <- visibleClosureRuleHeads env
      let exported = availsToNameSet (tcg_exports env)
          type_env = tcg_type_env env
          all_early_fams = [tc | (_, table, _, comps) <- candidates, tc <- table : comps]
          exported_tycons = survivingEarlyExportedTyCons all_early_fams (exportedInterfaceTypes exported type_env)
          lookupId nm = case lookupTypeEnv type_env nm of { Just (AnId v) -> Just v; _ -> Nothing }
      for_ candidates $ \(owner, table, helpers, companions) -> do
        let name = idName owner; all_families = table : companions
            local_eq_types = [tc | tc <- sourceEquationDatatypes all_families, tc `notElem` all_families
                                 , nameModule_maybe (tyConName tc) == Just (tcg_semantic_mod env)]
            unexported_types = sortBy stableNameCmp [tyConName tc | tc <- local_eq_types, not (publicCaseType env exported (const False) tc)]
            in_iface = any (`elementOfUniqSet` exported_tycons) all_families
            needs_pub = in_iface || name `elemNameSet` exported
            _ = lookupId
            local_hs = filter (\h -> nameModule_maybe h == Just (tcg_semantic_mod env)) helpers
            unexported_vals = filter (not . (`elemNameSet` exported)) (name : local_hs)
            emitErr detail = setSrcSpan (nameSrcSpan name) $
              addErr (TcRnOpenTypeEval name Nothing ("The dependent type needs this equation. " ++ detail ++ " to make it available."))
        case unexported_types of
          hidden : _ | needs_pub -> emitErr ("Export the constructors of " ++ occNameString (nameOccName hidden))
          _ | in_iface, missing : _ <- unexported_vals -> emitErr ("Export " ++ occNameString (nameOccName missing))
          _ -> pure ()
        callee_owners <- traverse sourceTableOwner (fromMaybe [] (sourceTableCallees table))
        let imported_callees = [v | Just v <- callee_owners, nameModule_maybe (idName v) /= nameModule_maybe name]
        helper_ids <- catMaybes <$> traverse (\nm -> maybe (tcLookupIdMaybe nm) (pure . Just) (lookupId nm)) helpers
        for_ (find (\v -> (not (isNominalStructuralSource v) && hasSourceMethodRule visible v) || idName v `elemNameSet` rule_heads) (owner : helper_ids ++ imported_callees)) $
          \bad -> setSrcSpan (nameSrcSpan name) (addErr (TcRnRetainedQuantificationError (RetainedSourceRule (idName bad))))

exportedInterfaceTypes :: NameSet -> TypeEnv -> [Type]
exportedInterfaceTypes exported = concatMap entity_types . typeEnvElts
  where
    dcTypes dc = [varType (DC.dataConWrapId dc), varType (DC.dataConWorkId dc)]
    entity_types (AnId v) | idName v `elemNameSet` exported = [varType v]
    entity_types (AConLike (RealDataCon dc)) | DC.dataConName dc `elemNameSet` exported = dcTypes dc
    entity_types (ATyCon tc)
      | tyConName tc `elemNameSet` exported =
          tyConKind tc : maybeToList (synTyConRhs_maybe tc) ++ concatMap dcTypes (tyConDataCons tc) ++
          [ty | ax <- maybeToList (closedFamilyTyConCoAxiom_maybe tc), br <- fromBranches (coAxiomBranches ax)
              , ty <- coAxBranchRHS br : coAxBranchLHS br ++ map varType (coAxBranchTyVars br)]
      | otherwise = [ty | dc <- tyConDataCons tc, DC.dataConName dc `elemNameSet` exported, ty <- dcTypes dc]
    entity_types _ = []

finalizeEarlyClosureTypes :: Maybe [CoreBind] -> NameSet -> TypeEnv -> TypeEnv
finalizeEarlyClosureTypes mb_tidy exported type_env =
    mapNameEnv (mark_nocode_helper . update) type_env
  where
    elts = typeEnvElts type_env
    tidy_list = maybe [] stripBreakpointTicks mb_tidy
    tidy_env = mkBindEnv tidy_list
    tidy_by_name = mkNameEnv [(idName v, v) | (v, _) <- tidy_list]
    companions_of = companionTyConLookup [tc | ATyCon tc <- elts]
    early_fams = [tc | AnId v <- elts, CheckedClosureSourceTableId o t _ <- [idDetails v]
                     , o == idName v, isEarlyClosureTable t, tc <- t : companions_of v]
    iface_tys = exportedInterfaceTypes exported type_env
    iface_tycons = tyConsOfTypes iface_tys
    exported_tycons = if null early_fams then iface_tycons else survivingEarlyExportedTyCons early_fams iface_tys
    root_tables = [t | AnId v <- elts, idName v `elemNameSet` exported, Just t <- [sourceTableId_maybe v]] ++
                  [tc | ATyCon tc <- elts, tc `elementOfUniqSet` iface_tycons]
    close_callees seen [] = seen
    close_callees seen (tc : rest)
      | tyConName tc `elemNameSet` seen = close_callees seen rest
      | otherwise = close_callees (extendNameSetList seen [tyConName tc]) (fromMaybe [] (sourceTableCallees tc) ++ rest)
    reachable_tables = close_callees emptyNameSet root_tables
    closure_helper_ids = mkNameSet
      [h | AnId v <- elts, CheckedClosureSourceTableId o t hs <- [idDetails v], o == idName v
         , idName v `elemNameSet` exported || any (`elementOfUniqSet` exported_tycons) (t : companions_of v), h <- hs]
    mark_nocode_helper (AnId v)
      | isNothing mb_tidy
      , idName v `elemNameSet` closure_helper_ids || maybe False ((`elemNameSet` reachable_tables) . tyConName) (sourceTableId_maybe v)
      = AnId (setIdExported v)
    mark_nocode_helper thing = thing
    update (AnId v)
      | CheckedClosureSourceTableId owner table helpers <- idDetails v
      , owner == idName v, isEarlyClosureTable table =
          let local_hs = filter (\h -> nameModule_maybe h == nameModule_maybe owner) helpers
          in if not (any (`elementOfUniqSet` exported_tycons) (table : companions_of v)) &&
                any (not . (`elemNameSet` exported)) (owner : local_hs)
               then AnId (setIdDetails v VanillaId)
               else
                 let floated = case lookupNameEnv tidy_by_name owner of
                       Just actual -> [nm | nm <- reachable tidy_env actual, nm /= owner, isExternalName nm
                                          , nameModule_maybe nm == nameModule_maybe owner, isJust (lookupTypeEnv type_env nm)]
                       Nothing -> []
                     merged = nameSetElemsStable $ mkNameSet $
                       filter (\nm -> nm /= owner && nameModule_maybe nm == nameModule_maybe owner) (helpers ++ floated)
                 in AnId (setIdDetails v (if null merged then SourceTableId owner table NominalSourceTable
                                                         else CheckedClosureSourceTableId owner table merged))
    update thing = thing

data StageBinds = StageBinds { sb_env :: BindEnv, sb_by_name :: NameEnv (Id, CoreExpr), sb_scope_ops :: NameSet }

mkStageBinds :: [(Id, CoreExpr)] -> StageBinds
mkStageBinds pairs = StageBinds bind_env (mkNameEnv [(idName v, p) | p@(v, _) <- reverse pairs]) $ mkNameSet
    [ idName op | s <- directScopeShapes bind_env (map fst pairs)
                , let (ren, ext) = ss_ren_ops s
                      sub_ops = maybe [] (\(sub, sext, mb_w) -> [sub, sext] ++ maybeToList mb_w) (ss_subst_ops s)
                , op <- [ren, ext] ++ sub_ops ]
  where
    bind_env = mkBindEnv pairs

validateEarlyIndexedLookups :: Bool -> [CoreRule] -> [CoreBind] -> Maybe [CoreBind] -> Maybe [CoreBind] -> TcM ()
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
          raw_desug_sb = mkStageBinds raw_desugared
          desug_sb = mkStageBinds (map (safeSimpleOptBind opts) raw_desugared)
          mb_simp_sb = mkStageBinds . stripBreakpointTicks <$> mb_simplified
          mb_tidy_sb = mkStageBinds . stripBreakpointTicks <$> mb_tidy
          active_rule_owners = mkNameSet
            [fn | Core.Rule{Core.ru_fn = fn, Core.ru_auto = auto, Core.ru_act = act} <- rules, not (auto || isNeverActive act)]
      for_ candidates $ \(owner, table, helpers, companions) -> do
        callee_owners <- traverse sourceTableOwner (fromMaybe [] (sourceTableCallees table))
        let name = idName owner
            imported_callees = [idName v | Just v <- callee_owners, nameModule_maybe (idName v) /= nameModule_maybe name]
            all_helpers = nameSetElemsStable (mkNameSet (helpers ++ imported_callees))
        case find (`elemNameSet` active_rule_owners) (name : all_helpers) of
          Just ruled -> setSrcSpan (nameSrcSpan name) (addErr (TcRnRetainedQuantificationError (RetainedSourceRule ruled)))
          Nothing -> do
            ok <- fmap isJust $ runMaybeT $ do
              guard (plugins_free && not hpc)
              expected <- MaybeT (pure (fromBranches . coAxiomBranches <$> closedFamilyTyConCoAxiom_maybe table))
              guard (not (null expected))
              simp_sb <- MaybeT (pure mb_simp_sb)
              checkEntry exported resolveTable expected table owner all_helpers companions raw_desug_sb desug_sb simp_sb mb_tidy_sb
            unless ok $ setSrcSpan (nameSrcSpan name) (addErr (TcRnNoSourceTable name))
      failIfErrsM
  where
    noUserIdRules v = all (\case Core.BuiltinRule{} -> True; r -> Core.isAutoRule r || isNeverActive (Core.ruleActivation r))
                          (idCoreRules v)

    lookupStage owner check_rules helpers sb = do
      (actual, rhs) <- MaybeT (pure (lookupNameEnv (sb_by_name sb) (idName owner)))
      guard (varType actual `eqType` varType owner)
      when check_rules $ do
        guard (noUserIdRules actual)
        for_ helpers $ \h -> when (nameModule_maybe h == nameModule_maybe (idName owner)) $ do
          (hv, _) <- MaybeT (pure (lookupNameEnv (sb_by_name sb) h))
          guard (noUserIdRules hv)
      pure (actual, rhs)

    checkOnlyCalls exported allow_floated allowed sb =
      let extra v = isPatErrorHelper (sb_env sb) v ||
                    (allow_floated && isFloatedConstructorHelper ((`elemNameSet` exported) . idName) (sb_env sb) v)
      in guard . hasOnlyAllowedCalls extra allowed

    hasPrefix p = any (isPrefixOf p . occNameString . nameOccName . tyConName)

    checkEntry exported resolveTable expected table owner helpers companions raw_desug desug simplified mb_tidy
      | hasPrefix "$closure:" companions || hasPrefix "$substMap:" companions = do
          let is_subst_map = hasPrefix "$substMap:" companions
          guard (length expected == 1 && length companions == (if is_subst_map then 1 else 3))
          (d_actual, d_rhs) <- lookupStage owner False helpers desug
          early_s <- MaybeT (pure (scopeFamilyInputs (sb_env desug) d_actual d_rhs))
          let exp_arity = if isJust (ss_sort early_s) then 2 * ss_arity early_s + 3 else ss_arity early_s + 2
              checkClosure sb = do
                (actual, raw_rhs) <- lookupStage owner True helpers sb
                let (mb_alias, rhs) = resolveForwardCseAlias (sb_env sb) actual raw_rhs
                for_ mb_alias (guard . noUserIdRules)
                shape <- MaybeT (pure (scopeFamilyInputs (sb_env sb) actual rhs))
                guard (sameScopeShape early_s shape)
          guard ((not is_subst_map || isNothing (ss_sort early_s)) && tyConArity table == exp_arity)
          checkClosure simplified
          for_ mb_tidy (\tidy -> when (idName owner `elemNameSet` exported) (checkClosure tidy))
      | otherwise = do
          (d_actual, d_rhs) <- lookupStage owner False helpers desug
          (_, raw_rhs) <- lookupStage owner False helpers raw_desug
          let mb_cnames = case [tc | tc <- companions, "$case:" `isPrefixOf` occNameString (nameOccName (tyConName tc))] of
                [tc] | Just ax <- closedFamilyTyConCoAxiom_maybe tc -> Just (tyConName tc, coAxiomName ax)
                _ -> Nothing
              checkIndexed check_rules allow_floated sb = do
                (actual, raw_s_rhs) <- lookupStage owner check_rules helpers sb
                let (mb_alias, rhs) = if allow_floated then resolveForwardCseAlias (sb_env sb) actual raw_s_rhs else (Nothing, raw_s_rhs)
                for_ mb_alias (guard . noUserIdRules)
                ci <- compileIndexedCore (sb_env sb) resolveTable mb_cnames (Just expected) actual rhs
                let w_names = maybe [] (pure . idName) (ci_mb_worker ci)
                checkOnlyCalls exported allow_floated (idName actual : w_names ++ maybe [] (pure . idName) mb_alias ++ helpers) sb rhs
                guard (length (ci_rows ci) == length expected)
                zipWithM_ sameIndexedRow expected [applyRow eq table | (eq, _) <- ci_rows ci]
                pure (ci_mb_worker ci)
              whenTidy act = for_ mb_tidy (\tidy -> when (idName owner `elemNameSet` exported) (act tidy))
          (do guard (null companions || (length companions == 1 && hasPrefix "$case:" companions))
              d_w <- checkIndexed False False desug
              checkOnlyCalls exported False (idName owner : maybeToList (idName <$> d_w) ++ helpers) raw_desug raw_rhs
              _ <- checkIndexed True True simplified
              whenTidy (\t -> () <$ checkIndexed True True t)) <|>
            (do guard (null companions || (length companions == 1 && hasPrefix "$weaken:" companions))
                checkOnlyCalls exported False (idName owner : helpers) raw_desug raw_rhs
                checkScopeOp exported False d_actual helpers desug d_rhs
                (s_act, s_rhs) <- lookupStage owner True helpers simplified
                checkScopeOp exported True s_act helpers simplified s_rhs
                whenTidy (\t -> lookupStage owner True helpers t >>= \(t_act, t_rhs) -> checkScopeOp exported True t_act helpers t t_rhs))

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
      let (_, _, cod_raw) = splitFunTys (snd (splitForAllTyVars (varType actual)))
          (_, _, out_ty) = splitFunTys (snd (splitForAllTyVars cod_raw))
      (res_tc, r_args) <- splitTyConApp_maybe out_ty
      guard (length r_args == arity)
      let env = sb_env sb
          check_sext vc = () <$ (checkedScopeExtension is_typed env vc c1 c2 rhs <|>
                                 checkedScopeExtension is_typed env vc c2 c1 rhs)
      checkScopeRenameExtension is_typed env c1 c2 rhs <|>
        checkScopeRenameExtension is_typed env c2 c1 rhs <|>
        foldr (<|>) Nothing (map check_sext (tyConDataCons res_tc))

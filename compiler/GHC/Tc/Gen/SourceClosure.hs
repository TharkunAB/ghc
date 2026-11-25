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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Fix (mfix)
import Data.Foldable (for_, traverse_)
import Data.List (elemIndex, find, isPrefixOf, nubBy, sortBy, sortOn)
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
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, mkOpenSubst, substExpr)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCo.FVs (tyConsOfTypes)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(MRefl), Scaled(..), Type(..), scaledThing)
import GHC.Core.TyCon.Set (TyConSet)
import GHC.Core.TyCon
  ( FamTyConFlav(ClosedTypeFamilyTyCon), AlgTyConFlav(VanillaAlgTyCon)
  , TyCon, TyConBinder, Injectivity(NotInjective), closedFamilyTyConCoAxiom_maybe, mkAlgTyCon
  , mkLevPolyDataTyConRhs, mkTyConKind, mkAnonTyConBinder, mkNamedTyConBinder, mkNamedTyConBinders
  , mkFamilyTyCon, mkTyConKindWithMatchability, isAlgTyCon, isPromotedDataCon, synTyConRhs_maybe
  , tyConArity, tyConDataCons, tyConKind, tyConName, tyConRepName_maybe )
import GHC.Core.Type
  ( FunTyFlag(FTF_T_T), Kind, coreFullView, getTyVar_maybe, isLiftedTypeKind, isManyTy, isOneTy, liftedTypeKind, lookupTyVar
  , mkAppTys, mkSpecForAllTys, mkTyConApp, mkTyVarTy, mkVisFunTyManyU
  , mkTyVarTys, splitForAllForAllTyBinders, splitForAllTyVars, splitFunTy_maybe, splitFunTys, splitTyConApp_maybe
  , substTyUnchecked, substTyWith, tyCoVarsOfTypes, typeKind )
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
import GHC.Types.Id
  ( Id, idCoreRules, idDetails, idName, isDataConWorkId_maybe, isDataConWrapId_maybe
  , setIdExported, sourceTableId_maybe )
import GHC.Types.Id.Info (IdDetails(CheckedClosureSourceTableId, SourceTableId, VanillaId))
import GHC.Types.Basic (SourceTableOrigin(NominalSourceTable))
import GHC.Types.Id.Make (DataConBangOpts(FixedBangOpts))
import GHC.Types.InlinePragma (isNeverActive)
import GHC.Types.Name
  ( Name, NamedThing(getName), isExternalName, isWiredInName, mkDataOcc, mkInstTyCoOcc, mkSourceTableOcc
  , mkTcOcc, mkTyVarOcc, nameModule_maybe, nameOccName, nameSrcSpan, nameStableString, occNameString
  , stableNameCmp )
import GHC.Types.Name.Env (NameEnv, filterNameEnv, lookupNameEnv, mapNameEnv, mkNameEnv, nonDetNameEnvElts)
import GHC.Types.Name.Reader (isLocalGRE, lookupGRE_Name)
import GHC.Types.Name.Set (NameSet, elemNameSet, extendNameSetList, mkNameSet, nameSetElemsStable)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Types.SrcLoc (noSrcSpan, unLoc)
import GHC.Types.Tickish (GenTickish(Breakpoint))
import GHC.Types.TyThing (TyThing(AConLike, ATyCon, AnId), implicitTyConThings)
import GHC.Types.TypeEnv (TypeEnv, lookupTypeEnv, typeEnvElts, typeEnvIds)
import GHC.Types.Unique (hasKey)
import GHC.Types.Unique.Set (elementOfUniqSet)
import GHC.Types.Var
  ( ForAllTyFlag(Specified), TyVar, VarBndr(Bndr), eqForAllVis, isCoVar, isId, isLocalId, isTyVar
  , Var, mkTyVar, mkTyVarBinders, setIdDetails, varType )
import GHC.Types.Var.Env (mkInScopeSet)
import GHC.Types.Var.Set (elemVarSet, mkVarSet, subVarSet, unionVarSet)

{- Note [Certifying a checked source closure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A rank-2 substitution callback can replace a prefix of the source
context and cross binders. Private TypeData constructors represent
the first replacement, another replacement over a checked closure,
one-binder shift, and lifting. Two nominal families interpret the
closures over term and variable constructors. The source owner names
a third family that supplies the replacements in binder order.

Check the original and final Core before allocating the families.
A traversal must scrutinize its input, cover every constructor, and
rebuild each lazy field from a recursive call on the corresponding
original field. The call can use the original callback or a checked
extension. Check the kind of every generated branch against the
promoted constructor telescope. A neutral term blocks reduction.

The subtree calls descend through the original fields. Each lookup
under a binder consumes one level of lifting. A shift can traverse
the resulting term, but cannot reintroduce a replacement. An
unavailable scrutinized field leaves the corresponding match stuck.
The rebuilt fields must stay lazy in both traversals.

Check the source datatypes of all generated equations, including the
private interpreters. Their source types and constructors must be
visible under the ordinary source equation policy. The generated
closure datatype belongs to the certificate and is private.

Keep the actual owner and all reachable external helper Names from
both Core graphs in IdDetails. A generated internal helper has no
interface Name and cannot be the target of a downstream rule. Check
its Core and traverse its dependencies before filtering Names.
Withhold publication if any external Name is absent after Tidy or
if a visible rule can rewrite a checked function. See Note [Checked
closure dependencies] in GHC.Tc.Gen.SourceTable.

GHCi inserts breakpoint ticks at source spans, including in the middle
of an application. Inspect the Core with only those ticks removed.
A breakpoint can keep a lifted let between the type and value lambdas
of a callback. After removing the ticks, skip the let in the function
view if the binder has no remaining use. Keep the unmodified Core for
bytecode generation, including the variables used by the debugger.
-}

data CallbackLift = NamedLift Id | InlinedRename | InlinedSubst Id

-- The two traversals, their extensions and the optional named weakening.
data ClosureOperations = ClosureOperations (Id, Id) (Id, Id, Maybe Id)

data FamilyInputs = FamilyInputs Id Int Kind Kind TyCon TyCon TyCon ClosureOperations
                                  DC.DataCon DC.DataCon DC.DataCon [(DC.DataCon, [Int])]

-- | Simplify ordinary non-dependent Core right-hand sides before inspecting closure shapes.
safeSimpleOptBind :: SimpleOpts -> (Id, CoreExpr) -> (Id, CoreExpr)
safeSimpleOptBind opts (v, rhs)
  | hasExistentialExpr rhs
  || hasNativeDependentExpr (Core.Type (varType v))
  || hasNativeDependentExpr rhs
  = (v, rhs)
  | otherwise
  = (v, simpleOptExpr opts rhs)

-- | Allocate the coercion axiom Name for a closure family, marking tables staged
-- during typechecking so post-typechecking checks leave SourceTable entries untouched.
mkClosureTableAxiom :: Bool -> Name -> TcM Name
mkClosureTableAxiom False root = newImplicitBinder root mkInstTyCoOcc
mkClosureTableAxiom True root =
    newImplicitBinder root (mkInstTyCoOcc . mkTcOcc . ("early:" ++) . occNameString)

-- | Test whether a closed family was allocated during early closure staging.
isEarlyClosureTable :: TyCon -> Bool
isEarlyClosureTable table
  | Just axiom <- closedFamilyTyConCoAxiom_maybe table
  = "D:early:" `isPrefixOf` occNameString (nameOccName (coAxiomName axiom))
  | otherwise = False

-- | Publish tables for eligible final Core bindings. The caller checks
-- the source-equation opt-in and excludes plugins and local user rules.
publishCheckedSourceClosures :: [CoreBind] -> [CoreBind] -> TypeEnv -> TcM ([Id], [TyCon])
publishCheckedSourceClosures original_binds final_binds final_types = do
    env <- getGblEnv
    rdr_env <- getGlobalRdrEnv
    opts <- initSimpleOpts <$> getDynFlags
    let here = Just (tcg_mod env)
        exported = availsToNameSet (tcg_exports env)
        imported name = isWiredInName name || ordinaryImport rdr_env name
        public = publicCaseType env exported imported
        unoptimized = inspect original_binds
        original = map (safeSimpleOptBind opts) unoptimized
        final = inspect final_binds
        inspect binds = [(v, stripTicksE breakpoint rhs) | (v, rhs) <- Core.flattenBinds binds]
        breakpoint Breakpoint{} = True
        breakpoint _ = False
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
        -- Allow an already early-staged closure root to supply helper shapes
        -- for any remaining VanillaId helpers under -fwrite-source-equations,
        -- while never overwriting the already-staged root itself.
        lookupClosureRoot old = do
          let owner = idName old
          guard (isExternalName owner && owner `elemNameSet` exported)
          actual <- lookupLocal owner
          case idDetails actual of
            VanillaId -> pure actual
            CheckedClosureSourceTableId{} -> pure actual
            _ -> Nothing
        dependencies include old actual = do
          let helpers = nameSetElemsStable $ mkNameSet $
                filter (\name -> name /= idName old && include name)
                       (reachable original old ++ reachable final actual)
          traverse lookupLocal helpers
        isConstructorId v = isJust (isDataConWorkId_maybe v <|> isDataConWrapId_maybe v)
        constructorOnly expr = case constructorCall expr of
          Just (_, args) -> all constructorOnly args
          Nothing -> False
        floatedConstructors v
          | not (idName v `elemNameSet` exported)
          , Just decl <- lookup v final
          = (case function decl of
               Just (_ : _, [], body) -> constructorOnly body
               _ -> False) || isJust (validConstructorAliasBody decl)
          | otherwise = False
        onlyCalls allowed old actual = do
          old_rhs <- lookup old unoptimized
          new_rhs <- lookup actual final
          let helper allow_floated v =
                isValueVar v &&
                idName v `notElem` allowed &&
                not (isConstructorId v) &&
                not (allow_floated && floatedConstructors v)
          guard (null (exprSomeFreeVarsList (helper False) old_rhs) &&
                 null (exprSomeFreeVarsList (helper True) new_rhs))
        candidate (old, original_rhs) = do
          actual <- lookupClosureRoot old
          original_shape <- familyInputs original old original_rhs
          final_rhs <- lookup actual final
          shape@(FamilyInputs _ _ _ _ term ext index _ _ _ _ _) <- familyInputs final actual final_rhs
          guard (sameShapes original_shape shape)
          guard (all ((== here) . nameModule_maybe . tyConName) [term, ext, index])
          helper_ids <- dependencies isExternalName old actual
          guard (not (null helper_ids))
          pure (actual, (original_shape, shape), helper_ids)
        indexed_candidate (old, original_rhs) = do
          let vanilla_entry = do
                actual <- lookupOwner old
                final_rhs <- lookup actual final
                let decoder = IndexedEntry IndexedDecoder <$> indexedSource original old original_rhs
                                                          <*> indexedSource final actual final_rhs
                    encoder = IndexedEntry IndexedEncoder <$> indexedEncoder original old original_rhs
                                                          <*> indexedEncoder final actual final_rhs
                    traversal = IndexedEntry IndexedTraversal <$> indexedTraversal original old original_rhs
                                                              <*> indexedTraversal final actual final_rhs
                shapes <- decoder <|> encoder <|> traversal
                let keep_helper name = case shapes of
                      IndexedEntry IndexedDecoder _ (IndexedSource _ _ _ _ (Just w) _ _)
                        | name == idName w -> isExternalName name
                      IndexedEntry IndexedDecoder _ _ -> True
                      _ -> isExternalName name
                -- The decoder retains every external helper identity, while an
                -- internal pre-tidy worker has already had its Core tree verified.
                -- The encoder and GADT traversal check floated type-only bodies.
                helper_ids <- dependencies keep_helper old actual
                pure (actual, shapes, helper_ids)
              untyped_entry = do
                actual <- lookupClosureRoot old
                final_rhs <- lookup actual final
                orig_u <- untypedFamilyInputs original old original_rhs
                fin_u@(UntypedFamilyInputs _ _ _ term _ index _ _ _ _ _ _) <-
                  untypedFamilyInputs final actual final_rhs
                guard (sameUntypedShapes orig_u fin_u)
                guard (all ((== here) . nameModule_maybe . tyConName) [term, index])
                helper_ids <- dependencies isExternalName old actual
                guard (not (null helper_ids))
                pure (actual, UntypedClosureEntry orig_u fin_u, helper_ids)
          vanilla_entry <|> untyped_entry
        sameId a b = idName a == idName b
        orig_typed_ops = directTypedOperationShapes original (map fst original)
        fin_typed_ops = directTypedOperationShapes final (map fst final)
        direct_typed_pairs =
          [ (o_s, f_s)
          | o_s@(FamilyInputs o_id _ _ _ term ext index _ _ _ _ _) <- orig_typed_ops
          , all ((== here) . nameModule_maybe . tyConName) [term, ext, index]
          , Just f_s <- [find (\(FamilyInputs f_id _ _ _ _ _ _ _ _ _ _ _) -> sameId o_id f_id) fin_typed_ops]
          , sameShapes o_s f_s ]
        orig_untyped_ops = directUntypedOperationShapes original (map fst original)
        fin_untyped_ops = directUntypedOperationShapes final (map fst final)
        direct_untyped_pairs =
          [ (o_u, f_u)
          | o_u@(UntypedFamilyInputs o_id _ _ term _ index _ _ _ _ _ _) <- orig_untyped_ops
          , all ((== here) . nameModule_maybe . tyConName) [term, index]
          , Just f_u <- [find (\(UntypedFamilyInputs f_id _ _ _ _ _ _ _ _ _ _ _) -> sameId o_id f_id) fin_untyped_ops]
          , sameUntypedShapes o_u f_u ]
        -- See Note [Renaming with an abstract callback].
        renaming_candidate (original_shape, shape) = do
          let (rename, extend) = renamingOperations original_shape
              (final_rename, final_extend) = renamingOperations shape
          guard (idName rename == idName final_rename && idName extend == idName final_extend)
          actual_rename <- lookupOwner final_rename
          actual_extend <- lookupOwner final_extend
          onlyCalls [] extend actual_extend
          helpers <- dependencies isExternalName rename actual_rename
          guard (actual_extend `elem` helpers)
          pure (actual_rename, (actual_extend, shape), helpers)
        -- See Note [Substitution with an abstract callback].
        substitution_extension (original_shape, shape) = do
          let (old_rename, _) = renamingOperations original_shape
              (rename, _) = renamingOperations shape
          (_, old_ext, Just old_weakening) <- pure (substitutionOperations original_shape)
          (_, ext, mb_fin_weakening) <- pure (substitutionOperations shape)
          guard (idName old_rename == idName rename && idName old_ext == idName ext &&
                 maybe True (\fw -> idName fw == idName old_weakening) mb_fin_weakening)
          actual_ext <- lookupOwner ext
          actual_weakening <- lookupOwner old_weakening
          old_weak_rhs <- lookup old_weakening original
          fin_weak_rhs <- lookup actual_weakening final
          guard (isJust (validConstructorAliasBody old_weak_rhs) &&
                 isJust (validConstructorAliasBody fin_weak_rhs))
          onlyCalls [] old_weakening actual_weakening
          onlyCalls [idName rename, idName old_weakening] old_ext actual_ext
          raw_helpers <- dependencies isExternalName old_ext actual_ext
          let helpers = nubBy sameId (actual_weakening : raw_helpers)
          guard (idName rename `elem` map idName helpers)
          pure (actual_ext, (rename, actual_weakening, shape), helpers)
        substitution_candidate (original_shape, shape) = do
          let (old_rename, _) = renamingOperations original_shape
              (rename, _) = renamingOperations shape
          (old_subst, old_ext, Just old_weakening) <- pure (substitutionOperations original_shape)
          (subst, ext, mb_fin_weakening) <- pure (substitutionOperations shape)
          guard (map idName [old_subst, old_rename, old_ext] == map idName [subst, rename, ext] &&
                 maybe True (\fw -> idName fw == idName old_weakening) mb_fin_weakening)
          actual_subst <- lookupOwner subst
          actual_ext <- lookupOwner ext
          actual_weakening <- lookupClosureRoot old_weakening
          let operation_names = map idName [subst, rename, ext, old_weakening]
          onlyCalls operation_names old_subst actual_subst
          raw_helpers <- dependencies isExternalName old_subst actual_subst
          let helpers = nubBy sameId (actual_weakening : raw_helpers)
          guard (actual_ext `elem` helpers)
          pure (actual_subst, (actual_ext, shape), helpers)
        lookup_candidate (old, original_rhs) = do
          actual <- lookupOwner old
          final_rhs <- lookup actual final
          startup <- indexedLookup original old original_rhs
          optimized <- indexedLookup final actual final_rhs
          pure (actual, (startup, optimized), [])
        alias_candidate (old, original_rhs) = do
          actual <- lookupOwner old
          final_rhs <- lookup actual final
          orig_spec <- checkedConstructorAlias old original_rhs
          fin_spec <- checkedConstructorAlias actual final_rhs
          guard (sameConstructorAliasSpec orig_spec fin_spec)
          onlyCalls [] old actual
          pure (actual, orig_spec, ([] :: [Id]))
        subst_map_candidate (old, original_rhs) = do
          actual <- lookupOwner old
          final_rhs <- lookup actual final
          orig_spec <- checkedSubstMap original old original_rhs
          fin_spec <- checkedSubstMap final actual final_rhs
          guard (sameSubstMapSpec orig_spec fin_spec)
          onlyCalls [] old actual
          pure (actual, orig_spec, ([] :: [Id]))
        resolveDepId v
          | nameModule_maybe (idName v) == here = lookupClosureRoot v
          | isJust (sourceTableId_maybe v)      = Just v
          | otherwise                           = Nothing
        sigma_comb_candidate (old, original_rhs) = do
          actual <- lookupOwner old
          final_rhs <- lookup actual final
          orig_spec <- checkedSigmaComb old original_rhs
          fin_spec <- checkedSigmaComb actual final_rhs
          guard (sameSigmaCombSpec orig_spec fin_spec)
          case orig_spec of
            SigmaSeq _ trav_id -> do
              resolved_trav <- resolveDepId trav_id
              onlyCalls [idName resolved_trav] old actual
              local_hs <- if nameModule_maybe (idName resolved_trav) == here
                            then dependencies isExternalName old actual
                            else pure []
              let hs = nubBy sameId (resolved_trav : local_hs)
              pure (actual, orig_spec, hs)
            _ -> do
              onlyCalls [] old actual
              pure (actual, orig_spec, [])
        subst_fwd_candidate map_specs (old, original_rhs) = do
          actual <- lookupOwner old
          final_rhs <- lookup actual final
          orig_fwd@(SubstForwardSpec _ trav_id map_id _ _ _) <- checkedSubstForwardOrig old original_rhs
          resolved_trav <- resolveDepId trav_id
          resolved_map <- resolveDepId map_id
          let mb_map_spec = lookup (idName resolved_map) map_specs
              (mb_alias, eff_final_rhs) = resolveForwardCseAlias final actual final_rhs
              base_allowed = [idName resolved_trav, idName resolved_map]
          guard (checkedSubstForwardFinal final actual eff_final_rhs orig_fwd mb_map_spec)
          onlyCalls (base_allowed ++ maybe [] (pure . idName) mb_alias) old actual
          for_ mb_alias $ \av -> onlyCalls base_allowed old av
          local_hs <- dependencies isExternalName old actual
          let hs = nubBy sameId ([resolved_trav, resolved_map] ++ local_hs)
          pure (actual, orig_fwd, hs)
        closures = mapMaybe candidate original
        indexed = mapMaybe indexed_candidate original
        lookups = mapMaybe lookup_candidate original
        raw_aliases = mapMaybe alias_candidate original
        raw_maps = mapMaybe subst_map_candidate original
        map_spec_alist = [(idName v, spec) | (v, spec, _) <- raw_maps] ++
          [ (idName v, spec)
          | (v, r) <- original
          , isExternalName (idName v)
          , Just spec <- [checkedSubstMap original v r] ]
        raw_sigmas = mapMaybe sigma_comb_candidate original
        raw_fwds = mapMaybe (subst_fwd_candidate map_spec_alist) original
    if null closures && null indexed && null lookups &&
       null direct_typed_pairs && null direct_untyped_pairs &&
       null raw_aliases && null raw_maps && null raw_sigmas && null raw_fwds
      then pure ([], [])
      else do
        visible <- visibleSourceMethodRules env
        rule_heads <- visibleClosureRuleHeads env
        let safe :: (Id, a, [Id]) -> Bool
            safe (owner, _, helpers) =
              all (\v -> not (hasSourceMethodRule visible v)
                      && not (idName v `elemNameSet` rule_heads)) (owner:helpers)
            all_typed_pairs = [(o, s) | (_, (o, s), _) <- closures] ++ direct_typed_pairs
        checked_closures <- catMaybes <$> traverse (publish public) (filter safe closures)
        checked_indexed <- publishIndexedEntries public lookupOwner lookupClosureRoot
                             unoptimized original final
                             (dependencies isExternalName) safe
                             (filter safe indexed) direct_untyped_pairs
        let renamings = filter safe (mapMaybe renaming_candidate all_typed_pairs)
            sameRename (owner, _, _) (other, _, _) = sameId owner other
            sameExtension (_, (ext, _), _) (_, (other, _), _) = sameId ext other
        extensions <- catMaybes <$> traverse (publishRenameExtension False public) (nubBy sameExtension renamings)
        let extension_tables = [(idName ext, table) | (ext, [table]) <- extensions]
        checked_renamings <- catMaybes <$> traverse (publishRenaming False public extension_tables) (nubBy sameRename renamings)
        let checked_tables entries = [(idName owner, table) | (owner, table : _) <- entries]
            subst_extensions = nubBy sameRename (filter safe (mapMaybe substitution_extension all_typed_pairs))
            weakening_inputs = [(weakening, shape) | (_, (_, weakening, shape), _) <- subst_extensions]
        weakenings <- catMaybes <$> traverse (publishWeakening False public) (nubBy (\a b -> sameId (fst a) (fst b)) weakening_inputs)
        subst_exts <- catMaybes <$> traverse (publishSubstitutionExtension False public
                        (checked_tables checked_renamings) (checked_tables weakenings)) subst_extensions
        let substitutions = nubBy sameRename (filter safe (mapMaybe substitution_candidate all_typed_pairs))
        checked_substs <- catMaybes <$> traverse (publishSubstitution False public
                            (checked_tables subst_exts)) substitutions
        checked_lookups <- catMaybes <$> traverse (publishIndexedLookup public) (filter safe lookups)
        let base_accepted = checked_closures ++ checked_indexed ++ extensions ++ checked_renamings ++
                            weakenings ++ subst_exts ++ checked_substs ++ checked_lookups
            already_done = mkNameSet (map (idName . fst) base_accepted)
            not_done (v, _, _) = not (idName v `elemNameSet` already_done)
        checked_aliases <- catMaybes <$> traverse (publishConstructorAlias False public)
                             (filter (\c -> not_done c && safe c) raw_aliases)
        let done_after_alias = already_done `extendNameSetList` map (idName . fst) checked_aliases
        checked_maps <- catMaybes <$> traverse (publishSubstMap False public)
                          (filter (\c@(v, _, _) -> not (idName v `elemNameSet` done_after_alias) && safe c) raw_maps)
        let done_after_maps = done_after_alias `extendNameSetList` map (idName . fst) checked_maps
            known_tables0 =
              [ (idName v, tc)
              | (v, tc : _) <- base_accepted ++ checked_aliases ++ checked_maps ] ++
              [ (idName v, tc)
              | v <- typeEnvIds final_types
              , Just tc <- [sourceTableId_maybe v] ]
            resolveTable v
              | Just tc <- lookup (idName v) known_tables0 = Just tc
              | otherwise = sourceTableId_maybe v
        checked_sigmas <- catMaybes <$> traverse (publishSigmaComb False public resolveTable)
                            (filter (\c@(v, _, _) -> not (idName v `elemNameSet` done_after_maps) && safe c) raw_sigmas)
        let done_after_sigmas = done_after_maps `extendNameSetList` map (idName . fst) checked_sigmas
            known_tables1 = [(idName v, tc) | (v, tc : _) <- checked_sigmas] ++ known_tables0
            resolveTable1 v
              | Just tc <- lookup (idName v) known_tables1 = Just tc
              | otherwise = sourceTableId_maybe v
        checked_fwds <- catMaybes <$> traverse (publishSubstForward False public resolveTable1)
                          (filter (\c@(v, _, _) -> not (idName v `elemNameSet` done_after_sigmas) && safe c) raw_fwds)
        let accepted = base_accepted ++ checked_aliases ++ checked_maps ++ checked_sigmas ++ checked_fwds
        pure (map fst accepted, concatMap snd accepted)
  where
    publish public (owner, (_, shape), helpers)
      | VanillaId <- idDetails owner = do
          generated <- allocateFamilies False shape
          pure $ do
            (root, families) <- generated
            guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
            let new = setIdDetails owner (CheckedClosureSourceTableId (idName owner) root (map idName helpers))
            pure (new, families)
      | otherwise = pure Nothing

{- Note [Checked indexed reconstruction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A unary function can open an existential package and recurse on one of
its fields. In a recursive branch, the new package must contain a
proper subfield of the selected field and the exact other fields.
The recursive call must appear below a lazy result constructor.

The final wrapper can pass the package's fields to a worker. Recover
that worker only when the wrapper forwards all erased and value fields
in order. Read its recursive call as an application of the original
owner to the reconstructed package. Compare the complete equations
from original and final Core before publishing.

Specialize each row with the nominal equalities bound by its actual
constructor alternatives. Check each promoted constructor at the
resulting kind. A cast can disappear only if its specialized endpoints
agree and its evidence comes from those alternatives. The equations
use the original closed family mechanism. They preserve a recursive
call whose argument is still abstract.

First check for a unary function with a case body. An ordinary exported
value can construct a retained package with an unused witness. Computing
the type of its optimized application would require an index for that
witness. The value has no traversal equations to publish.
-}

data IndexedPattern
  = IndexedVariable Id
  | IndexedConstructor Type DC.DataCon [Type] [IndexedPattern]

data IndexedRow = IndexedRow IndexedPattern [Var] CoreExpr

data IndexedKind = IndexedDecoder | IndexedEncoder | IndexedTraversal
  deriving (Eq)

data IndexedEntry
  = IndexedEntry IndexedKind IndexedSource IndexedSource
  | UntypedClosureEntry UntypedFamilyInputs UntypedFamilyInputs

-- One complete case on a recursive result. The operand is a proper
-- subfield of an input alternative.
data IndexedResultCase = IndexedResultCase [Type] Id IndexedPattern CoreExpr

-- The worker's additional type arguments are the package existentials.
data IndexedSource = IndexedSource [TyVar] Type [Id] Type (Maybe Id) [(Id, CoreExpr)] [IndexedRow]

-- | Expose exhaustive case paths and one exact worker wrapper.
indexedSource :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe IndexedSource
indexedSource bindings owner rhs = do
    (parameters, input, body) <- indexedFunction owner rhs
    [(pack, binders, nested)] <- caseOn input body
    (_, exs@(_:_), [], [], _, _) <- pure (DC.dataConFullSig pack)
    guard (DC.dataConTyCon pack `hasOnly` pack)
    (outer, proofs) <- indexedAlt input pack binders
    let (existentials, fields) = splitAt (length exs) binders
    (worker, tree) <- unwrap (parameters ++ existentials) fields nested
    guard (not (null (exprSomeFreeVarsList (== fromMaybe owner worker) tree)))
    rows <- descend outer proofs tree
    guard (length rows > 1)
    pure (IndexedSource parameters (varType input) [] (exprType body) worker [] rows)
  where
    tc `hasOnly` con = tyConDataCons tc == [con]

    unwrap _ _ body@Core.Case{} =
      pure (Nothing, body)
    unwrap tys fields body = do
      (Core.Var worker, args) <- pure (Core.collectArgs body)
      guard (worker /= owner &&
             (not (isExternalName (idName worker)) ||
              nameModule_maybe (idName worker) == nameModule_maybe (idName owner)))
      let (passed_types, passed_fields) = splitAt (length tys) args
      supplied <- traverse indexedTypeArg passed_types
      guard (eqTypes supplied (mkTyVarTys tys) &&
             length passed_fields == length fields &&
             and (zipWith isVar passed_fields fields))
      declaration <- lookup worker bindings
      (worker_tys, worker_fields, tree) <- function declaration
      let binders = worker_tys ++ worker_fields
          free = foldl' unionVarSet (exprFreeVars tree) (map exprFreeVars args)
          expanded = substExpr (mkOpenSubst (mkInScopeSet free) (zip binders args)) tree
      guard (length worker_tys == length tys && length worker_fields == length fields &&
             exprType declaration `eqType` varType worker &&
             exprType expanded `eqType` exprType body)
      pure (Just worker, expanded)

    descend pat proofs expr@(Core.Case (Core.Var scrutinee) binder result alternatives) = do
      guard (length alternatives > 0 && varType binder `eqType` varType scrutinee &&
             result `eqType` exprType expr &&
             scrutinee `elem` indexedFields pat)
      (tc, _) <- splitTyConApp_maybe (varType scrutinee)
      alts <- traverse read_alt alternatives
      guard (map (\(con, _, _) -> con) alts == tyConDataCons tc)
      concat <$> traverse (branch pat proofs scrutinee) alts
    descend pat proofs body = pure [IndexedRow pat proofs body]

    branch pat proofs scrutinee (con, binders, body) = do
      (node, equalities) <- indexedAlt scrutinee con binders
      descend (replace scrutinee node pat) (proofs ++ equalities) body

    read_alt (Core.Alt (Core.DataAlt con) fields body) = Just (con, fields, body)
    read_alt _ = Nothing

    replace v node (IndexedVariable field) | v == field = node
    replace v node (IndexedConstructor ty con source fields) =
      IndexedConstructor ty con source (map (replace v node) fields)
    replace _ _ pat = pat

    isVar (Core.Var actual) expected = actual == expected
    isVar _ _ = False

{- Note [Checked cases on recursive results]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A traversal can inspect the existential package returned by a smaller
call before it constructs its result. A family for the result case
preserves this demand. For a branch with E (There r) = H (E r), H
cannot reduce until it sees the package constructor. The family
telescope includes the indices of the recursive package and the
enclosing result, after specialization by the input branch.

Check one exhaustive case on the direct recursive call. The call
must consume an input field, and the result branch can use only
fields of the package it scrutinizes. Check both Core versions
including the private family equation. The simplifier can float
a constructor expression into a type-only helper. Check the body
of that helper at its exact type application. Do not treat an
arbitrary unfolding or function call as a constructor.
-}

-- | Find an indexed traversal with a complete case over its input.
indexedEncoder :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe IndexedSource
indexedEncoder bindings owner rhs = do
    (parameters, input, body) <- indexedFunction owner rhs
    (tc, _) <- splitTyConApp_maybe (varType input)
    alts <- caseOn input body
    guard (length alts > 1 && [con | (con, _, _) <- alts] == tyConDataCons tc)
    rows <- traverse (read_alt input) alts
    guard (length [() | IndexedRow pat _ branch <- rows,
                        isJust (indexedResultCase owner parameters pat branch)] == 1)
    pure (IndexedSource parameters (varType input) [] (exprType body) Nothing bindings rows)
  where
    read_alt input (con, binders, body) = do
      (pat, proofs) <- indexedAlt input con binders
      pure (IndexedRow pat proofs body)

{- Note [Checked GADT structural traversals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An exported function such as plug :: UCtx m n -> UTerm m -> UTerm n
scrutinizes an indexed GADT as its first value argument and carries
zero or more algebraic arguments unchanged across recursive calls.
Each alternative refines the nominal GADT index equalities before
checking constructor applications, carried variables, casts justified
solely by those case equalities, and recursive calls on strict subfields
of the matched constructor. Publish only when desugared and final Core
justify identical equations and all referenced datatypes are public.
-}

-- | Find a recursive GADT traversal carrying any remaining arguments unchanged.
indexedTraversal :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe IndexedSource
indexedTraversal bindings owner rhs = do
    (parameters, input : carried, body@Core.Case{}) <- function rhs
    guard (not (null parameters) && exprType rhs `eqType` varType owner)
    let (declared_tvs, tau) = splitForAllTyVars (varType owner)
    guard (length parameters == length declared_tvs)
    let instantiate = substTyWith (map snd declared_tvs) (mkTyVarTys parameters)
        domains = varType input : map varType carried
        result = exprType body
    (matches, arg_tys, output_ty) <- pure (splitFunTys tau)
    guard (all (`eqType` unmatchableDataConTy) (map fst declared_tvs ++ matches) &&
           all (\(Scaled mult _) -> isManyTy mult) arg_tys &&
           eqTypes (map varType parameters) (map (instantiate . varType . snd) declared_tvs) &&
           eqTypes (map (instantiate . scaledThing) arg_tys) domains &&
           instantiate output_ty `eqType` result &&
           all isAlgDataType (domains ++ [result]))
    (tc, _) <- splitTyConApp_maybe (varType input)
    let cons = tyConDataCons tc
    guard (not (null cons) &&
           (any (not . isLiftedTypeKind . varType) parameters ||
            any (not . DC.isVanillaDataCon) cons))
    alts <- caseOn input body
    guard (map (\(con, _, _) -> con) alts == cons &&
           not (null (exprSomeFreeVarsList (== owner) body)))
    rows <- traverse (read_alt input) alts
    pure (IndexedSource parameters (varType input) carried result Nothing bindings rows)
  where
    isAlgDataType ty
      | Just (tc, _) <- splitTyConApp_maybe ty
      = isAlgTyCon tc && isLiftedTypeKind (typeKind ty)
      | otherwise = False

    read_alt input (con, binders, branch) = do
      (pat, proofs) <- indexedAlt input con binders
      pure (IndexedRow pat proofs branch)

-- | Check the ordinary unary telescope before reading case branches.
indexedFunction :: Id -> CoreExpr -> Maybe ([TyVar], Id, CoreExpr)
indexedFunction owner rhs = do
    (parameters, [input], body@Core.Case{}) <- function rhs
    guard (exprType rhs `eqType` varType owner)
    let (_, tau) = splitForAllTyVars (exprType rhs)
    (_, [Scaled mult input_kind], output_kind) <- pure (splitFunTys tau)
    guard (isManyTy mult && input_kind `eqType` varType input &&
           output_kind `eqType` exprType body &&
           all (isLiftedTypeKind . typeKind) [input_kind, output_kind])
    pure (parameters, input, body)

-- | Check the case and its direct, structurally smaller scrutinee.
indexedResultCase :: Id -> [TyVar] -> IndexedPattern -> CoreExpr -> Maybe IndexedResultCase
indexedResultCase owner parameters pat expr = do
    Core.Case call binder kind [Core.Alt (Core.DataAlt con) binders body] <- pure expr
    (Core.Var callee, args) <- pure (Core.collectArgs call)
    let (prefix, suffix) = splitAt (length parameters) args
    tys <- traverse indexedTypeArg prefix
    [Core.Var operand] <- pure suffix
    (tc, _) <- splitTyConApp_maybe (varType binder)
    (_, _ : _, [], [], _, _) <- pure (DC.dataConFullSig con)
    guard (callee == owner && length tys == length parameters && operand `elem` indexedFields pat &&
           varType binder `eqType` exprType call && kind `eqType` exprType body &&
           tyConDataCons tc == [con])
    (pattern, []) <- indexedAlt binder con binders
    pure (IndexedResultCase tys operand pattern body)

indexedTypeArg :: CoreExpr -> Maybe Type
indexedTypeArg (Core.Type ty) = Just ty
indexedTypeArg _ = Nothing

indexedFields :: IndexedPattern -> [Id]
indexedFields (IndexedVariable v) = [v]
indexedFields (IndexedConstructor _ _ _ fields) = concatMap indexedFields fields

-- | Check the ordinary alternative telescope before using its equalities.
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
           null (DC.dataConRepArgBinders con) && null (DC.dataConStupidTheta con) &&
           all (not . DC.isBanged) (DC.dataConImplBangs con))
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

-- | Publish indexed and untyped closure entries after both stages agree.
publishIndexedEntries :: (TyCon -> Bool)
                      -> (Id -> Maybe Id)
                      -> (Id -> Maybe Id)
                      -> [(Id, CoreExpr)]
                      -> [(Id, CoreExpr)]
                      -> [(Id, CoreExpr)]
                      -> (Id -> Id -> Maybe [Id])
                      -> (forall a. (Id, a, [Id]) -> Bool)
                      -> [(Id, IndexedEntry, [Id])]
                      -> [(UntypedFamilyInputs, UntypedFamilyInputs)]
                      -> TcM [(Id, [TyCon])]
publishIndexedEntries public lookupOwner lookupClosureRoot unoptimized original final extDeps safe entries direct_u = do
    let indexed_rows = [(owner, (kind, orig, fin), helpers)
                       | (owner, IndexedEntry kind orig fin, helpers) <- entries]
        untyped_closures = [(owner, (orig, fin), helpers)
                           | (owner, UntypedClosureEntry orig fin, helpers) <- entries]
    checked_rows <- catMaybes <$> traverse (publishIndexed False public) indexed_rows
    checked_untyped <- publishUntypedEntries public lookupOwner lookupClosureRoot
                         unoptimized original final extDeps safe untyped_closures direct_u
    pure (checked_rows ++ checked_untyped)

-- | Publish closed families after both compiler stages justify their rows.
publishIndexed :: Bool -> (TyCon -> Bool) -> (Id, (IndexedKind, IndexedSource, IndexedSource), [Id])
               -> TcM (Maybe (Id, [TyCon]))
publishIndexed is_early public (owner, (kind, original, final), helpers) = runMaybeT $ do
    let IndexedSource parameters domain carried result _ _ _ = original
        (flags, _) = splitForAllForAllTyBinders (varType owner)
    guard (length flags == length parameters)
    name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (mkClosureTableAxiom is_early name)
    operands <- lift (traverse (freshVar "operand") (domain : map varType carried))
    let binders = zipWith (\(_, Bndr _ flag) -> mkNamedTyConBinder flag) flags parameters ++
                  map mkAnonTyConBinder operands
        make = mkFamily name axiom binders result
        header = make []
    private <- if kind == IndexedEncoder then Just <$> lift allocate_case else pure Nothing
    original_rows <- indexedEquations kind owner header private original
    final_rows <- indexedEquations kind owner header private final
    guard (length original_rows == length final_rows)
    zipWithM_ (same_equation header) original_rows final_rows
    let table = make [applyRow row table | (row, _) <- original_rows]
        families = table : [tc | (_, Just (tc, _)) <- original_rows]
        details = case helpers of
          [] -> SourceTableId (idName owner) table NominalSourceTable
          ids -> CheckedClosureSourceTableId (idName owner) table (map idName ids)
    guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
    pure (setIdDetails owner details, families)
  where
    allocate_case = do
      name <- newImplicitBinder (idName owner) (mkTcOcc . ("$case:" ++) . occNameString)
      axiom <- newImplicitBinder name mkInstTyCoOcc
      pure (name, axiom)

    same_equation header (old, old_case) (new, new_case) = do
      sameIndexedRow (applyRow old header) (applyRow new header)
      case (old_case, new_case) of
        (Nothing, Nothing) -> pure ()
        (Just (left, lhs), Just (right, rhs)) -> do
          guard (tyConKind left `eqType` tyConKind right)
          sameIndexedRow lhs rhs
        _ -> fail "different indexed result cases"

-- | Compare two generated rows with a bijection between their binders.
sameIndexedRow :: CoAxBranch -> CoAxBranch -> MaybeT TcM ()
sameIndexedRow left right = do
    let old_lhs = coAxBranchLHS left
        new_lhs = coAxBranchLHS right
    subst <- MaybeT (pure (tcMatchTys old_lhs new_lhs))
    renamed <- MaybeT $ pure $ traverse (\v -> lookupTyVar subst v >>= getTyVar_maybe) (coAxBranchTyVars left)
    guard (length renamed == length (coAxBranchTyVars right) &&
           mkVarSet renamed == mkVarSet (coAxBranchTyVars right) &&
           substTyUnchecked subst (coAxBranchRHS left) `eqType` coAxBranchRHS right)

data IndexedEquation = IndexedEquation [TyVar] [Type] (TyCon -> Type)

applyRow :: IndexedEquation -> TyCon -> CoAxBranch
applyRow (IndexedEquation vars lhs rhs) family = mkBranch vars lhs (rhs family)

-- | Instantiate only the actual case equalities. Translate the resulting row.
indexedEquations :: IndexedKind -> Id -> TyCon -> Maybe (Name, Name) -> IndexedSource
                 -> MaybeT TcM [(IndexedEquation, Maybe (TyCon, CoAxBranch))]
indexedEquations kind owner header private (IndexedSource parameters domain carried result worker bindings rows) =
    traverse row rows
  where
    row (IndexedRow pat proofs body) = do
      let equalities = map CoVarCo proofs
      subst <- MaybeT $ pure $ tcUnifyTys alwaysBindTv
          (map coercionLKind equalities) (map coercionRKind equalities)
      let specialize = substTyUnchecked subst
          args = map (specialize . mkTyVarTy) parameters
          result_kind = specialize result
      (packed, fields) <- indexedPatternTypes specialize pat
      carried_vars <- lift $ traverse (freshVar "carried" . specialize . varType) carried
      let carried_fields = zip carried (mkTyVarTys carried_vars)
      (value, helper) <- case kind of
        IndexedTraversal -> do
          value <- MaybeT $ pure $
            indexedTraversalResult bindings owner parameters domain carried result header
                                   specialize proofs pat fields carried_fields body
          pure (value, Nothing)
        _ -> case (private, indexedResultCase owner parameters pat body) of
          (Just names, Just alternative) ->
            result_case names result_kind specialize proofs fields alternative
          _ -> do
            value <- MaybeT $ pure $
              indexedResult bindings owner worker parameters domain header specialize proofs pat fields body
            pure (value, Nothing)
      let lhs = args ++ packed : mkTyVarTys carried_vars
          vars = tyCoVarsOfTypesWellScoped lhs
          branch = IndexedEquation vars lhs value
      guard (all isTyVar vars && typeKind (value header) `eqType` result_kind &&
             validBranch (applyRow branch header))
      pure (branch, helper)

    result_case (name, axiom) result_kind specialize proofs fields
                (IndexedResultCase types operand pat body) = do
      let tys = map specialize types
          input_kind = substTyWith parameters tys domain
          case_kind = substTyWith parameters tys result
          locals = tyCoVarsOfTypesWellScoped [case_kind, result_kind]
          vars = mkTyVarTys locals
      guard (all isTyVar locals)
      argument <- MaybeT (pure (lookup operand fields))
      guard (typeKind argument `eqType` input_kind)
      (packed, case_fields) <- indexedPatternTypes specialize pat
      guard (typeKind packed `eqType` case_kind)
      body_value <- MaybeT $ pure $
        indexedResult bindings owner Nothing parameters domain header specialize proofs pat case_fields body
      let rhs = body_value header
          lhs = vars ++ [packed]
          branch_vars = tyCoVarsOfTypesWellScoped lhs
          branch = mkBranch branch_vars lhs rhs
      guard (all isTyVar branch_vars && typeKind rhs `eqType` result_kind && validBranch branch)
      operand_ty <- lift (freshVar "case" case_kind)
      let binders = mkNamedTyConBinders Specified locals ++ [mkAnonTyConBinder operand_ty]
          helper = mkFamily name axiom binders result_kind [branch]
          value family = mkTyConApp helper (vars ++ [mkTyConApp family (tys ++ [argument])])
      pure (value, Just (helper, branch))

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

-- | Validate a promoted constructor and its exact erased worker layout.
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

{- Note [Checked lookup over two indexed operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An environment lookup first matches the environment, then the position.
Both matches refine the scope index. Check the nominal equalities before
using either set of fields. A recursive call consumes one direct field
from each input, with both kinds at the same specialized index.

A missing environment branch is permissible only when no constructor
of the position can inhabit the corresponding scope. Desugaring emits
a pattern failure there. The simplifier can name this failure, so check
the body behind that local name. Publish only if the original and final
Core give the same equations.
-}

data IndexedLookup = IndexedLookup Id [TyVar] [Type] Type [(IndexedPattern, IndexedRow)]

-- | Select a signed lookup over two operands at the same index.
indexedLookupType :: Type -> Bool
indexedLookupType = isJust . indexedLookupSignature

-- | Read the shared scope and value domains from the checked signature.
indexedLookupSignature :: Type -> Maybe (TyVar, [Type], Type)
indexedLookupSignature ty = do
    ([(scope_match, scope)], tau) <- pure (splitForAllTyVars ty)
    (matches, [Scaled left_mult left, Scaled right_mult right], result) <- pure (splitFunTys tau)
    (left_tc, [left_scope]) <- splitTyConApp_maybe left
    (right_tc, [right_scope]) <- splitTyConApp_maybe right
    guard (all (`eqType` unmatchableDataConTy) (scope_match : matches) &&
           all isManyTy [left_mult, right_mult] &&
           eqTypes [left_scope, right_scope] (map mkTyVarTy [scope, scope]) &&
           isAlgTyCon left_tc && isAlgTyCon right_tc &&
           any hasEqSpec (tyConDataCons left_tc) &&
           length (tyConDataCons right_tc) == 2 &&
           all hasEqSpec (tyConDataCons right_tc))
    pure (scope, [left, right], result)
  where
    hasEqSpec con = case DC.dataConFullSig con of
      (_, _, _ : _, _, _, _) -> True
      _ -> False

-- | Read the two nested matches of a structurally decreasing lookup.
indexedLookup :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe IndexedLookup
indexedLookup bindings owner rhs = do
    (parameters@[scope], [left, right], Core.Case (Core.Var input) _ result alts) <- function rhs
    guard (input == left)
    let left_kind = varType left
        right_kind = varType right
    (declared_scope, domains, result_type) <- indexedLookupSignature (varType owner)
    let instantiate = substTyWith [declared_scope] [mkTyVarTy scope]
    guard (varType scope `eqType` varType declared_scope &&
           eqTypes (map instantiate (domains ++ [result_type])) [left_kind, right_kind, result])
    (left_tc, [left_scope]) <- splitTyConApp_maybe left_kind
    (right_tc, [right_scope]) <- splitTyConApp_maybe right_kind
    guard (eqTypes [left_scope, right_scope] (map mkTyVarTy [scope, scope]))
    [(con, binders, rest)] <- pure [(dc, bs, e) | Core.Alt (Core.DataAlt dc) bs e <- alts,
                                               not (patternFailure e)]
    guard (all (validOuterAlt con) alts)
    traverse_ (noPosition left_kind right_kind right_tc)
              (filter (/= con) (tyConDataCons left_tc))
    (outer, outer_proofs) <- indexedAlt left con binders
    [(first, first_bs, first_rhs), (next, next_bs, next_rhs)] <- caseOn right rest
    guard (not (null outer_proofs) && [first, next] == tyConDataCons right_tc)
    rows <- traverse (\(dc, bs, e) -> do
        (pat, proofs) <- indexedAlt right dc bs
        guard (not (null proofs))
        pure (outer, IndexedRow pat (outer_proofs ++ proofs) e))
        [(first, first_bs, first_rhs), (next, next_bs, next_rhs)]
    pure (IndexedLookup owner parameters [left_kind, right_kind] result rows)
  where
    validOuterAlt con (Core.Alt alt _ e) = alt == Core.DataAlt con || patternFailure e

    patternFailure (Core.Case scrut _ _ []) = failure scrut
    patternFailure _ = False

    failure (Core.Var v)
      | not (isExternalName (idName v))
      , Just rhs <- lookup v bindings
      = directFailure rhs
    failure expr = directFailure expr

    directFailure expr | (Core.Var v, _) <- Core.collectArgs expr = v `hasKey` patErrorIdKey
    directFailure _ = False

    noPosition left_kind right_kind right_tc con = do
      subst <- tcUnifyTys alwaysBindTv [left_kind] [DC.dataConOrigResTy con]
      (tc, args) <- splitTyConApp_maybe (substTyUnchecked subst right_kind)
      guard (tc == right_tc && all (DC.dataConCannotMatch args) (tyConDataCons right_tc))

-- | Publish the nominal rows after checking the field projection and recursion.
publishIndexedLookup :: (TyCon -> Bool) -> (Id, (IndexedLookup, IndexedLookup), [Id])
                     -> TcM (Maybe (Id, [TyCon]))
publishIndexedLookup public (owner, (original, final), _) = runMaybeT $ do
    table <- makeIndexedLookupTable False public owner original (Just final)
    pure (setIdDetails owner (SourceTableId (idName owner) table NominalSourceTable), [table])

-- | Select functions whose signatures can head an early indexed lookup,
-- GADT structural traversal, or scope-indexed closure chain.
earlyClosureCandidateType :: Type -> Bool
earlyClosureCandidateType ty =
    indexedLookupType ty || gadtDomainType ty || gadtCallbackType ty
  where
    indexedDomain dom
      | Just (tc, args@(_ : _)) <- splitTyConApp_maybe dom
      = isAlgTyCon tc &&
        (any (not . DC.isVanillaDataCon) (tyConDataCons tc) ||
         any (not . isLiftedTypeKind . typeKind) args)
      | otherwise = False

    gadtDomainType t =
      let (tvs, tau) = splitForAllTyVars t
      in not (null tvs) && case splitFunTys tau of
           (_, Scaled mult first : _, _) -> isManyTy mult && indexedDomain first
           _ -> False

    indexedCallback cb =
      let (_, cb_tau) = splitForAllTyVars cb
      in case splitFunTys cb_tau of
           (_, [Scaled m cb_in], cb_out) -> isManyTy m && indexedDomain cb_in && indexedDomain cb_out
           _ -> False

    gadtCallbackType t =
      let (tvs, tau) = splitForAllTyVars t
      in not (null tvs) && case splitFunTys tau of
           (_, [Scaled mult cb], rest) | isManyTy mult && indexedCallback cb ->
             let (_, res_tau) = splitForAllTyVars rest
             in case splitFunTys res_tau of
                  (_, [Scaled m2 dom_in], dom_out) ->
                    isManyTy m2 && indexedDomain dom_in && indexedDomain dom_out
                  _ -> False
           (_, Scaled mult cb : rest_args@(_ : _), res)
             | isManyTy mult && indexedCallback cb ->
                 let (_, res_tau) = splitForAllTyVars res
                     valid_res = indexedDomain res_tau || case splitFunTys res_tau of
                       (_, [Scaled m2 dom_in], dom_out) ->
                         isManyTy m2 && indexedDomain dom_in && indexedDomain dom_out
                       _ -> False
                 in valid_res &&
                    all (\(Scaled m a) -> isManyTy m && (indexedDomain a || indexedCallback a)) rest_args
           _ -> False

-- | Stage eligible local indexed lookups, GADT traversals, and substitution
-- chains before checking dependent declarations in the rest of the group.
sourceEarlyClosureTables :: [Id] -> NameEnv (Id, CoreExpr)
                         -> TcM ([(Id, TyCon)], [TyCon], [Id])
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
            bindings = map (safeSimpleOptBind opts) unoptimized
            in_group_set = mkNameSet (map idName group_owners)
            fresh_owner v = idName v `elemNameSet` in_group_set && isNothing (sourceTableId_maybe v)
            lookupDef name = fst <$> lookupNameEnv definitions name
            extDeps owner =
              let names = nameSetElemsStable $ mkNameSet $
                    filter (\n -> n /= idName owner && isExternalName n && nameModule_maybe n == here)
                           (reachable bindings owner)
              in traverse lookupDef names
            isConstructorId v = isJust (isDataConWorkId_maybe v <|> isDataConWrapId_maybe v)
            onlyCalls allowed owner = do
              raw_rhs <- snd <$> lookupNameEnv definitions (idName owner)
              opt_rhs <- lookup owner bindings
              let bad v = isValueVar v && idName v `notElem` allowed && not (isConstructorId v)
              guard (all (null . exprSomeFreeVarsList bad) [raw_rhs, opt_rhs])

        -- 1. Indexed lookups (e.g. lookupEnv)
        lookups <- fmap catMaybes $ for_list group_owners $ \owner ->
          if not (fresh_owner owner && indexedLookupType (varType owner))
            then pure Nothing
            else runMaybeT $ do
              rhs <- MaybeT (pure (lookup owner bindings))
              checked <- MaybeT (pure (indexedLookup bindings owner rhs))
              table <- makeIndexedLookupTable True public owner checked Nothing
              pure (setIdDetails owner (SourceTableId (idName owner) table NominalSourceTable), table)
        let lookup_done = mkNameSet (map (idName . fst) lookups)

        -- 2. GADT structural traversals (e.g. plug, plugS)
        traversals <- fmap catMaybes $ for_list group_owners $ \owner ->
          if not (fresh_owner owner) || idName owner `elemNameSet` lookup_done
            then pure Nothing
            else runMaybeT $ do
              rhs <- MaybeT (pure (lookup owner bindings))
              src <- MaybeT (pure (indexedTraversal bindings owner rhs))
              helpers <- MaybeT (pure (extDeps owner))
              MaybeT ((fmap . fmap) (\(v, tcs) -> (v, tcs, helpers))
                        (publishIndexed True public (owner, (IndexedTraversal, src, src), helpers)))
        let trav_tables = [(v, tc) | (v, tc : _, _) <- traversals]
            trav_extra_tcs = [tc | (_, _ : rest, _) <- traversals, tc <- rest]
            trav_helpers = [h | (_, _, hs) <- traversals, h <- hs]
            trav_done = lookup_done `extendNameSetList` map (idName . fst) trav_tables

        -- 3. Typed substitution closures & helper chains
        let typed_roots =
              [ (owner, shape, helpers)
              | owner <- group_owners
              , fresh_owner owner, not (idName owner `elemNameSet` trav_done)
              , Just rhs <- [lookup owner bindings]
              , Just shape <- [familyInputs bindings owner rhs]
              , Just helpers <- [extDeps owner]
              , not (null helpers) ]
        typed_closures <- fmap catMaybes $ for_list typed_roots $ \(owner, shape, helpers) -> do
          generated <- allocateFamilies True shape
          pure $ do
            (root, families) <- generated
            guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
            let new = setIdDetails owner (CheckedClosureSourceTableId (idName owner) root (map idName helpers))
            pure (new, root, filter (/= root) families, helpers)
        let direct_typed_shapes = map (\(_, s, _) -> s) typed_roots ++
              directTypedOperationShapes bindings group_owners
            sameId a b = idName a == idName b
            renaming_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
              [ (actual_ren, (actual_ext, shape), helpers)
              | shape <- direct_typed_shapes
              , let (ren, ext) = renamingOperations shape
              , Just actual_ren <- [lookupDef (idName ren)]
              , Just actual_ext <- [lookupDef (idName ext)]
              , isJust (onlyCalls [] actual_ext)
              , Just helpers <- [extDeps actual_ren]
              , any (sameId actual_ext) helpers ]
            ext_cands = nubBy (\(_, (a, _), _) (_, (b, _), _) -> sameId a b)
              [ c | c@(_, (actual_ext, _), _) <- renaming_cands, fresh_owner actual_ext ]
        typed_exts <- fmap catMaybes $ for_list ext_cands $ \cand ->
          publishRenameExtension True public cand
        let ext_table_map = [(idName v, tc) | (v, [tc]) <- typed_exts] ++
              [(idName v, tc) | v <- group_owners, Just tc <- [sourceTableId_maybe v]]
        typed_rens <- fmap catMaybes $ for_list
          [ c | c@(actual_ren, _, _) <- renaming_cands, fresh_owner actual_ren ] $ \cand ->
            publishRenaming True public ext_table_map cand
        let ren_table_map = [(idName v, tc) | (v, [tc]) <- typed_rens] ++ ext_table_map
            subst_ext_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
              [ (actual_sext, (actual_ren, actual_weak, shape), helpers)
              | shape <- direct_typed_shapes
              , let (ren, _) = renamingOperations shape
              , (_, sext, Just weak) <- [substitutionOperations shape]
              , Just actual_ren <- [lookupDef (idName ren)]
              , Just actual_sext <- [lookupDef (idName sext)]
              , Just actual_weak <- [lookupDef (idName weak)]
              , isJust (onlyCalls [] actual_weak)
              , isJust (onlyCalls [idName actual_ren, idName actual_weak] actual_sext)
              , Just helpers <- [extDeps actual_sext]
              , all (\n -> n `elem` map idName helpers) [idName actual_ren, idName actual_weak] ]
            weak_cands = nubBy (\a b -> sameId (fst a) (fst b))
              [ (actual_weak, shape)
              | (_, (_, actual_weak, shape), _) <- subst_ext_cands
              , fresh_owner actual_weak ]
        typed_weaks <- fmap catMaybes $ for_list weak_cands (publishWeakening True public)
        let weak_table_map = [(idName v, tc) | (v, [tc]) <- typed_weaks] ++ ext_table_map
        typed_sexts <- fmap catMaybes $ for_list
          [ c | c@(actual_sext, _, _) <- subst_ext_cands, fresh_owner actual_sext ] $ \cand ->
            publishSubstitutionExtension True public ren_table_map weak_table_map cand
        let sext_table_map = [(idName v, tc) | (v, [tc]) <- typed_sexts] ++ ext_table_map
            subst_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
              [ (actual_sub, (actual_sext, shape), helpers)
              | shape <- direct_typed_shapes
              , let (ren, _) = renamingOperations shape
              , (sub, sext, Just weak) <- [substitutionOperations shape]
              , Just actual_sub <- [lookupDef (idName sub)]
              , fresh_owner actual_sub
              , Just actual_sext <- [lookupDef (idName sext)]
              , let op_names = [idName actual_sub, idName ren, idName actual_sext, idName weak]
              , isJust (onlyCalls op_names actual_sub)
              , Just helpers <- [extDeps actual_sub]
              , any (sameId actual_sext) helpers ]
        typed_substs <- fmap catMaybes $ for_list subst_cands $ \cand ->
          publishSubstitution True public sext_table_map cand

        -- 4. Untyped scope-indexed closures & helper chains
        let typed_done = trav_done `extendNameSetList`
              ([idName v | (v, _, _, _) <- typed_closures] ++
               [idName v | (v, _) <- typed_exts ++ typed_rens ++ typed_weaks ++ typed_sexts ++ typed_substs])
            untyped_roots =
              [ (owner, shape, helpers)
              | owner <- group_owners
              , fresh_owner owner, not (idName owner `elemNameSet` typed_done)
              , Just rhs <- [lookup owner bindings]
              , Just shape <- [untypedFamilyInputs bindings owner rhs]
              , Just helpers <- [extDeps owner]
              , not (null helpers) ]
            direct_untyped_shapes = map (\(_, s, _) -> s) untyped_roots ++
              directUntypedOperationShapes bindings group_owners
            uren_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
              [ (actual_ren, (actual_ext, shape), helpers)
              | shape <- direct_untyped_shapes
              , let (ren, ext) = untypedRenamingOperations shape
              , Just actual_ren <- [lookupDef (idName ren)]
              , Just actual_ext <- [lookupDef (idName ext)]
              , isJust (onlyCalls [] actual_ext)
              , Just helpers <- [extDeps actual_ren]
              , any (sameId actual_ext) helpers ]
            uext_cands = nubBy (\(_, (a, _), _) (_, (b, _), _) -> sameId a b)
              [ c | c@(_, (actual_ext, _), _) <- uren_cands, fresh_owner actual_ext ]
        untyped_exts <- fmap catMaybes $ for_list uext_cands (publishUntypedRenameExtension True public)
        let uext_table_map = [(idName v, tc) | (v, [tc]) <- untyped_exts] ++
              [(idName v, tc) | v <- group_owners, Just tc <- [sourceTableId_maybe v]]
        untyped_rens <- fmap catMaybes $ for_list
          [ c | c@(actual_ren, _, _) <- uren_cands, fresh_owner actual_ren ] $ \cand ->
            publishUntypedRenaming True public uext_table_map cand
        let uren_table_map = [(idName v, tc) | (v, [tc]) <- untyped_rens] ++ uext_table_map
            usubst_ext_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
              [ (actual_sext, (actual_ren, mb_actual_weak, shape), helpers)
              | shape <- direct_untyped_shapes
              , let (ren, _) = untypedRenamingOperations shape
              , Just (_, sext, mb_weak) <- [untypedSubstitutionOperations shape]
              , Just actual_ren <- [lookupDef (idName ren)]
              , Just actual_sext <- [lookupDef (idName sext)]
              , Just mb_actual_weak <- [case mb_weak of
                  Nothing -> Just Nothing
                  Just w  -> do
                    aw <- lookupDef (idName w)
                    w_rhs <- lookup aw bindings
                    guard (isJust (validConstructorAliasBody w_rhs) && isJust (onlyCalls [] aw))
                    pure (Just aw)]
              , let allowed_sext = idName actual_ren : maybe [] (pure . idName) mb_actual_weak
              , isJust (onlyCalls allowed_sext actual_sext)
              , Just raw_hs <- [extDeps actual_sext]
              , let helpers = nubBy sameId (maybe [] pure mb_actual_weak ++ raw_hs)
              , idName actual_ren `elem` map idName helpers ]
            uweak_cands = nubBy (\a b -> sameId (fst a) (fst b))
              [ (actual_weak, shape)
              | (_, (_, Just actual_weak, shape), _) <- usubst_ext_cands
              , fresh_owner actual_weak ]
        untyped_weaks <- fmap catMaybes $ for_list uweak_cands (publishUntypedWeakening True public)
        let uweak_table_map = [(idName v, tc) | (v, [tc]) <- untyped_weaks] ++ uext_table_map
        untyped_sexts <- fmap catMaybes $ for_list
          [ c | c@(actual_sext, _, _) <- usubst_ext_cands, fresh_owner actual_sext ] $ \cand ->
            publishUntypedSubstitutionExtension True public uren_table_map uweak_table_map cand
        let usext_table_map = [(idName v, tc) | (v, tc : _) <- untyped_sexts] ++ uext_table_map
            usubst_cands = nubBy (\(a, _, _) (b, _, _) -> sameId a b)
              [ (actual_sub, (actual_sext, shape), helpers)
              | shape <- direct_untyped_shapes
              , let (ren, _) = untypedRenamingOperations shape
              , Just (sub, sext, mb_weak) <- [untypedSubstitutionOperations shape]
              , Just actual_sub <- [lookupDef (idName sub)]
              , fresh_owner actual_sub
              , Just actual_sext <- [lookupDef (idName sext)]
              , let op_names = [idName actual_sub, idName ren, idName actual_sext] ++
                               maybe [] (pure . idName) mb_weak
              , isJust (onlyCalls op_names actual_sub)
              , Just raw_hs <- [extDeps actual_sub]
              , let mb_actual_weak = mb_weak >>= lookupDef . idName
                    helpers = nubBy sameId (maybeToList mb_actual_weak ++ raw_hs)
              , any (sameId actual_sext) helpers ]
        untyped_substs <- fmap catMaybes $ for_list usubst_cands $ \cand ->
          publishUntypedSubstitution True public usext_table_map cand
        let usubst_table_map = [(idName v, tc) | (v, [tc]) <- untyped_substs] ++
              [(idName v, tc) | v <- group_owners, Just tc <- [sourceTableId_maybe v]]
        untyped_closures <- fmap catMaybes $ for_list untyped_roots $ \(owner, shape, helpers) -> do
          let mb_subst_u_tc = do
                (sub_u, _, _) <- untypedSubstitutionOperations shape
                lookup (idName sub_u) usubst_table_map <|> sourceTableId_maybe sub_u
          generated <- case mb_subst_u_tc of
            Just subst_u_tc -> allocateUntypedSubstViaSubstU True subst_u_tc shape
            Nothing         -> allocateUntypedFamilies True shape
          pure $ do
            (root, families) <- generated
            guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
            let new = setIdDetails owner (CheckedClosureSourceTableId (idName owner) root (map idName helpers))
            pure (new, root, filter (/= root) families, helpers)

        -- 5. Standalone constructor aliases, named substitution maps, sigma combinators, and forwarders
        let op_entries0 = typed_exts ++ typed_rens ++ typed_weaks ++ typed_sexts ++ typed_substs ++
                          untyped_exts ++ untyped_rens ++ untyped_weaks ++ untyped_sexts ++ untyped_substs
            done0 = typed_done `extendNameSetList`
              ([idName v | (v, _, _, _) <- untyped_closures] ++ [idName v | (v, _) <- op_entries0])
            alias_cands =
              [ (owner, spec, ([] :: [Id]))
              | owner <- group_owners
              , fresh_owner owner, not (idName owner `elemNameSet` done0)
              , Just rhs <- [lookup owner bindings]
              , Just spec <- [checkedConstructorAlias owner rhs]
              , isJust (onlyCalls [] owner) ]
        early_aliases <- fmap catMaybes $ for_list alias_cands (publishConstructorAlias True public)
        let done1 = done0 `extendNameSetList` [idName v | (v, _) <- early_aliases]
            map_cands =
              [ (owner, spec, ([] :: [Id]))
              | owner <- group_owners
              , fresh_owner owner, not (idName owner `elemNameSet` done1)
              , Just rhs <- [lookup owner bindings]
              , Just spec <- [checkedSubstMap bindings owner rhs]
              , isJust (onlyCalls [] owner) ]
        early_maps <- fmap catMaybes $ for_list map_cands (publishSubstMap True public)
        let done2 = done1 `extendNameSetList` [idName v | (v, _) <- early_maps]
            early_known_tables0 =
              [ (idName v, tc) | (v, tc : _) <- op_entries0 ++ early_aliases ++ early_maps ] ++
              [ (idName v, tc) | (v, tc) <- lookups ++ trav_tables ] ++
              [ (idName v, r)  | (v, r, _, _) <- typed_closures ++ untyped_closures ] ++
              [ (idName v, tc) | v <- typeEnvIds (tcg_type_env env), Just tc <- [sourceTableId_maybe v] ]
            resolveEarlyTable0 v
              | Just tc <- lookup (idName v) early_known_tables0 = Just tc
              | otherwise = sourceTableId_maybe v
            resolveEarlyDepId v
              | nameModule_maybe (idName v) == here =
                  lookupDef (idName v) <|>
                  (case lookupTypeEnv (tcg_type_env env) (idName v) of
                     Just (AnId actual) | isJust (sourceTableId_maybe actual) -> Just actual
                     _ -> Nothing)
              | isJust (sourceTableId_maybe v) = Just v
              | otherwise = Nothing
            sigma_cands =
              [ (owner, spec, hs)
              | owner <- group_owners
              , fresh_owner owner, not (idName owner `elemNameSet` done2)
              , Just rhs <- [lookup owner bindings]
              , Just spec <- [checkedSigmaComb owner rhs]
              , Just hs <- [case spec of
                  SigmaSeq _ trav_id -> do
                    resolved_trav <- resolveEarlyDepId trav_id
                    onlyCalls [idName resolved_trav] owner
                    local_hs <- extDeps owner
                    pure (nubBy sameId (resolved_trav : local_hs))
                  _ -> [] <$ onlyCalls [] owner] ]
        early_sigmas <- fmap catMaybes $ for_list sigma_cands
                          (publishSigmaComb True public resolveEarlyTable0)
        let done3 = done2 `extendNameSetList` [idName v | (v, _) <- early_sigmas]
            early_known_tables1 = [(idName v, tc) | (v, tc : _) <- early_sigmas] ++ early_known_tables0
            resolveEarlyTable1 v
              | Just tc <- lookup (idName v) early_known_tables1 = Just tc
              | otherwise = sourceTableId_maybe v
            fwd_cands =
              [ (owner, fwd, hs)
              | owner <- group_owners
              , fresh_owner owner, not (idName owner `elemNameSet` done3)
              , Just rhs <- [lookup owner bindings]
              , Just fwd@(SubstForwardSpec _ trav_id map_id _ _ _) <- [checkedSubstForwardOrig owner rhs]
              , Just resolved_trav <- [resolveEarlyDepId trav_id]
              , Just resolved_map <- [resolveEarlyDepId map_id]
              , isJust (onlyCalls [idName resolved_trav, idName resolved_map] owner)
              , Just local_hs <- [extDeps owner]
              , let hs = nubBy sameId ([resolved_trav, resolved_map] ++ local_hs) ]
        early_fwds <- fmap catMaybes $ for_list fwd_cands
                        (publishSubstForward True public resolveEarlyTable1)

        let op_entries = op_entries0 ++ early_aliases ++ early_maps ++ early_sigmas ++ early_fwds
            op_tables = [(v, tc) | (v, tc : _) <- op_entries]
            op_extra_tcs = [tc | (_, _ : rest) <- op_entries, tc <- rest]
            op_helpers = [ h | (v, _) <- op_entries
                         , CheckedClosureSourceTableId _ _ hs <- [idDetails v]
                         , nm <- hs, Just h <- [lookupDef nm] ]
            closure_tables = [(v, r) | (v, r, _, _) <- typed_closures ++ untyped_closures]
            closure_companions = [tc | (_, _, tcs, _) <- typed_closures ++ untyped_closures, tc <- tcs]
            closure_helpers = [h | (_, _, _, hs) <- typed_closures ++ untyped_closures, h <- hs]
            all_tables = lookups ++ trav_tables ++ closure_tables ++ op_tables
            all_companions = trav_extra_tcs ++ closure_companions ++ op_extra_tcs
            all_helpers = nubBy (\a b -> idName a == idName b)
                            (trav_helpers ++ closure_helpers ++ op_helpers)
        pure (all_tables, all_companions, all_helpers)
  where
    for_list = flip traverse

-- | Recover FamilyInputs directly from a local subst or rename operation when
-- a declaration group uses rename/subst/ext without demanding substSingle yet.
directTypedOperationShapes :: [(Id, CoreExpr)] -> [Id] -> [FamilyInputs]
directTypedOperationShapes bindings candidates = mapMaybe from_subst candidates ++ mapMaybe from_rename candidates
  where
    from_subst subst_id = do
      rhs <- lookup subst_id bindings
      ([g_tv, _, a_tv], [sigma, scrutinee], traversal_case) <- function rhs
      let context_kind = varType g_tv
          sort_kind = varType a_tv
      (context_tc, []) <- splitTyConApp_maybe context_kind
      (sort_tc, []) <- splitTyConApp_maybe sort_kind
      guard (all isAlgTyCon [context_tc, sort_tc])
      (term, [_, _]) <- splitTyConApp_maybe (varType scrutinee)
      (_, cb_tau) <- pure (splitForAllTyVars (varType sigma))
      (_, [Scaled _ idx_ty], _) <- pure (splitFunTys cb_tau)
      (index, [_, _]) <- splitTyConApp_maybe idx_ty
      guard (all isAlgTyCon [term, index])
      [c1, c2] <- pure (tyConDataCons index)
      rows <- caseOn scrutinee traversal_case
      var <- find (\con -> case [ (bs, e) | (dc, bs, e) <- rows, dc == con ] of
                      [(bs, e)] | [field] <- filter isValueVar bs
                                , Just checked <- branchBody bs e
                                , Just (Core.Var use, [Core.Var x]) <- application checked
                                -> use == sigma && x == field
                      _ -> False) (tyConDataCons term)
      ext_con <- findPromotedExtCon term
      tryPair subst_id context_kind sort_kind term ext_con index sigma var rows c1 c2 <|>
        tryPair subst_id context_kind sort_kind term ext_con index sigma var rows c2 c1

    tryPair subst_id context_kind sort_kind term ext_con index sigma var rows hit miss = do
      (ops, recipes) <- checkedTraversals bindings subst_id sigma var hit miss rows
      pure (FamilyInputs subst_id 1 context_kind sort_kind term ext_con index ops var hit miss recipes)

    from_rename rename_id = do
      rhs <- lookup rename_id bindings
      ([g_tv, _, a_tv], [rho, operand], renaming) <- function rhs
      let context_kind = varType g_tv
          sort_kind = varType a_tv
      (context_tc, []) <- splitTyConApp_maybe context_kind
      (sort_tc, []) <- splitTyConApp_maybe sort_kind
      guard (all isAlgTyCon [context_tc, sort_tc])
      (term, [_, _]) <- splitTyConApp_maybe (varType operand)
      (_, cb_tau) <- pure (splitForAllTyVars (varType rho))
      (_, [Scaled _ idx_ty], _) <- pure (splitFunTys cb_tau)
      (index, [_, _]) <- splitTyConApp_maybe idx_ty
      guard (all isAlgTyCon [term, index])
      [c1, c2] <- pure (tyConDataCons index)
      let constructors = tyConDataCons term
      rows <- caseOn operand renaming
      guard (length rows == length constructors && all lazyConstructor constructors)
      var <- find (\con -> case [ (bs, e) | (dc, bs, e) <- rows, dc == con ] of
                      [(bs, e)] | [orig] <- filter isValueVar bs
                                , Just res <- branchBody bs e
                                , Just (vh, [renamed]) <- constructorCall res
                                , vh == con
                                , Just (Core.Var cb, [Core.Var arg]) <- application renamed
                                -> cb == rho && arg == orig
                      _ -> False) constructors
      ext_con <- findPromotedExtCon term
      tryRename rename_id context_kind sort_kind term ext_con index rho var rows c1 c2 <|>
        tryRename rename_id context_kind sort_kind term ext_con index rho var rows c2 c1

    tryRename rename_id context_kind sort_kind term ext_con index rho var rows hit miss = do
      lifted <- traverse (checkedRow bindings var hit miss rename_id rho)
                         [row | row@(con, _, _) <- rows, con /= var]
      let (recipes, exts) = unzip lifted
          steps = concatMap concat exts
          isRenLift (NamedLift _) = True
          isRenLift InlinedRename = True
          isRenLift (InlinedSubst _) = False
      guard (all isRenLift steps)
      ext_id <- case [v | NamedLift v <- steps] of
        v : vs | all (== v) vs -> Just v
        _ -> Nothing
      ext_rhs <- lookup ext_id bindings
      checkRenameExtension hit miss ext_rhs
      let ops = ClosureOperations (rename_id, ext_id) (rename_id, ext_id, Nothing)
      pure (FamilyInputs rename_id 1 context_kind sort_kind term ext_con index ops var hit miss recipes)

    findPromotedExtCon term =
      let arg_tys = [ scaledThing f
                    | con <- tyConDataCons term
                    , let (_, _, _, _, fs, _) = DC.dataConFullSig con
                    , f <- fs ]
          ext_tcs = [ ext_tc
                    | ty <- arg_tys
                    , Just (t, [ctx, _]) <- [splitTyConApp_maybe ty]
                    , t == term
                    , Just (ext_tc, [_, _]) <- [splitTyConApp_maybe ctx]
                    , isPromotedDataCon ext_tc ]
      in case ext_tcs of
           tc : _ -> Just tc
           []     -> Nothing

-- | Extract unary untyped renaming and substitution shapes directly from `substU` and `renameU`.
directUntypedOperationShapes :: [(Id, CoreExpr)] -> [Id] -> [UntypedFamilyInputs]
directUntypedOperationShapes bindings candidates =
    mapMaybe from_subst_u candidates ++ mapMaybe from_rename_u candidates
  where
    findUntypedExtCon term =
      let ext_tcs = [ ext_tc
                    | con <- tyConDataCons term
                    , let (_, _, _, _, fs, _) = DC.dataConFullSig con
                    , f <- fs
                    , Just (t, [ctx]) <- [splitTyConApp_maybe (scaledThing f)]
                    , t == term
                    , Just (ext_tc, [_]) <- [splitTyConApp_maybe ctx]
                    , isPromotedDataCon ext_tc ]
      in case ext_tcs of { tc : _ -> Just tc; [] -> Nothing }

    from_subst_u subst_id = do
      rhs <- lookup subst_id bindings
      ([g_tv, _], [sigma, scrutinee], traversal_case) <- function rhs
      let context_kind = varType g_tv
      (context_tc, []) <- splitTyConApp_maybe context_kind
      guard (isAlgTyCon context_tc)
      (term, [_]) <- splitTyConApp_maybe (varType scrutinee)
      (_, [Scaled _ idx_ty], _) <- pure (splitFunTys (varType sigma))
      (index, [_]) <- splitTyConApp_maybe idx_ty
      guard (all isAlgTyCon [term, index])
      [c1, c2] <- pure (tyConDataCons index)
      rows <- caseOn scrutinee traversal_case
      var <- find (\con -> case [ (bs, e) | (dc, bs, e) <- rows, dc == con ] of
                      [(bs, e)] | [field] <- filter isValueVar bs
                                , Just checked <- branchBody bs e
                                , Just (Core.Var use, [Core.Var x]) <- application checked
                                -> use == sigma && x == field
                      _ -> False) (tyConDataCons term)
      ext_con <- findUntypedExtCon term
      trySubstU subst_id context_kind term ext_con index sigma var rows c1 c2 <|>
        trySubstU subst_id context_kind term ext_con index sigma var rows c2 c1

    trySubstU subst_id context_kind term ext_con index sigma var rows hit miss = do
      (ren_ops, subst_ops, recipes) <- untypedCheckedTraversals bindings subst_id sigma var hit miss rows
      pure (UntypedFamilyInputs subst_id 1 context_kind term ext_con index ren_ops (Just subst_ops) var hit miss recipes)

    from_rename_u rename_id = do
      rhs <- lookup rename_id bindings
      ([g_tv, _], [rho, operand], renaming) <- function rhs
      let context_kind = varType g_tv
      (context_tc, []) <- splitTyConApp_maybe context_kind
      guard (isAlgTyCon context_tc)
      (term, [_]) <- splitTyConApp_maybe (varType operand)
      (_, [Scaled _ idx_ty], _) <- pure (splitFunTys (varType rho))
      (index, [_]) <- splitTyConApp_maybe idx_ty
      guard (all isAlgTyCon [term, index])
      [c1, c2] <- pure (tyConDataCons index)
      let constructors = tyConDataCons term
      rows <- caseOn operand renaming
      guard (length rows == length constructors && all lazyConstructor constructors)
      var <- find (\con -> case [ (bs, e) | (dc, bs, e) <- rows, dc == con ] of
                      [(bs, e)] | [orig] <- filter isValueVar bs
                                , Just res <- branchBody bs e
                                , Just (vh, [renamed]) <- constructorCall res
                                , vh == con
                                , Just (Core.Var cb, [Core.Var arg]) <- application renamed
                                -> cb == rho && arg == orig
                      _ -> False) constructors
      ext_con <- findUntypedExtCon term
      tryU rename_id context_kind term ext_con index rho var rows c1 c2 <|>
        tryU rename_id context_kind term ext_con index rho var rows c2 c1

    tryU rename_id context_kind term ext_con index rho var rows hit miss = do
      lifted <- traverse (untypedCheckedRow bindings var hit miss rename_id rho)
                         [row | row@(con, _, _) <- rows, con /= var]
      let (recipes, exts) = unzip lifted
          steps = concatMap concat exts
          isRenLift (NamedLift _) = True
          isRenLift InlinedRename = True
          isRenLift (InlinedSubst _) = False
      guard (all isRenLift steps)
      ext_id <- case [v | NamedLift v <- steps] of
        v : vs | all (== v) vs -> Just v
        _ -> Nothing
      ext_rhs <- lookup ext_id bindings
      checkUntypedRenameExtension bindings hit miss ext_rhs
      pure (UntypedFamilyInputs rename_id 1 context_kind term ext_con index (rename_id, ext_id) Nothing var hit miss recipes)

-- | Full metadata of an early-staged SourceClosure entry in TcGblEnv:
-- (owner, root family, external helper Names, companion private TyCons).
earlySourceClosureEntries :: TcGblEnv -> [(Id, TyCon, [Name], [TyCon])]
earlySourceClosureEntries env =
    sortBy (\(a, _, _, _) (b, _, _, _) -> stableNameCmp (idName a) (idName b))
      [ (fun, table, helpers, companions fun)
      | fun <- typeEnvIds (tcg_type_env env)
      , Just (table, helpers) <- [classify fun] ]
  where
    tcs_by_mod_occ =
      [ ((nameModule_maybe (tyConName tc), nameOccName (tyConName tc)), tc)
      | tc <- tcg_tcs env ]
    companions fun =
      let mod_mb = nameModule_maybe (idName fun)
          occ_str = occNameString (nameOccName (idName fun))
          wanted = [mkTcOcc (p ++ occ_str) | p <- ["$closure:", "$apply:", "$lookup:", "$weaken:", "$substMap:"]]
      in [ tc | occ <- wanted, Just tc <- [lookup (mod_mb, occ) tcs_by_mod_occ] ]
    classify fun = case idDetails fun of
      CheckedClosureSourceTableId name table helpers
        | name == idName fun
        , isEarlyClosureTable table -> Just (table, helpers)
      SourceTableId name table NominalSourceTable
        | name == idName fun
        , isEarlyClosureTable table -> Just (table, [])
      _ -> Nothing

-- | Collect early-staged indexed lookup, traversal, and closure owners installed in the module.
earlyIndexedLookups :: TcGblEnv -> [(Id, TyCon)]
earlyIndexedLookups env = [(fun, table) | (fun, table, _, _) <- earlySourceClosureEntries env]

isIndexedLookupTable :: TyCon -> Bool
isIndexedLookupTable table
  | Just axiom <- closedFamilyTyConCoAxiom_maybe table
  , [first, second] <- fromBranches (coAxiomBranches axiom)
  = promotedPairLHS first && promotedPairLHS second &&
    isJust (getTyVar_maybe (coAxBranchRHS first)) &&
    case splitTyConApp_maybe (coAxBranchRHS second) of
      Just (callee, [_, _, _]) -> callee == table
      _ -> False
  | otherwise = False
  where
    promotedPairLHS branch
      | [_, left, right] <- coAxBranchLHS branch
      , Just (left_con, _) <- splitTyConApp_maybe left
      , Just (right_con, _) <- splitTyConApp_maybe right
      = isPromotedDataCon left_con && isPromotedDataCon right_con
      | otherwise = False

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
        unused = [ entry
                 | entry@(_, table, _, _) <- entries
                 , not (tyConName table `elemNameSet` used) ]
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

-- | Collect imported active non-auto rule heads visible from this module,
-- including heads stored in stripped/no-code interfaces.
visibleClosureRuleHeads :: TcGblEnv -> TcM NameSet
visibleClosureRuleHeads env = do
  hsc <- getTopEnv
  liftIO $ visibleSourceRuleHeads hsc (tcg_mod env) (tcg_src env) (tcVisibleOrphanMods env)

-- | Normalize nested early-closure family applications along an algebraic type spine.
rewriteEarlyClosureApps :: [TyCon] -> Type -> Type
rewriteEarlyClosureApps early_fams = go
  where
    go (AppTy a b) = AppTy (go a) (go b)
    go (TyConApp tc tys)
      | let tys' = map go tys
      = if tc `elem` early_fams
          then reductionReducedType (normaliseTcApp emptyFamInstEnvs Nominal tc tys')
          else TyConApp tc tys'
    go (FunTy af ma mult arg res) = FunTy af (go ma) (go mult) (go arg) (go res)
    go ty = ty

-- | Collect TyCons from exported interface types after normalizing early-closure calls.
survivingEarlyExportedTyCons :: [TyCon] -> [Type] -> TyConSet
survivingEarlyExportedTyCons [] tys = tyConsOfTypes tys
survivingEarlyExportedTyCons early_fams tys = tyConsOfTypes (concatMap collect tys)
  where
    collect ty@(TyConApp tc args)
      | tc `elem` early_fams = [rewriteEarlyClosureApps early_fams ty]
      | otherwise = TyConApp tc [] : concatMap collect args
    collect (TyVarTy v) = collect (varType v)
    collect (TermAppTy f a) = collect (varType f) ++ collect a
    collect (TermVarTy v) = collect (varType v)
    collect (DepFunTy _ v b) = collect (varType v) ++ collect b
    collect (AppTy a b) = collect a ++ collect b
    collect (ForAllTy ma (Bndr v _) b) = collect ma ++ collect (varType v) ++ collect b
    collect (FunTy _ ma mult a r) = concatMap collect [ma, mult, a, r]
    collect (CastTy t _) = collect t
    collect (ExistsTy _ v b) = collect (varType v) ++ collect b
    collect (DepDictTy d b) = collect (varType d) ++ collect b
    collect (ClassAppTy _ t1 t2 t3 t4) = concatMap collect [t1, t2, t3, t4]
    collect _ = []

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
            _ -> Nothing
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
    breakpoint Breakpoint{} = True
    breakpoint _ = False
    tidy = maybe [] (\bs -> [(v, stripTicksE breakpoint r) | (v, r) <- Core.flattenBinds bs]) mb_tidy
    tcs_by_mod_occ =
      [ ((nameModule_maybe (tyConName tc), nameOccName (tyConName tc)), tc)
      | ATyCon tc <- typeEnvElts type_env ]
    companions_of v =
      let mod_mb = nameModule_maybe (idName v)
          occ_str = occNameString (nameOccName (idName v))
          wanted = [mkTcOcc (p ++ occ_str) | p <- ["$closure:", "$apply:", "$lookup:", "$weaken:", "$substMap:"]]
      in [ tc | occ <- wanted, Just tc <- [lookup (mod_mb, occ) tcs_by_mod_occ] ]
    early_fams =
      [ tc
      | AnId v <- typeEnvElts type_env
      , CheckedClosureSourceTableId owner table _ <- [idDetails v]
      , owner == idName v, isEarlyClosureTable table
      , tc <- table : companions_of v ]
    iface_tycons = tyConsOfTypes (exportedInterfaceTypes exported type_env)
    exported_tycons =
      survivingEarlyExportedTyCons early_fams (exportedInterfaceTypes exported type_env)
    root_tables =
      [ table
      | AnId v <- typeEnvElts type_env
      , idName v `elemNameSet` exported
      , Just table <- [sourceTableId_maybe v] ] ++
      [ tc
      | ATyCon tc <- typeEnvElts type_env
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
      | AnId v <- typeEnvElts type_env
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
                 let floated = case find (\(x, _) -> idName x == owner) tidy of
                       Just (actual, _) ->
                         [ nm | nm <- reachable tidy actual
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
          breakpoint Breakpoint{} = True
          breakpoint _ = False
          inspect binds = [(v, stripTicksE breakpoint rhs) | (v, rhs) <- Core.flattenBinds binds]
          raw_desugared = inspect desugared_binds
          desugared = map (safeSimpleOptBind opts) raw_desugared
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
              simplified <- MaybeT (pure (inspect <$> mb_simplified))
              checkEntry exported expected table owner all_helpers companions
                         raw_desugared desugared simplified (inspect <$> mb_tidy)
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

    lookupStage owner check_rules helpers binds = do
      (actual, rhs) <- MaybeT $ pure $ find (\(v, _) -> idName v == idName owner) binds
      guard (varType actual `eqType` varType owner)
      when check_rules $ do
        guard (noUserIdRules actual)
        for_ helpers $ \h ->
          when (nameModule_maybe h == nameModule_maybe (idName owner)) $ do
            (hv, _) <- MaybeT $ pure $ find (\(v, _) -> idName v == h) binds
            guard (noUserIdRules hv)
      pure (actual, rhs)

    isConstructorId v = isJust (isDataConWorkId_maybe v <|> isDataConWrapId_maybe v)
    constructorOnly expr = case constructorCall expr of
      Just (_, args) -> all constructorOnly args
      Nothing -> False
    floatedConstructors exported binds v
      | not (idName v `elemNameSet` exported)
      , Just decl <- lookup v binds
      = (case function decl of
           Just (_ : _, [], body) -> constructorOnly body
           _ -> False) || isJust (validConstructorAliasBody decl)
      | otherwise = False
    checkOnlyCalls exported allow_floated allowed binds rhs =
      let bad v = isValueVar v && idName v `notElem` allowed &&
                  not (isConstructorId v) &&
                  not (allow_floated && floatedConstructors exported binds v)
      in guard (null (exprSomeFreeVarsList bad rhs))

    hasCompanionPrefix p = any (isPrefixOf p . occNameString . nameOccName . tyConName)

    checkEntry exported expected table owner helpers companions raw_desug desug simplified mb_tidy
      | indexedLookupType (varType owner) && isIndexedLookupTable table = do
          guard (length expected == 2)
          checkLookup expected table owner False desug
          checkLookup expected table owner True simplified
          for_ mb_tidy $ \tidy ->
            when (idName owner `elemNameSet` exported) $
              checkLookup expected table owner True tidy
      | hasCompanionPrefix "$closure:" companions = do
          guard (length companions == 3 && length expected == 1)
          (d_actual, d_rhs) <- lookupStage owner False helpers desug
          case familyInputs desug d_actual d_rhs of
            Just early_shape@(FamilyInputs _ arity _ _ _ _ _ _ _ _ _ _) -> do
              guard (tyConArity table == 2 * arity + 3)
              let checkTyped check_rules binds = do
                    (actual, rhs) <- lookupStage owner check_rules helpers binds
                    shape <- MaybeT (pure (familyInputs binds actual rhs))
                    guard (sameShapes early_shape shape)
              checkTyped True simplified
              for_ mb_tidy $ \tidy ->
                when (idName owner `elemNameSet` exported) $
                  checkTyped True tidy
            Nothing -> do
              early_u@(UntypedFamilyInputs _ arity _ _ _ _ _ _ _ _ _ _) <-
                MaybeT (pure (untypedFamilyInputs desug d_actual d_rhs))
              guard (tyConArity table == arity + 2)
              let checkUntyped check_rules binds = do
                    (actual, rhs) <- lookupStage owner check_rules helpers binds
                    shape <- MaybeT (pure (untypedFamilyInputs binds actual rhs))
                    guard (sameUntypedShapes early_u shape)
              checkUntyped True simplified
              for_ mb_tidy $ \tidy ->
                when (idName owner `elemNameSet` exported) $
                  checkUntyped True tidy
      | hasCompanionPrefix "$substMap:" companions = do
          guard (length companions == 1 && length expected == 1)
          (d_actual, d_rhs) <- lookupStage owner False helpers desug
          early_u@(UntypedFamilyInputs _ arity _ _ _ _ _ _ _ _ _ _) <-
            MaybeT (pure (untypedFamilyInputs desug d_actual d_rhs))
          guard (tyConArity table == arity + 2)
          let checkUntyped check_rules binds = do
                (actual, rhs) <- lookupStage owner check_rules helpers binds
                shape <- MaybeT (pure (untypedFamilyInputs binds actual rhs))
                guard (sameUntypedShapes early_u shape)
          checkUntyped True simplified
          for_ mb_tidy $ \tidy ->
            when (idName owner `elemNameSet` exported) $
              checkUntyped True tidy
      | otherwise = do
          guard (null companions || (length companions == 1 && hasCompanionPrefix "$weaken:" companions))
          (d_actual, d_rhs) <- lookupStage owner False helpers desug
          case indexedTraversal desug d_actual d_rhs of
            Just d_trav -> do
              checkTraversal expected table owner d_trav
              let checkTravStage binds = do
                    (actual, rhs) <- lookupStage owner True helpers binds
                    trav <- MaybeT (pure (indexedTraversal binds actual rhs))
                    checkTraversal expected table actual trav
              checkTravStage simplified
              for_ mb_tidy $ \tidy ->
                when (idName owner `elemNameSet` exported) $
                  checkTravStage tidy
            Nothing -> do
              (_, raw_rhs) <- lookupStage owner False helpers raw_desug
              checkOnlyCalls exported False (idName owner : helpers) raw_desug raw_rhs
              case checkedSubstForwardOrig d_actual d_rhs of
                Just orig_fwd@(SubstForwardSpec _ _ map_id _ _ _) -> do
                  let mb_map_spec = do
                        (_, mr) <- find (\(v, _) -> idName v == idName map_id) desug
                        checkedSubstMap desug map_id mr
                      checkFwdStage binds = do
                        (actual, raw_rhs0) <- lookupStage owner True helpers binds
                        let (mb_alias, rhs) = resolveForwardCseAlias binds actual raw_rhs0
                        for_ mb_alias (guard . noUserIdRules)
                        let extra_allowed = maybe [] (pure . idName) mb_alias
                        checkOnlyCalls exported True (idName actual : extra_allowed ++ helpers) binds rhs
                        guard (checkedSubstForwardFinal binds actual rhs orig_fwd mb_map_spec)
                  checkFwdStage simplified
                  for_ mb_tidy $ \tidy ->
                    when (idName owner `elemNameSet` exported) $
                      checkFwdStage tidy
                Nothing -> do
                  checkOperation exported False d_actual helpers desug d_rhs
                  (s_actual, s_rhs) <- lookupStage owner True helpers simplified
                  checkOperation exported True s_actual helpers simplified s_rhs
                  for_ mb_tidy $ \tidy ->
                    when (idName owner `elemNameSet` exported) $ do
                      (t_actual, t_rhs) <- lookupStage owner True helpers tidy
                      checkOperation exported True t_actual helpers tidy t_rhs

    checkLookup expected table owner check_rules binds = do
      (actual, rhs) <- lookupStage owner check_rules [] binds
      checked <- MaybeT (pure (indexedLookup binds actual rhs))
      rows <- lookupRows table checked
      guard (length rows == length expected)
      zipWithM_ sameIndexedRow expected (map (`applyRow` table) rows)

    checkTraversal expected table actual trav = do
      rows <- indexedEquations IndexedTraversal actual table Nothing trav
      guard (length rows == length expected)
      zipWithM_ sameIndexedRow expected [applyRow row table | (row, _) <- rows]

    checkOperation exported allow_floated actual helpers binds rhs = do
      let allowed = idName actual : helpers
      checkOnlyCalls exported allow_floated allowed binds rhs
      let ok = isJust (validWeakening actual rhs <|>
                       (() <$ checkedSubstMap binds actual rhs) <|>
                       (() <$ checkedSigmaComb actual rhs) <|>
                       validTypedOp binds actual rhs <|>
                       validUntypedOp binds actual rhs)
      guard ok

    validWeakening actual rhs =
      (() <$ checkedConstructorAlias actual rhs) <|> do
        (tvs, inputs, body) <- function rhs
        (con, values) <- constructorCall body
        guard (not (null tvs) && length inputs <= 1 && lazyConstructor con &&
               length inputs == length values)
        zipWithM_ (\i val -> case val of { Core.Var y -> guard (i == y); _ -> Nothing }) inputs values

    validTypedOp binds actual rhs =
      let shapes = directTypedOperationShapes binds (map fst binds)
      in case [ () | s <- shapes
                   , let (ren, ext) = renamingOperations s
                         (sub, sext, mb_w) = substitutionOperations s
                   , idName actual `elem` ([idName ren, idName ext, idName sub, idName sext] ++
                                           maybe [] (pure . idName) mb_w) ] of
           _ : _ -> Just ()
           [] -> do
             (_, arg, exp_body) <- extensionBinders rhs
             (idx, _) <- splitTyConApp_maybe (varType arg)
             [c1, c2] <- pure (tyConDataCons idx)
             (res_tc, _) <- splitTyConApp_maybe (exprType exp_body)
             let check_sext vc = () <$ (checkedExtension binds vc c1 c2 rhs <|>
                                        checkedExtension binds vc c2 c1 rhs)
             checkRenameExtension c1 c2 rhs <|>
               checkRenameExtension c2 c1 rhs <|>
               foldr (<|>) Nothing (map check_sext (tyConDataCons res_tc))

    validUntypedOp binds actual rhs =
      let shapes = directUntypedOperationShapes binds (map fst binds)
      in case [ () | s <- shapes
                   , let (ren, ext) = untypedRenamingOperations s
                         mb_sub = untypedSubstitutionOperations s
                         sub_names = case mb_sub of
                           Just (sub, sext, mb_w) ->
                             [idName sub, idName sext] ++ maybe [] (pure . idName) mb_w
                           Nothing -> []
                   , idName actual `elem` ([idName ren, idName ext] ++ sub_names) ] of
           _ : _ -> Just ()
           [] -> do
             ([_, _], [_, arg], _) <- function rhs
             (idx, [_]) <- splitTyConApp_maybe (varType arg)
             [c1, c2] <- pure (tyConDataCons idx)
             (res_tc, [_]) <- splitTyConApp_maybe (snd3 (splitFunTys (snd (splitForAllTyVars (varType actual)))))
             let check_sext vc = () <$ (untypedCheckedExtension binds vc c1 c2 rhs <|>
                                        untypedCheckedExtension binds vc c2 c1 rhs)
             checkUntypedRenameExtension binds c1 c2 rhs <|>
               checkUntypedRenameExtension binds c2 c1 rhs <|>
               foldr (<|>) Nothing (map check_sext (tyConDataCons res_tc))
      where
        snd3 (_, _, c) = c

-- | Allocate the family and check each nominal equation.
makeIndexedLookupTable :: Bool -> (TyCon -> Bool) -> Id -> IndexedLookup -> Maybe IndexedLookup
                       -> MaybeT TcM TyCon
makeIndexedLookupTable is_early public owner original final = do
    let IndexedLookup _ parameters domains result _ = original
        (flags, _) = splitForAllForAllTyBinders (varType owner)
    guard (length flags == length parameters)
    name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (mkClosureTableAxiom is_early name)
    operands <- lift (traverse (freshVar "operand") domains)
    let binders = zipWith (\(_, Bndr _ flag) -> mkNamedTyConBinder flag) flags parameters ++
                  map mkAnonTyConBinder operands
        make = mkFamily name axiom binders result
        header = make []
    startup <- lookupRows header original
    for_ final $ \optimized -> do
      rows <- lookupRows header optimized
      zipWithM_ sameIndexedRow (map (`applyRow` header) startup) (map (`applyRow` header) rows)
    let table = make (map (`applyRow` table) startup)
    guard (all public (sourceEquationDatatypes [table]))
    pure table

-- | Specialize the nominal scope equalities and read both exact rows.
lookupRows :: TyCon -> IndexedLookup -> MaybeT TcM [IndexedEquation]
lookupRows header (IndexedLookup owner parameters domains result rows) =
    zipWithM row [False, True] rows
  where
    row recursive (outer, IndexedRow inner proofs body) = do
      let equalities = map CoVarCo proofs
      subst <- MaybeT $ pure $ tcUnifyTys alwaysBindTv
          (map coercionLKind equalities) (map coercionRKind equalities)
      let specialize = substTyUnchecked subst
      (left, left_fields) <- indexedPatternTypes specialize outer
      (right, right_fields) <- indexedPatternTypes specialize inner
      let lhs = map (specialize . mkTyVarTy) parameters ++ [left, right]
          vars = tyCoVarsOfTypesWellScoped lhs
          field values (Core.Cast expr co)
            | caseCoercion proofs co
            , specialize (coercionLKind co) `eqType` specialize (coercionRKind co) = field values expr
          field values (Core.Var v) = lookup v values
          field _ _ = Nothing
          readResult False = const <$> field left_fields body
          readResult True = do
            (Core.Var fn, args) <- pure (Core.collectArgs body)
            guard (fn == owner)
            let (prefix, suffix) = splitAt (length parameters) args
            tys <- traverse (fmap specialize . indexedTypeArg) prefix
            [x, y] <- pure suffix
            l <- field left_fields x
            r <- field right_fields y
            guard (length tys == length parameters &&
                   eqTypes (map typeKind [l, r]) (map (substTyWith parameters tys) domains))
            pure (\family -> mkTyConApp family (tys ++ [l, r]))
      rhs <- MaybeT (pure (readResult recursive))
      let branch = IndexedEquation vars lhs rhs
      guard (all isTyVar vars && typeKind (rhs header) `eqType` specialize result &&
             validBranch (applyRow branch header))
      pure branch

-- | Read constructors, bound fields and a structurally smaller self call.
indexedResult :: [(Id, CoreExpr)] -> Id -> Maybe Id -> [TyVar] -> Type -> TyCon -> (Type -> Type)
              -> [Var] -> IndexedPattern -> [(Id, Type)] -> CoreExpr -> Maybe (TyCon -> Type)
indexedResult bindings owner worker parameters domain header specialize proofs pat fields =
    result bindings False
  where
    result floats underneath (Core.Cast body co) = do
      guard (caseCoercion proofs co &&
             specialize (coercionLKind co) `eqType` specialize (coercionRKind co))
      result floats underneath body
    result floats underneath expr = do
      let (fun, args) = Core.collectArgs expr
      Core.Var callee <- pure fun
      if callee == owner || Just callee == worker
        then recursive underneath callee args
        else field callee args <|> constructor floats expr callee args <|> float floats underneath callee expr

    field callee [] = const <$> lookup callee fields
    field _ _ = Nothing

    constructor floats expr callee args = do
      (con, supplied, values) <- indexedConstructor specialize callee args
      guard (null bindings ||
             all (not . DC.isBanged) (DC.dataConImplBangs con) &&
             all (caseCoercion proofs) [co | Core.Coercion co <- args])
      children <- traverse (result floats True) values
      _ <- checkedPromoted con (specialize (exprType expr)) supplied (map ($ header) children)
      pure (\family -> mkTyConApp (DC.promoteDataCon con) (supplied ++ map ($ family) children))

    float floats underneath callee expr = do
      guard (isJust (lookup callee floats))
      result [] underneath (expandTypeHelper floats expr)

    recursive underneath callee args = do
      guard underneath
      IndexedConstructor _ package _ original <- pure pat
      let (prefix, suffix) = splitAt (length parameters) args
      tys <- traverse (fmap specialize . indexedTypeArg) prefix
      guard (length tys == length parameters)
      let kind = substTyWith parameters tys domain
          n_exs = length (DC.dataConExTyCoVars package)
      (supplied, actual_fields, worker_types) <-
        if callee == owner
          then do
            [operand] <- pure suffix
            guard (specialize (exprType operand) `eqType` kind)
            (Core.Var builder, operands) <- pure (Core.collectArgs operand)
            (con, supplied, values) <- indexedConstructor specialize builder operands
            guard (con == package)
            pure (Just supplied, values, [])
          else do
            let (locals, values) = splitAt n_exs suffix
            erased <- traverse (fmap specialize . indexedTypeArg) locals
            guard (length erased == n_exs)
            pure (Nothing, values, erased)
      actual <- traverse get_value actual_fields
      guard (oneSmaller original actual)
      values <- traverse (\v -> lookup v fields) actual
      source <- case supplied of
        Just written -> pure written
        Nothing -> do
          (_, universal_args) <- splitTyConApp_maybe kind
          let universals = DC.dataConUnivTyVars package
              variables = universals ++ DC.dataConExTyCoVars package
          guard (length universal_args == length universals)
          traverse (`lookup` zip variables (universal_args ++ worker_types)) (DC.dataConUserTyVars package)
      packed <- checkedPromoted package kind source values
      pure (\family -> mkTyConApp family (tys ++ [packed]))

    get_value (Core.Var v) = Just v
    get_value _ = Nothing

    oneSmaller original values
      | length original == length values
      , Just changes <- zipWithM compare_field original values
      = length (filter id changes) == 1
      | otherwise = False

    compare_field (IndexedVariable old) new = False <$ guard (old == new)
    compare_field tree@IndexedConstructor{} new = True <$ guard (new `elem` indexedFields tree)

indexedConstructor :: (Type -> Type) -> Id -> [CoreExpr]
                   -> Maybe (DC.DataCon, [Type], [CoreExpr])
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

sameShapes :: FamilyInputs -> FamilyInputs -> Bool
sameShapes (FamilyInputs _ arity g a term ext index _ var hit miss rows)
           (FamilyInputs _ arity' g' a' term' ext' index' _ var' hit' miss' rows') =
    arity == arity' && g `eqType` g' && a `eqType` a' && term == term' && ext == ext' && index == index' &&
    var == var' && hit == hit' && miss == miss' && rows == rows'

renamingOperations :: FamilyInputs -> (Id, Id)
renamingOperations (FamilyInputs _ _ _ _ _ _ _ (ClosureOperations operations _) _ _ _ _) = operations

substitutionOperations :: FamilyInputs -> (Id, Id, Maybe Id)
substitutionOperations (FamilyInputs _ _ _ _ _ _ _ (ClosureOperations _ operations) _ _ _ _) = operations

familyInputs :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe FamilyInputs
familyInputs bindings owner rhs = do
    (context : types, body : captures, raw_call) <- function rhs
    let call = unwrapLocalCallbackLets raw_call
    result : rev_sorts <- pure (reverse types)
    guard (not (null captures) && length captures == length rev_sorts)
    (Core.Var traversal, [raw_callback, Core.Var operand]) <- application call
    guard (operand == body)
    ([callback_sort], [argument], lookup_body) <- function (betaReduceTypeApps raw_callback)
    let sorts = reverse rev_sorts
        context_kind = varType context
        sort_kind = varType result
    (context_tycon, []) <- splitTyConApp_maybe context_kind
    (sort_tycon, []) <- splitTyConApp_maybe sort_kind
    guard (all isAlgTyCon [context_tycon, sort_tycon])
    guard (all (eqType sort_kind . varType) (callback_sort : sorts))
    (term, [body_ctx, TyVarTy body_sort]) <- splitTyConApp_maybe (varType body)
    (ext, [_, _]) <- splitTyConApp_maybe body_ctx
    (index, [index_ctx, TyVarTy index_sort]) <- splitTyConApp_maybe (varType argument)
    let context_ty = mkTyVarTy context
        extend k a = mkTyConApp ext [k, mkTyVarTy a]
        at a = mkTyConApp term [context_ty, mkTyVarTy a]
    guard (isPromotedDataCon ext && all isAlgTyCon [term, index])
    guard (foldl' extend context_ty sorts `eqType` body_ctx && body_sort == result &&
           and (zipWith (\capture sort -> varType capture `eqType` at sort) captures sorts) &&
           index_ctx `eqType` body_ctx && index_sort == callback_sort)
    case_rows <- caseOn argument lookup_body
    [(first, _, _), (second, _, _)] <- pure case_rows
    let index_cons = tyConDataCons index
    guard (first /= second && length index_cons == 2 &&
           first `elem` index_cons && second `elem` index_cons &&
           all lazyConstructor index_cons)
    (hit, miss, var) <-
        outcomes first second [] argument (reverse captures) lookup_body <|>
        outcomes second first [] argument (reverse captures) lookup_body
    guard (DC.dataConTyCon var == term)
    traversal_rhs <- lookup traversal bindings
    ([_, _, _], [sigma, scrutinee], traversal_case) <- function traversal_rhs
    rows <- caseOn scrutinee traversal_case
    [(var_bs, var_rhs)] <- pure [(bs, e) | (con, bs, e) <- rows, con == var]
    [field] <- pure (filter isValueVar var_bs)
    checked <- branchBody var_bs var_rhs
    (Core.Var use, [Core.Var x]) <- application checked
    guard (use == sigma && x == field)
    (operations, recipes) <- checkedTraversals bindings traversal sigma var hit miss rows
    pure (FamilyInputs owner (length captures) context_kind sort_kind term ext index operations var hit miss recipes)

-- Check both case traversals before emitting any nominal equations.
-- The recipe records the number of binder extensions for each field.
checkedTraversals :: [(Id, CoreExpr)]
                  -> Id -> Id
                  -> DC.DataCon -> DC.DataCon -> DC.DataCon
                  -> [(DC.DataCon, [Var], CoreExpr)]
                  -> Maybe (ClosureOperations, [(DC.DataCon, [Int])])
checkedTraversals bindings subst sigma var hit miss rows = do
    let constructors = tyConDataCons (DC.dataConTyCon var)
    guard (complete constructors rows)
    lifted <- traverse (checkedRow bindings var hit miss subst sigma) [row | row@(con, _, _) <- rows, con /= var]
    let (recipes, used_extenders) = unzip lifted
        subst_steps = concatMap concat used_extenders
    extender <- allSame [v | NamedLift v <- subst_steps]
    extender_rhs <- lookup extender bindings
    (rename, weakening) <- checkedExtension bindings var hit miss extender_rhs
    guard (all (isSubstLift rename) subst_steps)
    rename_rhs <- lookup rename bindings
    (tvs, [rho, operand], renaming) <- function rename_rhs
    guard (length tvs == 3)
    rename_rows <- caseOn operand renaming
    guard (complete constructors rename_rows)
    [(var_binders, var_rhs)] <- pure [(bs, rhs) | (con, bs, rhs) <- rename_rows, con == var]
    [original] <- pure (filter isValueVar var_binders)
    var_result <- branchBody var_binders var_rhs
    (var_head, [renamed]) <- constructorCall var_result
    guard (var_head == var)
    (Core.Var callback, [Core.Var argument]) <- application renamed
    guard (callback == rho && argument == original)
    renamed_rows <- traverse (checkedRow bindings var hit miss rename rho)
                             [row | row@(con, _, _) <- rename_rows, con /= var]
    let (rename_recipes, rename_extenders) = unzip renamed_rows
    guard (sortOn (nameStableString . DC.dataConName . fst) recipes == sortOn (nameStableString . DC.dataConName . fst) rename_recipes)
    let rename_steps = concatMap concat rename_extenders
    guard (all isRenameLift rename_steps)
    extend_rename <- allSame [v | NamedLift v <- rename_steps]
    extend_rename_rhs <- lookup extend_rename bindings
    checkRenameExtension hit miss extend_rename_rhs
    pure (ClosureOperations (rename, extend_rename) (subst, extender, weakening), recipes)
  where
    complete constructors alts =
      let heads = [con | (con, _, _) <- alts]
      in length heads == length constructors && all (`elem` heads) constructors &&
         all lazyConstructor constructors

    isSubstLift _ (NamedLift _) = True
    isSubstLift rename (InlinedSubst v) = rename == v
    isSubstLift _ InlinedRename = False

    isRenameLift (NamedLift _) = True
    isRenameLift InlinedRename = True
    isRenameLift (InlinedSubst _) = False

    allSame [] = Nothing
    allSame (v : vs) = v <$ guard (all (== v) vs)

-- Check every recursive call on exactly the original corresponding
-- field. The result keeps the same head and lazy field order.
checkedRow :: [(Id, CoreExpr)] -> DC.DataCon -> DC.DataCon -> DC.DataCon -> Id -> Id
           -> (DC.DataCon, [Var], CoreExpr)
           -> Maybe ((DC.DataCon, [Int]), [[CallbackLift]])
checkedRow bindings var hit miss traversal callback (con, binders, rhs) = do
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

    lifts (Core.Var original)
        | callback == original = Just []
    lifts captured = named captured <|> inlined captured

    named captured = eta_lifted <|> direct_lifted
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

    -- Simplification can inline one extension around the remaining callback.
    -- Check its constructors and each operand before stripping the lookup.
    inlined captured = do
        ([index], [argument], body) <- function captured
        rows <- caseOn argument body
        [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
        [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
        guard (length rows == 2 && all (not . isValueVar) hit_bs)
        fresh <- expandTypeHelper bindings <$> branchBody hit_bs hit_rhs
        forwarded <- branchBody miss_bs miss_rhs
        (lift_step, see_older) <- inlineStep fresh forwarded
        [older] <- pure (filter isValueVar miss_bs)
        let (head_expr, args) = Core.collectArgs see_older
        arg : preceding <- pure (reverse args)
        Core.Var input <- branchBody miss_bs arg
        guard (input == older)
        (lift_step :) <$> lifts (Core.Lam index (foldl' Core.App head_expr (reverse preceding)))

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
            _ <- checkedWeakening bindings miss weakening
            pure (InlinedSubst rename, lookup_older)
          _ -> Nothing

-- Check the function used when a renaming crosses a binder.
checkRenameExtension :: DC.DataCon -> DC.DataCon -> CoreExpr -> Maybe ()
checkRenameExtension hit miss rhs = do
    (capture, argument, body) <- extensionBinders rhs
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs &&
           lazyConstructor hit && lazyConstructor miss)
    hit_body <- branchBody hit_bs hit_rhs
    (fresh, []) <- constructorCall hit_body
    guard (fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (successor, [lookup_older]) <- constructorCall miss_body
    guard (successor == miss)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)

lazyConstructor :: DC.DataCon -> Bool
lazyConstructor con =
    not (any DC.isBanged (DC.dataConImplBangs con)) &&
    null (DC.dataConRepArgBinders con) &&
    null (DC.dataConStupidTheta con)

reachable :: [(Id, CoreExpr)] -> Id -> [Name]
reachable bindings root = map idName (walk [] [root])
  where
    local = mkVarSet (map fst bindings)
    walk seen [] = seen
    walk seen (v:rest)
      | v `elem` seen = walk seen rest
      | Just body <- lookup v bindings =
          walk (v:seen) (exprSomeFreeVarsList (\x -> x `elemVarSet` local) body ++ rest)
      | otherwise = walk seen rest

-- A final simplifier can lift a closed constructor-only expression
-- into a local type abstraction. Check at most that one definition.
expandTypeHelper :: [(Id, CoreExpr)] -> CoreExpr -> CoreExpr
expandTypeHelper bindings rhs
  | (Core.Var helper, args) <- Core.collectArgs rhs
  , Just declaration <- lookup helper bindings
  , Just (tvs, [], body) <- function declaration
  , Just tys <- traverse typeArg args
  , length tvs == length tys
  , let in_scope = mkInScopeSet (foldl' unionVarSet (exprFreeVars body)
                                    (map (exprFreeVars . Core.Type) tys))
        substitution = foldl' (\sub (tv, ty) -> extendTvSubst sub tv ty)
                             (mkEmptySubst in_scope) (zip tvs tys)
        expanded = substExpr substitution body
  , exprType expanded `eqType` exprType rhs
  = expanded
  | otherwise = rhs
  where
    typeArg (Core.Type ty) = Just ty
    typeArg _ = Nothing

-- | Check that `co` only widens multiplicity (`%1` to `%Many`), relaxes
-- matchability (`@M` to `@U`), or applies reflexive casts.
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
    isTyVar tv && eqForAllVis l r &&
    (coercionLKind ma `eqType` coercionRKind ma ||
     (coercionLKind ma `eqType` matchableDataConTy && coercionRKind ma `eqType` unmatchableDataConTy)) &&
    isSafeMultOrReflCo body
isSafeMultOrReflCo FunCo{ fco_afl = FTF_T_T, fco_afr = FTF_T_T
                         , fco_ma = cm, fco_mult = cw, fco_arg = c1, fco_res = c2 } =
    (coercionLKind cm `eqType` coercionRKind cm ||
     (coercionLKind cm `eqType` matchableDataConTy && coercionRKind cm `eqType` unmatchableDataConTy)) &&
    ((coercionLKind cw `eqType` coercionRKind cw) ||
     (isOneTy (coercionLKind cw) && isManyTy (coercionRKind cw))) &&
    coercionLKind c1 `eqType` coercionRKind c1 &&
    coercionLKind c2 `eqType` coercionRKind c2
isSafeMultOrReflCo _ = False

weakeningBody :: CoreExpr -> Maybe CoreExpr
weakeningBody (Core.Cast body co) = do
    guard (isSafeMultOrReflCo co)
    weakeningBody body
weakeningBody body = Just body

-- | Read a constructor callback or a named function that forwards to it.
checkedWeakening :: [(Id, CoreExpr)] -> DC.DataCon -> CoreExpr -> Maybe (Maybe Id)
checkedWeakening bindings miss rhs = do
    unwrapped <- weakeningBody rhs
    let direct = do
          (tvs, inputs, body) <- function unwrapped
          (shift, values) <- constructorCall body
          guard (length tvs <= 1 && length inputs <= 1 && shift == miss &&
                 length inputs == length values)
          zipWithM_ (\input value -> case value of
                       Core.Var actual -> guard (actual == input)
                       _ -> Nothing) inputs values
          pure Nothing
        named = do
          (tvs, [], partial) <- function unwrapped
          (Core.Var owner, types) <- pure (Core.collectArgs partial)
          guard (length tvs <= 1 && length types == 2 + length tvs && all Core.isTypeArg types)
          definition <- lookup owner bindings
          con <- validConstructorAliasBody definition
          let (erased, _) = splitForAllTyVars (varType owner)
          guard (length erased == 3 && con == miss)
          pure (Just owner)
    direct <|> named

extensionBinders :: CoreExpr -> Maybe (Id, Id, CoreExpr)
extensionBinders rhs = do
    let (binders, body) = collectExtensionBinders [] rhs
    case binders of
      [t1, t2, t3, capture, t4, argument]
        | all isTyVar [t1, t2, t3, t4] && all isValueVar [capture, argument]
        -> pure (capture, argument, body)
      [t1, t2, t3, t4, capture, argument]
        | all isTyVar [t1, t2, t3, t4] && all isValueVar [capture, argument]
        -> pure (capture, argument, body)
      _ -> Nothing
  where
    collectExtensionBinders acc (Core.Lam b e) = collectExtensionBinders (b : acc) e
    collectExtensionBinders acc (Core.Let (Core.NonRec b _) e)
      | isLocalId b
      , isLiftedTypeKind (typeKind (varType b))
      , not (b `elemVarSet` exprFreeVars e)
      = collectExtensionBinders acc e
    collectExtensionBinders acc e = (reverse acc, e)

-- | Inline non-recursive local callback bindings in ordinary non-dependent Core.
-- Leave existential and native dependent expressions untouched so @substTy@
-- never substitutes an uncertified value into a retained type index.
unwrapLocalCallbackLets :: CoreExpr -> CoreExpr
unwrapLocalCallbackLets expr
  | hasExistentialExpr expr || hasNativeDependentExpr expr
  = expr
unwrapLocalCallbackLets (Core.Let (Core.NonRec b b_rhs) inner)
  | isLocalId b, isValueVar b
  , VanillaId <- idDetails b
  , null (idCoreRules b)
  , not (b `elemVarSet` exprFreeVars b_rhs)
  = let free = exprFreeVars inner `unionVarSet` exprFreeVars b_rhs
        sub = mkOpenSubst (mkInScopeSet free) [(b, b_rhs)]
    in unwrapLocalCallbackLets (substExpr sub inner)
unwrapLocalCallbackLets (Core.Lam b body) = Core.Lam b (unwrapLocalCallbackLets body)
unwrapLocalCallbackLets (Core.App f a) = Core.App (unwrapLocalCallbackLets f) (unwrapLocalCallbackLets a)
unwrapLocalCallbackLets (Core.Cast body co) = Core.Cast (unwrapLocalCallbackLets body) co
unwrapLocalCallbackLets expr = expr

betaReduceTypeApps :: CoreExpr -> CoreExpr
betaReduceTypeApps expr
  | hasExistentialExpr expr || hasNativeDependentExpr expr
  = expr
  | (Core.Lam b body, Core.Type ty : rest) <- collect [] (unwrapLocalCallbackLets expr)
  , isTyVar b
  = let free = exprFreeVars body `unionVarSet` exprFreeVars (Core.Type ty)
        sub = extendTvSubst (mkEmptySubst (mkInScopeSet free)) b ty
    in betaReduceTypeApps (foldl' Core.App (substExpr sub body) rest)
  | (Core.Lam b body) <- unwrapLocalCallbackLets expr
  = Core.Lam b (betaReduceTypeApps body)
  | (Core.Cast body co) <- unwrapLocalCallbackLets expr
  = Core.Cast (betaReduceTypeApps body) co
  | otherwise = unwrapLocalCallbackLets expr
  where
    collect args (Core.App f a) = collect (a : args) f
    collect args f = (f, args)

checkedExtension :: [(Id, CoreExpr)]
                 -> DC.DataCon -> DC.DataCon -> DC.DataCon
                 -> CoreExpr -> Maybe (Id, Maybe Id)
checkedExtension bindings var hit miss rhs = do
    (capture, argument, body) <- extensionBinders rhs
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs)
    hit_body <- branchBody hit_bs hit_rhs
    (term, [v]) <- constructorCall (expandTypeHelper bindings hit_body)
    (fresh, []) <- constructorCall v
    guard (term == var && fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (Core.Var rename, [weakening, lookup_older]) <- application miss_body
    _ <- lookup rename bindings
    chosen <- checkedWeakening bindings miss weakening
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)
    pure (rename, chosen)

constructorCall :: CoreExpr -> Maybe (DC.DataCon, [CoreExpr])
constructorCall rhs = do
    unwrapped <- weakeningBody rhs
    (head_expr, fields) <- application unwrapped
    Core.Var make <- weakeningBody head_expr
    con <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
    pure (con, fields)

-- Check one lookup case per replacement. Only case evidence can
-- relate the predecessor lookup to the declared outer context.
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

function :: CoreExpr -> Maybe ([TyVar], [Id], CoreExpr)
function expr = do
    let (binders, body) = collect [] expr
        (ty_binders, tm_binders) = span isTyVar binders
    guard (all isValueVar tm_binders)
    pure (ty_binders, tm_binders, body)
  where
    -- See Note [Certifying a checked source closure].
    collect binders (Core.Lam binder body) = collect (binder:binders) body
    collect binders (Core.Let (Core.NonRec binder _) body)
      | isLocalId binder
      , isLiftedTypeKind (typeKind (varType binder))
      , not (binder `elemVarSet` exprFreeVars body)
      = collect binders body
    collect binders body = (reverse binders, body)

application :: CoreExpr -> Maybe (CoreExpr, [CoreExpr])
application expr = do
    let (head_expr, args) = Core.collectArgs expr
        (_, values) = span Core.isTyCoArg args
    guard (all (not . Core.isTyCoArg) values)
    pure (head_expr, values)

caseOn :: Id -> CoreExpr
       -> Maybe [(DC.DataCon, [Var], CoreExpr)]
caseOn target expr = do
    Core.Case (Core.Var scrutinee) _ _ alts <- pure expr
    guard (target == scrutinee)
    traverse dataAlt alts
  where
    dataAlt (Core.Alt (Core.DataAlt con) bs rhs) = Just (con, bs, rhs)
    dataAlt _ = Nothing

branchBody :: [Var] -> CoreExpr -> Maybe CoreExpr
branchBody binders expr = case expr of
    Core.Cast body co
      | caseCoercion (filter isCoVar binders) co -> branchBody binders body
      | otherwise -> Nothing
    body -> Just body

caseCoercion :: [Var] -> Coercion -> Bool
caseCoercion scope co = case co of
    Refl _ -> True
    GRefl _ _ MRefl -> True
    CoVarCo cv -> cv `elem` scope
    TyConAppCo _ _ cs -> all (caseCoercion scope) cs
    TransCo a b -> caseCoercion scope a && caseCoercion scope b
    SubCo c -> caseCoercion scope c
    SymCo c -> caseCoercion scope c
    SelCo _ c -> caseCoercion scope c
    _ -> False

isValueVar :: Id -> Bool
isValueVar v = isId v && not (isCoVar v)

{- Note [Renaming with an abstract callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The checked substitution traversal also verifies the renaming and
binder extension operations. Give these operations their own equations
when the provider exports both. The variable row keeps the rank-n
callback application below a lazy constructor. A binder row partially
applies the same checked extension that the client can name. Reduction
stops at an unknown lookup or term.

Check the extension before SimpleOpt and after optimization. Its only
free term Ids can be constructor workers or wrappers. The callback is
bound by the extension. User rules cannot target these constructors,
so the extension table has an empty helper list and checks its owner.

Use the original unmatchable callback kind. A runtime renaming can
map two distinct lookups to the same value. This certificate gives no
injectivity for that application.
-}

-- | Emit the binder extension once, even if several traversals use it.
publishRenameExtension :: Bool -> (TyCon -> Bool) -> (Id, (Id, FamilyInputs), [Id])
                       -> TcM (Maybe (Id, [TyCon]))
publishRenameExtension is_early public (_, (owner, shape), _) =
    publishLookupExtension is_early public owner [] shape Nothing

{- Note [Substitution with an abstract callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At a fresh lookup, an extension constructs the corresponding variable.
At an older lookup, it applies the callback and renames the resulting
term with a checked successor function. Use the same named successor
in the family equation and the proof that renames the callback result.
Its unmatchable rank-n kind comes from the ordinary function type.

When the provider also exports the checked substitution traversal, give
it equations over the same rank-n callback kind. The variable row
applies the callback directly, and each binder row lifts the callback
through the checked substitution extension.

Check both stages of each function. Preserve all reachable external
helpers (including any floated closed constructor-only type helper that
Tidy externalizes for unfolding serialization) as rule dependencies.
A successor is a lazy constructor applied to its sole input. Extra calls
in the original or final extension, traversal, or successor withhold the
equations; final Core may reference non-exported closed constructor-only
type abstractions floated out by the simplifier.
-}

-- | Publish a named weakening callback with the kind of its runtime owner.
publishWeakening :: Bool -> (TyCon -> Bool) -> (Id, FamilyInputs) -> TcM (Maybe (Id, [TyCon]))
publishWeakening is_early public (owner, FamilyInputs _ _ context sort _ ext index _ _ _ miss _) = runMaybeT $ do
    g <- lift (freshVar "g" context)
    b <- lift (freshVar "b" sort)
    a <- lift (freshVar "a" sort)
    let gk = mkTyVarTy g
        bk = mkTyVarTy b
        ak = mkTyVarTy a
        input = mkTyConApp index [gk,ak]
        result = mkTyConApp index [mkTyConApp ext [gk,bk],ak]
    x <- lift (freshVar "x" input)
    let binders = mkNamedTyConBinders Specified [g,b,a] ++ [mkAnonTyConBinder x]
        branches = do
          output <- conAt miss result [mkTyVarTy x]
          pure [renamingBranch [gk,bk,ak,mkTyVarTy x] output]
    publishRenamingTable is_early public owner [] binders result (const branches)

-- | Use the same checked renamed result that a client sees in the type.
publishSubstitutionExtension :: Bool -> (TyCon -> Bool) -> [(Name, TyCon)] -> [(Name, TyCon)]
                             -> (Id, (Id, Id, FamilyInputs), [Id]) -> TcM (Maybe (Id, [TyCon]))
publishSubstitutionExtension is_early public renamings weakenings (owner, (rename, weaken, shape), helpers)
  | Just renaming <- lookup (idName rename) renamings
  , Just weakening <- lookup (idName weaken) weakenings
  = publishLookupExtension is_early public owner helpers shape (Just (renaming,weakening))
  | otherwise = pure Nothing

publishLookupExtension :: Bool -> (TyCon -> Bool) -> Id -> [Id] -> FamilyInputs
                       -> Maybe (TyCon, TyCon) -> TcM (Maybe (Id, [TyCon]))
publishLookupExtension is_early public owner helpers shape substitution = runMaybeT $ do
    let FamilyInputs _ _ context sort term ext index _ var hit miss _ = shape
        output = case substitution of
          Nothing -> index
          Just _ -> term
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    b <- lift (freshVar "binder" sort)
    a <- lift (freshVar "a" sort)
    rank <- lift (freshVar "sort" sort)
    let gk = mkTyVarTy g
        dk = mkTyVarTy d
        bk = mkTyVarTy b
        ak = mkTyVarTy a
        lookupAt c s = mkTyConApp index [c,s]
        outputAt c s = mkTyConApp output [c,s]
        extend c = mkTyConApp ext [c,bk]
        domain = lookupAt (extend gk) ak
        result = outputAt (extend dk) ak
    rho <- lift (freshVar "rho" (closureCallbackKind index output rank gk dk))
    input <- lift (freshVar "input" domain)
    old <- lift (freshVar "index" (lookupAt gk ak))
    let r = mkTyVarTy rho
        x = mkTyVarTy old
        binders = mkNamedTyConBinders Specified [g,d,b] ++
                  [mkAnonTyConBinder rho, mkNamedTyConBinder Specified a, mkAnonTyConBinder input]
        branches = do
          fresh <- conAt hit (lookupAt (extend gk) bk) []
          new_index <- conAt hit (lookupAt (extend dk) bk) []
          previous <- conAt miss domain [x]
          let mapped = mkAppTys r [ak,x]
          (fresh', previous') <- case substitution of
            Nothing -> (new_index,) <$> conAt miss result [mapped]
            Just (rename,weakening) -> do
              v <- conAt var (outputAt (extend dk) bk) [new_index]
              let shift = mkTyConApp weakening [dk,bk]
              guard (typeKind shift `eqType` renamingKind index rank dk (extend dk))
              pure (v, mkTyConApp rename [dk,extend dk,ak,shift,mapped])
          guard (typeKind fresh' `eqType` outputAt (extend dk) bk && typeKind previous' `eqType` result)
          pure [renamingBranch [gk,dk,bk,r,bk,fresh] fresh',
                renamingBranch [gk,dk,bk,r,ak,previous] previous']
    publishRenamingTable is_early public owner helpers binders result (const branches)

publishRenaming :: Bool -> (TyCon -> Bool) -> [(Name, TyCon)] -> (Id, (Id, FamilyInputs), [Id])
                -> TcM (Maybe (Id, [TyCon]))
publishRenaming = publishClosureTraversal False

publishSubstitution :: Bool -> (TyCon -> Bool) -> [(Name, TyCon)] -> (Id, (Id, FamilyInputs), [Id])
                    -> TcM (Maybe (Id, [TyCon]))
publishSubstitution = publishClosureTraversal True

-- | A traversal can extend its callback only at each field's own binders.
publishClosureTraversal :: Bool -> Bool -> (TyCon -> Bool) -> [(Name, TyCon)]
                        -> (Id, (Id, FamilyInputs), [Id]) -> TcM (Maybe (Id, [TyCon]))
publishClosureTraversal is_subst is_early public extensions (owner, (extension, shape), helpers) = runMaybeT $ do
    extend <- MaybeT (pure (lookup (idName extension) extensions))
    let FamilyInputs _ _ context sort term ext index _ var _ _ recipes = shape
        output = if is_subst then term else index
        callbackKind = closureCallbackKind index output
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    a <- lift (freshVar "a" sort)
    rank <- lift (freshVar "sort" sort)
    let gk = mkTyVarTy g
        dk = mkTyVarTy d
        ak = mkTyVarTy a
        termAt c s = mkTyConApp term [c,s]
        result = termAt dk ak
    rho <- lift (freshVar "rho" (callbackKind rank gk dk))
    input <- lift (freshVar "input" (termAt gk ak))
    let binders = mkNamedTyConBinders Specified [g,d,a] ++ map mkAnonTyConBinder [rho,input]
        prepare (con, depths) = do
          let (con_vars, telescope) = splitForAllTyVars (tyConKind (DC.promoteDataCon con))
              (_, fields, codomain) = splitFunTys telescope
          (subst, vars) <- lift (freshenTyVarBndrs (map snd con_vars))
          let kind = substTyUnchecked subst codomain
          args <- lift (traverse (freshVar "field" . substTyUnchecked subst . scaledThing) fields)
          (tc, [source, sort_at]) <- MaybeT (pure (splitTyConApp_maybe kind))
          guard (tc == term)
          local <- lift (freshVar "rho" (callbackKind rank source dk))
          let r = mkTyVarTy local
              field_tys = mkTyVarTys args
              lhs = mkTyConApp (DC.promoteDataCon con) (mkTyVarTys vars ++ field_tys)
              liftField 0 ctx = (dk,r) <$ guard (ctx `eqType` source)
              liftField depth ctx = do
                (cl, [outer, introduced]) <- splitTyConApp_maybe ctx
                guard (cl == ext)
                (target, inherited) <- liftField (depth - 1) outer
                pure (mkTyConApp ext [target,introduced],
                      mkTyConApp extend [outer,target,introduced,inherited])
              rewrite family depth arg = do
                (t, [ctx, at]) <- splitTyConApp_maybe (typeKind arg)
                guard (t == term)
                (target, captured) <- liftField depth ctx
                guard (typeKind captured `eqType` callbackKind rank ctx target)
                pure (mkTyConApp family [ctx,target,at,captured,arg])
          pure $ \family -> do
            guard (typeKind lhs `eqType` kind)
            out <- if con == var
              then do [x] <- pure field_tys
                      guard (typeKind x `eqType` mkTyConApp index [source,sort_at])
                      let mapped = mkAppTys r [sort_at,x]
                      if is_subst
                        then mapped <$ guard (typeKind mapped `eqType` termAt dk sort_at)
                        else conAt con (termAt dk sort_at) [mapped]
              else zipWithM (rewrite family) depths field_tys >>= conAt con (termAt dk sort_at)
            pure (renamingBranch [source,dk,sort_at,r,lhs] out)
    rows <- traverse prepare ((var,[]) : recipes)
    publishRenamingTable is_early public owner helpers binders result (\family -> traverse ($ family) rows)

-- | Preserve the exact rank-n callback type when generating a table kind.
renamingKind :: TyCon -> TyVar -> Type -> Type -> Kind
renamingKind index = closureCallbackKind index index

closureCallbackKind :: TyCon -> TyCon -> TyVar -> Type -> Type -> Kind
closureCallbackKind index result rank g d =
    mkSpecForAllTys [(unmatchableDataConTy, rank)] $
      mkVisFunTyManyU (mkTyConApp index [g,mkTyVarTy rank]) (mkTyConApp result [d,mkTyVarTy rank])

renamingBranch :: [Type] -> Type -> CoAxBranch
renamingBranch lhs = mkBranch (tyCoVarsOfTypesWellScoped lhs) lhs

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

allocateFamilies :: Bool -> FamilyInputs -> TcM (Maybe (TyCon, [TyCon]))
allocateFamilies is_early (FamilyInputs owner arity context sort term ext lookup_tycon _ var hit miss recipes) = do
    (closure, constructors) <- allocateClosure owner context sort term ext
    case constructors of
      [replace, shift, lift, replace_next] -> do
        apply_name <- localName "$apply:"
        lookup_name <- localName "$lookup:"
        apply_axiom <- newImplicitBinder apply_name mkInstTyCoOcc
        lookup_axiom <- newImplicitBinder lookup_name mkInstTyCoOcc
        root_name <- newImplicitBinder (idName owner) mkSourceTableOcc
        root_axiom <- mkClosureTableAxiom is_early root_name
        g <- freshVar "g" context
        d <- freshVar "d" context
        a <- freshVar "a" sort
        b <- freshVar "b" sort
        let gk = mkTyVarTy g
            dk = mkTyVarTy d
            ak = mkTyVarTy a
            bk = mkTyVarTy b
            termAt p k = mkTyConApp term [p,k]
            ctxAt p k = mkTyConApp ext [p,k]
            lookupAt p k = mkTyConApp lookup_tycon [p,k]
            closureAt p q = mkTyConApp closure [p,q]
            ctx_g_a = ctxAt gk ak
            ctx_d_a = ctxAt dk ak
        cv <- freshVar "c" (closureAt gk dk)
        tree <- freshVar "t" (termAt gk bk)
        field <- freshVar "x" (lookupAt gk bk)
        carried <- freshVar "w" (termAt dk ak)
        capture_kinds <- traverse (\_ -> freshVar "captureKind" sort) [1..arity]
        let capture_sorts = mkTyVarTys capture_kinds
            body_context = foldl' ctxAt gk capture_sorts
        body <- freshVar "n" (termAt body_context bk)
        args <- traverse (freshVar "v" . termAt gk) capture_sorts
        x_d <- freshVar "older" (lookupAt dk bk)
        let prepare (ctor, exten) = do
              let (ctor_tvs, tel) = splitForAllTyVars (tyConKind (DC.promoteDataCon ctor))
                  (_, arg_kinds, res) = splitFunTys tel
              (renaming, tvs) <- freshenTyVarBndrs (map snd ctor_tvs)
              vars <- traverse (freshVar "arg" . substTyUnchecked renaming . scaledThing) arg_kinds
              let result = substTyUnchecked renaming res
              env <- freshVar "env" $ case splitTyConApp_maybe result of
                Just (_, [src, _]) -> closureAt src dk
                _ -> liftedTypeKind  -- Rejected by applyBranch.
              pure (ctor, exten, tvs, vars, result, env)
        apply_inputs <- traverse prepare ((var,[]) : recipes)
        let c = mkTyVarTy cv
            x = mkTyVarTy field
            w = mkTyVarTy carried
            outputs = termAt dk bk
            prefix = mkNamedTyConBinders Specified [g,d,b]
            apply_apply s t k env val = mkTyConApp apply [s,t,k,env,val]
            lookup_apply s t k env val = mkTyConApp lookup_fam [s,t,k,env,val]
            apply = mkFamily apply_name apply_axiom (prefix ++ map mkAnonTyConBinder [cv,tree])
                        outputs (fromMaybe [] apply_branches)
            lookup_fam = mkFamily lookup_name lookup_axiom (prefix ++ map mkAnonTyConBinder [cv,field])
                        outputs (fromMaybe [] lookup_branches)
            applyBranch (ctor, exten, tvs, vars, res, env_var) = do
                (actual_term, [src, sort_k]) <- splitTyConApp_maybe res
                guard (actual_term == term && (ctor == var || length exten == length vars))
                let fields = mkTyVarTys vars
                    lhs = mkTyConApp (DC.promoteDataCon ctor) (mkTyVarTys tvs ++ fields)
                    env = mkTyVarTy env_var
                    liftField 0 source = do
                      guard (source `eqType` src)
                      pure (dk, env)
                    liftField depth source = do
                      (scope, [outer, introduced]) <- splitTyConApp_maybe source
                      guard (scope == ext)
                      (target, inner) <- liftField (depth - 1) outer
                      let extended = ctxAt target introduced
                      lifted <- conAt lift (closureAt source extended) [inner]
                      pure (extended, lifted)
                    use depth value = do
                      (same, [source, k]) <- splitTyConApp_maybe (typeKind value)
                      guard (same == term)
                      (target, closure_value) <- liftField depth source
                      pure (apply_apply source target k closure_value value)
                out <- if ctor == var
                         then do [lookup_var] <- pure fields
                                 pure (lookup_apply src dk sort_k env lookup_var)
                         else zipWithM use exten fields >>= conAt ctor (termAt dk sort_k)
                guard (typeKind lhs `eqType` res && typeKind out `eqType` termAt dk sort_k)
                pure (mkBranch (tvs ++ [d,env_var] ++ vars) [src,dk,sort_k,env,lhs] out)
            apply_branches = traverse applyBranch apply_inputs
            lookup_branches = do
                r <- conAt replace (closureAt ctx_d_a dk) [w]
                z_in <- conAt hit (lookupAt ctx_d_a ak) []
                s_in <- conAt miss (lookupAt ctx_d_a bk) [mkTyVarTy x_d]
                v_d <- conAt var (termAt dk bk) [mkTyVarTy x_d]
                s <- conAt shift (closureAt gk ctx_g_a) []
                s_out <- conAt miss (lookupAt ctx_g_a bk) [x]
                v_out <- conAt var (termAt ctx_g_a bk) [s_out]
                l <- conAt lift (closureAt ctx_g_a ctx_d_a) [c]
                z_g <- conAt hit (lookupAt ctx_g_a ak) []
                z_d <- conAt hit (lookupAt ctx_d_a ak) []
                v0 <- conAt var (termAt ctx_d_a ak) [z_d]
                sx <- conAt miss (lookupAt ctx_g_a bk) [x]
                s_d <- conAt shift (closureAt dk ctx_d_a) []
                let weaken = apply_apply dk ctx_d_a bk s_d (lookup_apply gk dk bk c x)
                next <- conAt replace_next (closureAt ctx_g_a dk) [c,w]
                pure [ mkBranch [d,a,carried] [ctx_d_a,dk,ak,r,z_in] w
                     , mkBranch [d,a,b,carried,x_d] [ctx_d_a,dk,bk,r,s_in] v_d
                     , mkBranch [g,a,b,field] [gk,ctx_g_a,bk,s,x] v_out
                     , mkBranch [g,d,a,cv] [ctx_g_a,ctx_d_a,ak,l,z_g] v0
                     , mkBranch [g,d,a,b,cv,field] [ctx_g_a,ctx_d_a,bk,l,sx] weaken
                     , mkBranch [g,d,a,cv,carried] [ctx_g_a,dk,ak,next,z_g] w
                     , mkBranch [g,d,a,b,cv,carried,field] [ctx_g_a,dk,bk,next,sx]
                         (lookup_apply gk dk bk c x) ]
        pure $ do
            apply_equations <- apply_branches
            lookup_equations <- lookup_branches
            (first_sort, first_arg) : rest <- pure (zip capture_sorts (mkTyVarTys args))
            let initial_context = ctxAt gk first_sort
                addReplacement (src, env) (k, value) = do
                    let next_context = ctxAt src k
                    next <- conAt replace_next (closureAt next_context gk) [env,value]
                    pure (next_context,next)
            first <- conAt replace (closureAt initial_context gk) [first_arg]
            (_, replacements) <- foldM addReplacement (initial_context,first) rest
            let result = termAt gk bk
                t = apply_apply body_context gk bk replacements (mkTyVarTy body)
                index_vars = g : capture_kinds ++ [b]
                value_vars = body : args
                vars = index_vars ++ value_vars
                params = mkNamedTyConBinders Specified index_vars ++ map mkAnonTyConBinder value_vars
                root_equation = mkBranch vars (mkTyVarTys vars) t
                root = mkFamily root_name root_axiom params result [root_equation]
            guard (all validBranch (root_equation : apply_equations ++ lookup_equations))
            pure (root, [root,closure,apply,lookup_fam])
      _ -> pure Nothing
  where
    localName prefix = newImplicitBinder (idName owner) (mkTcOcc . (prefix ++) . occNameString)

freshVar :: String -> Kind -> TcM TyVar
freshVar str kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc str)

-- Match the promoted telescope at the intended kind. The order of
-- source quantification can differ from the order of GADT parameters.
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

mkFamily :: Name -> Name -> [TyConBinder] -> Kind -> [CoAxBranch] -> TyCon
mkFamily name axname binders result branches =
    let family = mkFamilyTyCon name (mkTyConKindWithMatchability unmatchableDataConTy binders result)
                    binders 0 result Nothing (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal axname family branches
    in family

-- Check the restrictions that ordinary family syntax imposes on
-- patterns, including the indices exposed by a promoted constructor.
validBranch :: CoAxBranch -> Bool
validBranch branch =
    all isTyFamFree lhs &&
    tyCoVarsOfTypes (coAxBranchRHS branch : lhs ++ map varType tvs) `subVarSet` mkVarSet tvs
  where
    lhs = coAxBranchLHS branch
    tvs = coAxBranchTyVars branch

mkBranch :: [TyVar] -> [Type] -> Type -> CoAxBranch
mkBranch vars lhs rhs =
    mkCoAxBranch vars [] [] lhs rhs (map (const Nominal) vars) noSrcSpan

allocateClosure :: Id -> Kind -> Kind -> TyCon -> TyCon
                -> TcM (TyCon, [DC.DataCon])
allocateClosure owner context_kind sort_kind term ext = do
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
    binder <- freshVar "binder" sort_kind
    let universals = [source, target]
        source_ty = mkTyVarTy source
        target_ty = mkTyVarTy target
        incoming_ty = mkTyVarTy incoming
        outgoing_ty = mkTyVarTy outgoing
        binder_ty = mkTyVarTy binder
        extend ctx = mkTyConApp ext [ctx, binder_ty]
        binders = map mkAnonTyConBinder universals
        kind = mkTyConKind binders liftedTypeKind
    closure <- mfix $ \result -> do
        let applied a b = mkTyConApp result [a,b]
            ctor name existentials user_tvs equalities fields res = do
              representative <- newTyConRepName name
              buildDataCon emptyFamInstEnvs (FixedBangOpts (map (const DC.HsLazy) fields))
                name False representative (map (const ordinary) fields) [] universals existentials
                (mkTyVarBinders Specified user_tvs) equalities []
                (map (Scaled manyDataConTy) fields) res result tags
        replace <- ctor replace_name [binder] [target,binder]
                     [DC.mkEqSpec source (extend target_ty)]
                     [mkTyConApp term [target_ty,binder_ty]] (applied (extend target_ty) target_ty)
        shift <- ctor shift_name [binder] [source,binder]
                   [DC.mkEqSpec target (extend source_ty)] [] (applied source_ty (extend source_ty))
        lift <- ctor lift_name [incoming,outgoing,binder] [incoming,outgoing,binder]
                  [DC.mkEqSpec source (extend incoming_ty), DC.mkEqSpec target (extend outgoing_ty)]
                  [applied incoming_ty outgoing_ty] (applied (extend incoming_ty) (extend outgoing_ty))
        replace_next <- ctor next_name [incoming,binder] [incoming,target,binder]
                          [DC.mkEqSpec source (extend incoming_ty)]
                          [applied incoming_ty target_ty, mkTyConApp term [target_ty,binder_ty]]
                          (applied (extend incoming_ty) target_ty)
        pure (mkAlgTyCon closure_name kind binders 0 liftedTypeKind [Nominal,Nominal] Nothing []
                (mkLevPolyDataTyConRhs True True [replace,shift,lift,replace_next]) (VanillaAlgTyCon closure_rep) True)
    pure (closure, tyConDataCons closure)

-- | Read constructors, pattern/carried variables, and decreasing recursive calls
-- in a GADT traversal branch.
indexedTraversalResult :: [(Id, CoreExpr)] -> Id -> [TyVar] -> Type -> [Id] -> Type
                       -> TyCon -> (Type -> Type) -> [Var] -> IndexedPattern
                       -> [(Id, Type)] -> [(Id, Type)] -> CoreExpr
                       -> Maybe (TyCon -> Type)
indexedTraversalResult bindings owner parameters domain carried result_ty header
                       specialize proofs pat pat_fields carried_fields =
    result bindings
  where
    all_fields = pat_fields ++ carried_fields

    validCast co =
      caseCoercion proofs co &&
      specialize (coercionLKind co) `eqType` specialize (coercionRKind co)

    unwrapCast (Core.Cast expr co) | validCast co = unwrapCast expr
    unwrapCast expr = Just expr

    result floats (Core.Cast body co) = do
      guard (validCast co)
      result floats body
    result floats expr = do
      let (fun, args) = Core.collectArgs expr
      Core.Var callee <- pure fun
      if callee == owner
        then recursive expr args
        else field callee args <|> constructor floats expr callee args <|> float floats callee expr

    field callee [] = const <$> lookup callee all_fields
    field _ _ = Nothing

    constructor floats expr callee args = do
      (con, supplied, values) <- indexedConstructor specialize callee args
      guard (lazyConstructor con && all (caseCoercion proofs) [co | Core.Coercion co <- args])
      children <- traverse (result floats) values
      _ <- checkedPromoted con (specialize (exprType expr)) supplied (map ($ header) children)
      pure (\family -> mkTyConApp (DC.promoteDataCon con) (supplied ++ map ($ family) children))

    float floats callee expr = do
      guard (isJust (lookup callee floats))
      result [] (expandTypeHelper floats expr)

    recursive expr args = do
      let (prefix, suffix) = splitAt (length parameters) args
      tys <- traverse (fmap specialize . indexedTypeArg) prefix
      guard (length tys == length parameters)
      sub_expr : rest_exprs <- pure suffix
      guard (length rest_exprs == length carried)
      Core.Var sub_var <- unwrapCast sub_expr
      guard (sub_var `elem` indexedFields pat)
      sub_ty <- lookup sub_var pat_fields
      carried_tys <- zipWithM (\ce expected -> do
          Core.Var actual <- unwrapCast ce
          guard (actual == expected)
          lookup actual carried_fields) rest_exprs carried
      let expected_input = substTyWith parameters tys domain
          expected_carried = map (substTyWith parameters tys . varType) carried
          expected_result = substTyWith parameters tys result_ty
      guard (typeKind sub_ty `eqType` expected_input &&
             eqTypes (map typeKind carried_tys) expected_carried &&
             specialize (exprType expr) `eqType` expected_result)
      pure (\family -> mkTyConApp family (tys ++ sub_ty : carried_tys))

{- Note [Untyped scope-indexed closures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Well-scoped untyped terms (such as UTerm (n :: Nat) with de Bruijn indices
Fin (n :: Nat) and scope extension 'Succ :: Nat -> Nat) use unary scope-indexed
traversals substU and renameU with ordinary rank-1 callbacks (Fin m -> UTerm n
and Fin m -> Fin n). Certify these closures and their renaming, weakening,
extension, and substitution operations (shiftFin, extU, renameU, extsU, substU)
with the unary counterpart of FamilyInputs so that open equations over abstract
rank-1 callbacks as well as capture-avoiding single/multi-binder substitutions
(substSingleU, etc.) publish nominal source equations. When T:substU is
available, publish T:substSingleU directly via T:substU so abstract terms unify
with T:substU.
-}

data UntypedFamilyInputs =
  UntypedFamilyInputs Id Int Kind TyCon TyCon TyCon (Id, Id) (Maybe (Id, Id, Maybe Id))
                      DC.DataCon DC.DataCon DC.DataCon [(DC.DataCon, [Int])]

sameUntypedShapes :: UntypedFamilyInputs -> UntypedFamilyInputs -> Bool
sameUntypedShapes (UntypedFamilyInputs _ arity g term ext index _ _ var hit miss rows)
                  (UntypedFamilyInputs _ arity' g' term' ext' index' _ _ var' hit' miss' rows') =
    arity == arity' && g `eqType` g' && term == term' && ext == ext' && index == index' &&
    var == var' && hit == hit' && miss == miss' && rows == rows'

untypedRenamingOperations :: UntypedFamilyInputs -> (Id, Id)
untypedRenamingOperations (UntypedFamilyInputs _ _ _ _ _ _ ops _ _ _ _ _) = ops

-- | Return the certified `(substU, extsU, Maybe shiftFin)` operations when present.
untypedSubstitutionOperations :: UntypedFamilyInputs -> Maybe (Id, Id, Maybe Id)
untypedSubstitutionOperations (UntypedFamilyInputs _ _ _ _ _ _ _ subst_ops _ _ _ _) = subst_ops

untypedFamilyInputs :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe UntypedFamilyInputs
untypedFamilyInputs bindings owner rhs = do
    ([context], body : captures, raw_call) <- function rhs
    let call = unwrapLocalCallbackLets raw_call
    guard (not (null captures) && exprType rhs `eqType` varType owner)
    (Core.Var traversal, [raw_callback, Core.Var operand]) <- application call
    guard (operand == body)
    ([], [argument], lookup_body) <- function (betaReduceTypeApps raw_callback)
    let context_kind = varType context
    (context_tycon, []) <- splitTyConApp_maybe context_kind
    guard (isAlgTyCon context_tycon)
    (term, [body_ctx]) <- splitTyConApp_maybe (varType body)
    (ext, [_]) <- splitTyConApp_maybe body_ctx
    (index, [index_ctx]) <- splitTyConApp_maybe (varType argument)
    let context_ty = mkTyVarTy context
        extend k = mkTyConApp ext [k]
        term_at_ctx = mkTyConApp term [context_ty]
    guard (isPromotedDataCon ext && all isAlgTyCon [term, index])
    guard (foldl' (\k _ -> extend k) context_ty captures `eqType` body_ctx &&
           all (\capture -> varType capture `eqType` term_at_ctx) captures &&
           index_ctx `eqType` body_ctx &&
           exprType call `eqType` term_at_ctx)
    case_rows <- caseOn argument lookup_body
    [(first, _, _), (second, _, _)] <- pure case_rows
    let index_cons = tyConDataCons index
    guard (first /= second && length index_cons == 2 &&
           first `elem` index_cons && second `elem` index_cons &&
           all lazyConstructor index_cons)
    (hit, miss, var) <-
        outcomes first second [] argument (reverse captures) lookup_body <|>
        outcomes second first [] argument (reverse captures) lookup_body
    guard (DC.dataConTyCon var == term)
    traversal_rhs <- lookup traversal bindings
    ([_, _], [sigma, scrutinee], traversal_case) <- function traversal_rhs
    rows <- caseOn scrutinee traversal_case
    [(var_bs, var_rhs)] <- pure [(bs, e) | (con, bs, e) <- rows, con == var]
    [field] <- pure (filter isValueVar var_bs)
    checked <- branchBody var_bs var_rhs
    (Core.Var use, [Core.Var x]) <- application checked
    guard (use == sigma && x == field)
    (ren_ops, subst_ops, recipes) <- untypedCheckedTraversals bindings traversal sigma var hit miss rows
    pure (UntypedFamilyInputs owner (length captures) context_kind term ext index
            ren_ops (Just subst_ops) var hit miss recipes)

untypedCheckedTraversals :: [(Id, CoreExpr)]
                         -> Id -> Id
                         -> DC.DataCon -> DC.DataCon -> DC.DataCon
                         -> [(DC.DataCon, [Var], CoreExpr)]
                         -> Maybe ((Id, Id), (Id, Id, Maybe Id), [(DC.DataCon, [Int])])
untypedCheckedTraversals bindings subst sigma var hit miss rows = do
    let constructors = tyConDataCons (DC.dataConTyCon var)
    guard (complete constructors rows)
    lifted <- traverse (untypedCheckedRow bindings var hit miss subst sigma)
                       [row | row@(con, _, _) <- rows, con /= var]
    let (recipes, used_extenders) = unzip lifted
        subst_steps = concatMap concat used_extenders
    extender <- allSame [v | NamedLift v <- subst_steps]
    extender_rhs <- lookup extender bindings
    (rename, mb_weakening) <- untypedCheckedExtension bindings var hit miss extender_rhs
    guard (all (isSubstLift rename) subst_steps)
    rename_rhs <- lookup rename bindings
    (tvs, [rho, operand], renaming) <- function rename_rhs
    guard (length tvs == 2)
    rename_rows <- caseOn operand renaming
    guard (complete constructors rename_rows)
    [(var_binders, var_rhs)] <- pure [(bs, rhs) | (con, bs, rhs) <- rename_rows, con == var]
    [original] <- pure (filter isValueVar var_binders)
    var_result <- branchBody var_binders var_rhs
    (var_head, [renamed]) <- constructorCall var_result
    guard (var_head == var)
    (Core.Var callback, [Core.Var argument]) <- application renamed
    guard (callback == rho && argument == original)
    renamed_rows <- traverse (untypedCheckedRow bindings var hit miss rename rho)
                             [row | row@(con, _, _) <- rename_rows, con /= var]
    let (rename_recipes, rename_extenders) = unzip renamed_rows
    guard (sortOn (nameStableString . DC.dataConName . fst) recipes ==
           sortOn (nameStableString . DC.dataConName . fst) rename_recipes)
    let rename_steps = concatMap concat rename_extenders
    guard (all isRenameLift rename_steps)
    extend_rename <- allSame [v | NamedLift v <- rename_steps]
    extend_rename_rhs <- lookup extend_rename bindings
    checkUntypedRenameExtension bindings hit miss extend_rename_rhs
    pure ((rename, extend_rename), (subst, extender, mb_weakening), recipes)
  where
    complete constructors alts =
      let heads = [con | (con, _, _) <- alts]
      in length heads == length constructors && all (`elem` heads) constructors &&
         all lazyConstructor constructors

    isSubstLift _ (NamedLift _) = True
    isSubstLift rename (InlinedSubst v) = rename == v
    isSubstLift _ InlinedRename = False

    isRenameLift (NamedLift _) = True
    isRenameLift InlinedRename = True
    isRenameLift (InlinedSubst _) = False

    allSame [] = Nothing
    allSame (v : vs) = v <$ guard (all (== v) vs)

untypedCheckedRow :: [(Id, CoreExpr)] -> DC.DataCon -> DC.DataCon -> DC.DataCon -> Id -> Id
                  -> (DC.DataCon, [Var], CoreExpr)
                  -> Maybe ((DC.DataCon, [Int]), [[CallbackLift]])
untypedCheckedRow bindings var hit miss traversal callback (con, binders, rhs) = do
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

    lifts (Core.Var original)
        | callback == original = Just []
    lifts captured = named captured <|> inlined captured

    named captured = case Core.collectArgs captured of
        (Core.Var extender, [Core.Type _, Core.Type _, inner]) ->
          (NamedLift extender :) <$> lifts inner
        _ -> Nothing

    inlined captured = do
        ([], [argument], body) <- function captured
        rows <- caseOn argument body
        [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
        [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
        guard (length rows == 2 && all (not . isValueVar) hit_bs)
        fresh <- expandTypeHelper bindings <$> branchBody hit_bs hit_rhs
        forwarded <- branchBody miss_bs miss_rhs
        (lift_step, see_older) <- inlineStep fresh forwarded
        [older] <- pure (filter isValueVar miss_bs)
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
            _ <- untypedCheckedWeakening bindings miss weakening
            pure (InlinedSubst rename, lookup_older)
          _ -> Nothing

-- | Match a unary weakening callback (`FS` or a named `shiftFin` helper),
-- returning `Just helper` when the callback names an external helper binding.
untypedCheckedWeakening :: [(Id, CoreExpr)] -> DC.DataCon -> CoreExpr -> Maybe (Maybe Id)
untypedCheckedWeakening bindings miss raw = do
    expr <- weakeningBody raw
    direct expr <|> eta expr <|> named_or_floated expr
  where
    direct expr = do
      (shift, []) <- constructorCall expr
      Nothing <$ guard (shift == miss)

    eta expr = do
      ([], [x], body) <- function expr
      (shift, [Core.Var y]) <- constructorCall body
      Nothing <$ guard (shift == miss && x == y)

    named_or_floated expr = do
      (tvs, [], partial) <- function expr
      (Core.Var helper, args) <- pure (Core.collectArgs (expr_or_partial tvs partial expr))
      tys <- traverse indexedTypeArg args
      decl <- lookup helper bindings
      con <- validConstructorAliasBody decl
      let (htvs, _) = splitForAllTyVars (varType helper)
      guard (length tvs <= 1 && length htvs == length tys && con == miss)
      pure $ if isExternalName (idName helper) && length htvs == 1
               then Just helper
               else Nothing

    expr_or_partial [] _ e = e
    expr_or_partial _ p _  = p

checkUntypedRenameExtension :: [(Id, CoreExpr)] -> DC.DataCon -> DC.DataCon -> CoreExpr -> Maybe ()
checkUntypedRenameExtension bindings hit miss rhs = do
    ([m, n], [capture, argument], body) <- function rhs
    guard (all isTyVar [m, n] && all isValueVar [capture, argument])
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs &&
           lazyConstructor hit && lazyConstructor miss)
    hit_body <- expandTypeHelper bindings <$> branchBody hit_bs hit_rhs
    (fresh, []) <- constructorCall hit_body
    guard (fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (successor, [lookup_older]) <- constructorCall miss_body
    guard (successor == miss)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)

untypedCheckedExtension :: [(Id, CoreExpr)]
                        -> DC.DataCon -> DC.DataCon -> DC.DataCon
                        -> CoreExpr -> Maybe (Id, Maybe Id)
untypedCheckedExtension bindings var hit miss rhs = do
    ([m, n], [capture, argument], body) <- function rhs
    guard (all isTyVar [m, n] && all isValueVar [capture, argument])
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
    _ <- lookup rename bindings
    chosen <- untypedCheckedWeakening bindings miss weakening
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)
    pure (rename, chosen)

publishUntypedEntries :: (TyCon -> Bool)
                      -> (Id -> Maybe Id)
                      -> (Id -> Maybe Id)
                      -> [(Id, CoreExpr)]
                      -> [(Id, CoreExpr)]
                      -> [(Id, CoreExpr)]
                      -> (Id -> Id -> Maybe [Id])
                      -> (forall a. (Id, a, [Id]) -> Bool)
                      -> [(Id, (UntypedFamilyInputs, UntypedFamilyInputs), [Id])]
                      -> [(UntypedFamilyInputs, UntypedFamilyInputs)]
                      -> TcM [(Id, [TyCon])]
publishUntypedEntries _ _ _ _ _ _ _ _ [] [] = pure []
publishUntypedEntries public lookupOwner lookupClosureRoot unoptimized original final extDeps safe closures direct_u = do
    let sameId a b = idName a == idName b
        all_pairs = [(o, s) | (_, (o, s), _) <- closures] ++ direct_u
        isConstructorId v = isJust (isDataConWorkId_maybe v <|> isDataConWrapId_maybe v)
        constructorOnly expr = case constructorCall expr of
          Just (_, args) -> all constructorOnly args
          Nothing -> False
        floatedConstructors v
          | Just decl <- lookup v final
          , isNothing (lookupOwner v)
          = (case function decl of
               Just (_ : _, [], body) -> constructorOnly body
               _ -> False) || isJust (validConstructorAliasBody decl)
          | otherwise = False
        onlyCalls allowed old actual = do
          old_rhs <- lookup old unoptimized
          new_rhs <- lookup actual final
          let bad allow_floated v =
                isValueVar v && idName v `notElem` allowed &&
                not (isConstructorId v) &&
                not (allow_floated && floatedConstructors v)
          guard (null (exprSomeFreeVarsList (bad False) old_rhs) &&
                 null (exprSomeFreeVarsList (bad True) new_rhs))
        renaming_candidate (original_shape, shape) = do
          let (rename, extend) = untypedRenamingOperations original_shape
              (final_rename, final_extend) = untypedRenamingOperations shape
          guard (idName rename == idName final_rename && idName extend == idName final_extend)
          actual_rename <- lookupOwner final_rename
          actual_extend <- lookupOwner final_extend
          onlyCalls [] extend actual_extend
          helpers <- extDeps rename actual_rename
          guard (actual_extend `elem` helpers)
          pure (actual_rename, (actual_extend, shape), helpers)
        subst_ext_candidate (original_shape, shape) = do
          let (old_rename, _) = untypedRenamingOperations original_shape
              (rename, _) = untypedRenamingOperations shape
          Just (_, old_ext, mb_old_weak) <- pure (untypedSubstitutionOperations original_shape)
          Just (_, ext, mb_fin_weak) <- pure (untypedSubstitutionOperations shape)
          guard (idName old_rename == idName rename && idName old_ext == idName ext &&
                 maybe True (\fw -> fmap idName mb_old_weak == Just (idName fw)) mb_fin_weak)
          actual_ext <- lookupOwner ext
          mb_actual_weak <- case mb_old_weak of
            Nothing -> pure Nothing
            Just old_w -> do
              aw <- lookupOwner old_w
              old_w_rhs <- lookup old_w original
              fin_w_rhs <- lookup aw final
              guard (isJust (validConstructorAliasBody old_w_rhs) &&
                     isJust (validConstructorAliasBody fin_w_rhs))
              onlyCalls [] old_w aw
              pure (Just aw)
          let allowed = idName rename : maybe [] (pure . idName) mb_old_weak
          onlyCalls allowed old_ext actual_ext
          raw_hs <- extDeps old_ext actual_ext
          let helpers = nubBy sameId (maybeToList mb_actual_weak ++ raw_hs)
          guard (idName rename `elem` map idName helpers)
          pure (actual_ext, (rename, mb_actual_weak, shape), helpers)
        subst_candidate (original_shape, shape) = do
          let (old_rename, _) = untypedRenamingOperations original_shape
              (rename, _) = untypedRenamingOperations shape
          Just (old_subst, old_ext, mb_old_weak) <- pure (untypedSubstitutionOperations original_shape)
          Just (subst, ext, mb_fin_weak) <- pure (untypedSubstitutionOperations shape)
          guard (map idName [old_subst, old_rename, old_ext] == map idName [subst, rename, ext] &&
                 maybe True (\fw -> fmap idName mb_old_weak == Just (idName fw)) mb_fin_weak)
          actual_subst <- lookupOwner subst
          actual_ext <- lookupOwner ext
          mb_actual_weak <- case mb_old_weak of
            Nothing    -> pure Nothing
            Just old_w -> Just <$> lookupClosureRoot old_w
          let op_names = [idName subst, idName rename, idName ext] ++ maybe [] (pure . idName) mb_old_weak
          onlyCalls op_names old_subst actual_subst
          raw_hs <- extDeps old_subst actual_subst
          let helpers = nubBy sameId (maybeToList mb_actual_weak ++ raw_hs)
          guard (actual_ext `elem` helpers)
          pure (actual_subst, (actual_ext, shape), helpers)
        renamings = filter safe (mapMaybe renaming_candidate all_pairs)
        sameRename (owner, _, _) (other, _, _) = sameId owner other
        sameExtension (_, (ext, _), _) (_, (other, _), _) = sameId ext other
    extensions <- catMaybes <$> traverse (publishUntypedRenameExtension False public) (nubBy sameExtension renamings)
    let extension_tables = [(idName ext, table) | (ext, [table]) <- extensions]
    checked_renamings <- catMaybes <$> traverse (publishUntypedRenaming False public extension_tables) (nubBy sameRename renamings)
    let checked_tables entries = [(idName owner, table) | (owner, table : _) <- entries]
        subst_extensions = nubBy sameRename (filter safe (mapMaybe subst_ext_candidate all_pairs))
        weakening_inputs = [(weak, shape) | (_, (_, Just weak, shape), _) <- subst_extensions]
    weakenings <- catMaybes <$> traverse (publishUntypedWeakening False public)
                    (nubBy (\a b -> sameId (fst a) (fst b)) weakening_inputs)
    subst_exts <- catMaybes <$> traverse (publishUntypedSubstitutionExtension False public
                    (checked_tables checked_renamings) (checked_tables weakenings)) subst_extensions
    let substitutions = nubBy sameRename (filter safe (mapMaybe subst_candidate all_pairs))
    checked_substs <- catMaybes <$> traverse (publishUntypedSubstitution False public
                        (checked_tables subst_exts)) substitutions
    let subst_u_tables = checked_tables checked_substs
    checked_closures <- catMaybes <$> traverse (publish_closure subst_u_tables) closures
    pure (checked_closures ++ extensions ++ checked_renamings ++ weakenings ++ subst_exts ++ checked_substs)
  where
    publish_closure subst_u_tables (owner, (_, shape), helpers)
      | VanillaId <- idDetails owner = do
          let mb_subst_tc = do
                (sub_u, _, _) <- untypedSubstitutionOperations shape
                lookup (idName sub_u) subst_u_tables <|> sourceTableId_maybe sub_u
          generated <- case mb_subst_tc of
            Just subst_tc -> allocateUntypedSubstViaSubstU False subst_tc shape
            Nothing       -> allocateUntypedFamilies False shape
          pure $ do
            (root, families) <- generated
            guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
            let new = setIdDetails owner (CheckedClosureSourceTableId (idName owner) root (map idName helpers))
            pure (new, families)
      | otherwise = pure Nothing

publishUntypedRenameExtension :: Bool -> (TyCon -> Bool) -> (Id, (Id, UntypedFamilyInputs), [Id])
                              -> TcM (Maybe (Id, [TyCon]))
publishUntypedRenameExtension is_early public (_, (owner, shape), _) = runMaybeT $ do
    let UntypedFamilyInputs _ _ context _ ext index _ _ _ hit miss _ = shape
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    let gk = mkTyVarTy g
        dk = mkTyVarTy d
        lookupAt c = mkTyConApp index [c]
        extend c = mkTyConApp ext [c]
        domain = lookupAt (extend gk)
        result = lookupAt (extend dk)
    r_var <- lift (freshVar "r" (untypedRenamingKind index gk dk))
    input <- lift (freshVar "input" domain)
    old <- lift (freshVar "index" (lookupAt gk))
    let r = mkTyVarTy r_var
        x = mkTyVarTy old
        binders = mkNamedTyConBinders Specified [g, d] ++
                  map mkAnonTyConBinder [r_var, input]
        branches = do
          fresh <- conAt hit domain []
          fresh' <- conAt hit result []
          previous <- conAt miss domain [x]
          previous' <- conAt miss result [mkAppTys r [x]]
          pure [renamingBranch [gk, dk, r, fresh] fresh',
                renamingBranch [gk, dk, r, previous] previous']
    publishRenamingTable is_early public owner [] binders result (const branches)

-- | Publish a nominal family for a named unary weakening helper (`shiftFin`).
publishUntypedWeakening :: Bool -> (TyCon -> Bool) -> (Id, UntypedFamilyInputs)
                        -> TcM (Maybe (Id, [TyCon]))
publishUntypedWeakening is_early public (owner, UntypedFamilyInputs _ _ context _ ext index _ _ _ _ miss _) = runMaybeT $ do
    g <- lift (freshVar "g" context)
    let gk = mkTyVarTy g
        input = mkTyConApp index [gk]
        result = mkTyConApp index [mkTyConApp ext [gk]]
    x <- lift (freshVar "x" input)
    let binders = mkNamedTyConBinders Specified [g] ++ [mkAnonTyConBinder x]
        branches = do
          output <- conAt miss result [mkTyVarTy x]
          pure [renamingBranch [gk, mkTyVarTy x] output]
    publishRenamingTable is_early public owner [] binders result (const branches)

-- | Publish a nominal family for a unary substitution-extension helper (`extsU`).
publishUntypedSubstitutionExtension :: Bool -> (TyCon -> Bool) -> [(Name, TyCon)] -> [(Name, TyCon)]
                                    -> (Id, (Id, Maybe Id, UntypedFamilyInputs), [Id])
                                    -> TcM (Maybe (Id, [TyCon]))
publishUntypedSubstitutionExtension is_early public renamings weakenings (owner, (rename, mb_weaken, shape), helpers) =
    runMaybeT $ do
      renaming <- MaybeT (pure (lookup (idName rename) renamings))
      (weakening, extra_tcs) <- case mb_weaken of
        Just w_id -> (, []) <$> MaybeT (pure (lookup (idName w_id) weakenings))
        Nothing -> do
          let UntypedFamilyInputs _ _ context _ ext index _ _ _ _ miss _ = shape
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
          pure (w_tc, [w_tc])
      let UntypedFamilyInputs _ _ context term ext index _ _ var hit miss _ = shape
      g <- lift (freshVar "g" context)
      d <- lift (freshVar "d" context)
      let gk = mkTyVarTy g
          dk = mkTyVarTy d
          lookupAt c = mkTyConApp index [c]
          termAt c = mkTyConApp term [c]
          extend c = mkTyConApp ext [c]
          domain = lookupAt (extend gk)
          result = termAt (extend dk)
      r_var <- lift (freshVar "r" (untypedCallbackKind index term gk dk))
      input <- lift (freshVar "input" domain)
      old <- lift (freshVar "index" (lookupAt gk))
      let r = mkTyVarTy r_var
          x = mkTyVarTy old
          binders = mkNamedTyConBinders Specified [g, d] ++ map mkAnonTyConBinder [r_var, input]
          branches = do
            fresh <- conAt hit domain []
            new_index <- conAt hit (lookupAt (extend dk)) []
            fresh' <- conAt var result [new_index]
            previous <- conAt miss domain [x]
            let shift = mkTyConApp weakening [dk]
                mapped = mkAppTys r [x]
            guard (typeKind shift `eqType` untypedRenamingKind index dk (extend dk))
            let previous' = mkTyConApp renaming [dk, extend dk, shift, mapped]
            guard (typeKind fresh' `eqType` result && typeKind previous' `eqType` result)
            pure [renamingBranch [gk, dk, r, fresh] fresh',
                  renamingBranch [gk, dk, r, previous] previous']
      (new_owner, [table]) <- publishRenamingTable is_early public owner helpers binders result (const branches)
      pure (new_owner, table : extra_tcs)

-- | Publish a nominal family for a unary renaming traversal (`renameU`).
publishUntypedRenaming :: Bool -> (TyCon -> Bool) -> [(Name, TyCon)] -> (Id, (Id, UntypedFamilyInputs), [Id])
                       -> TcM (Maybe (Id, [TyCon]))
publishUntypedRenaming = publishUntypedTraversal False

-- | Publish a nominal family for a unary substitution traversal (`substU`).
publishUntypedSubstitution :: Bool -> (TyCon -> Bool) -> [(Name, TyCon)] -> (Id, (Id, UntypedFamilyInputs), [Id])
                           -> TcM (Maybe (Id, [TyCon]))
publishUntypedSubstitution = publishUntypedTraversal True

-- | Publish a nominal family for a unary scope-indexed traversal (`renameU` or `substU`).
publishUntypedTraversal :: Bool -> Bool -> (TyCon -> Bool) -> [(Name, TyCon)]
                        -> (Id, (Id, UntypedFamilyInputs), [Id])
                        -> TcM (Maybe (Id, [TyCon]))
publishUntypedTraversal is_subst is_early public extensions (owner, (extension, shape), helpers) = runMaybeT $ do
    extend <- MaybeT (pure (lookup (idName extension) extensions))
    let UntypedFamilyInputs _ _ context term ext index _ _ var _ _ recipes = shape
        out_tc = if is_subst then term else index
        cbKind = untypedCallbackKind index out_tc
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    let gk = mkTyVarTy g
        dk = mkTyVarTy d
        termAt c = mkTyConApp term [c]
        result = termAt dk
    r_var <- lift (freshVar "r" (cbKind gk dk))
    input <- lift (freshVar "input" (termAt gk))
    let binders = mkNamedTyConBinders Specified [g, d] ++ map mkAnonTyConBinder [r_var, input]
        prepare (con, depths) = do
          let (con_vars, telescope) = splitForAllTyVars (tyConKind (DC.promoteDataCon con))
              (_, arg_kinds, codomain) = splitFunTys telescope
          (subst, vars) <- lift (freshenTyVarBndrs (map snd con_vars))
          let kind = substTyUnchecked subst codomain
          args <- lift (traverse (freshVar "field" . substTyUnchecked subst . scaledThing) arg_kinds)
          (tc, [source]) <- MaybeT (pure (splitTyConApp_maybe kind))
          guard (tc == term)
          local <- lift (freshVar "r" (cbKind source dk))
          let r = mkTyVarTy local
              field_tys = mkTyVarTys args
              lhs = mkTyConApp (DC.promoteDataCon con) (mkTyVarTys vars ++ field_tys)
              liftField 0 ctx = (dk, r) <$ guard (ctx `eqType` source)
              liftField depth ctx = do
                (cl, [outer]) <- splitTyConApp_maybe ctx
                guard (cl == ext)
                (target, inherited) <- liftField (depth - 1) outer
                pure (mkTyConApp ext [target],
                      mkTyConApp extend [outer, target, inherited])
              rewrite family depth arg = do
                (t, [ctx]) <- splitTyConApp_maybe (typeKind arg)
                guard (t == term)
                (target, captured) <- liftField depth ctx
                guard (typeKind captured `eqType` cbKind ctx target)
                pure (mkTyConApp family [ctx, target, captured, arg])
          pure $ \family -> do
            guard (typeKind lhs `eqType` kind)
            out <- if con == var
              then do [x] <- pure field_tys
                      guard (typeKind x `eqType` mkTyConApp index [source])
                      let mapped = mkAppTys r [x]
                      if is_subst
                        then mapped <$ guard (typeKind mapped `eqType` termAt dk)
                        else conAt con (termAt dk) [mapped]
              else zipWithM (rewrite family) depths field_tys >>= conAt con (termAt dk)
            pure (renamingBranch [source, dk, r, lhs] out)
    rows <- traverse prepare ((var, []) : recipes)
    publishRenamingTable is_early public owner helpers binders result (\family -> traverse ($ family) rows)

-- | Build `index g %U -> index d` for unary renaming callbacks.
untypedRenamingKind :: TyCon -> Type -> Type -> Kind
untypedRenamingKind index = untypedCallbackKind index index

-- | Build `index g %U -> out d` for unary renaming or substitution callbacks.
untypedCallbackKind :: TyCon -> TyCon -> Type -> Type -> Kind
untypedCallbackKind index out g d =
    mkVisFunTyManyU (mkTyConApp index [g]) (mkTyConApp out [d])

-- | Emit T:substSingleU directly via T:substU and a companion map family when
-- T:substU is certified, so open applications unify with T:substU.
allocateUntypedSubstViaSubstU :: Bool -> TyCon -> UntypedFamilyInputs -> TcM (Maybe (TyCon, [TyCon]))
allocateUntypedSubstViaSubstU is_early subst_tc
  (UntypedFamilyInputs owner arity context term ext lookup_tycon _ _ var hit miss _) = do
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
      guard (typeKind partial_map `eqType` untypedCallbackKind lookup_tycon term body_ctx gk)
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

{- Note [Named substitution callbacks and forwarding wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Programs often define substitution callbacks as named helpers instead of inline
lambdas:
  1. Constructor aliases (`ids = UVar`, `shift = FS`, `shiftFin x = FS x`),
     which partially apply to unmatchable `%U` callbacks.
  2. Named substitution maps (`singleSubst`, `doubleSubst`, `singleSubstU`,
     `doubleSubstU`), which match `Z`, `S Z`, ..., `S^k x -> Var x` in any
     capture argument order, with rank-2 or prenex type signatures.
  3. Sigma-calculus combinators (`ren`, `consSubst`, `seqSubst`) over unary
     `Fin`/`UTerm` and binary `Lookup`/`Term` types.
  4. Forwarding wrappers (`substSingle`, `substDouble`, `substSingleU`), which
     pass a certified substitution map to certified `subst` or `substU` in any
     parameter order.

Publish each helper as a nominal closed family. Publish each forwarding wrapper
directly in terms of `T:subst` or `T:substU` applied to `T:singleSubst` or
`T:doubleSubst`, rather than private `$closure:` constructors. Open equations
such as `substSingle n v :~: subst (singleSubst v) n` then hold by `Refl`, and
constructor applications reduce through `T:subst`. Under `-O2`, GHC can inline a
substitution map into its forwarding wrapper or share two wrappers by CSE.
Accept both the direct call and the inlined case tree in final Core, and still
check the helper's own final binding and rewrite rules.
-}

-- | Collect consecutive type and value lambdas, skipping dead lifted local lets.
collectMixedBinders :: CoreExpr -> ([Var], CoreExpr)
collectMixedBinders = go []
  where
    go acc (Core.Lam b e)
      | isTyVar b || isValueVar b = go (b : acc) e
    go acc (Core.Let (Core.NonRec b _) e)
      | isLocalId b
      , isLiftedTypeKind (typeKind (varType b))
      , not (b `elemVarSet` exprFreeVars e)
      = go acc e
    go acc (Core.Cast e co)
      | isSafeMultOrReflCo co = go acc e
    go acc e = (reverse acc, e)

-- | Split interleaved Specified type binders and unrestricted visible arrows,
-- expanding type synonyms (`Rename m n`, `Subst m n`) at each step.
splitMixedPiTys :: Type -> Maybe ([Either TyVar Type], Type)
splitMixedPiTys raw_ty = case splitForAllForAllTyBinders raw_ty of
    ((match, Bndr tv Specified) : rest_bndrs, inner)
      | isTyVar tv, match `eqType` unmatchableDataConTy
      , Just rest_specs <- traverse (\case
          (m, Bndr v Specified) | isTyVar v -> Just (m, v)
          _ -> Nothing) rest_bndrs -> do
          let rebuilt = mkSpecForAllTys rest_specs inner
          (more, res) <- splitMixedPiTys rebuilt
          pure (Left tv : more, res)
    (_ : _, _) -> Nothing
    ([], tau)
      | Just (FTF_T_T, match, mult, dom, cod) <- splitFunTy_maybe tau
      , match `eqType` unmatchableDataConTy, isManyTy mult -> do
          (more, res) <- splitMixedPiTys cod
          pure (Right dom : more, res)
      | Just _ <- splitFunTy_maybe tau -> Nothing
      | otherwise ->
          let expanded = coreFullView tau
          in Just ([], expanded)

-- | Test whether `ty` is an indexed GADT or promoted-index algebraic datatype.
isIndexedAlgDomain :: Type -> Bool
isIndexedAlgDomain ty
  | Just (tc, args@(_ : _)) <- splitTyConApp_maybe ty
  = isAlgTyCon tc &&
    (any (not . DC.isVanillaDataCon) (tyConDataCons tc) ||
     any (not . isLiftedTypeKind . typeKind) args)
  | otherwise = False

-- | Certified point-free or 1-argument constructor alias (`ids = UVar`, `shift = FS`).
-- See Note [Named substitution callbacks and forwarding wrappers].
data ConstructorAliasSpec = ConstructorAliasSpec [TyVar] Type Type DC.DataCon

-- | Compare constructor-alias specifications across compiler stages.
sameConstructorAliasSpec :: ConstructorAliasSpec -> ConstructorAliasSpec -> Bool
sameConstructorAliasSpec (ConstructorAliasSpec tvs1 _ _ c1) (ConstructorAliasSpec tvs2 _ _ c2) =
    length tvs1 == length tvs2 && c1 == c2

-- | Match a point-free or eta-expanded lazy unary data constructor body.
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

-- | Certify `owner` as a scope-indexed constructor alias (`ids` or `shift`).
checkedConstructorAlias :: Id -> CoreExpr -> Maybe ConstructorAliasSpec
checkedConstructorAlias owner rhs = do
    con <- validConstructorAliasBody rhs
    (pis, cod_ty) <- splitMixedPiTys (varType owner)
    let (left_pis, rest_pis) = span (\case { Left _ -> True; _ -> False }) pis
        all_tvs = [tv | Left tv <- left_pis]
    [Right dom_ty] <- pure rest_pis
    guard (not (null all_tvs) && length all_tvs <= 3 &&
           mkVarSet all_tvs `subVarSet` tyCoVarsOfTypes [dom_ty, cod_ty] &&
           isIndexedAlgDomain dom_ty && isIndexedAlgDomain cod_ty)
    (dom_tc, dom_args) <- splitTyConApp_maybe dom_ty
    (cod_tc, cod_args) <- splitTyConApp_maybe cod_ty
    guard (cod_tc == DC.dataConTyCon con && length dom_args == length cod_args &&
           length dom_args <= 2 && DC.dataConSourceArity con == 1)
    let valid_weaken = dom_tc == cod_tc && case (dom_args, cod_args) of
          ([d0], [c0]) | Just (ext_tc, [inner]) <- splitTyConApp_maybe c0 ->
            isPromotedDataCon ext_tc && inner `eqType` d0
          ([d0, s0], [c0, s1]) | Just (ext_tc, [inner, _]) <- splitTyConApp_maybe c0 ->
            isPromotedDataCon ext_tc && inner `eqType` d0 && s0 `eqType` s1
          _ -> False
        valid_var = dom_tc /= cod_tc && eqTypes dom_args cod_args
    guard (valid_weaken || valid_var)
    pure (ConstructorAliasSpec all_tvs dom_ty cod_ty con)

-- | Publish a nominal family for a certified constructor alias.
publishConstructorAlias :: Bool -> (TyCon -> Bool) -> (Id, ConstructorAliasSpec, [Id])
                        -> TcM (Maybe (Id, [TyCon]))
publishConstructorAlias is_early public (owner, ConstructorAliasSpec all_tvs dom_ty cod_ty con, helpers) =
    runMaybeT $ do
      (subst, fresh_all) <- lift (freshenTyVarBndrs all_tvs)
      let inst_dom = substTyUnchecked subst dom_ty
          inst_cod = substTyUnchecked subst cod_ty
      x <- lift (freshVar "x" inst_dom)
      let binders = mkNamedTyConBinders Specified fresh_all ++ [mkAnonTyConBinder x]
          lhs_tys = mkTyVarTys fresh_all ++ [mkTyVarTy x]
          branches = do
            out <- conAt con inst_cod [mkTyVarTy x]
            pure [renamingBranch lhs_tys out]
      publishRenamingTable is_early public owner helpers binders inst_cod (const branches)

-- | Certified named substitution-map helper (`singleSubst`, `doubleSubst`, `singleSubstU`).
-- See Note [Named substitution callbacks and forwarding wrappers].
data SubstMapSpec
  = UntypedSubstMapSpec TyVar Int TyCon TyCon TyCon DC.DataCon DC.DataCon DC.DataCon [Int]
  | TypedSubstMapSpec [TyVar] (Maybe TyVar) TyVar [Type] [Type] Type Type TyCon TyCon TyCon
                      DC.DataCon DC.DataCon DC.DataCon [Int]

-- | Compare substitution-map specifications across compiler stages.
sameSubstMapSpec :: SubstMapSpec -> SubstMapSpec -> Bool
sameSubstMapSpec (UntypedSubstMapSpec _ k1 t1 e1 i1 v1 h1 m1 p1)
                 (UntypedSubstMapSpec _ k2 t2 e2 i2 v2 h2 m2 p2) =
    k1 == k2 && t1 == t2 && e1 == e2 && i1 == i2 && v1 == v2 && h1 == h2 && m1 == m2 && p1 == p2
sameSubstMapSpec (TypedSubstMapSpec o1 r1 _ cs1 _ _ _ t1 e1 i1 v1 h1 m1 p1)
                 (TypedSubstMapSpec o2 r2 _ cs2 _ _ _ t2 e2 i2 v2 h2 m2 p2) =
    length o1 == length o2 && isJust r1 == isJust r2 && length cs1 == length cs2 &&
    t1 == t2 && e1 == e2 && i1 == i2 && v1 == v2 && h1 == h2 && m1 == m2 && p1 == p2
sameSubstMapSpec _ _ = False

-- | Match nested lookup cases (`Z -> cap_{i_0}`, `S Z -> cap_{i_1}`, ..., `S^k x -> Var x`),
-- returning the capture argument indices selected at depths `0 .. k - 1` and the `Var` constructor.
matchSubstMapOutcomes :: DC.DataCon -> DC.DataCon -> [Var] -> Id -> [Id] -> Int -> CoreExpr
                      -> Maybe ([Int], DC.DataCon)
matchSubstMapOutcomes hit miss scope argument captures depth_left expr = do
    body <- branchBody scope expr
    if depth_left == 0
      then do
        (con, [raw_x]) <- constructorCall body
        Core.Var x <- branchBody scope raw_x
        guard (x == argument && lazyConstructor con)
        pure ([], con)
      else do
        rows <- caseOn argument body
        [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
        [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
        guard (length rows == 2 && all (not . isValueVar) hit_bs)
        Core.Var selected <- branchBody (scope ++ hit_bs) hit_rhs
        idx <- elemIndex selected captures
        [older] <- pure (filter isValueVar miss_bs)
        (rest_indices, con) <-
          matchSubstMapOutcomes hit miss (scope ++ miss_bs) older captures (depth_left - 1) miss_rhs
        pure (idx : rest_indices, con)

-- | Certify a named unary or binary substitution-map helper.
checkedSubstMap :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe SubstMapSpec
checkedSubstMap _ owner rhs = do
    guard (exprType rhs `eqType` varType owner)
    untyped_map <|> typed_map
  where
    untyped_map = do
      (pis, res_ty) <- splitMixedPiTys (varType owner)
      Left ctx_tv : val_pis <- pure pis
      val_tys <- traverse (\case { Right t -> Just t; _ -> Nothing }) val_pis
      idx_ty : rev_caps@(_ : _) <- pure (reverse val_tys)
      let cap_tys = reverse rev_caps
          k = length cap_tys
          ctx_ty = mkTyVarTy ctx_tv
      (context_tc, []) <- splitTyConApp_maybe (varType ctx_tv)
      guard (isAlgTyCon context_tc)
      (term, [r_ctx]) <- splitTyConApp_maybe res_ty
      (index, [idx_ctx]) <- splitTyConApp_maybe idx_ty
      (ext, [_]) <- splitTyConApp_maybe idx_ctx
      guard (isPromotedDataCon ext && all isAlgTyCon [term, index] &&
             r_ctx `eqType` ctx_ty &&
             all (`eqType` res_ty) cap_tys &&
             foldl' (\c _ -> mkTyConApp ext [c]) ctx_ty [1 .. k] `eqType` idx_ctx)
      [c1, c2] <- pure (tyConDataCons index)
      guard (all lazyConstructor [c1, c2])
      ([_], all_vals, body) <- function (betaReduceTypeApps rhs)
      guard (length all_vals == k + 1)
      (cap_vars, [arg_var]) <- pure (splitAt k all_vals)
      (hit, miss, perm, var_con) <-
        ((\(p, vc) -> (c1, c2, p, vc)) <$> matchSubstMapOutcomes c1 c2 [] arg_var cap_vars k body) <|>
        ((\(p, vc) -> (c2, c1, p, vc)) <$> matchSubstMapOutcomes c2 c1 [] arg_var cap_vars k body)
      guard (DC.dataConTyCon var_con == term &&
             length perm == k && all (\i -> i `elem` perm) [0 .. k - 1])
      pure (UntypedSubstMapSpec ctx_tv k term ext index var_con hit miss perm)

    typed_map = do
      (pis, res_ty) <- splitMixedPiTys (varType owner)
      let (outer_lefts, after_outer) = span (\case { Left _ -> True; _ -> False }) pis
          outer_tvs = [tv | Left tv <- outer_lefts]
      guard (not (null outer_tvs) && not (null after_outer))
      (cap_tys, mb_rank2_tv, idx_ty) <- parse_typed_tail after_outer
      let k = length cap_tys
      guard (k >= 1)
      (term, [TyVarTy ctx_tv, TyVarTy idx_sort_tv]) <- splitTyConApp_maybe res_ty
      guard (case mb_rank2_tv of
               Just r_tv -> r_tv == idx_sort_tv && length outer_tvs == k + 1
               Nothing   -> idx_sort_tv `elem` outer_tvs && length outer_tvs == k + 2)
      let context_kind = varType ctx_tv
          sort_kind = varType idx_sort_tv
      (context_tc, []) <- splitTyConApp_maybe context_kind
      (sort_tc, []) <- splitTyConApp_maybe sort_kind
      guard (all isAlgTyCon [context_tc, sort_tc, term])
      (index, [body_ctx, TyVarTy idx_sort_tv']) <- splitTyConApp_maybe idx_ty
      guard (isAlgTyCon index && idx_sort_tv' == idx_sort_tv)
      (ext, chain_sorts) <- peel_ctx_chain ctx_tv body_ctx
      guard (length chain_sorts == k &&
             all ((`eqType` sort_kind) . typeKind) chain_sorts)
      cap_sorts <- traverse (\ct -> do
          (t_tc, [c_arg, s_arg]) <- splitTyConApp_maybe ct
          guard (t_tc == term && c_arg `eqType` mkTyVarTy ctx_tv && typeKind s_arg `eqType` sort_kind)
          pure s_arg) cap_tys
      [c1, c2] <- pure (tyConDataCons index)
      guard (all lazyConstructor [c1, c2])
      let (mixed_bndrs, body) = collectMixedBinders (betaReduceTypeApps rhs)
          ty_bndrs = filter isTyVar mixed_bndrs
          val_bndrs = filter isValueVar mixed_bndrs
      guard (length mixed_bndrs == length ty_bndrs + length val_bndrs &&
             length ty_bndrs == k + 2 && length val_bndrs == k + 1)
      (cap_vars, [arg_var]) <- pure (splitAt k val_bndrs)
      (hit, miss, perm, var_con) <-
        ((\(p, vc) -> (c1, c2, p, vc)) <$> matchSubstMapOutcomes c1 c2 [] arg_var cap_vars k body) <|>
        ((\(p, vc) -> (c2, c1, p, vc)) <$> matchSubstMapOutcomes c2 c1 [] arg_var cap_vars k body)
      guard (DC.dataConTyCon var_con == term &&
             length perm == k && all (\i -> i `elem` perm) [0 .. k - 1] &&
             and (zipWith eqType [cap_sorts !! i | i <- perm] (reverse chain_sorts)))
      pure (TypedSubstMapSpec outer_tvs mb_rank2_tv idx_sort_tv chain_sorts cap_tys idx_ty res_ty
              term ext index var_con hit miss perm)

    parse_typed_tail [Left r_tv, Right idx_ty] = Just ([], Just r_tv, idx_ty)
    parse_typed_tail [Right idx_ty] = Just ([], Nothing, idx_ty)
    parse_typed_tail (Right ct : rest@(_ : _)) = do
      (cts, mb_r, idx_ty) <- parse_typed_tail rest
      pure (ct : cts, mb_r, idx_ty)
    parse_typed_tail _ = Nothing

    peel_ctx_chain ctx_tv ty
      | Just (ext_tc, [inner, s]) <- splitTyConApp_maybe ty
      , isPromotedDataCon ext_tc =
          if inner `eqType` mkTyVarTy ctx_tv
            then Just (ext_tc, [s])
            else do
              (ext_tc', prev) <- peel_ctx_chain ctx_tv inner
              guard (ext_tc == ext_tc')
              pure (ext_tc, prev ++ [s])
      | otherwise = Nothing

-- | Publish a nominal family for a certified named substitution-map helper.
publishSubstMap :: Bool -> (TyCon -> Bool) -> (Id, SubstMapSpec, [Id]) -> TcM (Maybe (Id, [TyCon]))
publishSubstMap is_early public (owner, spec, helpers) = runMaybeT $ case spec of
    UntypedSubstMapSpec ctx_tv k term ext index var_con hit miss perm -> do
      g <- lift (freshVar "g" (varType ctx_tv))
      let gk = mkTyVarTy g
          termAt p = mkTyConApp term [p]
          ctxAt p = mkTyConApp ext [p]
          lookupAt p = mkTyConApp index [p]
          body_ctx = foldl' (\c _ -> ctxAt c) gk [1 .. k]
      caps <- lift (traverse (\_ -> freshVar "v" (termAt gk)) [1 .. k])
      input <- lift (freshVar "input" (lookupAt body_ctx))
      older <- lift (freshVar "x" (lookupAt gk))
      let cap_tys = mkTyVarTys caps
          binders = mkNamedTyConBinders Specified [g] ++
                    map mkAnonTyConBinder (caps ++ [input])
          wrap_miss 0 ctx pat_fn = pat_fn ctx
          wrap_miss d ctx pat_fn = do
            (cl, [inner_ctx]) <- splitTyConApp_maybe ctx
            guard (cl == ext)
            inner_pat <- wrap_miss (d - 1) inner_ctx pat_fn
            conAt miss (lookupAt ctx) [inner_pat]
          branches = do
            hit_brs <- traverse (\(d, ci) -> do
                pat <- wrap_miss d body_ctx (\ctx -> conAt hit (lookupAt ctx) [])
                pure (renamingBranch (gk : cap_tys ++ [pat]) (cap_tys !! ci)))
              (zip [0 .. k - 1] perm)
            miss_pat <- wrap_miss k body_ctx (\_ -> Just (mkTyVarTy older))
            miss_rhs <- conAt var_con (termAt gk) [mkTyVarTy older]
            pure (hit_brs ++ [renamingBranch (gk : cap_tys ++ [miss_pat]) miss_rhs])
      publishRenamingTable is_early public owner helpers binders (termAt gk) (const branches)

    TypedSubstMapSpec outer_tvs mb_rank2_tv idx_sort_tv chain_sorts cap_tys idx_ty res_ty
                      term ext index var_con hit miss perm -> do
      let all_tvs = outer_tvs ++ maybeToList mb_rank2_tv
      (subst, fresh_all) <- lift (freshenTyVarBndrs all_tvs)
      let (fresh_outer, fresh_rank2) = splitAt (length outer_tvs) fresh_all
          inst = substTyUnchecked subst
          inst_caps = map inst cap_tys
          inst_idx = inst idx_ty
          inst_res = inst res_ty
          inst_chain = map inst chain_sorts
          inst_sort = inst (mkTyVarTy idx_sort_tv)
      (t_tc, [gk, _]) <- MaybeT (pure (splitTyConApp_maybe inst_res))
      guard (t_tc == term)
      caps <- lift (traverse (freshVar "v") inst_caps)
      input <- lift (freshVar "input" inst_idx)
      older <- lift (freshVar "x" (mkTyConApp index [gk, inst_sort]))
      let cap_var_tys = mkTyVarTys caps
          outer_var_tys = mkTyVarTys fresh_outer
          binders = mkNamedTyConBinders Specified fresh_outer ++
                    map mkAnonTyConBinder caps ++
                    mkNamedTyConBinders Specified fresh_rank2 ++
                    [mkAnonTyConBinder input]
          lookupAt c s = mkTyConApp index [c, s]
          termAt c s = mkTyConApp term [c, s]
          body_ctx = foldl' (\c s -> mkTyConApp ext [c, s]) gk inst_chain
          lhs_for target_sort pat =
            case mb_rank2_tv of
              Just _ -> outer_var_tys ++ cap_var_tys ++ [target_sort, pat]
              Nothing ->
                let specialize_outer tv =
                      if mkTyVarTy tv `eqType` inst_sort then target_sort else mkTyVarTy tv
                in map specialize_outer fresh_outer ++ cap_var_tys ++ [pat]
          wrap_miss 0 ctx target_sort pat_fn = pat_fn ctx target_sort
          wrap_miss d ctx target_sort pat_fn = do
            (cl, [inner_ctx, _]) <- splitTyConApp_maybe ctx
            guard (cl == ext)
            inner_pat <- wrap_miss (d - 1) inner_ctx target_sort pat_fn
            conAt miss (lookupAt ctx target_sort) [inner_pat]
          branches = do
            let k = length caps
            hit_brs <- traverse (\(d, ci) -> do
                let rhs_val = cap_var_tys !! ci
                (_, [_, cap_s]) <- splitTyConApp_maybe (typeKind rhs_val)
                pat <- wrap_miss d body_ctx cap_s (\ctx s -> conAt hit (lookupAt ctx s) [])
                let lhs = lhs_for cap_s pat
                guard (typeKind rhs_val `eqType` termAt gk cap_s)
                pure (renamingBranch lhs rhs_val))
              (zip [0 .. k - 1] perm)
            miss_pat <- wrap_miss (length caps) body_ctx inst_sort (\_ _ -> Just (mkTyVarTy older))
            miss_rhs <- conAt var_con (termAt gk inst_sort) [mkTyVarTy older]
            pure (hit_brs ++ [renamingBranch (lhs_for inst_sort miss_pat) miss_rhs])
      publishRenamingTable is_early public owner helpers binders inst_res (const branches)

-- | Domain and telescope shape of a sigma-calculus combinator.
-- See Note [Named substitution callbacks and forwarding wrappers].
data SigmaCombShape
  = UntypedSigmaShape [TyVar] [TyVar] TyCon (Maybe TyCon) TyCon
  | TypedSigmaShape [TyVar] (Maybe TyVar) [TyVar] TyCon (Maybe TyCon) TyCon

-- | Certified sigma-calculus combinator (`ren`, `consSubst`, or `seqSubst`).
data SigmaCombSpec
  = SigmaRen SigmaCombShape DC.DataCon
  | SigmaCons SigmaCombShape Bool DC.DataCon DC.DataCon
  | SigmaSeq SigmaCombShape Id

-- | Compare sigma-combinator domain shapes across compiler stages.
sameSigmaCombShape :: SigmaCombShape -> SigmaCombShape -> Bool
sameSigmaCombShape (UntypedSigmaShape out1 sem1 i1 e1 t1) (UntypedSigmaShape out2 sem2 i2 e2 t2) =
    length out1 == length out2 &&
    map (`elemIndex` out1) sem1 == map (`elemIndex` out2) sem2 &&
    i1 == i2 && e1 == e2 && t1 == t2
sameSigmaCombShape (TypedSigmaShape out1 in1 sem1 i1 e1 t1) (TypedSigmaShape out2 in2 sem2 i2 e2 t2) =
    length out1 == length out2 && isJust in1 == isJust in2 &&
    map (`elemIndex` (out1 ++ maybeToList in1)) sem1 ==
      map (`elemIndex` (out2 ++ maybeToList in2)) sem2 &&
    i1 == i2 && e1 == e2 && t1 == t2
sameSigmaCombShape _ _ = False

-- | Compare sigma-combinator specifications across compiler stages.
sameSigmaCombSpec :: SigmaCombSpec -> SigmaCombSpec -> Bool
sameSigmaCombSpec (SigmaRen s1 c1) (SigmaRen s2 c2) = sameSigmaCombShape s1 s2 && c1 == c2
sameSigmaCombSpec (SigmaCons s1 sw1 h1 m1) (SigmaCons s2 sw2 h2 m2) =
    sameSigmaCombShape s1 s2 && sw1 == sw2 && h1 == h2 && m1 == m2
sameSigmaCombSpec (SigmaSeq s1 tr1) (SigmaSeq s2 tr2) =
    sameSigmaCombShape s1 s2 && idName tr1 == idName tr2
sameSigmaCombSpec _ _ = False

-- | Certify `owner` as `ren`, `consSubst`, or `seqSubst`.
checkedSigmaComb :: Id -> CoreExpr -> Maybe SigmaCombSpec
checkedSigmaComb owner rhs = do
    guard (exprType rhs `eqType` varType owner)
    check_ren <|> check_cons <|> check_seq
  where
    same_tv_set xs ys =
      length xs == length ys &&
      mkVarSet xs `subVarSet` mkVarSet ys &&
      mkVarSet ys `subVarSet` mkVarSet xs

    split_outer_and_tail pis = do
      let (lefts, rest) = span (\case { Left _ -> True; _ -> False }) pis
          outer_tvs = [tv | Left tv <- lefts]
      guard (not (null outer_tvs) && not (null rest))
      let parse_tail [Left c_tv, Right in_ty] = Just ([], Just c_tv, in_ty)
          parse_tail [Right in_ty]            = Just ([], Nothing, in_ty)
          parse_tail (Right a : more@(_ : _)) = do
            (as, mb_c, in_ty) <- parse_tail more
            pure (a : as, mb_c, in_ty)
          parse_tail _ = Nothing
      (val_args, mb_inner_tv, in_ty) <- parse_tail rest
      pure (outer_tvs, val_args, mb_inner_tv, in_ty)

    untyped_cb ty = do
      ([Right dom], cod) <- splitMixedPiTys ty
      (i_tc, [m]) <- splitTyConApp_maybe dom
      (o_tc, [n]) <- splitTyConApp_maybe cod
      guard (isIndexedAlgDomain dom && isIndexedAlgDomain cod)
      pure (i_tc, m, o_tc, n)

    typed_cb ty = do
      ([Left s_tv, Right dom], cod) <- splitMixedPiTys ty
      (i_tc, [g, TyVarTy s1]) <- splitTyConApp_maybe dom
      (o_tc, [d, TyVarTy s2]) <- splitTyConApp_maybe cod
      guard (s1 == s_tv && s2 == s_tv && isIndexedAlgDomain dom && isIndexedAlgDomain cod)
      pure (s_tv, i_tc, g, o_tc, d)

    check_ren = untyped_ren <|> typed_ren
    untyped_ren = do
      (pis, out_ty) <- splitMixedPiTys (varType owner)
      (outer_tvs, [cb_ty], Nothing, in_ty) <- split_outer_and_tail pis
      (index, TyVarTy m_tv, index2, TyVarTy n_tv) <- untyped_cb cb_ty
      (in_tc, [TyVarTy m2]) <- splitTyConApp_maybe in_ty
      (term, [TyVarTy n2]) <- splitTyConApp_maybe out_ty
      guard (index == index2 && index == in_tc && index /= term &&
             m2 == m_tv && n2 == n_tv && same_tv_set outer_tvs [m_tv, n_tv])
      ([_, _], [rho_v, x_v], body) <- function (betaReduceTypeApps rhs)
      (con, [call]) <- constructorCall body
      (Core.Var fn_v, [Core.Var arg_v]) <- application call
      guard (DC.dataConTyCon con == term && lazyConstructor con && fn_v == rho_v && arg_v == x_v)
      pure (SigmaRen (UntypedSigmaShape outer_tvs [m_tv, n_tv] index Nothing term) con)

    typed_ren = do
      (pis, out_ty) <- splitMixedPiTys (varType owner)
      (outer_tvs, [cb_ty], mb_inner_tv, in_ty) <- split_outer_and_tail pis
      (_, index, TyVarTy g_tv, index2, TyVarTy d_tv) <- typed_cb cb_ty
      (in_tc, [TyVarTy g2, TyVarTy b_tv]) <- splitTyConApp_maybe in_ty
      (term, [TyVarTy d2, TyVarTy b2]) <- splitTyConApp_maybe out_ty
      let all_tvs = outer_tvs ++ maybeToList mb_inner_tv
      guard (index == index2 && index == in_tc && index /= term &&
             g2 == g_tv && d2 == d_tv && b2 == b_tv &&
             maybe True (== b_tv) mb_inner_tv &&
             same_tv_set all_tvs [g_tv, d_tv, b_tv])
      let (bndrs, body) = collectMixedBinders (betaReduceTypeApps rhs)
      [rho_v, x_v] <- pure (filter isValueVar bndrs)
      guard (length (filter isTyVar bndrs) == 3 && length bndrs == 5)
      (con, [call]) <- constructorCall body
      (Core.Var fn_v, [Core.Var arg_v]) <- application call
      guard (DC.dataConTyCon con == term && lazyConstructor con && fn_v == rho_v && arg_v == x_v)
      pure (SigmaRen (TypedSigmaShape outer_tvs mb_inner_tv [g_tv, d_tv, b_tv] index Nothing term) con)

    check_cons = untyped_cons <|> typed_cons
    untyped_cons = do
      (pis, out_ty) <- splitMixedPiTys (varType owner)
      (outer_tvs, [a1_ty, a2_ty], Nothing, in_ty) <- split_outer_and_tail pis
      (term, [TyVarTy n_tv]) <- splitTyConApp_maybe out_ty
      (index, [ext_m]) <- splitTyConApp_maybe in_ty
      (ext_tc, [TyVarTy m_tv]) <- splitTyConApp_maybe ext_m
      guard (isPromotedDataCon ext_tc && all isAlgTyCon [term, index] &&
             same_tv_set outer_tvs [m_tv, n_tv])
      (swapped, index', m1, term', n1) <-
        (if a1_ty `eqType` out_ty
           then (\(i, m, t, n) -> (False, i, m, t, n)) <$> untyped_cb a2_ty
           else Nothing) <|>
        (if a2_ty `eqType` out_ty
           then (\(i, m, t, n) -> (True, i, m, t, n)) <$> untyped_cb a1_ty
           else Nothing)
      guard (index' == index && term' == term && m1 `eqType` mkTyVarTy m_tv && n1 `eqType` mkTyVarTy n_tv)
      [c1, c2] <- pure (tyConDataCons index)
      guard (all lazyConstructor [c1, c2])
      ([_, _], [v1, v2, arg_v], body) <- function (betaReduceTypeApps rhs)
      let (t_var, s_var) = if swapped then (v2, v1) else (v1, v2)
      (hit, miss) <- match_cons_body t_var s_var arg_v c1 c2 body <|>
                     match_cons_body t_var s_var arg_v c2 c1 body
      pure (SigmaCons (UntypedSigmaShape outer_tvs [m_tv, n_tv] index (Just ext_tc) term) swapped hit miss)

    typed_cons = do
      (pis, out_ty) <- splitMixedPiTys (varType owner)
      (outer_tvs, [a1_ty, a2_ty], mb_inner_tv, in_ty) <- split_outer_and_tail pis
      (term, [TyVarTy d_tv, TyVarTy c_tv]) <- splitTyConApp_maybe out_ty
      (index, [ext_g, TyVarTy c_in]) <- splitTyConApp_maybe in_ty
      (ext_tc, [TyVarTy g_tv, TyVarTy a_tv]) <- splitTyConApp_maybe ext_g
      let all_tvs = outer_tvs ++ maybeToList mb_inner_tv
      guard (isPromotedDataCon ext_tc && c_in == c_tv && all isAlgTyCon [term, index] &&
             maybe True (== c_tv) mb_inner_tv &&
             same_tv_set all_tvs [g_tv, d_tv, a_tv, c_tv])
      let expected_t = mkTyConApp term [mkTyVarTy d_tv, mkTyVarTy a_tv]
      (swapped, _, index', g1, term', d1) <-
        (if a1_ty `eqType` expected_t
           then (\(s, i, g, t, d) -> (False, s, i, g, t, d)) <$> typed_cb a2_ty
           else Nothing) <|>
        (if a2_ty `eqType` expected_t
           then (\(s, i, g, t, d) -> (True, s, i, g, t, d)) <$> typed_cb a1_ty
           else Nothing)
      guard (index' == index && term' == term && g1 `eqType` mkTyVarTy g_tv && d1 `eqType` mkTyVarTy d_tv)
      [c1, c2] <- pure (tyConDataCons index)
      guard (all lazyConstructor [c1, c2])
      let (bndrs, body) = collectMixedBinders (betaReduceTypeApps rhs)
      [v1, v2, arg_v] <- pure (filter isValueVar bndrs)
      guard (length (filter isTyVar bndrs) == 4 && length bndrs == 7)
      let (t_var, s_var) = if swapped then (v2, v1) else (v1, v2)
      (hit, miss) <- match_cons_body t_var s_var arg_v c1 c2 body <|>
                     match_cons_body t_var s_var arg_v c2 c1 body
      pure (SigmaCons (TypedSigmaShape outer_tvs mb_inner_tv [g_tv, d_tv, a_tv, c_tv] index (Just ext_tc) term)
              swapped hit miss)

    match_cons_body t_var s_var arg_v hit miss body = do
      rows <- caseOn arg_v body
      [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
      [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
      guard (length rows == 2 && all (not . isValueVar) hit_bs)
      Core.Var hv <- branchBody hit_bs hit_rhs
      guard (hv == t_var)
      [older] <- pure (filter isValueVar miss_bs)
      mbody <- branchBody miss_bs miss_rhs
      (Core.Var fn_v, [arg_e]) <- application mbody
      Core.Var in_v <- branchBody miss_bs arg_e
      guard (fn_v == s_var && in_v == older)
      pure (hit, miss)

    check_seq = untyped_seq <|> typed_seq
    untyped_seq = do
      (pis, out_ty) <- splitMixedPiTys (varType owner)
      (outer_tvs, [cb1_ty, cb2_ty], Nothing, in_ty) <- split_outer_and_tail pis
      (i1, TyVarTy m_tv, t1, TyVarTy n_tv) <- untyped_cb cb1_ty
      (i2, TyVarTy n2, t2, TyVarTy k_tv) <- untyped_cb cb2_ty
      (in_tc, [TyVarTy m2]) <- splitTyConApp_maybe in_ty
      (term, [TyVarTy k2]) <- splitTyConApp_maybe out_ty
      guard (i1 == i2 && i1 == in_tc && t1 == t2 && t1 == term && i1 /= term &&
             m2 == m_tv && n2 == n_tv && k2 == k_tv &&
             same_tv_set outer_tvs [m_tv, n_tv, k_tv])
      ([_, _, _], [sig_v, tau_v, x_v], raw_body) <- function (betaReduceTypeApps rhs)
      let body = unwrapLocalCallbackLets raw_body
      (Core.Var trav_id, [tau_arg, inner_call]) <- application body
      guard (peelEtaVar tau_v tau_arg && trav_id /= owner)
      (Core.Var sig_fn, [Core.Var x_arg]) <- application inner_call
      guard (sig_fn == sig_v && x_arg == x_v)
      pure (SigmaSeq (UntypedSigmaShape outer_tvs [m_tv, n_tv, k_tv] i1 Nothing term) trav_id)

    typed_seq = do
      (pis, out_ty) <- splitMixedPiTys (varType owner)
      (outer_tvs, [cb1_ty, cb2_ty], mb_inner_tv, in_ty) <- split_outer_and_tail pis
      (_, i1, TyVarTy g_tv, t1, TyVarTy d_tv) <- typed_cb cb1_ty
      (_, i2, TyVarTy d2, t2, TyVarTy e_tv) <- typed_cb cb2_ty
      (in_tc, [TyVarTy g2, TyVarTy c_tv]) <- splitTyConApp_maybe in_ty
      (term, [TyVarTy e2, TyVarTy c2]) <- splitTyConApp_maybe out_ty
      let all_tvs = outer_tvs ++ maybeToList mb_inner_tv
      guard (i1 == i2 && i1 == in_tc && t1 == t2 && t1 == term && i1 /= term &&
             g2 == g_tv && d2 == d_tv && e2 == e_tv && c2 == c_tv &&
             maybe True (== c_tv) mb_inner_tv &&
             same_tv_set all_tvs [g_tv, d_tv, e_tv, c_tv])
      let (bndrs, raw_body) = collectMixedBinders (betaReduceTypeApps rhs)
      [sig_v, tau_v, x_v] <- pure (filter isValueVar bndrs)
      guard (length (filter isTyVar bndrs) == 4 && length bndrs == 7)
      let body = unwrapLocalCallbackLets raw_body
      (Core.Var trav_id, [tau_arg, inner_call]) <- application body
      guard (peelEtaVar tau_v tau_arg && trav_id /= owner)
      (Core.Var sig_fn, [Core.Var x_arg]) <- application inner_call
      guard (sig_fn == sig_v && x_arg == x_v)
      pure (SigmaSeq (TypedSigmaShape outer_tvs mb_inner_tv [g_tv, d_tv, e_tv, c_tv] i1 Nothing term) trav_id)

-- | Test whether `raw_e` is a direct or eta-expanded reference to `target`.
peelEtaVar :: Id -> CoreExpr -> Bool
peelEtaVar target raw_e = isJust $ do
    e <- weakeningBody raw_e
    let (ebndrs, raw_ebody) = collectMixedBinders (unwrapLocalCallbackLets e)
    ebody <- weakeningBody raw_ebody
    (raw_fn, args) <- pure (Core.collectArgs ebody)
    Core.Var fn <- weakeningBody raw_fn
    guard (fn == target && length args == length ebndrs)
    zipWithM_ (\b a -> case (isTyVar b, a) of
                 (True, Core.Type (TyVarTy tv)) -> guard (tv == b)
                 (False, raw_v) -> do
                   Core.Var v <- weakeningBody raw_v
                   guard (v == b)
                 _ -> Nothing) ebndrs args

-- | Resolve a CSE alias when `-O2` rewrites `actual = other_wrapper` of identical type.
resolveForwardCseAlias :: [(Id, CoreExpr)] -> Id -> CoreExpr -> (Maybe Id, CoreExpr)
resolveForwardCseAlias binds actual rhs
  | (av, ar) : _ <- [ (v, r)
                    | (v, r) <- binds
                    , idName v /= idName actual
                    , varType v `eqType` varType actual
                    , peelEtaVar v rhs ]
  = (Just av, ar)
  | otherwise = (Nothing, rhs)

-- | Publish a nominal family for a certified sigma-calculus combinator.
publishSigmaComb :: Bool -> (TyCon -> Bool) -> (Id -> Maybe TyCon) -> (Id, SigmaCombSpec, [Id])
                 -> TcM (Maybe (Id, [TyCon]))
publishSigmaComb is_early public resolveTable (owner, spec, helpers) = runMaybeT $ case spec of
    SigmaRen (UntypedSigmaShape outer_tvs [m_tv, n_tv] index _ term) var_con -> do
      (subst, fresh_outer) <- lift (freshenTyVarBndrs outer_tvs)
      let inst tv = substTyUnchecked subst (mkTyVarTy tv)
          mk_t = inst m_tv
          nk_t = inst n_tv
          in_k = mkTyConApp index [mk_t]
          out_k = mkTyConApp term [nk_t]
          outer_tys = mkTyVarTys fresh_outer
      r_var <- lift (freshVar (pickFresh1 fresh_outer "r") (untypedRenamingKind index mk_t nk_t))
      x_var <- lift (freshVar "x" in_k)
      let r = mkTyVarTy r_var
          x = mkTyVarTy x_var
          binders = mkNamedTyConBinders Specified fresh_outer ++ map mkAnonTyConBinder [r_var, x_var]
          branches = do
            out <- conAt var_con out_k [mkAppTys r [x]]
            pure [renamingBranch (outer_tys ++ [r, x]) out]
      publishRenamingTable is_early public owner helpers binders out_k (const branches)

    SigmaRen (TypedSigmaShape outer_tvs mb_inner_tv [g_tv, d_tv, b_tv] index _ term) var_con -> do
      let all_tvs = outer_tvs ++ maybeToList mb_inner_tv
      (subst, fresh_all) <- lift (freshenTyVarBndrs all_tvs)
      let (fresh_outer, fresh_inner) = splitAt (length outer_tvs) fresh_all
          inst tv = substTyUnchecked subst (mkTyVarTy tv)
          gk = inst g_tv
          dk = inst d_tv
          bk = inst b_tv
          in_k = mkTyConApp index [gk, bk]
          out_k = mkTyConApp term [dk, bk]
          outer_tys = mkTyVarTys fresh_outer
          inner_tys = mkTyVarTys fresh_inner
      rank <- lift (freshVar "a" (varType b_tv))
      r_var <- lift (freshVar (pickFresh1 (rank : fresh_all) "r") (renamingKind index rank gk dk))
      x_var <- lift (freshVar "x" in_k)
      let r = mkTyVarTy r_var
          x = mkTyVarTy x_var
          binders = mkNamedTyConBinders Specified fresh_outer ++
                    [mkAnonTyConBinder r_var] ++
                    mkNamedTyConBinders Specified fresh_inner ++
                    [mkAnonTyConBinder x_var]
          branches = do
            out <- conAt var_con out_k [mkAppTys r [bk, x]]
            pure [renamingBranch (outer_tys ++ [r] ++ inner_tys ++ [x]) out]
      publishRenamingTable is_early public owner helpers binders out_k (const branches)

    SigmaCons (UntypedSigmaShape outer_tvs [m_tv, n_tv] index (Just ext) term) swapped hit miss -> do
      (subst, fresh_outer) <- lift (freshenTyVarBndrs outer_tvs)
      let inst tv = substTyUnchecked subst (mkTyVarTy tv)
          mk_t = inst m_tv
          nk_t = inst n_tv
          dom_k = mkTyConApp index [mkTyConApp ext [mk_t]]
          out_k = mkTyConApp term [nk_t]
          outer_tys = mkTyVarTys fresh_outer
      t_var <- lift (freshVar "t" out_k)
      p_var <- lift (freshVar (pickFresh1 fresh_outer "p") (untypedCallbackKind index term mk_t nk_t))
      in_var <- lift (freshVar "input" dom_k)
      x_var <- lift (freshVar "x" (mkTyConApp index [mk_t]))
      let val_bndrs = if swapped then [p_var, t_var] else [t_var, p_var]
          val_tys = mkTyVarTys val_bndrs
          binders = mkNamedTyConBinders Specified fresh_outer ++ map mkAnonTyConBinder (val_bndrs ++ [in_var])
          branches = do
            z_pat <- conAt hit dom_k []
            s_pat <- conAt miss dom_k [mkTyVarTy x_var]
            let s_rhs = mkAppTys (mkTyVarTy p_var) [mkTyVarTy x_var]
            guard (typeKind s_rhs `eqType` out_k)
            pure [ renamingBranch (outer_tys ++ val_tys ++ [z_pat]) (mkTyVarTy t_var)
                 , renamingBranch (outer_tys ++ val_tys ++ [s_pat]) s_rhs ]
      publishRenamingTable is_early public owner helpers binders out_k (const branches)

    SigmaCons (TypedSigmaShape outer_tvs mb_inner_tv [g_tv, d_tv, a_tv, c_tv] index (Just ext) term)
              swapped hit miss -> do
      let all_tvs = outer_tvs ++ maybeToList mb_inner_tv
      (subst, fresh_all) <- lift (freshenTyVarBndrs all_tvs)
      let (fresh_outer, fresh_inner) = splitAt (length outer_tvs) fresh_all
          inst tv = substTyUnchecked subst (mkTyVarTy tv)
          gk = inst g_tv
          dk = inst d_tv
          ak = inst a_tv
          ck = inst c_tv
          ext_g = mkTyConApp ext [gk, ak]
          dom_k = mkTyConApp index [ext_g, ck]
          out_k = mkTyConApp term [dk, ck]
      rank <- lift (freshVar "b" (varType c_tv))
      t_var <- lift (freshVar "t" (mkTyConApp term [dk, ak]))
      p_var <- lift (freshVar (pickFresh1 (rank : fresh_all) "p") (closureCallbackKind index term rank gk dk))
      in_var <- lift (freshVar "input" dom_k)
      x_var <- lift (freshVar "x" (mkTyConApp index [gk, ck]))
      let val_bndrs = if swapped then [p_var, t_var] else [t_var, p_var]
          val_tys = mkTyVarTys val_bndrs
          binders = mkNamedTyConBinders Specified fresh_outer ++
                    map mkAnonTyConBinder val_bndrs ++
                    mkNamedTyConBinders Specified fresh_inner ++
                    [mkAnonTyConBinder in_var]
          lhs_for target_s pat =
            case mb_inner_tv of
              Just _ -> mkTyVarTys fresh_outer ++ val_tys ++ [target_s, pat]
              Nothing ->
                [if mkTyVarTy tv `eqType` ck then target_s else mkTyVarTy tv | tv <- fresh_outer] ++
                val_tys ++ [pat]
          branches = do
            z_pat <- conAt hit (mkTyConApp index [ext_g, ak]) []
            s_pat <- conAt miss dom_k [mkTyVarTy x_var]
            let s_rhs = mkAppTys (mkTyVarTy p_var) [ck, mkTyVarTy x_var]
            guard (typeKind s_rhs `eqType` out_k)
            pure [ renamingBranch (lhs_for ak z_pat) (mkTyVarTy t_var)
                 , renamingBranch (lhs_for ck s_pat) s_rhs ]
      publishRenamingTable is_early public owner helpers binders out_k (const branches)

    SigmaSeq (UntypedSigmaShape outer_tvs [m_tv, n_tv, k_tv] index _ term) trav_id -> do
      subst_tc <- MaybeT (pure (resolveTable trav_id))
      (subst, fresh_outer) <- lift (freshenTyVarBndrs outer_tvs)
      let inst tv = substTyUnchecked subst (mkTyVarTy tv)
          mk_t = inst m_tv
          nk_t = inst n_tv
          kk_t = inst k_tv
          in_k = mkTyConApp index [mk_t]
          out_k = mkTyConApp term [kk_t]
          outer_tys = mkTyVarTys fresh_outer
          (p_nm, q_nm) = pickFresh2 fresh_outer
      p_var <- lift (freshVar p_nm (untypedCallbackKind index term mk_t nk_t))
      q_var <- lift (freshVar q_nm (untypedCallbackKind index term nk_t kk_t))
      x_var <- lift (freshVar "x" in_k)
      let p = mkTyVarTy p_var
          q = mkTyVarTy q_var
          x = mkTyVarTy x_var
          rhs_ty = mkTyConApp subst_tc [nk_t, kk_t, q, mkAppTys p [x]]
          binders = mkNamedTyConBinders Specified fresh_outer ++ map mkAnonTyConBinder [p_var, q_var, x_var]
          branches = do
            guard (typeKind rhs_ty `eqType` out_k)
            pure [renamingBranch (outer_tys ++ [p, q, x]) rhs_ty]
      publishRenamingTable is_early public owner helpers binders out_k (const branches)

    SigmaSeq (TypedSigmaShape outer_tvs mb_inner_tv [g_tv, d_tv, e_tv, c_tv] index _ term) trav_id -> do
      subst_tc <- MaybeT (pure (resolveTable trav_id))
      let all_tvs = outer_tvs ++ maybeToList mb_inner_tv
      (subst, fresh_all) <- lift (freshenTyVarBndrs all_tvs)
      let (fresh_outer, fresh_inner) = splitAt (length outer_tvs) fresh_all
          inst tv = substTyUnchecked subst (mkTyVarTy tv)
          gk = inst g_tv
          dk = inst d_tv
          ek = inst e_tv
          ck = inst c_tv
          in_k = mkTyConApp index [gk, ck]
          out_k = mkTyConApp term [ek, ck]
          outer_tys = mkTyVarTys fresh_outer
          inner_tys = mkTyVarTys fresh_inner
      rank1 <- lift (freshVar "a" (varType c_tv))
      rank2 <- lift (freshVar "b" (varType c_tv))
      let (p_nm, q_nm) = pickFresh2 (rank1 : rank2 : fresh_all)
      p_var <- lift (freshVar p_nm (closureCallbackKind index term rank1 gk dk))
      q_var <- lift (freshVar q_nm (closureCallbackKind index term rank2 dk ek))
      x_var <- lift (freshVar "x" in_k)
      let p = mkTyVarTy p_var
          q = mkTyVarTy q_var
          x = mkTyVarTy x_var
          rhs_ty = mkTyConApp subst_tc [dk, ek, ck, q, mkAppTys p [ck, x]]
          binders = mkNamedTyConBinders Specified fresh_outer ++
                    [mkAnonTyConBinder p_var, mkAnonTyConBinder q_var] ++
                    mkNamedTyConBinders Specified fresh_inner ++
                    [mkAnonTyConBinder x_var]
          branches = do
            guard (typeKind rhs_ty `eqType` out_k)
            pure [renamingBranch (outer_tys ++ [p, q] ++ inner_tys ++ [x]) rhs_ty]
      publishRenamingTable is_early public owner helpers binders out_k (const branches)

    _ -> MaybeT (pure Nothing)
  where
    pickFresh1 :: [TyVar] -> String -> String
    pickFresh1 taken preferred =
      case filter (`notElem` used) (preferred : ["p", "q", "r", "f", "g", "h"]) of
        c : _ -> c
        []    -> preferred
      where
        used = map (occNameString . nameOccName . getName) taken

    pickFresh2 :: [TyVar] -> (String, String)
    pickFresh2 taken =
      let c1 = pickFresh1 taken "p"
          used = c1 : map (occNameString . nameOccName . getName) taken
          c2 = case filter (`notElem` used) ["q", "r", "f", "g", "h", "p"] of
                 c : _ -> c
                 []    -> "q"
      in (c1, c2)

-- | Certified forwarding wrapper (`substSingle`, `substDouble`, `substSingleU`),
-- recording: `(is_typed, trav_id, map_id, core_tvs, map_ty_args, (body_param_idx, cap_param_indices))`.
data SubstForwardSpec =
  SubstForwardSpec Bool Id Id [TyVar] [Type] (Int, [Int])

-- | Peel rank-2 / value eta wrappers around `map_id @ty_1 ... @ty_m cap_1 ... cap_k`.
peelSubstMapCall :: [Id] -> CoreExpr -> Maybe (Id, [Type], [Id])
peelSubstMapCall expected_caps raw_cb = do
    cb0 <- weakeningBody raw_cb
    let (eta_bndrs, inner) = collectMixedBinders (unwrapLocalCallbackLets cb0)
    guard (length eta_bndrs <= 2)
    (Core.Var map_id, all_args) <- pure (Core.collectArgs inner)
    guard (length all_args >= length eta_bndrs)
    let (prefix_args, suffix_args) = splitAt (length all_args - length eta_bndrs) all_args
    zipWithM_ (\b a -> case (isTyVar b, a) of
                 (True, Core.Type (TyVarTy tv)) -> guard (tv == b)
                 (False, Core.Var v)            -> guard (v == b)
                 _                              -> Nothing) eta_bndrs suffix_args
    let prefix_fvs = foldl' unionVarSet (exprFreeVars (Core.Var map_id)) (map exprFreeVars prefix_args)
    guard (all (not . (`elemVarSet` prefix_fvs)) eta_bndrs)
    let (ty_args_raw, val_args_raw) = span Core.isTypeArg prefix_args
    map_tys <- traverse indexedTypeArg ty_args_raw
    cap_vars <- traverse (\case { Core.Var v -> Just v; _ -> Nothing }) val_args_raw
    guard (not (null cap_vars) &&
           length cap_vars == length expected_caps &&
           mkVarSet cap_vars == mkVarSet expected_caps)
    pure (map_id, map_tys, cap_vars)

-- | Extract a forwarding-wrapper specification from unoptimized Core.
checkedSubstForwardOrig :: Id -> CoreExpr -> Maybe SubstForwardSpec
checkedSubstForwardOrig owner rhs = do
    guard (exprType rhs `eqType` varType owner)
    (tvs, params, raw_call) <- function (betaReduceTypeApps rhs)
    guard (not (null tvs) && length params >= 2)
    let call = unwrapLocalCallbackLets raw_call
    (Core.Var trav_id, [raw_cb, Core.Var body_var]) <- application call
    guard (trav_id /= owner)
    body_idx <- elemIndex body_var params
    let expected_caps = [p | p <- params, p /= body_var]
    (map_id, map_tys, cap_vars) <- peelSubstMapCall expected_caps raw_cb
    guard (map_id /= owner && map_id /= trav_id)
    cap_indices <- traverse (`elemIndex` params) cap_vars
    let res_ty = exprType call
        body_ty = varType body_var
    is_typed <- case (splitTyConApp_maybe body_ty, splitTyConApp_maybe res_ty) of
      (Just (bt, [_]), Just (rt, [_])) -> False <$ guard (bt == rt && isAlgTyCon bt)
      (Just (bt, [_, bs]), Just (rt, [_, rs])) ->
        True <$ guard (bt == rt && isAlgTyCon bt && bs `eqType` rs)
      _ -> Nothing
    pure (SubstForwardSpec is_typed trav_id map_id tvs map_tys (body_idx, cap_indices))

-- | Verify that final Core preserves the forwarding call or its exact inlined map.
checkedSubstForwardFinal :: [(Id, CoreExpr)] -> Id -> CoreExpr -> SubstForwardSpec -> Maybe SubstMapSpec -> Bool
checkedSubstForwardFinal _ actual final_rhs
  (SubstForwardSpec _ orig_trav orig_map orig_tvs _ (orig_body_idx, orig_cap_indices)) mb_map_spec =
    isJust $ do
      guard (exprType final_rhs `eqType` varType actual)
      (tvs, params, raw_call) <- function (betaReduceTypeApps final_rhs)
      guard (length tvs == length orig_tvs && orig_body_idx < length params)
      let call = unwrapLocalCallbackLets raw_call
          body_var = params !! orig_body_idx
          expected_caps = [p | p <- params, p /= body_var]
      (Core.Var fin_trav, [raw_cb, Core.Var fin_body]) <- application call
      guard (idName fin_trav == idName orig_trav && fin_body == body_var)
      let direct_call = do
            (fin_map, _, cap_vars) <- peelSubstMapCall expected_caps raw_cb
            fin_indices <- traverse (`elemIndex` params) cap_vars
            guard (idName fin_map == idName orig_map && fin_indices == orig_cap_indices)
          inlined_call = do
            spec <- mb_map_spec
            let passed_caps = [params !! i | i <- orig_cap_indices]
                (_, cb_body_bndrs, cb_body) =
                  let (bs, b) = collectMixedBinders (betaReduceTypeApps raw_cb)
                  in (filter isTyVar bs, filter isValueVar bs, b)
            [arg_var] <- pure cb_body_bndrs
            let (hit, miss, expected_var_con, expected_perm) = case spec of
                  UntypedSubstMapSpec _ _ _ _ _ vc h m p -> (h, m, vc, p)
                  TypedSubstMapSpec _ _ _ _ _ _ _ _ _ _ vc h m p -> (h, m, vc, p)
            (inlined_perm, vc') <-
              matchSubstMapOutcomes hit miss [] arg_var passed_caps (length passed_caps) cb_body
            guard (vc' == expected_var_con &&
                   [orig_cap_indices !! j | j <- inlined_perm] ==
                   [orig_cap_indices !! j | j <- expected_perm])
      direct_call <|> inlined_call

-- | Publish `T:substSingle` / `T:substDouble` directly via `T:subst` and `T:singleSubst` / `T:doubleSubst`.
publishSubstForward :: Bool -> (TyCon -> Bool) -> (Id -> Maybe TyCon) -> (Id, SubstForwardSpec, [Id])
                    -> TcM (Maybe (Id, [TyCon]))
publishSubstForward is_early public resolveTable
  (owner, SubstForwardSpec is_typed trav_id map_id core_tvs map_tys (body_idx, cap_indices), helpers) =
    runMaybeT $ do
      subst_tc <- MaybeT (pure (resolveTable trav_id))
      map_tc <- MaybeT (pure (resolveTable map_id))
      let (decl_tvs_with_flags, tau) = splitForAllForAllTyBinders (varType owner)
          decl_tvs = [tv | (_, Bndr tv Specified) <- decl_tvs_with_flags]
      guard (length decl_tvs == length decl_tvs_with_flags && length decl_tvs == length core_tvs)
      (_, scaled_doms, decl_res) <- pure (splitFunTys tau)
      let doms = map scaledThing scaled_doms
      guard (length doms == length cap_indices + 1 && body_idx < length doms)
      (subst, fresh_tvs) <- lift (freshenTyVarBndrs decl_tvs)
      let inst_decl = substTyUnchecked subst
          inst_core = substTyWith core_tvs (mkTyVarTys fresh_tvs)
          inst_doms = map inst_decl doms
          inst_res = inst_decl decl_res
          inst_map_tys = map inst_core map_tys
      param_vars <- lift (traverse (freshVar "arg") inst_doms)
      let param_tys = mkTyVarTys param_vars
          body_ty_val = param_tys !! body_idx
          cap_ty_vals = [param_tys !! i | i <- cap_indices]
          partial_map = mkTyConApp map_tc (inst_map_tys ++ cap_ty_vals)
          binders = mkNamedTyConBinders Specified fresh_tvs ++ map mkAnonTyConBinder param_vars
      rhs_ty <- if is_typed
        then do
          (term_tc1, [src_ctx, res_sort]) <- MaybeT (pure (splitTyConApp_maybe (inst_doms !! body_idx)))
          (term_tc2, [tgt_ctx, res_sort2]) <- MaybeT (pure (splitTyConApp_maybe inst_res))
          guard (term_tc1 == term_tc2 && res_sort `eqType` res_sort2)
          pure (mkTyConApp subst_tc [src_ctx, tgt_ctx, res_sort, partial_map, body_ty_val])
        else do
          (term_tc1, [src_ctx]) <- MaybeT (pure (splitTyConApp_maybe (inst_doms !! body_idx)))
          (term_tc2, [tgt_ctx]) <- MaybeT (pure (splitTyConApp_maybe inst_res))
          guard (term_tc1 == term_tc2)
          pure (mkTyConApp subst_tc [src_ctx, tgt_ctx, partial_map, body_ty_val])
      guard (typeKind rhs_ty `eqType` inst_res)
      let lhs_tys = mkTyVarTys fresh_tvs ++ param_tys
          branches = Just [renamingBranch lhs_tys rhs_ty]
      publishRenamingTable is_early public owner helpers binders inst_res (const branches)

allocateUntypedFamilies :: Bool -> UntypedFamilyInputs -> TcM (Maybe (TyCon, [TyCon]))
allocateUntypedFamilies is_early (UntypedFamilyInputs owner arity context term ext lookup_tycon _ _ var hit miss recipes) = do
    (closure, constructors) <- allocateUntypedClosure owner context term ext
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
        let gk = mkTyVarTy g
            dk = mkTyVarTy d
            termAt p = mkTyConApp term [p]
            ctxAt p = mkTyConApp ext [p]
            lookupAt p = mkTyConApp lookup_tycon [p]
            closureAt p q = mkTyConApp closure [p, q]
            ctx_g = ctxAt gk
            ctx_d = ctxAt dk
        cv <- freshVar "c" (closureAt gk dk)
        tree <- freshVar "t" (termAt gk)
        field <- freshVar "x" (lookupAt gk)
        carried <- freshVar "w" (termAt dk)
        let body_context = foldl' (\k _ -> ctxAt k) gk [1..arity]
        body <- freshVar "n" (termAt body_context)
        args <- traverse (\_ -> freshVar "v" (termAt gk)) [1..arity]
        x_d <- freshVar "older" (lookupAt dk)
        let prepare (ctor, exten) = do
              let (ctor_tvs, tel) = splitForAllTyVars (tyConKind (DC.promoteDataCon ctor))
                  (_, arg_kinds, res) = splitFunTys tel
              (renaming, tvs) <- freshenTyVarBndrs (map snd ctor_tvs)
              vars <- traverse (freshVar "arg" . substTyUnchecked renaming . scaledThing) arg_kinds
              let result = substTyUnchecked renaming res
              env <- freshVar "env" $ case splitTyConApp_maybe result of
                Just (_, [src]) -> closureAt src dk
                _ -> liftedTypeKind
              pure (ctor, exten, tvs, vars, result, env)
        apply_inputs <- traverse prepare ((var, []) : recipes)
        let c = mkTyVarTy cv
            x = mkTyVarTy field
            w = mkTyVarTy carried
            outputs = termAt dk
            prefix = mkNamedTyConBinders Specified [g, d]
            apply_apply s t env val = mkTyConApp apply [s, t, env, val]
            lookup_apply s t env val = mkTyConApp lookup_fam [s, t, env, val]
            apply = mkFamily apply_name apply_axiom (prefix ++ map mkAnonTyConBinder [cv, tree])
                        outputs (fromMaybe [] apply_branches)
            lookup_fam = mkFamily lookup_name lookup_axiom (prefix ++ map mkAnonTyConBinder [cv, field])
                        outputs (fromMaybe [] lookup_branches)
            applyBranch (ctor, exten, tvs, vars, res, env_var) = do
                (actual_term, [src]) <- splitTyConApp_maybe res
                guard (actual_term == term && (ctor == var || length exten == length vars))
                let fields = mkTyVarTys vars
                    lhs = mkTyConApp (DC.promoteDataCon ctor) (mkTyVarTys tvs ++ fields)
                    env = mkTyVarTy env_var
                    liftField 0 source = do
                      guard (source `eqType` src)
                      pure (dk, env)
                    liftField depth source = do
                      (scope, [outer]) <- splitTyConApp_maybe source
                      guard (scope == ext)
                      (target, inner) <- liftField (depth - 1) outer
                      let extended = ctxAt target
                      lifted <- conAt lift_con (closureAt source extended) [inner]
                      pure (extended, lifted)
                    use depth value = do
                      (same, [source]) <- splitTyConApp_maybe (typeKind value)
                      guard (same == term)
                      (target, closure_value) <- liftField depth source
                      pure (apply_apply source target closure_value value)
                out <- if ctor == var
                         then do [lookup_var] <- pure fields
                                 pure (lookup_apply src dk env lookup_var)
                         else zipWithM use exten fields >>= conAt ctor (termAt dk)
                guard (typeKind lhs `eqType` res && typeKind out `eqType` termAt dk)
                let branch_vars = tyCoVarsOfTypesWellScoped [src, dk, env, lhs]
                pure (mkBranch branch_vars [src, dk, env, lhs] out)
            apply_branches = traverse applyBranch apply_inputs
            lookup_branches = do
                r <- conAt replace (closureAt ctx_d dk) [w]
                z_in <- conAt hit (lookupAt ctx_d) []
                s_in <- conAt miss (lookupAt ctx_d) [mkTyVarTy x_d]
                v_d <- conAt var (termAt dk) [mkTyVarTy x_d]
                s <- conAt shift (closureAt gk ctx_g) []
                s_out <- conAt miss (lookupAt ctx_g) [x]
                v_out <- conAt var (termAt ctx_g) [s_out]
                l <- conAt lift_con (closureAt ctx_g ctx_d) [c]
                z_g <- conAt hit (lookupAt ctx_g) []
                z_d <- conAt hit (lookupAt ctx_d) []
                v0 <- conAt var (termAt ctx_d) [z_d]
                sx <- conAt miss (lookupAt ctx_g) [x]
                s_d <- conAt shift (closureAt dk ctx_d) []
                let weaken = apply_apply dk ctx_d s_d (lookup_apply gk dk c x)
                next <- conAt replace_next (closureAt ctx_g dk) [c, w]
                pure [ mkBranch [d, carried] [ctx_d, dk, r, z_in] w
                     , mkBranch [d, carried, x_d] [ctx_d, dk, r, s_in] v_d
                     , mkBranch [g, field] [gk, ctx_g, s, x] v_out
                     , mkBranch [g, d, cv] [ctx_g, ctx_d, l, z_g] v0
                     , mkBranch [g, d, cv, field] [ctx_g, ctx_d, l, sx] weaken
                     , mkBranch [g, d, cv, carried] [ctx_g, dk, next, z_g] w
                     , mkBranch [g, d, cv, carried, field] [ctx_g, dk, next, sx]
                         (lookup_apply gk dk c x) ]
        pure $ do
            apply_equations <- apply_branches
            lookup_equations <- lookup_branches
            first_arg : rest <- pure (mkTyVarTys args)
            let initial_context = ctxAt gk
                addReplacement (src, env) value = do
                    let next_context = ctxAt src
                    next <- conAt replace_next (closureAt next_context gk) [env, value]
                    pure (next_context, next)
            first <- conAt replace (closureAt initial_context gk) [first_arg]
            (_, replacements) <- foldM addReplacement (initial_context, first) rest
            let result = termAt gk
                t = apply_apply body_context gk replacements (mkTyVarTy body)
                value_vars = body : args
                vars = g : value_vars
                params = mkNamedTyConBinders Specified [g] ++ map mkAnonTyConBinder value_vars
                root_equation = mkBranch vars (mkTyVarTys vars) t
                root = mkFamily root_name root_axiom params result [root_equation]
            guard (all validBranch (root_equation : apply_equations ++ lookup_equations))
            pure (root, [root, closure, apply, lookup_fam])
      _ -> pure Nothing
  where
    localName prefix = newImplicitBinder (idName owner) (mkTcOcc . (prefix ++) . occNameString)

allocateUntypedClosure :: Id -> Kind -> TyCon -> TyCon -> TcM (TyCon, [DC.DataCon])
allocateUntypedClosure owner context_kind term ext = do
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
    let universals = [source, target]
        source_ty = mkTyVarTy source
        target_ty = mkTyVarTy target
        incoming_ty = mkTyVarTy incoming
        outgoing_ty = mkTyVarTy outgoing
        extend ctx = mkTyConApp ext [ctx]
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
        replace <- ctor replace_name [] [target]
                     [DC.mkEqSpec source (extend target_ty)]
                     [mkTyConApp term [target_ty]] (applied (extend target_ty) target_ty)
        shift <- ctor shift_name [] [source]
                   [DC.mkEqSpec target (extend source_ty)] [] (applied source_ty (extend source_ty))
        lift_con <- ctor lift_name [incoming, outgoing] [incoming, outgoing]
                      [DC.mkEqSpec source (extend incoming_ty), DC.mkEqSpec target (extend outgoing_ty)]
                      [applied incoming_ty outgoing_ty] (applied (extend incoming_ty) (extend outgoing_ty))
        replace_next <- ctor next_name [incoming] [incoming, target]
                          [DC.mkEqSpec source (extend incoming_ty)]
                          [applied incoming_ty target_ty, mkTyConApp term [target_ty]]
                          (applied (extend incoming_ty) target_ty)
        pure (mkAlgTyCon closure_name kind binders 0 liftedTypeKind [Nominal, Nominal] Nothing []
                (mkLevPolyDataTyConRhs True True [replace, shift, lift_con, replace_next])
                (VanillaAlgTyCon closure_rep) True)
    pure (closure, tyConDataCons closure)

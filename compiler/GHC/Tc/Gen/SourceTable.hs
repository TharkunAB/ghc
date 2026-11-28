-- | Certify and publish nominal closed type-family equations (`SourceTable`)
-- from desugared and post-Simplifier Core bindings, dictionary methods, and
-- fusion-guarded list functions.
--
-- See Note [Certifying source equations from Core].
module GHC.Tc.Gen.SourceTable
  ( recordSourceUses, recordUnfoldedSourceUses, sourceTableTyCons, sourceCaseTables
  , sourceMapperType, sourceMapperTable, sourceCharacterTables, sourceRetainedWitnessTables
  , publishRetainedRecordWitness, publishPlainRecordFieldSelector
  , checkLocalRetainedWitnessExports, isMonomorphicRetainedWitness
  , finalCoreMappers, verifyFinalCoreMapper, failedFinalListTables, finalCoreSourceFamilies
  , isFinalCoreIdentity, indexedProjectionSource, isFinalCoreReverseWrapper
  , sourcePairBuilderField, lookupObservedPairField, lookupPairObserver, lookupPairRecipe
  , canObservePairSource, sourceRuleOwners, sourceTableOccurrence, tcLookupSourceTableId
  , sourceTableCallees, sourceTableDependencies, sourceTableTypeUses, sourceTableFamilyUse
  , sourceTableFamilyAppUse, sourceTableReferenceUse, sourceCaseKindUse, sourceCaseVisibleArity
  , isSourceConstructorCase, sourceTableOwner, monomorphicEnumCaseArity, groundFirstHead
  , sourceLocalBooleanCases, isFinalStructuralSource, isStructuralSourceTable
  , isNominalStructuralSource, isSimpleNominalTable, isLargeElimTable, isLargeElimResultKind
  , pruneDeadNonRecLets, isSafeLocalUninterpretedRhs, toKindMatchableArrows, rebuildSourceFamilyWith, tidySourceAxBranch
  , visibleSourceMethodRules, unloadedSourceRuleHeads, hasSourceMethodRule, hasSourceMethodRuleAt
  , sourceEquationDatatypes, publicCaseType, ordinaryImport
  , sourceBinarySelector, sourceMapSelector, lookupSourceUnaryMethod, lookupSourceBinaryMethod, lookupSourceContainerMethod
  , lookupSourceGroundDFun, lookupSourceCheckedMapMethod, publishListFoldMethods
  , keepCheckedMapWorkers, publishCheckedMapMethods, addSourceMethods, certifyLocalInstanceMethods, sourceRuleOwnersAt
  , recordSourceMethodUse, recordSourceLiteralUse, sourceLiteralWorkers
  , lookupBootModuleIdMaybe, isSourceFilterGuard
  , OptionalSourceUses, optionalSourceUses, registerOptionalSourceUses
  , checkOptionalSourceUses, checkSourceMethodUses
  , withSourceProofSignature, recordSourceProofAliasUse
  , sourceProofRulesFor, takeSourceProofRulesFor
  , captureSourceMethodProof, closedSourceProofArgs, checkAndInstallSourceProofs
  ) where

import GHC.Prelude

import GHC.Builtin.KnownKeys (eqClassKey, patErrorIdKey)
import GHC.Builtin.KnownOccs (eqClassOpOcc, minusClassOpOcc)
import GHC.Builtin.Modules (gHC_CLASSES, gHC_INTERNAL_BASE, gHC_INTERNAL_NUM_INTEGER, gHC_TYPES, mkGhcInternalModule)
import GHC.Builtin.PrimOps (PrimOp(DataToTagLargeOp, DataToTagSmallOp))
import GHC.Builtin.WiredIn.Types
  ( nilDataCon, consDataCon, boolTy, falseDataCon, trueDataCon, charTy, integerTy, integerTyConName
  , listTyCon, manyDataConTy, matchableDataConTy, mkListTy, mkTupleTy, promotedTupleDataCon, tupleDataCon, tupleTyCon
  , unmatchableDataConTy )
import GHC.Core (CoreRule(..), isAutoRule, ruleActivation)
import qualified GHC.Core as Core
import GHC.Core.CharCase (certDefault, certOwner, certResultType, certRows, certify, charCaseResultType)
import GHC.Core.Class (Class, classAllSelIds, classMethods, className, classTyCon, classTyVars)
import GHC.Core.Coercion
  ( abstractNativeIndices, coercionLKind, coercionRKind, coercionRole, hasCoercionHole
  , isReflCo, mkCoVarCo, mkNomReflCo, mkSymCo, mkTyConAppCo )
import GHC.Core.Coercion.Axiom
  ( ClosedTyFam(..), CoAxBranch, CoAxiomRule(UnbranchedAxiom), coAxiomBranches, coAxiomName, coAxiomRole
  , coAxBranchCoVars, coAxBranchLHS, coAxBranchRHS, coAxBranchRoles, coAxBranchSpan, coAxBranchTyVars, fromBranches )
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon
import GHC.Core.Dependent (hasExistentialExpr, hasNativeDependentExpr, hasNativeDependentExprRhs)
import GHC.Core.FVs (exprSomeFreeVarsList, exprsFreeVars)
import GHC.Core.FamInstEnv (FamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch, reduceTyFamApp_maybe)
import GHC.Core.InstEnv
  ( ClsInst(is_tcs, is_tvs, is_tys), classInstances, instanceCantMatch, instanceDFunId
  , isIncoherent, lookupUniqueInstEnv, roughMatchTcs )
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.OpenWhnf (openDataConFieldTypes)
import GHC.Core.Predicate (classMethodInstTy, getClassPredTys_maybe, isPredTy, mkClassPred)
import GHC.Core.Reduction (reductionCoercion, reductionReducedType)
import GHC.Core.RoughMap (RoughMatchTc(RM_KnownTc))
import GHC.Core.Rules (RuleEnv(..), emptyRuleBase, emptyRuleEnv, extendRuleBaseList, getRules)
import GHC.Core.SimpleOpt (SimpleOpts, defaultSimpleOpts, simpleOptExpr)
import qualified GHC.Core.Subst as CoreSubst
import GHC.Core.Tidy (tidyCbvInfoTop)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCo.FVs (tyCoVarsOfTypeList, tyConsOfTypes)
import GHC.Core.TyCo.Rep (Coercion(AxiomCo, SymCo), Type(TyConApp, AppTy, FunTy, TyVarTy, TermVarTy, DepFunTy, DepDictTy, ForAllTy, ExistsTy, CastTy))
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Unify (alwaysBindTv, tcMatchTys, tcUnifyTys)
import GHC.Core.Utils (exprType, findAlt, mergeAlts, stripTicksE, stripTicksTopE)
import GHC.Data.Bag (emptyBag, isEmptyBag)
import GHC.Data.FastString (fsLit)
import GHC.Data.Graph.Directed (Node(..), SCC(..), stronglyConnCompFromEdgedVerticesUniq)
import GHC.Data.OrdList (fromOL)
import GHC.Driver.Backend (backendGeneratesCode)
import GHC.Driver.Config (initSimpleOpts)
import GHC.Driver.DynFlags (DynFlags(ghcMode, homeUnitId_), isOneShot, unitSettings)
import GHC.Driver.Env (hsc_HUG, hsc_home_unit, hugRuleInterfacesBelow, hugRulesBelow, lookupIfaceByModuleHsc, visibleUnloadedSourceRuleHeads)
import GHC.Driver.Plugins (hasActiveLatePlugins)
import GHC.Driver.Session
  ( GeneralFlag(Opt_EnableRewriteRules, Opt_Hpc, Opt_WriteIfSimplifiedCore, Opt_WriteInterface, Opt_WriteSourceEquations)
  , HasDynFlags(getDynFlags), backend, gopt )
import GHC.Hs
import {-# SOURCE #-} GHC.HsToCore.Binds (dsTopLHsBinds)
import GHC.HsToCore.Errors.Ppr ()
import GHC.HsToCore.Monad (initDsTc)
import GHC.Iface.Env (lookupOrig)
import GHC.Iface.Load (loadInterfaceForName)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Settings (UnitSettings(unitSettings_ghcInternalUnitId))
import GHC.Tc.Errors.Types (RetainedQuantificationError(..), TcRnMessage(..))
import GHC.Tc.Instance.Family (tcGetFamInstEnvs)
import GHC.Tc.TyCl.Build (newImplicitBinder)
import GHC.Tc.Types (SourceCheckState(..), SourceRuleUses(..), TcGblEnv(..), tcVisibleOrphanMods)
import GHC.Tc.Types.BasicTypes (TcBinder(TcIdBndr, TcIdBndr_ExpType), TcTyThing(ATcId, ATcTyCon, tct_id))
import GHC.Tc.Types.ErrCtxt (FunAppCtxtFunArg(..), HsCtxt(..))
import GHC.Tc.Types.Evidence ((<.>), EvBind(..), EvBindsState(..), EvTerm(..), HsWrapper(..), TcEvBinds(..), collectHsWrapBinders, evBindMapBinds, mkWpEvApps, mkWpTyApps)
import GHC.Tc.Types.LclEnv (getLclEnvBinderStack, getLclEnvErrCtxt, getLclEnvTypeEnv)
import GHC.Tc.Utils.Env (tcGetInstEnvs, tcLookupId, tcLookupIdMaybe)
import GHC.Tc.Utils.Instantiate (freshenTyVarBndrs)
import GHC.Tc.Utils.Monad
  ( TcM, addErr, attemptM, checkNoErrs, failM, failWithTc, getEps, getGblEnv, getGlobalRdrEnv, getLclEnv
  , getTcEvBindsMap, getTcEvBindsState, getTopEnv, goptM, keepAlive, liftZonkM, newSysName
  , readTcRef, recordHomeRuleDependencies, setSrcSpan, setSrcSpanA, updTcRef, xoptM )
import GHC.Tc.Utils.TcType (isMetaTyVar, isTyFamFree)
import GHC.Tc.Zonk.TcType (zonkTcType, zonkTcTypes)
import GHC.Tc.Zonk.Type (withReadOnlyFinalZonk, zonkTopDecls)
import GHC.Types.Avail (availsToNameSet)
import GHC.Types.Basic (LeftOrRight(..), Origin(..), SourceTableOrigin(..), UnfoldingSource(VanillaSrc), isMarkedCbv, pickLR)
import GHC.Types.Error (errorsFound)
import GHC.Types.Id
  ( globaliseId, idArity, idCbvMarks_maybe, idCoreRules, idInfo, idInlineActivation, idInlinePragma, idMult, idName
  , idUnfolding, isClassOpId_maybe, isDataConId_maybe, isDataConWorkId_maybe, isDeadEndId, isJoinId, isPrimOpId_maybe
  , mkLocalId, mkSysLocal, mkSysLocalM, mkVanillaGlobal, realIdUnfolding, setIdExported, setIdInfo, setIdType, setSourceMethodIds
  , sourceLiteralCons_maybe, sourceMethodAliases, sourceMethodDFunClass, sourceMethodTarget_maybe, sourceTableId_maybe )
import GHC.Types.Id.Info (IdDetails(CheckedClosureSourceTableId, DFunId, RecSelId, SourceTableId, VanillaId, sel_sourceTable), SourceDFun(..), setSourceProofRuleInfo)
import GHC.Types.InlinePragma (ActivationX(ActiveAfter, ActiveBefore), PhaseNum, isDefaultInlinePragma, isNeverActive, isOpaquePragma)
import GHC.Types.Name
  ( Name, OccName, SourceProofArg(..), SourceProofArgs, SourceProofRule, isExternalName, isFieldOcc, isVarOcc
  , mkInstTyCoOcc, mkSourceFstOcc, mkSourceSndOcc, mkSourceTableOcc, mkTyVarOcc, mkVarOcc, nameModule_maybe
  , nameOccName, nameSrcSpan, nameStableString, occNameString, setOccNameSpace, sourceTableTermOcc, stableNameCmp, tidyNameOcc, tvName )
import GHC.Types.Name.Env (NameEnv, delFromNameEnv, elemNameEnv, extendNameEnv, extendNameEnv_C, isEmptyNameEnv, lookupNameEnv, mkNameEnv, nonDetNameEnvElts, plusNameEnv_C)
import GHC.Types.Name.Reader (GlobalRdrEnv, gre_imp, isLocalGRE, is_decl, is_isboot, lookupGRE_Name)
import GHC.Types.Name.Set
  ( NameSet, elemNameSet, emptyNameSet, extendNameSet, extendNameSetList, filterNameSet
  , intersectNameSet, intersectsNameSet, isEmptyNameSet, minusNameSet, mkNameSet, nameSetElemsStable
  , unionNameSet, unionNameSets, unitNameSet )
import GHC.Types.SourceFile (hscSourceToIsBoot, isHsBootOrSig)
import GHC.Types.SrcLoc
import GHC.Types.Tickish (CoreTickish, GenTickish(Breakpoint))
import GHC.Types.TyThing (TyThing(..))
import GHC.Types.TypeEnv (TypeEnv, extendTypeEnvWithIds, lookupTypeEnv, typeEnvElts, typeEnvIds)
import GHC.Types.Unique (hasKey, minLocalUnique)
import GHC.Types.Unique.FM (plusUFM_C)
import GHC.Types.Unique.Set (addOneToUniqSet, elementOfUniqSet, emptyUniqSet, mkUniqSet, nonDetEltsUniqSet, sizeUniqSet)
import GHC.Types.Unique.Supply (UniqSupply)
import GHC.Types.Var (CoVar, Id, VarBndr(..), idDetails, isCoVar, isId, mkTyVar, setIdDetails, varName)
import GHC.Types.Var.Env (InScopeSet, extendInScopeSet, mkInScopeSet, uniqAway)
import GHC.Types.Var.Set (TyCoVarSet, delVarSet, elemVarSet, emptyVarSet, intersectVarSet, isEmptyVarSet, mkVarSet)
import GHC.Unit.External (eps_rule_base)
import GHC.Unit.Home (homeUnitId)
import GHC.Unit.Home.Graph (lookupHugByModule)
import GHC.Unit.Home.ModInfo (hm_details)
import GHC.Unit.Module (Module, getModuleInstantiation, isHoleModule)
import GHC.Unit.Module.Deps (imp_boot_mods)
import GHC.Unit.Module.Env (lookupInstalledModuleEnv, moduleSetElts)
import GHC.Unit.Module.ModDetails (md_rules)
import GHC.Unit.Module.ModIface (mi_boot, mi_hash_fn)
import GHC.Unit.Types (GenWithIsBoot(..), ghcInternalUnitId, isInteractiveModule, moduleName, moduleUnitId)
import GHC.Utils.Misc (equalLength, lengthAtLeast, lengthIs)
import GHC.Utils.Monad (allM, anyM)
import GHC.Utils.Outputable (ppr, text)
import GHC.Utils.Panic (panic, pprPanic)

import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, forM, forM_, guard, unless, when, zipWithM, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Data, cast, gmapQ)
import Data.Functor.Identity (Identity(..))
import Data.List (find, findIndex, nubBy, sortBy, stripPrefix, unsnoc)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Semigroup ((<>))
import qualified Data.Set as Set

{- Note [Certifying source equations from Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A term-level function `f` promoted into a dependent type (GHC Proposal #378,
§4.6 and §4.11; GHC Proposal #509) reduces on open type arguments via a private
nominal closed type family `T:f` attached to `f`'s `IdDetails` (`SourceTableId`).
Following Eisenberg and Stolarek (Haskell Symposium 2014) and Weirich et al.
(ICFP 2017; ICFP 2019), `compileCoreEquations` compiles desugared Core decision
trees (Maranget, ML 2008) into `[CoAxBranch]` equations over unmatchable (`%U`,
GHC Proposal #242; Kiss et al., ICFP 2019) function arrows:
  1. Exhaustive lazy `Case (Var v)` splits (expanding `DEFAULT` branches over
     finite ADTs and refuting uninhabited types via `case v of {}` or `absurd`)
     produce nominal constructor patterns without forcing unused arguments.
  2. Non-variable Boolean guard cases (`filter`, `dropWhile`, `span`, and
     checked structural predicates) lift to a private `#guard:` companion
     closed family (Danvy and Nielsen, PPDP 2001 / BRICS RS-01-23) so open
     predicate calls stay stuck until a concrete `Bool` constructor appears.
  3. Under `-O` / `Opt_EnableRewriteRules`, `FinalCoreSourceTable` entries are
     re-compiled after the Core Simplifier (`verifyFinalCoreMapper` and
     `checkedFinalCaseTable`) and retained only when post-Simplifier Core yields
     alpha-identical `CoAxBranch` equations (`matchesCompiledTable`) and every
     active `RULES` pragma preserves call-by-need definedness in the presence of
     `seq` (Gill, Launchbury, and Peyton Jones, FPCA 1993; Johann and
     Voigtländer, Fundam. Inform. 2006; Schmidt-Schauß et al., LMCS 2015).
-}

{- Note [Cold ghc-internal interface lookups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During a cold build of `ghc-internal`, early modules compile before
`GHC.Internal.Base` and `GHC.Internal.Bignum.Integer` write their `.hi` files.
`isBootModuleReady` verifies that a non-boot `ModIface` is already loaded in
`lookupIfaceByModuleHsc` before `lookupBootModuleIdMaybe` queries `lookupOrig`.
-}

sourceTableOccurrence :: TcGblEnv -> Id -> Maybe Id
sourceTableOccurrence env actual
  | nameModule_maybe name == Just (tcg_semantic_mod env) =
      case lookupTypeEnv (tcg_type_env env) name of
        Just (AnId fun) | fun == actual -> Just fun
        _ -> Nothing
  | otherwise = Just actual
  where
    name = varName actual

tcLookupSourceTableId :: Id -> TcM (Maybe Id)
tcLookupSourceTableId = tcLookupSourceTableIdAt Nothing

isBootModuleReady :: Module -> TcM Bool
isBootModuleReady mod = do
  dflags <- getDynFlags
  env <- getGblEnv
  let in_ghc_internal = homeUnitId_ dflags == ghcInternalUnitId ||
                        homeUnitId_ dflags == unitSettings_ghcInternalUnitId (unitSettings dflags)
  if not in_ghc_internal || tcg_semantic_mod env == mod then pure True else do
    hsc_env <- getTopEnv
    mb_iface <- liftIO (lookupIfaceByModuleHsc hsc_env mod)
    pure (maybe False ((== NotBoot) . mi_boot) mb_iface)

lookupBootModuleIdMaybe :: Module -> OccName -> TcM (Maybe Id)
lookupBootModuleIdMaybe mod occ = do
  ready <- isBootModuleReady mod
  if not ready then pure Nothing else lookupOrig mod occ >>= tcLookupIdMaybe

recordSourceTableUse :: TcGblEnv -> Name -> TcM ()
recordSourceTableUse env name =
    updTcRef (tcg_source_check env) (\s -> s { scs_table_uses = extendNameSet (scs_table_uses s) name })

recordSourceTableUses :: TcGblEnv -> [Name] -> TcM ()
recordSourceTableUses env names =
    updTcRef (tcg_source_check env) (\s -> s { scs_table_uses = extendNameSetList (scs_table_uses s) names })

freshSysTyVar :: String -> Kind -> TcM TyVar
freshSysTyVar occ kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc occ)

coreTypeArgs :: [Core.CoreExpr] -> Maybe [Type]
coreTypeArgs = traverse (\case Core.Type ty -> Just ty; _ -> Nothing)

tcLookupSourceTableIdAt :: Maybe Name -> Id -> TcM (Maybe Id)
tcLookupSourceTableIdAt first actual = do
    env <- getGblEnv
    case sourceTableOccurrence env actual of
      Just fun | Just table <- sourceTableId_maybe fun -> do
        dependencies <- sourceTableDependencies table
        case dependencies of
          Nothing -> pure Nothing
          Just callees -> do
            build_use <- if finalCoreMapperOwner fun && isFinalSourceListTable table
              then fmap (\b -> [(b, Nothing)]) <$> lookupBootModuleIdMaybe gHC_INTERNAL_BASE (mkVarOcc "build")
              else pure Nothing
            recordSourceUses ((fun, first) : [(v, Nothing) | v <- callees] ++ fromMaybe [] build_use)
            forM_ (fun : callees) $ \owner ->
              when (nameModule_maybe (idName owner) == Just (tcg_semantic_mod env)) (keepAlive (idName owner))
            recordSourceTableUse env (tyConName table)
            pure (Just fun)
      _ -> pure Nothing

sourceTableOwner :: TyCon -> TcM (Maybe Id)
sourceTableOwner table
  | Just occ <- sourceEquationOwnerOcc (nameOccName (tyConName table))
  , Just mod <- nameModule_maybe (tyConName table) = do
      name <- lookupOrig mod occ
      env <- getGblEnv
      owner <- if mod == tcg_semantic_mod env
        then pure $ case lookupTypeEnv (tcg_type_env env) name of { Just (AnId fun) -> Just fun; _ -> Nothing }
        else if isInteractiveModule mod
        then tcLookupIdMaybe name
        else do
          absent <- importedSourceOwnerAbsent table
          if absent then pure Nothing else tcLookupIdMaybe name
      pure $ do
        fun <- owner
        actual <- sourceTableId_maybe fun
        guard (not (finalCoreMapperOwner fun && isJust (indexedProjectionType (varType fun))) ||
               isJust (indexedProjectionSource fun))
        guard (tyConName actual == tyConName table ||
               ((finalCoreMapperOwner fun || isNominalSourceOwner fun) && isFilterGuardFor actual table &&
                maybe False (any ((== tyConName table) . tyConName)) (sourceTableCallees actual)) ||
               (isNominalStructuralSource fun && booleanSelector actual == Just table))
        pure fun
  | otherwise = pure Nothing
  where
    isNominalSourceOwner v = case idDetails v of
      SourceTableId _ _ NominalSourceTable -> True
      _                                    -> False

sourceEquationOwnerOcc :: OccName -> Maybe OccName
sourceEquationOwnerOcc occ = do
    term <- sourceTableTermOcc occ
    pure (maybe term mkVarOcc (stripPrefix "#guard:" (occNameString term)))

importedSourceOwnerAbsent :: TyCon -> TcM Bool
importedSourceOwnerAbsent table = do
    env <- getGblEnv
    case (sourceEquationOwnerOcc (nameOccName (tyConName table)), nameModule_maybe (tyConName table)) of
      (Just occ, Just mod)
        | mod /= tcg_semantic_mod env, not (isInteractiveModule mod) -> do
            iface <- loadInterfaceForName (text "source table owner") (tyConName table)
            pure (isNothing (mi_hash_fn iface occ))
      _ -> pure False

sourceFamilyTyCons :: [Type] -> [TyCon]
sourceFamilyTyCons tys =
    case [tc | tc <- nonDetEltsUniqSet (tyConsOfTypes tys), isJust (sourceTableTermOcc (nameOccName (tyConName tc)))] of
      []   -> []
      [tc] -> [tc]
      tcs  -> sortBy (\a b -> stableNameCmp (tyConName a) (tyConName b)) tcs

sourceTableCallees :: TyCon -> Maybe [TyCon]
sourceTableCallees table = case famTyConFlav_maybe table of
    Just (ClosedTypeFamilyTyCon (CTF Nothing)) -> Just []
    Just (ClosedTypeFamilyTyCon (CTF (Just axiom))) ->
      Just [tc | tc <- sourceFamilyTyCons (map coAxBranchRHS (fromBranches (coAxiomBranches axiom)))
               , tyConName tc /= tyConName table]
    _ -> Nothing

sourceTableDependencies :: TyCon -> TcM (Maybe [Id])
sourceTableDependencies root = do
    owner <- sourceTableOwner root
    case idDetails <$> owner of
      Just (CheckedClosureSourceTableId _ _ helpers)
        | not (null helpers) -> do
            ids <- traverse tcLookupIdMaybe helpers
            maybe (pure Nothing) (\checked -> walk (unitNameSet (tyConName root)) (reverse checked) [root]) (sequence ids)
        | otherwise -> pure Nothing
      _ -> walk (unitNameSet (tyConName root)) [] [root]
  where
    walk _ owners [] = pure (Just (nubBy (\a b -> idName a == idName b) (reverse owners)))
    walk seen owners (table : rest)
      | Just referenced <- sourceTableCallees table = do
          let wanted = [tc | tc <- referenced, not (tyConName tc `elemNameSet` seen)]
          found <- traverse sourceTableOwner wanted
          case sequence found of
            Nothing -> pure Nothing
            Just callees -> do
              let separate called fun = not (maybe False (`isFilterGuardFor` called) (sourceTableId_maybe fun))
                  new_owners = [fun | (called, fun) <- zip wanted callees, separate called fun]
                  closure_hs = [h | fun <- new_owners, CheckedClosureSourceTableId _ _ hs <- [idDetails fun], h <- hs]
              mb_chs <- traverse tcLookupIdMaybe closure_hs
              case sequence mb_chs of
                Nothing -> pure Nothing
                Just chs -> walk (extendNameSetList seen (map tyConName wanted))
                                 (reverse chs ++ reverse new_owners ++ owners) (wanted ++ rest)
      | otherwise = pure Nothing

sourceTableTypeUses :: Type -> TcM ()
sourceTableTypeUses ty
  | TyConApp tc args <- coreFullView ty = do
      when (isJust (sourceTableTermOcc (nameOccName (tyConName tc)))) (record_or_use tc (Just args))
      mapM_ sourceTableTypeUses args
  | otherwise = mapM_ (\tc -> record_or_use tc Nothing) (sourceFamilyTyCons [ty])

sourceTableReferenceUse :: TyCon -> TcM ()
sourceTableReferenceUse table = record_or_use table Nothing

record_or_use :: TyCon -> Maybe [Type] -> TcM ()
record_or_use table args
  | isTcTyCon table = pure ()
  | otherwise = do
      lcl <- getLclEnvTypeEnv <$> getLclEnv
      case lookupNameEnv lcl (tyConName table) of
        Just (ATcTyCon _) -> pure ()
        _ -> do
          owner <- sourceTableOwner table
          case owner of
            Just fun | finalCoreMapperOwner fun -> getGblEnv >>= (`recordSourceTableUse` tyConName table)
            _ -> maybe (sourceTableFamilyUse table) (sourceTableFamilyAppUse table) args

{- Note [Source family evidence uses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unsaturated references to a source family record only its interface identity.
When the solver reduces a saturated family application, `sourceTableFamilyAppUse`
checks the owner and transitive callees, specializing rule-conflict checks to a
generative first kind argument (`groundFirstHead`) when available.
-}

sourceTableFamilyUse :: TyCon -> TcM ()
sourceTableFamilyUse table = sourceTableFamilyUseAt table Nothing

sourceTableFamilyAppUse :: TyCon -> [Type] -> TcM ()
sourceTableFamilyAppUse table args
  | not (args `lengthAtLeast` tyConArity table)
  , isJust (sourceTableTermOcc (nameOccName (tyConName table))) =
      getGblEnv >>= (`recordSourceTableUse` tyConName table)
  | isNothing (indexedSourceTableVisibleArity table)
  , Just visible <- sourceCaseVisibleArity table <|>
                    (length (dropWhile isInvisibleTyConBinder (tyConBinders table)) <$ sourcePairBuilderField table)
  , lengthIs args (tyConArity table) = do
      kinds <- liftZonkM (zonkTcTypes (take (tyConArity table - visible) args))
      sourceTableFamilyUseAt table (groundFirstHead kinds)
  | otherwise = sourceTableFamilyUse table

sourceCaseKindUse :: TyCon -> [Kind] -> TcM ()
sourceCaseKindUse table kinds
  | isNothing (indexedSourceTableVisibleArity table)
  , Just visible <- sourceCaseVisibleArity table
  , lengthIs kinds (tyConArity table - visible) = sourceTableFamilyUseAt table (groundFirstHead kinds)
  | otherwise = sourceTableFamilyUse table

groundFirstHead :: [Type] -> Maybe Name
groundFirstHead args@(first : _)
  | all noFreeVarsOfType args, all isTyFamFree args
  , Just (tc, _) <- tcSplitTyConApp_maybe first, isGenerativeTyCon tc Nominal = Just (tyConName tc)
groundFirstHead _ = Nothing

sourceTableFamilyUseAt :: TyCon -> Maybe Name -> TcM ()
sourceTableFamilyUseAt table first
  | isTcTyCon table || isNothing (sourceTableTermOcc (nameOccName (tyConName table))) = pure ()
  | Just (ClosedTypeFamilyTyCon CTF_Abstract) <- famTyConFlav_maybe table = pure ()
  | otherwise = do
      lcl <- getLclEnvTypeEnv <$> getLclEnv
      case lookupNameEnv lcl (tyConName table) of
        Just (ATcTyCon _) -> pure ()
        _ -> do
          owner <- sourceTableOwner table
          checked <- maybe (pure Nothing) (tcLookupSourceTableIdAt first) owner
          when (isNothing checked) $ do
            absent <- importedSourceOwnerAbsent table
            env <- getGblEnv
            let local_synth = nameModule_maybe (tyConName table) == Just (tcg_semantic_mod env)
            case sourceTableCallees table of
              Just [] | absent, not (isSourceFilterGuard table) -> recordSourceTableUse env (tyConName table)
              Just callees
                | absent || local_synth
                , Just _ <- stripPrefix "T:$depfun$" (occNameString (nameOccName (tyConName table))) -> do
                    mapM_ (`sourceTableFamilyUseAt` Nothing) callees
                    recordSourceTableUse env (tyConName table)
              _ | local_synth, isJust (closedFamilyTyConCoAxiom_maybe table) ->
                    recordSourceTableUse env (tyConName table)
              _ -> failWithTc (TcRnRetainedQuantificationError
                               (RetainedUncheckedSourceCall (maybe (tyConName table) idName owner)))

visibleSourceMethodRules :: TcGblEnv -> TcM RuleEnv
visibleSourceMethodRules env = do
    hsc <- getTopEnv
    eps <- getEps
    let mod = tcg_mod env
        visible = tcVisibleOrphanMods env
        from_import rules m = do
          info <- lookupHugByModule m (hsc_HUG hsc)
          case info of
            Nothing -> pure rules
            Just hmi -> do
              below <- hugRulesBelow hsc (moduleUnitId m) (GWIB (moduleName m) NotBoot)
              pure (extendRuleBaseList (plusUFM_C (++) rules below) (md_rules (hm_details hmi)))
    home_rules <- liftIO $ if isInteractiveModule mod
      then foldM from_import emptyRuleBase (moduleSetElts visible)
      else hugRulesBelow hsc (homeUnitId (hsc_home_unit hsc)) (GWIB (moduleName mod) (hscSourceToIsBoot (tcg_src env)))
    pure emptyRuleEnv { re_home_rules = home_rules, re_eps_rules = eps_rule_base eps, re_visible_orphs = visible }

unloadedSourceRuleHeads :: TcGblEnv -> TcM NameSet
unloadedSourceRuleHeads env = do
    hsc <- getTopEnv
    liftIO $ visibleUnloadedSourceRuleHeads hsc False (tcg_mod env) (tcg_src env) (tcVisibleOrphanMods env)

hasSourceMethodRule :: RuleEnv -> Id -> Bool
hasSourceMethodRule rules = hasSourceMethodRuleAt rules Nothing

hasSourceMethodRuleAt :: RuleEnv -> Maybe Name -> Id -> Bool
hasSourceMethodRuleAt rules first = hasSourceMethodRuleAtArgs rules first []

hasSourceMethodRuleAtArgs :: RuleEnv -> Maybe Name -> SourceProofArgs -> Id -> Bool
hasSourceMethodRuleAtArgs rules first checked fun = any user_rule (getRules rules (globaliseId fun))
  where
    user_rule BuiltinRule{} = False
    user_rule Rule{ru_rough = Just other : _, ru_args = Core.Type ty : _}
      | Just actual <- first, actual /= other
      , Just (tc, _) <- tcSplitTyConApp_maybe ty
      , tyConName tc == other, isGenerativeTyCon tc Nominal = False
    user_rule Rule{ru_args = args}
      | not (null checked)
      , dictionary : values <- filter (not . Core.isTypeArg) args
      , not (Core.isTyCoArg dictionary), isPredTy (exprType dictionary), not (any Core.isTyCoArg values)
      , sourceProofArgsApart checked (closedSourceProofArgs (map Just values)) = False
    user_rule rule = not (isAutoRule rule || isNeverActive (ruleActivation rule))

closedSourceProofArgs :: [Maybe Core.CoreExpr] -> SourceProofArgs
closedSourceProofArgs args =
    case map (maybe SourceProofUnknown (constructorArg view)) args of
      shapes | all (== SourceProofUnknown) shapes -> []
             | otherwise -> shapes
  where
    view e | (Core.Var fun, as) <- Core.collectArgs e, Just con <- isDataConWorkId_maybe fun
           , let fields = dropWhile Core.isTypeArg as, not (any Core.isTyCoArg fields) = Just (con, fields)
           | otherwise = Nothing

constructorArg :: (e -> Maybe (DataCon, [e])) -> e -> SourceProofArg Name
constructorArg view = go
  where
    go expr
      | Just (con, fields) <- view expr, let tc = dataConTyCon con
      , isVanillaDataCon con, isVanillaAlgTyCon tc, isBoxedDataTyCon tc
      , isNothing (dataConWrapId_maybe con), lengthIs fields (dataConSourceArity con)
      = SourceProofCon (dataConName con) (map go fields)
      | otherwise = SourceProofUnknown

sourceProofArgsApart :: SourceProofArgs -> SourceProofArgs -> Bool
sourceProofArgsApart checked rule = lengthIs rule (length checked) && or (zipWith apart checked rule)
  where
    apart (SourceProofCon a as) (SourceProofCon b bs)
      | a /= b = True
      | lengthIs bs (length as) = or (zipWith apart as bs)
    apart _ _ = False

localSourceRuleApart :: Name -> SourceProofArgs -> LRuleDecl GhcTc -> Bool
localSourceRuleApart owner checked (L _ HsRule{rd_lhs = lhs})
  | not (null checked), (HsVar _ (L _ v), args) <- applications True (unLoc lhs) [], idName v == owner
  = sourceProofArgsApart checked (map (constructorArg view) args)
  | otherwise = False
  where
    view expr | (XExpr (ConLikeTc (RealDataCon con)), fs) <- applications False expr [] = Just (con, fs)
              | otherwise = Nothing
    applications allow expr args = case expr of
      HsApp _ fun arg -> applications allow (unLoc fun) (unLoc arg : args)
      HsAppType _ fun _ -> applications allow (unLoc fun) args
      HsPar _ e -> applications allow (unLoc e) args
      XExpr (WrapExpr w e) | transparent allow w -> applications allow e args
      _ -> (expr, args)
    transparent _ WpHole = True
    transparent _ WpTyApp{} = True
    transparent allow WpEvApp{} = allow
    transparent allow WpLet{} = allow
    transparent allow (WpCompose a b) = transparent allow a && transparent allow b
    transparent allow (WpSubType w) = transparent allow w
    transparent _ (WpExistsPack {}) = False
    transparent _ _ = False

{- Note [Certified list fusion rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Standard `map` traversal rules (`map (\x -> x) xs = xs`, `map/cons`, and
`map f (map g xs) = map (\x -> f (g x)) xs`) and `zip`/`zipWith` `foldr/build`
fusion rules (Gill, Launchbury, and Peyton Jones, FPCA 1993) preserve lazy list
equations under `seq` when the fold, step, and left-producer adapter satisfy the
head-normal-form hypotheses of Johann and Voigtländer (Fundam. Inform. 2006,
Theorem 8.2) and carry no extra call-by-value marks or user rules on helpers.
-}

exactRuleVars :: [Var] -> [Var] -> Bool
exactRuleVars binders vars =
    lengthIs binders (length vars) && sizeUniqSet (mkUniqSet vars) == length vars &&
    mkUniqSet binders == mkUniqSet vars

checkedMapperRules :: RuleEnv -> Id -> Bool
checkedMapperRules env owner = case activeMapperRules env owner of
    [rule] | Just table <- sourceTableId_maybe owner, isTwoListCaseTable table ->
      isJust (checkedImportedSpecConstr env owner table rule)
    rules | Just table <- sourceTableId_maybe owner, isFinalCoreCaseTable table || isCompositionSourceTable table ->
      null rules
    [] -> False
    rules -> all (\r -> identityTraversal r || consTraversal r || composeTraversals r) rules
  where
    identityTraversal Rule{ru_fn = fn, ru_bndrs = [a, xs],
                           ru_args = [Core.Type i, Core.Type o, Core.Lam x (Core.Var y), Core.Var arg],
                           ru_rhs = Core.Var res} =
      fn == idName owner && getTyVar_maybe i == Just a && eqType i o &&
      isId x && x == y && eqType (varType x) i &&
      isId xs && xs == arg && arg == res && eqType (varType xs) (mkListTy i)
    identityTraversal _ = False

    consTraversal Rule{ru_fn = fn, ru_bndrs = bs, ru_args = [Core.Type i, Core.Type o, Core.Var f, spine], ru_rhs = rhs}
      | fn == idName owner, Just a <- getTyVar_maybe i, Just b <- getTyVar_maybe o
      , (Core.Var in_c, [Core.Type in_t, Core.Var x, Core.Var xs]) <- Core.collectArgs spine
      , in_c == dataConWorkId consDataCon, eqType in_t i, exactRuleVars bs [a, b, f, x, xs]
      , all isId [f, x, xs], Just (dom, rng) <- caseArrow (varType f), eqTypes [i, o] [dom, rng]
      , eqTypes [varType x, varType xs] [i, mkListTy i]
      , (Core.Var out_c, [Core.Type out_t, hd, tl]) <- Core.collectArgs rhs
      , out_c == dataConWorkId consDataCon, eqType out_t o
      , (Core.Var cb, [Core.Var h']) <- Core.collectArgs hd, cb == f, h' == x
      , Just (a', b', Core.Var f', Core.Var t') <- mapperCall tl =
          eqTypes [a', b'] [i, o] && f' == f && t' == xs
    consTraversal _ = False

    composeTraversals Rule{ru_fn = fn, ru_bndrs = bs, ru_args = [Core.Type mid, Core.Type o, Core.Var f, inner], ru_rhs = rhs}
      | fn == idName owner, Just (i, mid', Core.Var g, Core.Var xs) <- mapperCall inner, eqType mid' mid
      , Just [a, b, c] <- traverse getTyVar_maybe [i, mid, o], exactRuleVars bs [a, b, c, f, g, xs]
      , all isId [f, g, xs], Just (f_in, f_out) <- caseArrow (varType f), Just (g_in, g_out) <- caseArrow (varType g)
      , eqTypes [f_in, f_out, g_in, g_out, varType xs] [mid, o, i, mid, mkListTy i]
      , Just (i', o', Core.Lam x body, Core.Var xs') <- mapperCall rhs
      , eqTypes [i', o'] [i, o], xs' == xs, isId x, x `notElem` bs, eqType (varType x) i
      , (Core.Var f', [g_app]) <- Core.collectArgs body, (Core.Var g', [Core.Var x']) <- Core.collectArgs g_app =
          f' == f && g' == g && x' == x
    composeTraversals _ = False

    mapperCall expr = case Core.collectArgs expr of
      (Core.Var m, [Core.Type i, Core.Type o, cb, s]) | m == owner -> Just (i, o, cb, s)
      _ -> Nothing

activeMapperRules :: RuleEnv -> Id -> [CoreRule]
activeMapperRules env = filter (not . isNeverActive . ruleActivation) . getRules env . globaliseId

zipFusionRuleHelpers :: Id -> TyCon -> CoreRule -> Maybe (PhaseNum, Id, Id, Id)
zipFusionRuleHelpers owner table Rule{ru_fn = fn, ru_bndrs = binders, ru_act = ActiveBefore phase, ru_args = lhs, ru_rhs = rhs} = do
    Core.Var ys : Core.Var xs : rev_args <- pure (reverse lhs)
    (a, b, output, callback, parameters) <- zipRuleOperands (reverse rev_args)
    let variables = parameters ++ maybeToList callback ++ [xs, ys]
        list = mkListTy output
    guard (fn == idName owner && all isId [xs, ys] && exactRuleVars binders variables &&
           eqTypes (map varType [xs, ys]) (map mkListTy [a, b]))
    (Core.Var build, [Core.Type element, generator]) <- pure (Core.collectArgs rhs)
    guard (element `eqType` output && exprType rhs `eqType` list)
    ([accumulator, cons, nil], fold_call) <- pure (Core.collectBinders generator)
    guard (isTyVar accumulator && all isId [cons, nil])
    let r = mkTyVarTy accumulator
    (Core.Var fold, [Core.Type x, Core.Type y, Core.Type state, step_call, Core.Var seed, Core.Var l, Core.Var rgt]) <-
      pure (Core.collectArgs fold_call)
    guard (eqTypes [a, b, r] [x, y, state] && [seed, l, rgt] == [nil, xs, ys])
    (step, Core.Var actual) <- zipStepOperands a b r output callback step_call
    ActiveAfter build_phase <- pure (idInlineActivation build)
    guard (actual == cons && phase == build_phase)
    [build_body, fold_body, step_body] <- traverse (\v -> case realIdUnfolding v of
      Core.CoreUnfolding{Core.uf_tmpl = body} -> Just body; _ -> Nothing) [build, fold, step]
    guard (checkedZipFoldAlgebra fold fold_body && checkedZipStep (isJust callback) step step_body)
    ([element_var, builder], algebra) <- pure (Core.collectBinders build_body)
    guard (isTyVar element_var && isId builder)
    let e = mkTyVarTy element_var
    (Core.Var apply, [Core.Type result, cons_arg, nil_arg]) <- pure (Core.collectArgs algebra)
    guard (apply == builder && result `eqType` mkListTy e &&
           listRuleConstructor consDataCon e cons_arg && listRuleConstructor nilDataCon e nil_arg)
    let cons_value = Core.mkConApp consDataCon [Core.Type output]
    list_step <- instantiate step_body $ case callback of
      Nothing -> map Core.Type [a, b, list, list] ++ [cons_value]
      Just f  -> map Core.Type [output, list, list, a, b] ++ [cons_value, Core.Var f]
    list_result <- instantiate fold_body
      [Core.Type a, Core.Type b, Core.Type list, list_step, Core.mkConApp nilDataCon [Core.Type output], Core.Var xs, Core.Var ys]
    cct <- compileZipCore (const True) owner (safeSimpleOptExpr defaultSimpleOpts (Core.mkLams variables list_result))
    (phase, build, fold, step) <$ guard (matchesCompiledTable table Nothing cct)
  where
    instantiate expression args = do
      let (parameters, body) = Core.collectBinders expression
          (applied, remaining) = splitAt (length args) parameters
          rest = Core.mkLams remaining body
      guard (equalLength applied args &&
             and (zipWith (\v arg -> not (isCoVar v || Core.isCoArg arg) && isTyVar v == Core.isTypeArg arg) applied args))
      pure (CoreSubst.substExpr (CoreSubst.mkOpenSubst (mkInScopeSet (exprsFreeVars (rest : args))) (zip applied args)) rest)
zipFusionRuleHelpers _ _ _ = Nothing

checkedZipInverse :: Id -> Id -> Id -> PhaseNum -> CoreRule -> Bool
checkedZipInverse owner fold step phase
    Rule{ru_fn = fn, ru_act = ActiveAfter inv_phase, ru_bndrs = binders, ru_args = [Core.Type a, Core.Type b, Core.Type r, step_call, nil], ru_rhs = rhs}
  = isJust $ do
    let (raw_fn, args) = Core.collectArgs rhs
    Core.Var zipper <- pure raw_fn
    (a', b', output, callback, parameters) <- zipRuleOperands args
    guard (inv_phase == phase && fn == idName fold && zipper == owner &&
           eqTypes [a, b, r] [a', b', mkListTy output] && exactRuleVars binders (parameters ++ maybeToList callback))
    (step', cons) <- zipStepOperands a b r output callback step_call
    guard (step == step' && listRuleConstructor consDataCon output cons && listRuleConstructor nilDataCon output nil)
checkedZipInverse _ _ _ _ _ = False

zipRuleOperands :: [Core.CoreExpr] -> Maybe (Type, Type, Type, Maybe Id, [TyVar])
zipRuleOperands (Core.Type a : Core.Type b : rest) = do
    parameters <- traverse getTyVar_maybe [a, b]
    case rest of
      [] -> pure (a, b, mkTupleTy Boxed [a, b], Nothing, parameters)
      [Core.Type output, Core.Var cb] | isId cb ->
        (\res -> (a, b, output, Just cb, parameters ++ [res])) <$> getTyVar_maybe output
      _ -> Nothing
zipRuleOperands _ = Nothing

zipStepOperands :: Type -> Type -> Type -> Type -> Maybe Id -> Core.CoreExpr -> Maybe (Id, Core.CoreExpr)
zipStepOperands a b r output callback expr = case (callback, Core.collectArgs expr) of
    (Nothing, (Core.Var step, [Core.Type x, Core.Type y, Core.Type c, Core.Type d, k]))
      | eqTypes [a, b, r, r] [x, y, c, d] -> Just (step, k)
    (Just f, (Core.Var step, [Core.Type e, Core.Type c, Core.Type d, Core.Type x, Core.Type y, k, Core.Var f']))
      | eqTypes [output, r, r, a, b] [e, c, d, x, y], f' == f -> Just (step, k)
    _ -> Nothing

listRuleConstructor :: DataCon -> Type -> Core.CoreExpr -> Bool
listRuleConstructor con element value = case Core.collectArgs value of
  (Core.Var worker, [Core.Type ty]) -> worker == dataConWorkId con && ty `eqType` element
  _ -> False

checkedZipFoldAlgebra :: Id -> Core.CoreExpr -> Bool
checkedZipFoldAlgebra folder expression = isJust $ do
    ([a, b, r, k, seed, left, right], Core.Let (Core.Rec [(go, loop)]) start) <- pure (Core.collectBinders expression)
    let exact_call f vs e = case Core.collectArgs e of
          (Core.Var fun, as) -> fun == f && traverse (\case Core.Var v -> Just v; _ -> Nothing) as == Just vs
          _ -> False
        split_list scrut e = do
          Core.Case (Core.Var act) _ _
            [Core.Alt (Core.DataAlt nil) [] (Core.Var s), Core.Alt (Core.DataAlt cons) [hv, tv] rest] <- pure e
          (s, hv, tv, rest) <$ guard (act == scrut && nil == nilDataCon && cons == consDataCon)
        marks = fromMaybe [] (idCbvMarks_maybe folder)
    guard (all isTyVar [a, b, r] && all isId [k, seed, left, right] && exact_call go [left, right] start &&
           not (any isMarkedCbv (take 2 marks ++ drop 3 marks ++ drop 1 (fromMaybe [] (idCbvMarks_maybe go)))))
    ([xs, ys], outer) <- pure (Core.collectBinders loop)
    (e_l, x, xt, inner) <- split_list xs outer
    (e_r, y, yt, both) <- split_list ys inner
    (Core.Var step, [Core.Var hl, Core.Var hr, tail_call]) <- pure (Core.collectArgs both)
    guard (e_l == seed && e_r == seed && step == k && [hl, hr] == [x, y] && exact_call go [xt, yt] tail_call)

checkedZipStep :: Bool -> Id -> Core.CoreExpr -> Bool
checkedZipStep has_cb step expression = isJust $ do
    let (bndrs, body) = Core.collectBinders expression
    guard (not (any isMarkedCbv (fromMaybe [] (idCbvMarks_maybe step))))
    (Core.Var cont, [head_res, Core.Var tail_res]) <- pure (Core.collectArgs body)
    if has_cb
      then do
        [a, b, c, d, e, k, f, x, y, r] <- pure bndrs
        (Core.Var cb, [Core.Var hl, Core.Var hr]) <- pure (Core.collectArgs head_res)
        guard (all isTyVar [a, b, c, d, e] && all isId [k, f, x, y, r] &&
               cont == k && cb == f && tail_res == r && [hl, hr] == [x, y])
      else do
        [a, b, c, d, k, x, y, r] <- pure bndrs
        (Core.Var ctor, [Core.Type l, Core.Type rgt, Core.Var hl, Core.Var hr]) <- pure (Core.collectArgs head_res)
        guard (all isTyVar [a, b, c, d] && all isId [k, x, y, r] && cont == k && tail_res == r &&
               ctor == dataConWorkId (tupleDataCon Boxed 2) && eqTypes [l, rgt] (mkTyVarTys [a, b]) && [hl, hr] == [x, y])

checkedZipLeftFusion :: Id -> Id -> CoreRule -> Maybe Id
checkedZipLeftFusion fold build
    Rule{ru_fn = fn, ru_bndrs = binders, ru_args = [Core.Type a, Core.Type b, Core.Type r, Core.Var step, Core.Var seed, producer, Core.Var right], ru_rhs = rhs} = do
    type_vars <- traverse getTyVar_maybe [a, b, r]
    (Core.Var builder, [Core.Type element, Core.Var generator]) <- pure (Core.collectArgs producer)
    guard (fn == idName fold && builder == build && eqType element a && all isId [step, seed, generator, right] &&
           exactRuleVars binders (type_vars ++ [step, seed, generator, right]))
    (Core.Var g, [Core.Type result, on_cons, Core.Lam ignored (Core.Var on_nil), Core.Var ys]) <- pure (Core.collectArgs rhs)
    (FTF_T_T, _, mult, domain, range) <- splitFunTy_maybe result
    guard (g == generator && isId ignored && eqType (varType ignored) (mkListTy b) && on_nil == seed && ys == right &&
           isManyTy mult && eqTypes [domain, range] [mkListTy b, r])
    (Core.Var adapt, [Core.Type a', Core.Type b', Core.Type c', Core.Type r', Core.Var k, Core.Var z]) <- pure (Core.collectArgs on_cons)
    Core.CoreUnfolding{Core.uf_tmpl = body} <- pure (realIdUnfolding adapt)
    adapt <$ guard (eqTypes [a, b, r, r] [a', b', c', r'] && k == step && z == seed && checkedZipLeftAdapter adapt body)
checkedZipLeftFusion _ _ _ = Nothing

checkedZipLeftAdapter :: Id -> Core.CoreExpr -> Bool
checkedZipLeftAdapter adapt expression = isJust $ do
    ([a, b, c, d, k, z, x, recur, right], body) <- pure (Core.collectBinders expression)
    guard (all isTyVar [a, b, c, d] && all isId [k, z, x, recur, right] && idArity adapt == 5 &&
           not (any isMarkedCbv (take 4 (fromMaybe [] (idCbvMarks_maybe adapt)))))
    Core.Case (Core.Var input) _ _
      [Core.Alt (Core.DataAlt nil) [] (Core.Var seed), Core.Alt (Core.DataAlt cons) [y, ys] next] <- pure body
    (Core.Var cont, [Core.Var hl, Core.Var hr, rest]) <- pure (Core.collectArgs next)
    (Core.Var f, [Core.Var tr]) <- pure (Core.collectArgs rest)
    guard (input == right && nil == nilDataCon && cons == consDataCon && seed == z &&
           cont == k && hl == x && hr == y && f == recur && tr == ys)

finalCoreRuleConflict :: RuleEnv -> NameSet -> NameSet -> Id -> Maybe RetainedQuantificationError
finalCoreRuleConflict visible unloaded local owner
  | idName owner `elemNameSet` local || idName owner `elemNameSet` unloaded = Just fallback
  | checkedMapperRules visible owner = Nothing
  | Just table <- sourceTableId_maybe owner
  , isTwoListCaseTable table || isListZipWithTable table
  , [rule] <- activeMapperRules visible owner
  , Just (phase, build, fold, step) <- zipFusionRuleHelpers owner table rule =
      let fold_rules = activeMapperRules visible fold
          inverse = checkedZipInverse owner fold step phase
          dependencies = map (foldRuleDependencies inverse phase build fold) fold_rules
          helper_conflict = find changed (build : step : concat (catMaybes dependencies))
          fold_conflict = Just (RetainedSourceRule (idName fold))
      in if idName fold `elemNameSet` local || idName fold `elemNameSet` unloaded ||
            any (\r -> usesStep step r && not (inverse r)) fold_rules
         then fold_conflict
         else case helper_conflict of
           Just helper -> Just (RetainedSourceRule (idName helper))
           Nothing | any isNothing dependencies -> fold_conflict
                   | any inverse fold_rules     -> Nothing
                   | otherwise                  -> Just fallback
  | otherwise = Just fallback
  where
    usesStep step Rule{ru_args = _ : _ : _ : arg : _} | (Core.Var fun, _) <- Core.collectArgs arg = fun == step
    usesStep _ _ = False
    foldRuleDependencies inverse phase build fold rule
      | inverse rule = Just []
      | Just adapter <- checkedZipLeftFusion fold build rule = Just [adapter]
      | Rule{ru_rhs = rhs} <- rule, (Core.Var peer, _) <- Core.collectArgs rhs
      , peer /= owner, not (idName peer `elemNameSet` local), not (idName peer `elemNameSet` unloaded)
      , Just table <- sourceTableId_maybe peer, isTwoListCaseTable table || isListZipWithTable table
      , [forward] <- activeMapperRules visible peer
      , Just (p_phase, p_build, p_fold, p_step) <- zipFusionRuleHelpers peer table forward
      , phase == p_phase, build == p_build, fold == p_fold, checkedZipInverse peer fold p_step phase rule
      = Just [p_step]
      | otherwise = Nothing
    changed v = idName v `elemNameSet` local || idName v `elemNameSet` unloaded || not (null (activeMapperRules visible v))
    fallback = RetainedFinalCoreRulesEnabled (idName owner)

recordSourceMethodUse :: Id -> Id -> Id -> TcM ()
recordSourceMethodUse dfun selector target = do
    recordSourceUses (sourceMethodOps dfun selector target)
    captureSourceMethodProof dfun selector target ([], [])

captureSourceMethodProof :: Id -> Id -> Id -> (SourceProofArgs, [Id]) -> TcM ()
captureSourceMethodProof dfun selector target (checked, leaves) = do
    env <- getGblEnv
    let ops = sourceMethodOps dfun selector target
    updTcRef (tcg_source_check env) $ \s ->
      let state = scs_rule_uses s
      in case sourceProofSignature state of
           Just owner ->
             let new_deps = [(idName v, first, finalCoreMapperOwner v, if v == selector then checked else []) | (v, first) <- ops]
                            ++ [(idName v, Nothing, False, []) | v <- leaves]
                 !defs' = extendNameEnv_C (++) (sourceProofDefinitions state) owner new_deps
             in s { scs_rule_uses = state { sourceProofDefinitions = defs' } }
           Nothing -> s

withSourceProofSignature :: Maybe Name -> TcM a -> TcM a
withSourceProofSignature (Just owner) action | isExternalName owner = do
    enabled <- xoptM LangExt.DependentTypes
    if not enabled then action else do
      env <- getGblEnv
      let check_ref = tcg_source_check env
      before <- scs_rule_uses <$> readTcRef check_ref
      updTcRef check_ref $ \s ->
        s { scs_rule_uses = (scs_rule_uses s) { sourceProofSignature = Just owner } }
      mb_res <- attemptM action
      case mb_res of
        Just result -> do
          updTcRef check_ref $ \s ->
            s { scs_rule_uses = (scs_rule_uses s) { sourceProofSignature = sourceProofSignature before } }
          pure result
        Nothing -> do
          updTcRef check_ref (\s -> s { scs_rule_uses = before })
          failM
withSourceProofSignature _ action = action

recordSourceProofAliasUse :: Maybe Name -> TyCon -> TcM ()
recordSourceProofAliasUse owner tc
  | deps@(_ : _) <- synTyConSourceProofRules tc = do
      env <- getGblEnv
      updTcRef (tcg_source_check env) $ \s ->
        let state = scs_rule_uses s
        in case owner <|> sourceProofSignature state of
             Just name | isExternalName name ->
               let !defs' = extendNameEnv_C (++) (sourceProofDefinitions state) name deps
               in s { scs_rule_uses = state { sourceProofDefinitions = defs' } }
             _ -> s
  | otherwise = pure ()

sourceProofRulesFor :: Name -> TcM [SourceProofRule]
sourceProofRulesFor name = do
    env <- getGblEnv
    defs <- sourceProofDefinitions . scs_rule_uses <$> readTcRef (tcg_source_check env)
    pure (stableSourceProofRules (fromMaybe [] (lookupNameEnv defs name)))

takeSourceProofRulesFor :: Name -> TcM [SourceProofRule]
takeSourceProofRulesFor name = do
    rules <- sourceProofRulesFor name
    unless (null rules) $ do
      env <- getGblEnv
      updTcRef (tcg_source_check env) $ \s ->
        let state = scs_rule_uses s
            !defs' = delFromNameEnv (sourceProofDefinitions state) name
        in s { scs_rule_uses = state { sourceProofDefinitions = defs' } }
    pure rules

checkAndInstallSourceProofs :: TcGblEnv -> TcM TcGblEnv
checkAndInstallSourceProofs env = do
    state <- scs_rule_uses <$> readTcRef (tcg_source_check env)
    let occurrences = sourceProofOccurrences state
        imported = mkNameEnv [(name, ops) | (name, _, ops) <- occurrences]
        rhs_proofs = mkNameEnv
          [ (idName owner, deps)
          | not (null occurrences)
          , L loc (XHsBindsLR (AbsBinds { abs_sig = True, abs_exports = [ABE { abe_poly = owner }] })) <- tcg_binds env
          , let deps = concat [ops | (_, site, ops) <- occurrences, site `isSubspanOf` locA loc]
          , not (null deps) ]
        definitions = plusNameEnv_C (++) rhs_proofs (plusNameEnv_C (++) (sourceProofDefinitions state) imported)
    rewriting <- goptM Opt_EnableRewriteRules
    when (rewriting && not (null occurrences)) $ do
      visible <- visibleSourceMethodRules env
      unloaded <- unloadedSourceRuleHeads env
      conflicts <- forM (reverse occurrences) $ \(_, loc, ops) -> do
        conflict <- listToMaybe . catMaybes <$> traverse (changed visible unloaded) ops
        forM_ conflict (\reason -> setSrcSpan loc (addErr (TcRnRetainedQuantificationError reason)))
        pure conflict
      when (all isNothing conflicts) (recordCommittedHomeRules env)
    if isEmptyNameEnv definitions then pure env else do
      let bindings = fromOL (tcg_dus env)
          exports = availsToNameSet (tcg_exports env)
          close_uses v = foldr add_uses (unitNameSet (idName v)) bindings
          add_uses (Just defs, uses) needed | defs `intersectsNameSet` needed = uses `unionNameSet` needed
          add_uses _ needed = needed
          proof_id v
            | not rewriting, isOpaquePragma (idInlinePragma v), idName v `elemNameSet` sourceProofClosedResults state = Nothing
            | idName v `elemNameSet` exports
            , let deps = concatMap (\nm -> fromMaybe [] (lookupNameEnv definitions nm)) (nameSetElemsStable (close_uses v))
            , not (null deps) = Just (setIdInfo v (setSourceProofRuleInfo (idInfo v) (stableSourceProofRules deps)))
            | otherwise = Nothing
          types = tcg_type_env env
      pure env { tcg_type_env = extendTypeEnvWithIds types (mapMaybe proof_id (typeEnvIds types)) }
  where
    changed visible unloaded (owner, arg, mapper, checked) = do
      let relevant = filter (not . localSourceRuleApart owner checked) (tcg_rules env)
      local <- sourceRuleOwnersAtTc arg relevant
      let conflict = Just (RetainedFinalCoreRulesEnabled owner)
      if owner `elemNameSet` local then pure conflict else
        if mapper then do
          worker <- tcLookupIdMaybe owner
          let helper_rules = sourceRuleOwners relevant `minusNameSet` unitNameSet owner
          case worker of
            Just v | finalCoreMapperOwner v, checked == [SourceProofUnknown] ->
                       either Just (const Nothing) <$> checkedAppendFusionRules visible unloaded helper_rules v
                   | finalCoreMapperOwner v ->
                       pure (finalCoreRuleConflict visible unloaded helper_rules v)
            _ -> pure conflict
        else pure $ if owner `elemNameSet` unloaded ||
                       hasSourceMethodRuleAtArgs visible arg checked (mkVanillaGlobal owner liftedTypeKind)
                    then conflict else Nothing

stableSourceProofRules :: [SourceProofRule] -> [SourceProofRule]
stableSourceProofRules = nubBy (==) . sortBy (\(a, b, c, args) (d, e, f, other) ->
    stableNameCmp a d <> cmpArg b e <> compare c f <> compare (map (fmap nameStableString) args) (map (fmap nameStableString) other))
  where
    cmpArg Nothing Nothing   = EQ
    cmpArg Nothing _         = LT
    cmpArg _ Nothing         = GT
    cmpArg (Just a) (Just b) = stableNameCmp a b

{- Note [Select the dictionary for a promoted method]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Promoted class selectors (`fmap`, `foldr`, `(==)`, `compare`, and multi-parameter
or superclass-bearing methods) resolve their unique visible dictionary (`DFunId`)
from the receiver or operand domain, verify apartness of all competing instances,
and hand the instantiated predicate back to `GHC.Tc.Gen.HsType` so lexical Givens
and active `RULES` on the selector, `DFunId`, and worker are checked together.
-}

sourceUnarySelectorWithTarget :: Id -> Maybe (Class, TyVar)
sourceUnarySelectorWithTarget selector = do
    cls <- isClassOpId_maybe selector
    let params = classTyVars cls
    (dom, result) <- caseArrow (classMethodInstTy selector (mkTyVarTys params))
    target <- getTyVar_maybe dom
    guard (target `elem` params && (result `eqType` dom || isJust (ordinaryDataType result)))
    pure (cls, target)

lookupSourceUnaryMethod :: Id -> Type -> TcM (Maybe (Id, Id, TyCon))
lookupSourceUnaryMethod selector domain
  | Just (cls, target_tv) <- sourceUnarySelectorWithTarget selector = do
      env <- getGblEnv
      selected <- if lengthIs (classTyVars cls) 1
        then fmap (, [domain]) <$> lookupSourceGroundDFun cls domain
        else lookupSourceMultiGroundDFun cls target_tv domain
      case selected of
        Just (dfun, pred_args) | Just name <- sourceMethodTarget_maybe dfun selector -> do
          worker <- lookupInstalledOrGlobalId env name
          pure $ do
            method <- worker
            guard (varType method `eqType` classMethodInstTy selector pred_args)
            (dfun, method,) <$> sourceTableId_maybe method
        _ -> pure Nothing
  | otherwise = pure Nothing

sourceBinarySelectorWithTarget :: Id -> Maybe (Class, TyVar)
sourceBinarySelectorWithTarget selector = do
    cls <- isClassOpId_maybe selector
    let params = classTyVars cls
    (left, rest) <- caseArrow (classMethodInstTy selector (mkTyVarTys params))
    (right, result) <- caseArrow rest
    target <- getTyVar_maybe left
    guard (target `elem` params && left `eqType` right && (result `eqType` left || isJust (ordinaryDataType result)))
    pure (cls, target)

sourceBinarySelector :: Id -> Maybe Class
sourceBinarySelector = fmap fst . sourceBinarySelectorWithTarget

lookupSourceBinaryMethod :: Id -> Type -> TcM (Maybe (Id, Id, TyCon))
lookupSourceBinaryMethod selector domain
  | Just (cls, target_tv) <- sourceBinarySelectorWithTarget selector = do
      env <- getGblEnv
      selected <- if lengthIs (classTyVars cls) 1
        then fmap (, [domain]) <$> lookupSourceGroundDFun cls domain
        else lookupSourceMultiGroundDFun cls target_tv domain
      case selected of
        Just (dfun, pred_args) | Just name <- sourceMethodTarget_maybe dfun selector -> do
          worker <- lookupInstalledOrGlobalId env name
          pure $ do
            method <- worker
            guard (varType method `eqType` classMethodInstTy selector pred_args)
            (dfun, method,) <$> sourceTableId_maybe method
        _ -> pure Nothing
  | otherwise = pure Nothing

lookupSourceMultiGroundDFun :: Class -> TyVar -> Type -> TcM (Maybe (Id, [Type]))
lookupSourceMultiGroundDFun cls target_tv domain
  | noFreeVarsOfType domain, isTyFamFree domain
  , Just target_idx <- findIndex (== target_tv) (classTyVars cls) = do
      insts <- tcGetInstEnvs
      env <- getGblEnv
      matches <- fmap catMaybes $ forM (classInstances insts cls) $ \inst -> do
        (renaming, fresh_tvs) <- freshenTyVarBndrs (is_tvs inst)
        let fresh_tys = substTys renaming (is_tys inst)
        pure $ case drop target_idx fresh_tys of
          pat_ty : _
            | Just subst <- tcMatchTys [pat_ty] [domain]
            , Just inst_args <- traverse (lookupTyVar subst) fresh_tvs
            , let pred_args = map (substTy subst) fresh_tys
            , all (\t -> noFreeVarsOfType t && isTyFamFree t) pred_args -> Just (inst, inst_args, pred_args)
          _ -> Nothing
      case matches of
        [(inst, [], pred_args)]
          | not (isIncoherent inst), Right (chosen, []) <- lookupUniqueInstEnv insts cls pred_args
          , instanceDFunId chosen == instanceDFunId inst -> do
              let name = idName (instanceDFunId inst)
              candidate <- lookupInstalledOrGlobalId env name
              pure $ do
                dfun <- candidate
                (dfun, pred_args) <$ guard (varType dfun `eqType` mkClassPred cls pred_args)
        _ -> pure Nothing
  | otherwise = pure Nothing

lookupInstalledOrGlobalId :: TcGblEnv -> Name -> TcM (Maybe Id)
lookupInstalledOrGlobalId env name =
  case lookupTypeEnv (tcg_type_env env) name of
    Just (AnId v) -> pure (Just v)
    _ | nameModule_maybe name == Just (tcg_semantic_mod env) -> pure Nothing
      | otherwise -> tcLookupIdMaybe name

lookupSourceContainerMethod :: Id -> Type -> TcM (Maybe (Id, Id, TyCon))
lookupSourceContainerMethod selector domain
  | Just cls <- isClassOpId_maybe selector
  , let fold_method = listFoldSelector cls selector
  , fold_method || sourceMapSelector selector = do
      env <- getGblEnv
      selected <- lookupSourceGroundDFun cls domain
      case selected of
        Just dfun -> do
          target <- case sourceMethodTarget_maybe dfun selector of
            Just term -> lookupInstalledOrGlobalId env term
            Nothing -> pure $ do
              Core.DFunUnfolding [] con fields <- pure (idUnfolding dfun)
              slots <- dictionarySlotsMulti cls [domain] (Core.mkApps (Core.Var (dataConWorkId con)) fields)
              Core.Var method <- lookup selector slots
              pure method
          pure $ do
            method <- target
            guard (varType method `eqType` classMethodInstTy selector [domain])
            table <- sourceTableId_maybe method
            (dfun, method, table) <$ guard (not fold_method || isListFoldTable table)
        _ -> pure Nothing
  | otherwise = pure Nothing

lookupSourceCheckedMapMethod :: Class -> Id -> Type -> TcM (Maybe (Id, Type, Id, TyCon))
lookupSourceCheckedMapMethod cls selector domain
  | isTyFamFree domain, Just (head_tc, _) <- tcSplitTyConApp_maybe domain
  , isGenerativeTyCon head_tc Nominal, Just (_, recv_pat) <- sourceMapSelectorWithTarget selector = do
      insts <- tcGetInstEnvs
      env <- getGblEnv
      let check_selected inst args pred_args allow_empty = do
            let name = idName (instanceDFunId inst)
            candidate <- lookupInstalledOrGlobalId env name
            case candidate of
              Just dfun
                | let predicate = mkClassPred cls pred_args
                , Just applied <- erasedMethodType (varType dfun) args, applied `eqType` predicate
                , Just target <- sourceMethodTarget_maybe dfun selector -> do
                    worker <- lookupInstalledOrGlobalId env target
                    pure $ do
                      method <- worker
                      method_ty <- erasedMethodType (varType method) args
                      guard (method_ty `eqType` classMethodInstTy selector pred_args)
                      table <- sourceTableId_maybe method
                      sig <- constructorCaseType (tyConKind table)
                      guard (isJust (checkedMapPrefixAllowEmpty allow_empty sig) && sourceCaseVisibleArity table == Just 2)
                      pure (dfun, predicate, method, table)
              _ -> pure Nothing
      if lengthIs (classTyVars cls) 1
        then case lookupUniqueInstEnv insts cls [domain] of
          Right (inst, args)
            | noFreeVarsOfType domain || (not (isIncoherent inst) && all (apart1 inst) (classInstances insts cls)) ->
                check_selected inst args [domain] False
          _ -> pure Nothing
        else do
          let cls_tvs = classTyVars cls
              all_insts = classInstances insts cls
              inst_recv_ty inst_tys = substTyWith cls_tvs inst_tys recv_pat
          matched <- fmap catMaybes $ forM all_insts $ \inst -> do
            (renaming, fresh_tvs) <- freshenTyVarBndrs (is_tvs inst)
            let fresh_tys = substTys renaming (is_tys inst)
            pure $ do
              subst <- tcMatchTys [inst_recv_ty fresh_tys] [domain]
              inst_args <- traverse (lookupTyVar subst) fresh_tvs
              let pred_args = map (substTy subst) fresh_tys
              (inst, inst_args, pred_args) <$ guard (all isTyFamFree pred_args)
          case matched of
            [(inst, args, pred_args)]
              | not (isIncoherent inst), Right (chosen, chosen_args) <- lookupUniqueInstEnv insts cls pred_args
              , instanceDFunId chosen == instanceDFunId inst, eqTypes chosen_args args -> do
                  others_apart <- allM (apartMulti inst pred_args inst_recv_ty) all_insts
                  if others_apart then check_selected inst args pred_args True else pure Nothing
            _ -> pure Nothing
  | otherwise = pure Nothing
  where
    rough = RM_KnownTc (className cls) : roughMatchTcs [domain]
    apart1 chosen other = instanceDFunId other == instanceDFunId chosen || instanceCantMatch (is_tcs other) rough
    apartMulti chosen pred_args inst_recv_ty other
      | instanceDFunId other == instanceDFunId chosen = pure True
      | otherwise = do
          (renaming, _) <- freshenTyVarBndrs (is_tvs other)
          let other_tys = substTys renaming (is_tys other)
          pure $ if all noFreeVarsOfType pred_args
            then isNothing (tcMatchTys other_tys pred_args)
            else isNothing (tcUnifyTys alwaysBindTv [inst_recv_ty other_tys] [domain]) &&
                 isNothing (tcUnifyTys alwaysBindTv other_tys pred_args)

erasedMethodType :: Type -> [Type] -> Maybe Type
erasedMethodType ty args = do
    let prefix = take (length args) (fst (splitForAllForAllTyBinders ty))
    guard (equalLength prefix args)
    _ <- traverse ordinaryCaseBinder prefix
    pure (piResultTys ty args)

lookupSourceGroundDFun :: Class -> Type -> TcM (Maybe Id)
lookupSourceGroundDFun cls domain
  | noFreeVarsOfType domain, isTyFamFree domain = do
      insts <- tcGetInstEnvs
      env <- getGblEnv
      case lookupUniqueInstEnv insts cls [domain] of
        Right (inst, []) -> do
          let name = idName (instanceDFunId inst)
          candidate <- lookupInstalledOrGlobalId env name
          pure (candidate >>= \dfun -> dfun <$ guard (varType dfun `eqType` mkClassPred cls [domain]))
        _ -> pure Nothing
  | otherwise = pure Nothing

listFoldSelector :: Class -> Id -> Bool
listFoldSelector cls selector = isJust $ do
    [parameter] <- pure (classTyVars cls)
    let (_, body) = splitForAllForAllTyBinders (classMethodInstTy selector [mkTyVarTy parameter])
    (_, s1) <- caseArrow body
    (_, s2) <- caseArrow s1
    (receiver, _) <- caseArrow s2
    (head_ty, _) <- splitAppTy_maybe receiver
    guard (getTyVar_maybe head_ty == Just parameter)

sourceMapSelectorWithTarget :: Id -> Maybe (Class, Type)
sourceMapSelectorWithTarget selector = do
    cls <- isClassOpId_maybe selector
    let cls_tvs = classTyVars cls
        (_, body) = splitForAllForAllTyBinders (classMethodInstTy selector (mkTyVarTys cls_tvs))
    (callback, rest) <- caseArrow body
    (input, output) <- caseArrow callback
    (receiver, result) <- caseArrow rest
    (recv_head, recv_arg) <- splitAppTy_maybe receiver
    (res_head, res_arg) <- splitAppTy_maybe result
    let (head_fn, head_args) = splitAppTys recv_head
    head_tvs <- traverse getTyVar_maybe (head_fn : head_args)
    guard (not (null head_tvs) && all (`elem` cls_tvs) head_tvs &&
           sizeUniqSet (mkUniqSet head_tvs) == length head_tvs &&
           recv_head `eqType` res_head && recv_arg `eqType` input && res_arg `eqType` output)
    pure (cls, recv_head)

sourceMapSelector :: Id -> Bool
sourceMapSelector = isJust . sourceMapSelectorWithTarget

dictionarySlotsMulti :: Class -> [Type] -> Core.CoreExpr -> Maybe [(Id, Core.CoreExpr)]
dictionarySlotsMulti cls pred_args expr
  | equalLength pred_args (classTyVars cls)
  , isNewTyCon (classTyCon cls), [selector] <- classAllSelIds cls
  , Core.Cast field co <- stripTicksTopE isSourceBreakpoint expr
  , coercionRKind co `eqType` mkClassPred cls pred_args, exprType field `eqType` coercionLKind co
  = Just [(selector, field)]
  | equalLength pred_args (classTyVars cls)
  , (Core.Var constructor, args) <- Core.collectArgs (stripTicksTopE isSourceBreakpoint expr)
  , Just con <- isDataConWorkId_maybe constructor, con == tyConSingleDataCon (classTyCon cls)
  , let (ty_args, fields) = splitAt (length pred_args) args; selectors = classAllSelIds cls
  , Just applied <- coreTypeArgs ty_args, eqTypes applied pred_args, lengthIs fields (length selectors)
  = Just (zip selectors fields)
  | otherwise = Nothing

checkedMapPrefixAllowEmpty :: Bool -> ConstructorCaseType -> Maybe [Type]
checkedMapPrefixAllowEmpty allow_empty (ConstructorCaseType _ tc params [callback, _] 1 result)
  | Just (prefix, input) <- unsnoc params, allow_empty || not (null prefix)
  , Just (from, to) <- caseArrow callback, input `eqType` from, result `eqType` mkTyConApp tc (prefix ++ [to])
  = Just prefix
checkedMapPrefixAllowEmpty _ _ = Nothing

fixedMapTypes :: ConstructorCaseType -> [Type]
fixedMapTypes sig@(ConstructorCaseType binders _ _ [callback, _] 1 _)
  | Just prefix <- checkedMapPrefixAllowEmpty True sig
  , Just (from, _) <- caseArrow callback, Just mapped_tv <- getTyVar_maybe from
  , let qvars = mkTyVarTys (binderVars binders)
  = [ty | ty <- prefix, mapped_tv `notElem` tyCoVarsOfTypeList ty, caseKind qvars ty]
fixedMapTypes _ = []

data CheckedMethodTarget = FreshMethodWorker Id CompiledCoreTable | ExistingMethodWorker Id

keepCheckedMapWorkers :: UniqSupply -> TcGblEnv -> [Core.CoreBind] -> [Core.CoreBind]
keepCheckedMapWorkers supply env binds
  | isEmptyNameSet targets = binds
  | otherwise = map (\case Core.NonRec v r -> Core.NonRec (mark v) r
                           Core.Rec ps -> Core.Rec [(mark v, r) | (v, r) <- ps]) binds
  where
    targets = mkNameSet [idName o | (_, fs) <- checkedMapWorkers supply env binds, (_, FreshMethodWorker o _) <- fs]
    mark v | idName v `elemNameSet` targets = setIdExported v | otherwise = v

publishCheckedMapMethods :: UniqSupply -> TcGblEnv -> [Core.CoreBind] -> Maybe (TcM ([Id], [TyCon]))
publishCheckedMapMethods supply env binds
  | null candidates = Nothing
  | otherwise = Just $ do
      let owners = nubBy (\(a, _) (b, _) -> a == b) [(o, cct) | (_, fs) <- candidates, (_, FreshMethodWorker o cct) <- fs]
      tables <- forM owners $ \(owner, cct) -> do
        table <- publishCompiledTable owner cct
        pure (setIdDetails owner (SourceTableId (idName owner) table NominalSourceTable), table)
      let checked = mkNameEnv [(idName o, o) | (o, _) <- tables]
          resolve (FreshMethodWorker o _)   = lookupNameEnv checked (idName o)
          resolve (ExistingMethodWorker t)  = Just t
          publish (dfun, fs) = addSourceMethods dfun [(sel, t) | (sel, e) <- fs, Just t <- [resolve e]]
      pure (map fst tables ++ map publish candidates, map snd tables)
  where
    isExt (_, FreshMethodWorker o _)  = isExternalName (idName o)
    isExt (_, ExistingMethodWorker o) = isExternalName (idName o)
    candidates = [(dfun, ext) | (dfun, fs) <- checkedMapWorkers supply env binds, let ext = filter isExt fs, not (null ext)]

checkedMapWorkers :: UniqSupply -> TcGblEnv -> [Core.CoreBind] -> [(Id, [(Id, CheckedMethodTarget)])]
checkedMapWorkers _ env binds
  | null dfuns = []
  | otherwise  = mapMaybe selected dfuns
  where
    pairs = Core.flattenBinds binds
    dfuns = [p | p@(v, _) <- pairs, DFunId{} <- [idDetails v]]
    owners = mkNameEnv [(idName v, p) | p@(v, _) <- pairs]
    exported = availsToNameSet (tcg_exports env)
    local_rules = sourceRuleOwners (tcg_rules env)
    visible tc = tc == listTyCon || isBoxedTupleTyCon tc ||
                 publicCaseType env exported (ordinaryImport (tcg_rdr_env env)) tc
    current_id v
      | nameModule_maybe (idName v) == Just (tcg_semantic_mod env) =
          case lookupTypeEnv (tcg_type_env env) (idName v) of
            Just (AnId installed) -> installed
            _ -> maybe v fst (lookupNameEnv owners (idName v))
      | otherwise = v
    selected (dfun, rhs) = do
      cls <- sourceMethodDFunClass (varType dfun)
      guard (not (idName dfun `elemNameSet` local_rules))
      let (binders, body) = Core.collectBinders rhs
      guard (all isTyVar binders && exprType rhs `eqType` varType dfun)
      (_, pred_args) <- getClassPredTys_maybe (exprType body)
      slots <- dictionarySlotsMulti cls pred_args body
      let methods = mapMaybe (checkSlot cls pred_args binders) slots
      (dfun, methods) <$ guard (not (null methods))

    checkSlot cls pred_args binders slot@(selector, _)
      | selector `elem` classMethods cls, not (idName selector `elemNameSet` local_rules) =
          checkMapSlot cls pred_args binders slot <|> checkBinarySlot pred_args binders slot
      | otherwise = Nothing

    checkMapSlot cls pred_args binders (selector, field) = do
      guard (sourceMapSelector selector)
      (Core.Var worker, args) <- pure (Core.collectArgs (stripTicksTopE isSourceBreakpoint field))
      types <- coreTypeArgs args
      (owner, rhs) <- lookupNameEnv owners (idName worker)
      guard (eqTypes types (mkTyVarTys binders) && not (idName owner `elemNameSet` exported) &&
             not (idName owner `elemNameSet` local_rules) && isNothing (sourceTableId_maybe (current_id owner)) &&
             exprType field `eqType` classMethodInstTy selector pred_args)
      sig <- constructorCaseType (varType owner)
      guard (isJust (checkedMapPrefixAllowEmpty (length pred_args > 1) sig) &&
             (length (classTyVars cls) > 1 || lengthIs pred_args 1))
      (selector,) . FreshMethodWorker owner <$> compileOrdinaryCase visible (const Nothing) sig owner rhs

    checkBinarySlot pred_args binders (selector, field) = do
      guard (null binders && all (\t -> noFreeVarsOfType t && isTyFamFree t) pred_args && isJust (sourceBinarySelector selector))
      let stripped = stripTicksTopE isSourceBreakpoint field
      Core.Var worker <- pure stripped
      guard (exprType stripped `eqType` classMethodInstTy selector pred_args && not (idName worker `elemNameSet` local_rules))
      let cur = current_id worker
      case sourceTableId_maybe cur of
        Just _ | varType cur `eqType` classMethodInstTy selector pred_args -> pure (selector, ExistingMethodWorker cur)
        _ -> do
          (owner, rhs) <- lookupNameEnv owners (idName worker)
          guard (not (idName owner `elemNameSet` exported) && isNothing (sourceTableId_maybe (current_id owner)) &&
                 varType owner `eqType` classMethodInstTy selector pred_args)
          let clean_rhs = safeSimpleOptExpr defaultSimpleOpts (stripTicksE isSourceBreakpoint rhs)
          cct <- compileStructuralCore visible owner clean_rhs <|>
                 (constructorCaseType (varType owner) >>= \s -> compileOrdinaryCase visible (const Nothing) s owner clean_rhs)
          pure (selector, FreshMethodWorker owner cct)

addSourceMethods :: Id -> [(Id, Id)] -> Id
addSourceMethods dfun methods =
  case idDetails (setSourceMethodIds dfun methods) of
    DFunId terminating (Just checked) ->
      let selected = map fst (sdf_methods checked)
          aliases = sdf_methods checked ++ filter ((`notElem` selected) . fst) (sourceMethodAliases dfun)
      in setIdDetails dfun (DFunId terminating (Just checked { sdf_methods = aliases }))
    _ -> dfun

publishListFoldMethods :: Module -> [Core.CoreBind] -> [Core.CoreBind]
publishListFoldMethods mod binds
  | not (any (any is_list_dfun . Core.bindersOf) binds) = binds
  | otherwise = map (\case Core.NonRec v r -> Core.NonRec (publish v r) r
                           Core.Rec ps -> Core.Rec [(publish v r, r) | (v, r) <- ps]) binds
  where
    list_ty = mkTyConTy listTyCon
    is_list_dfun v | DFunId{} <- idDetails v, Just (_, [dom]) <- getClassPredTys_maybe (varType v) = dom `eqType` list_ty
                   | otherwise = False
    locals = mkNameEnv [(idName v, v) | bind <- binds, v <- Core.bindersOf bind]
    current f | nameModule_maybe (idName f) == Just mod = lookupNameEnv locals (idName f) | otherwise = Just f
    publish v rhs = fromMaybe v $ do
      DFunId{} <- pure (idDetails v)
      (cls, [domain]) <- getClassPredTys_maybe (varType v)
      guard (domain `eqType` list_ty)
      slots <- dictionarySlotsMulti cls [domain] rhs
      let methods = [ (sel, target) | (sel, Core.Var f) <- slots, sel `elem` classMethods cls, listFoldSelector cls sel
                                    , Just target <- [current f], varType target `eqType` classMethodInstTy sel [domain]
                                    , Just table <- [sourceTableId_maybe target], isListFoldTable table ]
      addSourceMethods v methods <$ guard (not (null methods))

sourceMethodOps :: Id -> Id -> Id -> [(Id, Maybe Name)]
sourceMethodOps dfun selector target = [(selector, first), (dfun, Nothing), (target, Nothing)]
  where
    first = do
      (_, domain : _) <- getClassPredTys_maybe (snd (splitForAllForAllTyBinders (varType dfun)))
      (tc, _) <- tcSplitTyConApp_maybe domain
      tyConName tc <$ guard (isGenerativeTyCon tc Nominal)

sourceLiteralWorkers :: TcM (Maybe [Id])
sourceLiteralWorkers = fmap sequence $
    traverse (\(mod, occ) -> lookupBootModuleIdMaybe mod (mkVarOcc occ)) $
      (gHC_TYPES, "isTrue#") :
      [(gHC_INTERNAL_NUM_INTEGER, occ) | occ <- ["integerLt", "integerEq", "integerSub", "integerLt#", "integerEq#", "integerCompare"]]

recordSourceLiteralUse :: Id -> Id -> TcM ()
recordSourceLiteralUse dfun selector = do
    ops <- sourceLiteralUses dfun selector
    maybe (failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall (idName selector)))) recordSourceUses ops

sourceLiteralUses :: Id -> Id -> TcM (Maybe [(Id, Maybe Name)])
sourceLiteralUses dfun selector = case sourceLiteralCons_maybe dfun selector of
  Nothing -> pure Nothing
  Just (zero, _) -> do
    let num_mod = mkGhcInternalModule (fsLit "GHC.Internal.Num")
    mb_cmp <- lookupBootModuleIdMaybe gHC_CLASSES (mkVarOcc "<")
    mb_eq <- lookupBootModuleIdMaybe gHC_CLASSES eqClassOpOcc
    mb_sub <- lookupBootModuleIdMaybe num_mod minusClassOpOcc
    mb_oth <- lookupBootModuleIdMaybe gHC_INTERNAL_BASE (mkVarOcc "otherwise")
    mb_workers <- sourceLiteralWorkers
    insts <- tcGetInstEnvs
    let int_dict op mod = do
          cls <- isClassOpId_maybe op
          Right (inst, []) <- pure (lookupUniqueInstEnv insts cls [integerTy])
          let d = instanceDFunId inst
          d <$ guard (nameModule_maybe (idName d) == Just mod)
    pure $ do
      cmp <- mb_cmp; eq <- mb_eq; sub <- mb_sub; oth <- mb_oth; workers <- mb_workers
      ord_d <- int_dict cmp gHC_INTERNAL_NUM_INTEGER
      eq_d <- int_dict eq gHC_INTERNAL_NUM_INTEGER
      num_d <- int_dict sub num_mod
      pure $ (selector, Just (tyConName (dataConTyCon zero))) :
             [(op, Just integerTyConName) | op <- [cmp, eq, sub, selector]] ++
             [(v, Nothing) | v <- [dfun, ord_d, eq_d, num_d, oth] ++ workers]

{- Note [Optional source dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lazy local aliases validate source certificates and visible `RULES` without
recording use immediately. Committed evidence variables mark `ebs_needs` so
`commitOptionalSourceUses` publishes only dependencies demanded by final proofs.
-}

data OptionalSourceUses = OptionalSourceUses [Id] [(Id, Maybe Name)] [Name]

optionalSourceUses :: [Id] -> [(Id, Id, Maybe Id)] -> TcM (Maybe OptionalSourceUses)
optionalSourceUses heads methods = do
    plan <- optionalSourceUsePlan heads methods
    case plan of
      Nothing -> pure Nothing
      Just (owners, ops, tables) -> do
        changed <- changedSourceUse ops
        pure (if isNothing changed then Just (OptionalSourceUses owners ops tables) else Nothing)

registerOptionalSourceUses :: TcEvBinds -> CoVar -> OptionalSourceUses -> TcM ()
registerOptionalSourceUses evidence proof (OptionalSourceUses owners ops tables) = do
    env <- getGblEnv
    updTcRef (tcg_source_check env) (\s -> let !e = (evidence, proof, owners, ops, tables) in s { scs_optional_uses = e : scs_optional_uses s })

optionalSourceUsePlan :: [Id] -> [(Id, Id, Maybe Id)] -> TcM (Maybe ([Id], [(Id, Maybe Name)], [Name]))
optionalSourceUsePlan heads methods = do
    env <- getGblEnv
    let roots = nubBy (\a b -> idName a == idName b) (heads ++ [t | (_, _, Just t) <- methods])
        source fun = case sourceTableOccurrence env fun of
          Just actual | Just table <- sourceTableId_maybe actual ->
            fmap (\cs -> (actual : cs, tyConName table)) <$> sourceTableDependencies table
          _ -> pure Nothing
        method (dfun, sel, Just target)
          | sourceMethodTarget_maybe dfun sel == Just (idName target) = pure (Just (sourceMethodOps dfun sel target))
          | otherwise = pure Nothing
        method (dfun, sel, Nothing) = sourceLiteralUses dfun sel
    sources <- traverse source roots
    method_ops <- traverse method methods
    pure $ do
      checked <- sequence sources
      checked_methods <- sequence method_ops
      let owners = nubBy (\a b -> idName a == idName b) (concatMap fst checked)
      pure (owners, [(v, Nothing) | v <- owners] ++ concat checked_methods, map snd checked)

{- Note [Spine-strict accumulator proofs over list append]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
List append `(++)` is not demand-equivalent under `-O2` `build`/`augment` fusion
for lazy consumers (`map f (xs ++ ys)`), so general `(++)` equations stay gated
under `Opt_EnableRewriteRules`. Inside a top-level nominal proof of
`shunt (xs ++ ys) zs :~: shunt ys (shunt xs zs)` where `shunt` is a spine-strict
tail-recursive list accumulator, both sides force the spine of `xs` and demand
`ys` identically by parametricity with `seq` (Johann and Voigtländer, Fundam.
Inform. 2006), provided `(++)` and its fusion helpers carry only stock
`ghc-internal` rules (`checkedAppendFusionRules`).
-}

enclosingSpineStrictAppendProof :: Id -> TcM (Maybe (Name, Id))
enclosingSpineStrictAppendProof append_owner = do
    append_name <- lookupOrig gHC_INTERNAL_BASE (mkVarOcc "++")
    if idName append_owner /= append_name || not (finalCoreMapperOwner append_owner)
      then pure Nothing
      else case sourceTableId_maybe append_owner of
        Nothing -> pure Nothing
        Just append_tc -> do
          lcl_env <- getLclEnv
          env <- getGblEnv
          let bndrs = getLclEnvBinderStack lcl_env
              has_exp_bndr = any (\case TcIdBndr_ExpType{} -> True; _ -> False) bndrs
              not_top_ids = reverse [b | TcIdBndr b tl <- bndrs, not (isTopLevel tl)]
          case not_top_ids of
            outer_bndr : inner_bndrs
              | not has_exp_bndr
              , all (not . hasNativeDependentExpr . Core.Type . varType) inner_bndrs
              , Just consumer_tc <- spineStrictAppendSig append_tc (varType outer_bndr) -> do
                  proof_name <- lookupOrig (tcg_semantic_mod env) (nameOccName (idName outer_bndr))
                  let valid_top = case lookupNameEnv (getLclEnvTypeEnv lcl_env) proof_name of
                        Just (ATcId { tct_id = top_id }) ->
                          isExternalName (idName top_id) && isJust (spineStrictAppendSig append_tc (varType top_id))
                        _ -> False
                      bad_expr ExprWithTySig{} = True
                      bad_expr (HsPar _ e)     = bad_expr (unLoc e)
                      bad_expr (HsApp _ f a)   = bad_expr (unLoc f) || bad_expr (unLoc a)
                      bad_expr _               = False
                      bad_ctxt PatMonoBindsCtxt{} = True
                      bad_ctxt UserSigCtxt{}      = True
                      bad_ctxt TypeCtxt{}         = True
                      bad_ctxt KindCtxt{}         = True
                      bad_ctxt (FunAppCtxt (FunAppCtxtTy _ _) _)     = True
                      bad_ctxt (FunAppCtxt (FunAppCtxtExpr f a) _)   = bad_expr f || bad_expr a
                      bad_ctxt (ExprCtxt e)                          = bad_expr e
                      bad_ctxt (MatchCtxt (FunRhs { mc_fun = L _ nm })) = nm /= proof_name && nm /= idName outer_bndr
                      bad_ctxt _ = False
                  if valid_top && not (any bad_ctxt (getLclEnvErrCtxt lcl_env))
                    then fmap (proof_name,) <$> sourceTableOwner consumer_tc
                    else pure Nothing
            _ -> pure Nothing

spineStrictAppendSig :: TyCon -> Type -> Maybe TyCon
spineStrictAppendSig append_tc ty = do
    let peel_dep acc t = case coreFullView t of
          DepFunTy DepFunVisible b rest -> peel_dep (b : acc) rest
          other -> (reverse acc, other)
        (bndrs, eq_ty) = peel_dep [] (snd (splitForAllForAllTyBinders ty))
    guard (sizeUniqSet (mkUniqSet bndrs) == 3)
    TyConApp eq_tc [_, side1, side2] <- pure (coreFullView eq_ty)
    guard (occNameString (nameOccName (tyConName eq_tc)) == ":~:")
    match_sides bndrs side1 side2 <|> match_sides bndrs side2 side1
  where
    is_acc_consumer tc = isJust $ do
      guard (isReverseWorkerTable tc)
      [b0, _] <- fromBranches . coAxiomBranches <$> closedFamilyTyConCoAxiom_maybe tc
      [_, _, acc] <- pure (coAxBranchLHS b0)
      guard (isJust (getTyVar_maybe acc) && coAxBranchRHS b0 `eqType` acc)
    match_sides bndrs lhs rhs = do
      TyConApp f1 [_, app_node, TermVarTy zs] <- pure (coreFullView lhs)
      TyConApp app_tc [_, TermVarTy xs, TermVarTy ys] <- pure (coreFullView app_node)
      TyConApp f2 [_, TermVarTy ys', inner] <- pure (coreFullView rhs)
      TyConApp f3 [_, TermVarTy xs', TermVarTy zs'] <- pure (coreFullView inner)
      f1 <$ guard (is_acc_consumer f1 && f1 == f2 && f1 == f3 && tyConName app_tc == tyConName append_tc &&
                   xs == xs' && ys == ys' && zs == zs' && mkUniqSet [xs, ys, zs] == mkUniqSet bndrs)

checkedAppendFusionRules :: RuleEnv -> NameSet -> NameSet -> Id -> TcM (Either RetainedQuantificationError [Id])
checkedAppendFusionRules visible unloaded local_rules append_owner = do
    append_name <- lookupOrig gHC_INTERNAL_BASE (mkVarOcc "++")
    mb_build <- lookupBootModuleIdMaybe gHC_INTERNAL_BASE (mkVarOcc "build")
    let boot_mods = [gHC_INTERNAL_BASE, mkGhcInternalModule (fsLit "GHC.Internal.CString"), mkGhcInternalModule (fsLit "GHC.Internal.List")]
        is_boot_rule BuiltinRule{} = True
        is_boot_rule r@Rule{ru_origin = orig} = isAutoRule r || isNeverActive (ruleActivation r) || orig `elem` boot_mods
    if idName append_owner /= append_name || idName append_owner `elemNameSet` local_rules ||
       idName append_owner `elemNameSet` unloaded
      then pure (Left (RetainedFinalCoreRulesEnabled (idName append_owner)))
      else do
        let app_rules = activeMapperRules visible append_owner
        if null app_rules || not (all is_boot_rule app_rules)
          then pure (Left (RetainedSourceRule (idName append_owner)))
          else do
            let rule_globals Rule{ru_bndrs = bs, ru_args = as, ru_rhs = r} =
                  let bset = mkVarSet bs
                      keep v = isId v && not (v `elemVarSet` bset) && isExternalName (idName v) && isNothing (isDataConId_maybe v)
                  in concatMap (exprSomeFreeVarsList keep) (r : as)
                rule_globals BuiltinRule{} = []
                helpers = nubBy (\a b -> idName a == idName b) (maybeToList mb_build ++ concatMap rule_globals app_rules)
                bad_h h = idName h `elemNameSet` local_rules || idName h `elemNameSet` unloaded ||
                          not (all is_boot_rule (getRules visible (globaliseId h)))
            pure (maybe (Right helpers) (Left . RetainedSourceRule . idName) (find bad_h helpers))

changedSourceUse :: [(Id, Maybe Name)] -> TcM (Maybe RetainedQuantificationError)
changedSourceUse ops = do
    env <- getGblEnv
    visible <- visibleSourceMethodRules env
    unloaded <- unloadedSourceRuleHeads env
    rewriting <- goptM Opt_EnableRewriteRules
    let local_rules = sourceRuleOwners (tcg_rules env)
        check [] = pure Nothing
        check ((v, arg) : rest)
          | isNominalStructuralSource v, not rewriting = check rest
          | finalCoreMapperOwner v =
              if not rewriting then check rest else do
                mb_shunt <- enclosingSpineStrictAppendProof v
                case mb_shunt of
                  Just _ -> checkedAppendFusionRules visible unloaded local_rules v >>= \case
                    Left reason -> pure (Just reason)
                    Right _     -> check rest
                  Nothing -> maybe (check rest) (pure . Just) (finalCoreRuleConflict visible unloaded local_rules v)
          | otherwise = do
              local <- sourceRuleOwnersAtTc arg (tcg_rules env)
              if idName v `elemNameSet` local || idName v `elemNameSet` unloaded || hasSourceMethodRuleAt visible arg v
                then pure (Just (RetainedSourceRule (idName v)))
                else check rest
    check ops

commitOptionalSourceUses :: TcGblEnv -> TcM ()
commitOptionalSourceUses env = do
    pending <- scs_optional_uses <$> readTcRef (tcg_source_check env)
    remaining <- fmap catMaybes $ forM pending $ \entry@(evidence, proof, owners, ops, tables) -> do
      needed <- case evidence of
        TcEvBinds var -> (proof `elemVarSet`) . ebs_needs <$> getTcEvBindsState var
        EvBinds{}     -> pure False
      if not needed then pure (Just entry) else do
        publishSourceUses ops
        recordSourceTableUses env tables
        forM_ owners $ \o -> when (nameModule_maybe (idName o) == Just (tcg_semantic_mod env)) (keepAlive (idName o))
        pure Nothing
    updTcRef (tcg_source_check env) (\s -> s { scs_optional_uses = remaining })

recordSourceUses :: [(Id, Maybe Name)] -> TcM ()
recordSourceUses ops = do
    changed <- changedSourceUse ops
    case changed of
      Just reason -> failWithTc (TcRnRetainedQuantificationError reason)
      Nothing -> do
        env <- getGblEnv
        filtered <- fmap catMaybes $ forM ops $ \(v, arg) -> do
          mb_shunt <- enclosingSpineStrictAppendProof v
          case mb_shunt of
            Just (proof_owner, shunt_owner) -> do
              let deps = [(idName v, Nothing, True, [SourceProofUnknown]), (idName shunt_owner, Nothing, False, [])]
              updTcRef (tcg_source_check env) $ \s ->
                let state = scs_rule_uses s
                    !defs' = extendNameEnv_C (++) (sourceProofDefinitions state) proof_owner deps
                in s { scs_rule_uses = state { sourceProofDefinitions = defs' } }
              pure Nothing
            Nothing -> pure (Just (v, arg))
        unless (null filtered) (publishSourceUses filtered)

recordUnfoldedSourceUses :: [(Id, Maybe Name)] -> TcM ()
recordUnfoldedSourceUses = recordSourceUses . map (\(v, a) -> (if isFinalCoreIdentity v then setIdDetails v VanillaId else v, a))

publishSourceUses :: [(Id, Maybe Name)] -> TcM ()
publishSourceUses ops = do
    env <- getGblEnv
    let combine a b | isEmptyNameSet a || isEmptyNameSet b = emptyNameSet | otherwise = unionNameSet a b
        add !acc (v, arg) = extendNameEnv_C combine acc (idName v) (maybe emptyNameSet unitNameSet arg)
    recordSourceTableUses env (map (idName . fst) ops)
    updTcRef (tcg_source_check env) $ \sc ->
      let s = scs_rule_uses sc
          !t = foldl' add (sourceRuleTypes s) ops
      in sc { scs_rule_uses = s { sourceRuleTypes = t } }

checkSourceMethodUses :: TcGblEnv -> TcM ()
checkSourceMethodUses env =
    checkSourceMethodUsesWithPending env >> updTcRef (tcg_source_check env) (\s -> s { scs_optional_uses = [] })

checkOptionalSourceUses :: TcGblEnv -> TcM ()
checkOptionalSourceUses env = do
    pending <- scs_optional_uses <$> readTcRef (tcg_source_check env)
    unless (null pending) (checkNoErrs (checkSourceMethodUsesWithPending env))

checkSourceMethodUsesWithPending :: TcGblEnv -> TcM ()
checkSourceMethodUsesWithPending env = do
    commitOptionalSourceUses env
    scs <- readTcRef (tcg_source_check env)
    let uses = filterNameSet (\n -> let o = nameOccName n in isVarOcc o || isFieldOcc o) (scs_table_uses scs)
        unfolded = scs_evaluated_uses scs
    rewriting <- goptM Opt_EnableRewriteRules
    let uses_final_core v = finalCoreMapperOwner v && not (isFinalCoreIdentity v && idName v `elemNameSet` unfolded)
        needs_rules v = uses_final_core v || isNominalStructuralSource v
    checked_uses <- if rewriting then pure uses else
      mkNameSet <$> filterM (fmap (maybe True (not . needs_rules)) . tcLookupIdMaybe) (nameSetElemsStable uses)
    unless (isEmptyNameSet checked_uses) $ do
      types <- sourceRuleTypes . scs_rule_uses <$> readTcRef (tcg_source_check env)
      let arguments name | Just ts <- lookupNameEnv types name, not (isEmptyNameSet ts) = map Just (nameSetElemsStable ts)
                         | otherwise = [Nothing]
          affected rule name = anyM (\arg -> elemNameSet name <$> sourceRuleOwnersAtTc arg [rule]) (arguments name)
          report name = addErr (TcRnRetainedQuantificationError (RetainedSourceRule name))
      local <- forM (tcg_rules env) $ \rule -> do
        let cands = checked_uses `intersectNameSet` sourceRuleOwners [rule]
        (rule,) . mkNameSet <$> filterM (affected rule) (nameSetElemsStable cands)
      let imported = checked_uses `minusNameSet` unionNameSets (map snd local)
      forM_ local (\(rule, names) -> setSrcSpanA (getLoc rule) (mapM_ report (nameSetElemsStable names)))
      unless (isEmptyNameSet imported) $ do
        visible <- visibleSourceMethodRules env
        unloaded <- unloadedSourceRuleHeads env
        conflicts <- forM (nameSetElemsStable imported) $ \name -> do
          v <- tcLookupId name
          let local_rules = sourceRuleOwners (tcg_rules env) `minusNameSet` unitNameSet name
              conflict
                | uses_final_core v = finalCoreRuleConflict visible unloaded local_rules v
                | name `elemNameSet` unloaded || any (\arg -> hasSourceMethodRuleAt visible arg v) (arguments name) =
                    Just (RetainedSourceRule name)
                | otherwise = Nothing
          forM_ conflict $ \case
            RetainedFinalCoreRulesEnabled _ -> report name
            reason -> addErr (TcRnRetainedQuantificationError reason)
          pure conflict
        when (all isNothing conflicts) (recordCommittedHomeRules env)
    when rewriting $ do
      defs <- sourceProofDefinitions . scs_rule_uses <$> readTcRef (tcg_source_check env)
      append_name <- lookupOrig gHC_INTERNAL_BASE (mkVarOcc "++")
      when (any (any (\(n, _, m, c) -> n == append_name && m && c == [SourceProofUnknown])) (nonDetNameEnvElts defs)) $ do
        mb_append <- lookupBootModuleIdMaybe gHC_INTERNAL_BASE (mkVarOcc "++")
        forM_ mb_append $ \append_id -> do
          visible <- visibleSourceMethodRules env
          unloaded <- unloadedSourceRuleHeads env
          res <- checkedAppendFusionRules visible unloaded (sourceRuleOwners (tcg_rules env)) append_id
          case res of
            Left reason -> do
              let bad_name = case reason of { RetainedSourceRule n -> n; RetainedFinalCoreRulesEnabled n -> n; _ -> append_name }
              case [r | r <- tcg_rules env, bad_name `elemNameSet` sourceRuleOwners [r]] of
                r : _ -> setSrcSpanA (getLoc r) (addErr (TcRnRetainedQuantificationError (RetainedSourceRule bad_name)))
                []    -> addErr (TcRnRetainedQuantificationError reason)
            Right _ -> recordCommittedHomeRules env

recordCommittedHomeRules :: TcGblEnv -> TcM ()
recordCommittedHomeRules env = do
    let here = tcg_mod env
    unless (isInteractiveModule here || isHsBootOrSig (tcg_src env)) $ do
      hsc <- getTopEnv
      deps <- liftIO $ hugRuleInterfacesBelow hsc (homeUnitId (hsc_home_unit hsc)) (GWIB (moduleName here) NotBoot)
      recordHomeRuleDependencies deps

sourceRuleOwners :: [LRuleDecl GhcTc] -> NameSet
sourceRuleOwners = sourceRuleOwnersAt Nothing

sourceRuleOwnersAt :: Maybe Name -> [LRuleDecl GhcTc] -> NameSet
sourceRuleOwnersAt arg rules = runIdentity (sourceRuleOwnersWith Identity arg rules)

sourceRuleOwnersAtTc :: Maybe Name -> [LRuleDecl GhcTc] -> TcM NameSet
sourceRuleOwnersAtTc = sourceRuleOwnersWith (liftZonkM . zonkTcType)

sourceRuleOwnersWith :: Monad m => (Type -> m Type) -> Maybe Name -> [LRuleDecl GhcTc] -> m NameSet
sourceRuleOwnersWith zonk arg rules = unionNameSets <$> sequence
    [ case headOf (unLoc lhs) of
        Just (owner, Just ty) | Just expected <- arg -> do
          actual <- zonk ty
          pure $ case tcSplitTyConApp_maybe actual of
            Just (tc, _) | isGenerativeTyCon tc Nominal, tyConName tc /= expected -> emptyNameSet
            _ -> unitNameSet owner
        Just (owner, _) -> pure (unitNameSet owner)
        Nothing -> pure lhs_fvs
    | L _ HsRule{rd_ext = (HsRuleRn lhs_fvs _, _), rd_lhs = lhs, rd_act = act} <- rules
    , not (isNeverActive act) ]
  where
    headOf (HsVar _ (L _ f)) = Just (varName f, Nothing)
    headOf (XExpr (HsRecSelTc (FieldOcc _ (L _ f)))) = Just (varName f, Nothing)
    headOf (HsApp _ f _) = headOf (unLoc f)
    headOf (HsAppType ty f _) = (\(fn, inner) -> (fn, inner <|> Just ty)) <$> headOf (unLoc f)
    headOf (HsPar _ f) = headOf (unLoc f)
    headOf (XExpr (WrapExpr _ f)) | isNothing arg = headOf f
    headOf (XExpr (WrapExpr w f)) = (\a (fn, inner) -> (fn, inner <|> a)) <$> first_arg w <*> headOf f
    headOf (XExpr (ExpandedThingTc (HSE _ expansion))) = headOf (unLoc expansion)
    headOf _ = Nothing

    first_arg (WpCompose outer inner) = (<|>) <$> first_arg inner <*> first_arg outer
    first_arg (WpTyApp ty) = Just (Just ty)
    first_arg WpHole       = Just Nothing
    first_arg WpLet{}      = Just Nothing
    first_arg WpEvApp{}    = Just Nothing
    first_arg (WpSubType w) = first_arg w
    first_arg (WpExistsPack {}) = Nothing
    first_arg _            = Nothing
-- ---------------------------------------------------------------------------
-- Core telescope, datatype, and source-table classification predicates
-- ---------------------------------------------------------------------------

helperInvisibleArity :: TyCon -> Int
helperInvisibleArity = length . takeWhile isInvisibleTyConBinder . tyConBinders

data ConstructorCaseType = ConstructorCaseType [ForAllTyBinder] TyCon [Type] [Type] Int Type

hasSourceTableAxiom :: Bool -> (Type -> Bool) -> TyCon -> Bool
hasSourceTableAxiom require_nom pred_fn table =
    isJust (sourceTableTermOcc (nameOccName (tyConName table))) &&
    (case famTyConFlav_maybe table of
       Just (ClosedTypeFamilyTyCon (CTF Nothing))   -> True
       Just (ClosedTypeFamilyTyCon (CTF (Just ax))) -> not require_nom || coAxiomRole ax == Nominal
       _                                            -> False) &&
    pred_fn (tyConKind table)

isBinaryListCase :: ConstructorCaseType -> Bool
isBinaryListCase (ConstructorCaseType _ root _ _ pos _) = root == listTyCon && pos == 0

isTwoListCase :: ConstructorCaseType -> Bool
isTwoListCase (ConstructorCaseType binders root _ domains pos result) =
    root == listTyCon && pos == 0 && twoListDomains domains &&
    not (lengthIs binders 1 && eqTypes domains [result, result])

isTwoListCaseTable :: TyCon -> Bool
isTwoListCaseTable = hasSourceTableAxiom False (maybe False isTwoListCase . constructorCaseType)

isTwoStructuralCase :: ConstructorCaseType -> Bool
isTwoStructuralCase (ConstructorCaseType [] root [] [left, right] pos result) =
    (pos == 0 || pos == 1) && left `eqType` right &&
    (left `eqType` result || result `eqType` boolTy ||
     maybe False (\(tc, args) -> null args && isEnumerationTyCon tc) (ordinaryDataType result)) &&
    isStructuralSize root left
isTwoStructuralCase _ = False

isSizeListCase :: ConstructorCaseType -> Bool
isSizeListCase (ConstructorCaseType _ root [] [bound, input] 0 result)
  | Just (tc, _) <- caseDatatype input = tc == listTyCon && input `eqType` result && isStructuralSize root bound
isSizeListCase _ = False

isStructuralSize :: TyCon -> Type -> Bool
isStructuralSize root ty
  | Just fields <- traverse (`openDataConFieldTypes` []) (tyConDataCons root)
  , lengthIs fields 2, [child] <- concat fields = child `eqType` ty
  | otherwise = False

isSizeListCaseTable :: TyCon -> Bool
isSizeListCaseTable = hasSourceTableAxiom False (maybe False isSizeListCase . constructorCaseType)

isTwoStructuralCaseTable :: TyCon -> Bool
isTwoStructuralCaseTable = hasSourceTableAxiom False (maybe False isTwoStructuralCase . constructorCaseType)

isStructuralSourceTable :: TyCon -> Bool
isStructuralSourceTable table
  | isReverseWorkerTable table || isJust (booleanSelector table) = True
  | isTwoStructuralCaseTable table, Just axiom <- closedFamilyTyConCoAxiom_maybe table =
      any (\br -> case coAxBranchLHS br of
                    [_, r] | Just (c, _) <- splitTyConApp_maybe r -> isJust (isPromotedDataCon_maybe c)
                    _ -> False) (fromBranches (coAxiomBranches axiom))
  | otherwise = False

isNominalStructuralSource :: Id -> Bool
isNominalStructuralSource owner
  | SourceTableId _ table NominalSourceTable <- idDetails owner = isStructuralSourceTable table
  | otherwise = False

isFinalStructuralSource :: Id -> Bool
isFinalStructuralSource owner =
    finalCoreMapperOwner owner && maybe False isTwoStructuralCaseTable (sourceTableId_maybe owner)

twoListDomains :: [Type] -> Bool
twoListDomains ds = lengthIs ds 2 && all (\ty -> fmap fst (caseDatatype ty) == Just listTyCon) ds

constructorCaseType :: Type -> Maybe ConstructorCaseType
constructorCaseType tau = do
    let (telescope, body) = splitForAllForAllTyBinders tau
    binders <- traverse ordinaryCaseBinder telescope
    let quantified = binderVars binders; kinds = mkTyVarTys quantified
    (first, rest) <- caseArrow body
    (domain, carried, domains, pos, result) <- case (caseArrow first, caseArrow rest) of
      (Just (i, o), Just (second, res)) | all (caseKind kinds) [i, o] -> Just (second, [], [first, second], 1, res)
      (Nothing, Just (second, res)) -> Just (first, [second], [first, second], 0, res)
      (Nothing, Nothing)            -> Just (first, [], [first], 0, rest)
      _                             -> Nothing
    (tc, args) <- caseDatatype domain
    let sig = ConstructorCaseType binders tc args domains pos result
    guard (all (isLiftedTypeKind . varType) quantified ||
           (isRetainedCaseRoot domain tc && noFreeVarsOfType (mkForAllTys telescope liftedTypeKind)))
    let two_lists = twoListDomains domains
    if isSizeListCase sig || (null binders && (two_lists || isTwoStructuralCase sig))
      then guard (all (caseKind kinds) args)
      else do
        let fixed = fixedMapTypes sig
            domParam ty | ty `eqType` liftedTypeKind || any (eqType ty) fixed = Just Nothing
                        | otherwise = Just <$> getTyVar_maybe ty
        params <- catMaybes <$> traverse domParam args
        guard ((isJust (monomorphicEnumCaseArity tau) || not (null params)) &&
               sizeUniqSet (mkUniqSet params) == length params)
    guard (tyCoVarsOfTypes domains == mkUniqSet quantified && caseKind kinds result && all (caseKind kinds) carried &&
           (tc /= listTyCon || pos == 1 || two_lists) && (tc /= tupleTyCon Boxed 2 || pos == 1))
    pure sig

directRetainedWitnessType :: Type -> Maybe (DataCon, ConstructorCaseType)
directRetainedWitnessType ty =
    monomorphicRetainedWitnessType ty <|> do
      sig@(ConstructorCaseType _ tc args [domain] 0 result) <- constructorCaseType ty
      [con] <- pure (tyConDataCons tc)
      witness : _ <- pure (dataConOrigArgTys con)
      guard (isJust (getTyVar_maybe result) && isRetainedCaseRoot domain tc &&
             result `eqType` substTyWith (dataConUnivTyVars con) args (scaledThing witness))
      pure (con, sig)

monomorphicRetainedWitnessType :: Type -> Maybe (DataCon, ConstructorCaseType)
monomorphicRetainedWitnessType ty = do
    res@(_, ConstructorCaseType [] _ _ _ _ _) <- retainedWitnessType ty
    pure res

retainedWitnessType :: Type -> Maybe (DataCon, ConstructorCaseType)
retainedWitnessType ty = do
    let (telescope, body) = splitForAllForAllTyBinders ty
    binders <- traverse ordinaryCaseBinder telescope
    (domain, result) <- caseArrow body
    (tc, args) <- caseDatatype domain
    [con] <- pure (tyConDataCons tc)
    witness : _ <- pure (dataConOrigArgTys con)
    let inst_witness_ty = substTyWith (dataConUnivTyVars con) args (scaledThing witness)
    guard (all (isLiftedTypeKind . varType) (binderVars binders) &&
           eqTypes args (mkTyVarTys (binderVars binders)) &&
           (null binders || not (null (dataConFieldLabels con))) &&
           isRetainedCaseRoot domain tc && result `eqType` inst_witness_ty)
    pure (con, ConstructorCaseType binders tc args [domain] 0 result)

ordinaryCaseBinder :: (Type, ForAllTyBinder) -> Maybe ForAllTyBinder
ordinaryCaseBinder (ma, binder@(Bndr v (Invisible _))) | ma `eqType` unmatchableDataConTy, isTyVar v = Just binder
ordinaryCaseBinder _ = Nothing

monomorphicGuardedListCaseType :: Type -> Maybe ConstructorCaseType
monomorphicGuardedListCaseType ty = do
    (predicate, rest) <- caseArrow ty
    (input, result) <- caseArrow rest
    (root, [element]) <- caseDatatype input
    guard (root == listTyCon && noFreeVarsOfType element && caseKind [] element &&
           predicate `eqType` mkVisFunTyManyU element boolTy && result `eqType` input)
    pure (ConstructorCaseType [] root [element] [predicate, input] 1 result)

isRetainedCaseRoot :: Type -> TyCon -> Bool
isRetainedCaseRoot domain tc
  | [con] <- tyConDataCons tc, dataConHasPromotedRetained con = isJust (nativeIndexDataConTyArgs domain con)
  | otherwise = False

caseArrow :: Type -> Maybe (Type, Type)
caseArrow ty = do
    (FTF_T_T, ma, mult, arg, res) <- splitFunTy_maybe ty
    (arg, res) <$ guard (ma `eqType` unmatchableDataConTy && isManyTy mult && all (isLiftedTypeKind . typeKind) [arg, res])

splitCaseOperand :: Int -> [a] -> Maybe ([a], a, [a])
splitCaseOperand i xs = case splitAt i xs of { (before, x : after) -> Just (before, x, after); _ -> Nothing }

monomorphicEnumCaseArity :: Type -> Maybe Int
monomorphicEnumCaseArity = fmap (subtract 1 . length) . monomorphicEnumCaseKinds

monomorphicEnumCaseKinds :: Type -> Maybe [Type]
monomorphicEnumCaseKinds ty = do
    (domain, rest) <- caseArrow ty
    let kinds = maybe [domain, rest] (\(c, r) -> [domain, c, r]) (caseArrow rest)
        isEnum t = case caseDatatype t of
          Just (tc, []) -> all (maybe False null . (`openDataConFieldTypes` [])) (tyConDataCons tc)
          _ -> False
    kinds <$ guard (all isEnum kinds)

caseDatatype :: Type -> Maybe (TyCon, [Type])
caseDatatype ty = do
    (tc, args) <- splitTyConApp_maybe ty
    (tc, args) <$ guard (isVanillaAlgTyCon tc && isLiftedDataOrNewTyCon tc &&
                         lengthIs args (tyConArity tc) && not (null (tyConDataCons tc)))

emptyCaseDatatype :: Type -> Maybe (TyCon, [Type])
emptyCaseDatatype ty = do
    (tc, args) <- splitTyConApp_maybe ty
    (tc, args) <$ guard (isAlgTyCon tc && isBoxedDataTyCon tc && isLiftedAlgTyCon tc &&
                         not (isAbstractTyCon tc) && null (tyConStupidTheta tc) &&
                         lengthIs args (tyConArity tc) && null (tyConDataCons tc))

caseKind :: [Type] -> Type -> Bool
caseKind = casePublicKind (const True)

casePublicKind :: (TyCon -> Bool) -> [Type] -> Type -> Bool
casePublicKind public parameters kind
  | kind `eqType` liftedTypeKind || any (eqType kind) parameters = True
  | Just (tc, args) <- caseDatatype kind = public tc && all (casePublicKind public parameters) args
  | otherwise = False

ordinaryDataType :: Type -> Maybe (TyCon, [Type])
ordinaryDataType ty
  | Just (tc, args) <- caseDatatype ty, null (tyConStupidTheta tc)
  , all (tcIsLiftedTypeKind . tyVarKind) (tyConTyVars tc), all ordinaryField args
  , strictlyPositiveTyCon tc
  , strictlyPositiveSourceField emptyNameSet emptyVarSet ty
  , let cons = tyConDataCons tc
  , (null args && isEnumerationTyCon tc) || all (ordinaryCon args) cons = Just (tc, args)
  | otherwise = Nothing
  where
    ordinaryField f = tcIsLiftedTypeKind (typeKind f) && noFreeVarsOfType f && isTauTy f
    ordinaryCon args con =
      isVanillaDataCon con && dataConUserTyVars con == dataConUnivTyVars con &&
      maybe False (all ordinaryField) (openDataConFieldTypes con args)

ordinaryArrows :: Type -> Maybe ([Type], Type)
ordinaryArrows ty
  | Just (arg, rest) <- caseArrow ty = ordinaryDataType arg *> ((\(as, r) -> (arg : as, r)) <$> ordinaryArrows rest)
  | otherwise = ([], ty) <$ ordinaryDataType ty

polySourceType :: [TyVar] -> Type -> Bool
polySourceType = polySourceTypeWith emptyNameSet

polySourceTypeWith :: NameSet -> [TyVar] -> Type -> Bool
polySourceTypeWith seen allowed_tvs ty
  | Just tv <- getTyVar_maybe ty = tv `elem` allowed_tvs && tcIsLiftedTypeKind (tyVarKind tv)
  | isTyFamFree ty, Just (tc, args) <- caseDatatype ty, null (tyConStupidTheta tc)
  , let tc_tvs = tyConTyVars tc
  , equalLength args tc_tvs, all (tcIsLiftedTypeKind . tyVarKind) tc_tvs
  , all (polySourceArgTypeWith seen allowed_tvs) args =
      tyConName tc `elemNameSet` seen || (null args && isEnumerationTyCon tc) ||
      let seen' = extendNameSet seen (tyConName tc); formal = mkTyVarTys tc_tvs
      in strictlyPositiveTyCon tc &&
         all (\con -> isVanillaDataCon con && dataConUserTyVars con == dataConUnivTyVars con &&
                      maybe False (all (\f -> strictlyPositiveSourceField seen' emptyVarSet f &&
                                              polySourceArgTypeWith seen' tc_tvs f))
                            (openDataConFieldTypes con formal))
             (tyConDataCons tc)
  | otherwise = False

polySourceArgType :: [TyVar] -> Type -> Bool
polySourceArgType = polySourceArgTypeWith emptyNameSet

polySourceArgTypeWith :: NameSet -> [TyVar] -> Type -> Bool
polySourceArgTypeWith seen allowed_tvs ty =
    (strictlyPositiveSourceField seen emptyVarSet ty && polySourceTypeWith seen allowed_tvs ty) ||
    (tcIsLiftedTypeKind (typeKind ty) && isTyFamFree ty && isTauTy ty &&
     strictlyPositiveSourceField seen emptyVarSet ty &&
     all (\tv -> tv `elem` allowed_tvs && tcIsLiftedTypeKind (tyVarKind tv)) (tyCoVarsOfTypeList ty))

strictlyPositiveTyCon :: TyCon -> Bool
strictlyPositiveTyCon tc
  | isAlgTyCon tc, isLiftedDataOrNewTyCon tc, not (isAbstractTyCon tc), null (tyConStupidTheta tc)
  , let tc_tvs  = tyConTyVars tc
        formal  = mkTyVarTys tc_tvs
        rec_tcs = unitNameSet (tyConName tc)
  , all (\con -> isVanillaDataCon con && dataConUserTyVars con == dataConUnivTyVars con &&
                 maybe False (all (strictlyPositiveSourceField rec_tcs emptyVarSet))
                       (openDataConFieldTypes con formal))
        (tyConDataCons tc)
  = True
  | otherwise = False

-- | Hereditary strict positivity check for datatypes scrutinized by certified
-- source equations. Rejects negative recursive occurrences such as
-- @data Loop = MkLoop (Loop -> Nat)@ or indirect contravariant containers
-- @data Contra a = MkContra (a -> Nat); data Ind = MkInd (Contra Ind)@.
strictlyPositiveSourceField :: NameSet -> TyCoVarSet -> Type -> Bool
strictlyPositiveSourceField rec_tcs pos_tvs ty0
  | Just ty' <- coreView ty0 = strictlyPositiveSourceField rec_tcs pos_tvs ty'
  | no_target ty0 = True
  | otherwise = case ty0 of
      TyVarTy _ -> True
      FunTy _ _ _ a b ->
        no_target a && strictlyPositiveSourceField rec_tcs pos_tvs b
      DepFunTy _ b body ->
        no_target (varType b) && no_target (idMult b) &&
        strictlyPositiveSourceField rec_tcs pos_tvs body
      DepDictTy d body ->
        no_target (varType d) &&
        strictlyPositiveSourceField rec_tcs pos_tvs body
      ForAllTy _ b body ->
        no_target (binderType b) &&
        strictlyPositiveSourceField rec_tcs (delVarSet pos_tvs (binderVar b)) body
      ExistsTy _ b body ->
        no_target (varType b) &&
        strictlyPositiveSourceField rec_tcs (delVarSet pos_tvs b) body
      CastTy t _ -> strictlyPositiveSourceField rec_tcs pos_tvs t
      TyConApp tc args
        | isAlgTyCon tc, isLiftedDataOrNewTyCon tc, not (isAbstractTyCon tc), null (tyConStupidTheta tc)
        , let tc_tvs = tyConTyVars tc
        , equalLength args tc_tvs
        , all (\con -> isVanillaDataCon con && dataConUserTyVars con == dataConUnivTyVars con)
              (tyConDataCons tc)
        -> if tyConName tc `elemNameSet` rec_tcs
             then all no_target args || eqTypes args (mkTyVarTys tc_tvs)
             else strictlyPositiveTyCon tc &&
                  all (strictlyPositiveSourceField rec_tcs pos_tvs) args &&
                  let active_tvs = mkVarSet [ tv | (tv, arg) <- zip tc_tvs args, not (no_target arg) ]
                      rec_tcs'   = extendNameSet rec_tcs (tyConName tc)
                      formal     = mkTyVarTys tc_tvs
                  in isEmptyVarSet active_tvs ||
                     all (\con -> maybe False
                                    (all (strictlyPositiveSourceField rec_tcs' active_tvs))
                                    (openDataConFieldTypes con formal))
                         (tyConDataCons tc)
      _ -> False
  where
    no_target t = isEmptyVarSet (tyCoVarsOfType t `intersectVarSet` pos_tvs)
               && not (any ((`elemNameSet` rec_tcs) . tyConName) (nonDetEltsUniqSet (tyConsOfType t)))

isPairPermutationType :: Type -> Bool
isPairPermutationType ty = isJust $ do
    (_, x, y, result) <- pairCaseType ty
    guard (x /= y && result `eqType` mkTyConApp (tupleTyCon Boxed 2) (mkTyVarTys [y, x]))

isPermutingConstructorCase :: ConstructorCaseType -> Bool
isPermutingConstructorCase (ConstructorCaseType _ root args [_] 0 result) = isJust $ do
    (target, new_args) <- caseDatatype result
    old <- traverse getTyVar_maybe args
    new <- traverse getTyVar_maybe new_args
    guard (root == target && old /= new && mkUniqSet old == mkUniqSet new && length old == length new)
isPermutingConstructorCase _ = False

pairCaseType :: Type -> Maybe ([ForAllTyBinder], TyVar, TyVar, Type)
pairCaseType tau = do
    let (telescope, body) = splitForAllForAllTyBinders tau
    binders <- traverse liftedSourceBinder telescope
    (domain, result) <- caseArrow body
    (tuple, [first, second]) <- splitTyConApp_maybe domain
    guard (tuple == tupleTyCon Boxed 2)
    x <- getTyVar_maybe first
    y <- getTyVar_maybe second
    let field ty = ty `eqType` first || ty `eqType` second
        params = binderVars binders
    guard ((field result || case splitTyConApp_maybe result of
              Just (tc, [a, b]) -> tc == tuple && field a && field b; _ -> False) &&
           x `elem` params && y `elem` params && all (\v -> v == x || v == y) params)
    pure (binders, x, y, result)

liftedSourceBinder :: (Matchability, ForAllTyBinder) -> Maybe ForAllTyBinder
liftedSourceBinder (ma, binder@(Bndr a (Invisible _)))
  | ma `eqType` unmatchableDataConTy, isTyVar a, varType a `eqType` liftedTypeKind = Just binder
liftedSourceBinder _ = Nothing

constructorHelperType :: Type -> Maybe ([ForAllTyBinder], [Type], Type)
constructorHelperType ty = do
    (binders@(_ : _), domains, result) <- splitCoreSignature ty
    guard (all (isLiftedTypeKind . varType) (binderVars binders) &&
           all (caseKind (mkTyVarTys (binderVars binders))) (result : domains))
    pure (binders, domains, result)

isIdentitySourceType :: Type -> Bool
isIdentitySourceType ty = case constructorHelperType ty of
    Just ([Bndr a _], [domain], result) -> getTyVar_maybe domain == Just a && domain `eqType` result
    _ -> False

isFinalCoreIdentity :: Id -> Bool
isFinalCoreIdentity v = finalCoreMapperOwner v && isIdentitySourceType (varType v)

compositionSourceType :: Type -> Maybe ([ForAllTyBinder], [Type], Type)
compositionSourceType ty = do
    let (parameters, body) = splitForAllForAllTyBinders ty
    guard (lengthIs parameters 3)
    telescope <- traverse liftedSourceBinder parameters
    (f, rest) <- caseArrow body; (g, arrow) <- caseArrow rest; (input, output) <- caseArrow arrow
    (middle, f_out) <- caseArrow f; (g_in, g_out) <- caseArrow g
    guard (eqTypes [input, middle, output] [g_in, g_out, f_out])
    variables <- traverse getTyVar_maybe [input, middle, output]
    (telescope, [f, g, input], output) <$ guard (mkUniqSet (binderVars telescope) == mkUniqSet variables)

isCompositionSourceTable :: TyCon -> Bool
isCompositionSourceTable table = isJust $ do
    _ <- sourceTableTermOcc (nameOccName (tyConName table))
    (telescope, domains, _) <- compositionSourceType (tyConKind table)
    axiom <- closedFamilyTyConCoAxiom_maybe table
    [branch] <- pure (fromBranches (coAxiomBranches axiom))
    let vars = coAxBranchTyVars branch
    (params, [f, g, x]) <- pure (splitAt (length telescope) vars)
    guard (coAxiomRole axiom == Nominal && lengthIs params 3 && lengthIs domains 3 && null (coAxBranchCoVars branch) &&
           eqTypes (coAxBranchLHS branch) (mkTyVarTys vars) &&
           coAxBranchRHS branch `eqType` mkAppTy (mkTyVarTy f) (mkAppTy (mkTyVarTy g) (mkTyVarTy x)))

listFoldType :: Type -> Maybe ConstructorCaseType
listFoldType ty = do
    let (telescope, body) = splitForAllForAllTyBinders ty
    binders <- traverse ordinaryCaseBinder telescope
    guard (all (isLiftedTypeKind . varType . binderVar) binders)
    (cb, r1) <- caseArrow body; (seed, r2) <- caseArrow r1; (list, result) <- caseArrow r2
    (el, cb_r) <- caseArrow cb; (st, out) <- caseArrow cb_r
    (tc, [param]) <- caseDatatype list
    guard (tc == listTyCon && el `eqType` param && all (eqType seed) [st, out, result])
    case mkTyVarTys (binderVars binders) of
      []     -> guard (noFreeVarsOfType el && noFreeVarsOfType seed)
      [a, b] -> guard (eqTypes [el, seed] [a, b] || eqTypes [el, seed] [b, a])
      _      -> Nothing
    pure (ConstructorCaseType binders tc [el] [cb, seed, list] 2 result)

isListFoldTable :: TyCon -> Bool
isListFoldTable = hasSourceTableAxiom True (isJust . listFoldType)

listZipWithType :: Type -> Maybe ConstructorCaseType
listZipWithType ty = do
    let (telescope, body) = splitForAllForAllTyBinders ty
    binders <- traverse ordinaryCaseBinder telescope
    [a, b, c] <- pure (mkTyVarTys (binderVars binders))
    (cb, r1) <- caseArrow body; (left, r2) <- caseArrow r1; (right, result) <- caseArrow r2
    guard (all (isLiftedTypeKind . varType . binderVar) binders &&
           cb `eqType` mkVisFunTyManyU a (mkVisFunTyManyU b c) && eqTypes [left, right, result] (map mkListTy [a, b, c]))
    pure (ConstructorCaseType binders listTyCon [a] [cb, left, right] 1 result)

isListZipWithTable :: TyCon -> Bool
isListZipWithTable = hasSourceTableAxiom True (isJust . listZipWithType)

listReverseWrapperType :: Type -> Maybe [ForAllTyBinder]
listReverseWrapperType ty = do
    (binders@[Bndr el _], [domain], output) <- constructorHelperType ty
    binders <$ guard (domain `eqType` mkListTy (mkTyVarTy el) && domain `eqType` output)

isReverseWorkerTable :: TyCon -> Bool
isReverseWorkerTable table
  | hasSourceTableAxiom True (maybe False isBinaryListCase . constructorCaseType) table
  , Just axiom <- closedFamilyTyConCoAxiom_maybe table =
      any (\br -> fmap fst (splitTyConApp_maybe (coAxBranchRHS br)) == Just table) (fromBranches (coAxiomBranches axiom))
  | otherwise = False

isFinalCoreReverseWrapper :: Id -> Bool
isFinalCoreReverseWrapper owner = finalCoreMapperOwner owner && isJust (sourceTableId_maybe owner >>= reverseWrapperWorker)

reverseWrapperWorker :: TyCon -> Maybe TyCon
reverseWrapperWorker table = do
    guard (hasSourceTableAxiom True (isJust . listReverseWrapperType) table)
    [branch] <- fromBranches . coAxiomBranches <$> closedFamilyTyConCoAxiom_maybe table
    [element, operand] <- pure (coAxBranchTyVars branch)
    let kind = mkTyVarTy element; left = mkTyVarTy operand
    (worker, [actual, input, nil]) <- splitTyConApp_maybe (coAxBranchRHS branch)
    worker <$ guard (null (coAxBranchCoVars branch) && eqTypes (coAxBranchLHS branch) [kind, left] &&
                     eqTypes [actual, input, nil] [kind, left, mkTyConApp (promoteDataCon nilDataCon) [kind]] &&
                     isReverseWorkerTable worker)

partialConstructorCaseType :: Type -> Maybe ConstructorCaseType
partialConstructorCaseType ty = do
    (domains@(first : _ : _), result) <- ordinaryArrows ty
    (root, args) <- ordinaryDataType first
    (_, []) <- ordinaryDataType result
    guard (root /= listTyCon && root /= tupleTyCon Boxed 2 && all (caseKind []) (result : domains))
    pure (ConstructorCaseType [] root args domains 0 result)

isPartialConstructorCaseTable :: TyCon -> Bool
isPartialConstructorCaseTable table = isJust $ do
    _ <- sourceTableTermOcc (nameOccName (tyConName table))
    ConstructorCaseType _ root _ domains _ _ <- partialConstructorCaseType (tyConKind table)
    axiom <- closedFamilyTyConCoAxiom_maybe table
    let branchRoot br = do
          lhs : carried <- pure (coAxBranchLHS br)
          (promoted, args) <- splitTyConApp_maybe lhs
          con <- isPromotedDataCon_maybe promoted
          let fields = drop (length (dataConUnivTyVars con)) args
          con <$ guard (equalLength domains (lhs : carried) && dataConTyCon con == root &&
                        lengthIs fields (dataConSourceArity con) &&
                        eqTypes (fields ++ carried) (mkTyVarTys (coAxBranchTyVars br)))
    roots <- traverse branchRoot (fromBranches (coAxiomBranches axiom))
    guard (not (null roots) && length roots < length (tyConDataCons root) && sizeUniqSet (mkUniqSet roots) == length roots)

checkedBranchPredicate :: Id -> Maybe TyCon
checkedBranchPredicate callee = do
    table <- sourceTableId_maybe callee
    table <$ guard ((isFinalStructuralSource callee || (isNominalStructuralSource callee && isTwoStructuralCaseTable table)) &&
                    tyConResKind table `eqType` boolTy)

booleanSelector :: TyCon -> Maybe TyCon
booleanSelector table = do
    _ <- partialConstructorCaseType (tyConKind table)
    select <- sourceTableCallees table >>= find (isFilterGuardFor table)
    let result = tyConResKind table
    (domains, selected) <- ordinaryArrows (tyConKind select)
    select <$ guard (eqTypes domains [boolTy, result, result] && selected `eqType` result)

filterGuardOcc :: OccName -> OccName
filterGuardOcc = mkSourceTableOcc . mkVarOcc . ("#guard:" ++) . occNameString

isSourceFilterGuard :: TyCon -> Bool
isSourceFilterGuard tc = isJust (sourceTableTermOcc (nameOccName (tyConName tc)) >>= stripPrefix "#guard:" . occNameString)

isFilterGuardFor :: TyCon -> TyCon -> Bool
isFilterGuardFor primary candidate =
    nameModule_maybe root == nameModule_maybe guard_name &&
    maybe False ((== nameOccName guard_name) . filterGuardOcc) (sourceTableTermOcc (nameOccName root))
  where
    root = tyConName primary; guard_name = tyConName candidate

finalCoreSourceFamilies :: Id -> [TyCon]
finalCoreSourceFamilies owner = do
    primary <- maybeToList (sourceTableId_maybe owner)
    primary : filter (isFilterGuardFor primary) (fromMaybe [] (sourceTableCallees primary))

finalCoreMapperOwner :: Id -> Bool
finalCoreMapperOwner v = case idDetails v of { SourceTableId _ _ FinalCoreSourceTable -> True; _ -> False }

finalCoreMappers :: [Core.CoreBind] -> [Id]
finalCoreMappers = filter finalCoreMapperOwner . concatMap Core.bindersOf

isFinalCoreCaseTable :: TyCon -> Bool
isFinalCoreCaseTable table =
    isFinalCoreListCaseTable table || isTwoStructuralCaseTable table ||
    isPartialConstructorCaseTable table || isJust (booleanSelector table)

isFinalCoreListCaseTableBase :: TyCon -> Bool
isFinalCoreListCaseTableBase table =
    isListFoldTable table || isListZipWithTable table || isReverseWorkerTable table ||
    isJust (reverseWrapperWorker table) || isTwoListCaseTable table || isSizeListCaseTable table

isFinalCoreListCaseTable :: TyCon -> Bool
isFinalCoreListCaseTable table = isFinalCoreListCaseTableBase table || isFinalSourceListTable table

isFinalSourceListTable :: TyCon -> Bool
isFinalSourceListTable table = isJust $ do
    guard (hasSourceTableAxiom True (not . hasArrowDomain) table &&
           not (isFinalCoreListCaseTableBase table || isTwoStructuralCaseTable table ||
                isPartialConstructorCaseTable table || isJust (booleanSelector table) || isSourceFilterGuard table))
    branches <- fromBranches . coAxiomBranches <$> closedFamilyTyConCoAxiom_maybe table
    let tcs = tyConsOfTypes (map coAxBranchRHS branches)
    guard (lengthAtLeast branches 2 && not (isAppendBranches table branches) &&
           (promoteDataCon consDataCon `elementOfUniqSet` tcs || promoteDataCon nilDataCon `elementOfUniqSet` tcs))
  where
    hasArrowDomain ty = go (snd (splitForAllForAllTyBinders ty))
      where go t | Just (dom, rest) <- caseArrow t = isJust (caseArrow dom) || go rest | otherwise = False
    isAppendBranches fam [b1, b2]
      | Just (_, TyVarTy v1) <- unsnoc (coAxBranchLHS b1), coAxBranchRHS b1 `eqType` mkTyVarTy v1
      , lengthIs (coAxBranchLHS b2) (tyConArity fam)
      , Just (c, [_, _, tail_ty]) <- splitTyConApp_maybe (coAxBranchRHS b2), c == promoteDataCon consDataCon
      , Just (rec_fam, _) <- splitTyConApp_maybe tail_ty = rec_fam == fam
    isAppendBranches _ _ = False

isSourceBreakpoint :: CoreTickish -> Bool
isSourceBreakpoint Breakpoint{} = True
isSourceBreakpoint _            = False

sourceMapperType :: Type -> Bool
sourceMapperType tau =
    maybe False isPermutingConstructorCase (constructorCaseType tau) || isPairPermutationType tau || isJust mapper
  where
    mapper = do
      ConstructorCaseType binders root params [callback, _] 1 result <- constructorCaseType tau
      guard (all (tcIsLiftedTypeKind . tyVarKind) (binderVars binders))
      (input, output) <- caseArrow callback
      a <- getTyVar_maybe input; b <- getTyVar_maybe output
      vars <- traverse getTyVar_maybe params
      guard (a `elem` vars && (a == b || b `notElem` vars) && mkUniqSet (binderVars binders) == mkUniqSet (b : vars) &&
             result `eqType` mkTyConApp root [if v == a then output else mkTyVarTy v | v <- vars])

isSourceConstructorCase :: TyCon -> Bool
isSourceConstructorCase = isJust . sourceCaseVisibleArity

sourceCaseVisibleArity :: TyCon -> Maybe Int
sourceCaseVisibleArity table
  | isJust (reverseWrapperWorker table) = Just 1
  | isListFoldTable table = Just 3
  | not (hasSourceTableAxiom True (const True) table) = Nothing
  | Just axiom <- closedFamilyTyConCoAxiom_maybe table =
      let branches = fromBranches (coAxiomBranches axiom)
          kind = tyConKind table
      in case pairCaseType kind of
          Just (tele, _, _, _) -> case traverse (fmap fst . caseBranch tele) branches of
            Just [con] | con == tupleDataCon Boxed 2 -> Just 1
            _ -> Nothing
          Nothing
            | Just (expected, ConstructorCaseType tele _ _ _ _ _) <- retainedWitnessType kind -> do
                [branch] <- pure branches
                (con, witness : _) <- caseBranch tele branch
                ([pattern_ty], _) <- branchPatterns tele branch
                (_, args) <- splitTyConApp_maybe pattern_ty
                let arity = length tele
                1 <$ guard (con == expected && eqTypes (take arity args) (mkTyVarTys (take arity (coAxBranchTyVars branch))) &&
                            coAxBranchRHS branch `eqType` witness)
            | otherwise -> case constructorCaseType kind <|> monomorphicGuardedListCaseType kind of
                Just (ConstructorCaseType tele tc _ domains pos _)
                  | isNothing (monomorphicEnumCaseArity kind), let vis = length domains
                  , Just roots <- traverse (projectionBranch tele tc vis pos) branches
                  , mkUniqSet roots == mkUniqSet (tyConDataCons tc) -> Just vis
                _ -> indexedSourceTableVisibleArity table
  | otherwise = Nothing
  where
    projectionBranch tele tc vis root branch = do
      (patterns, vars) <- branchPatterns tele branch
      guard (lengthIs patterns vis)
      (before, root_pat, after) <- splitCaseOperand root patterns
      (root_con, _) <- constructorPattern root_pat
      leaves <- concat <$> traverse patternLeaves patterns
      root_con <$ guard (dataConTyCon root_con == tc && eqTypes leaves vars &&
                         resultPattern (before ++ after) vars False (coAxBranchRHS branch))

    patternLeaves ty | isJust (getTyVar_maybe ty) = Just [ty]
                     | otherwise = constructorPattern ty >>= fmap concat . traverse patternLeaves . snd

    resultPattern carried vars under_con ty
      | any (eqType ty) vars = True
      | Just (helper, args) <- splitTyConApp_maybe ty, isFilterGuardFor table helper
      , lengthIs args (tyConArity helper), [pred_res, p, x, xs] <- drop (helperInvisibleArity helper) args
      , any (eqType p) carried, Just (dom, res) <- caseArrow (typeKind p) =
          dom `eqType` typeKind x && res `eqType` boolTy && pred_res `eqType` mkAppTy p x &&
          all (\v -> any (eqType v) vars) [p, x, xs]
      | Just (con, fields) <- constructorPattern ty =
          lengthIs fields (dataConRepArity con) && all (resultPattern carried vars True) fields
      | under_con, Just (fun, arg) <- splitAppTy_maybe ty, any (eqType fun) carried, any (eqType arg) vars
      , Just (dom, _) <- caseArrow (typeKind fun) = typeKind arg `eqType` dom
      | Just (helper, args) <- splitTyConApp_maybe ty, hasSourceTableAxiom False (const True) helper
      , lengthIs args (tyConArity helper) = all (resultPattern carried vars under_con) (drop (helperInvisibleArity helper) args)
      | otherwise = False

    caseBranch tele branch = do
      ([pattern_ty], vars) <- branchPatterns tele branch
      (con, fields) <- constructorPattern pattern_ty
      (con, fields) <$ guard (eqTypes fields vars)

    branchPatterns tele branch = do
      let (params, patterns) = splitAt (length tele) (coAxBranchLHS branch)
          (qvars, fields) = splitAt (length tele) (coAxBranchTyVars branch)
      (patterns, mkTyVarTys fields) <$ guard (null (coAxBranchCoVars branch) && eqTypes params (mkTyVarTys qvars))

    constructorPattern pattern_ty = do
      (promoted, args) <- splitTyConApp_maybe pattern_ty
      con <- isPromotedDataCon_maybe promoted
      let (params, fields) = splitAt (length (dataConUserTyVars con)) args
      guard (lengthIs params (length (dataConUserTyVars con)))
      unless (null (dataConRepArgBinders con)) $
        guard (isRetainedCaseRoot (mkTyConApp (dataConTyCon con) params) (dataConTyCon con) &&
               Core.nativeIndexArgKinds con params fields)
      pure (con, fields)

indexedSourceTableVisibleArity :: TyCon -> Maybe Int
indexedSourceTableVisibleArity table
  | hasSourceTableAxiom True (const True) table
  , not (tcIsLiftedTypeKind (tyConResKind table))
  , Just (res_tc, _ : _) <- splitTyConApp_maybe (tyConResKind table), isAlgTyCon res_tc
  , let binders = tyConBinders table
        invis_tvs = [tv | b@(Bndr tv _) <- binders, isInvisibleTyConBinder b]
        visible = length [() | Bndr _ AnonTCB <- binders]
  , visible >= 1
  , any (\tv -> let k = tyVarKind tv in not (tcIsLiftedTypeKind k) && maybe False (isAlgTyCon . fst) (splitTyConApp_maybe k)) invis_tvs
  = Just visible
  | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Unified multi-arity Core decision-tree and #guard: continuation compiler
-- ---------------------------------------------------------------------------

data CorePat = PatVar Id | PatCon DataCon [Type] [CorePat]
data GuardSpec = CallbackGuard Id Id Id | PredicateGuard CoreRhs

data CoreRhs
  = RhsVar Id
  | RhsCon DataCon [Type] [CoreRhs]
  | RhsCall TyCon [Type] [CoreRhs]
  | RhsSelf [Type] [CoreRhs]
  | RhsApply Id [CoreRhs]
  | RhsGuard GuardSpec CoreRhs CoreRhs
  | RhsTupleProj LeftOrRight Type CoreRhs

data CompiledCoreRow = CompiledCoreRow [CorePat] CoreRhs

data CompiledCoreTable = CompiledCoreTable
  { cct_binders    :: [ForAllTyBinder]
  , cct_domains    :: [Type]
  , cct_result     :: Type
  , cct_rows       :: [CompiledCoreRow]
  , cct_used_build :: Bool
  }

data CoreCompileSpec = CoreCompileSpec
  { ccs_public            :: TyCon -> Bool
  , ccs_helper            :: Id -> Maybe TyCon
  , ccs_predicate         :: Id -> Maybe TyCon
  , ccs_bindings          :: [(Id, Core.CoreExpr)]
  , ccs_spec_worker       :: Maybe Id
  , ccs_partial_root      :: Bool
  , ccs_allow_build       :: Bool
  , ccs_allow_zero_cases  :: Bool
  , ccs_allow_empty_root  :: Bool
  , ccs_allow_tail_self   :: Bool
  , ccs_allow_accum_self  :: Bool
  , ccs_allow_sct_self    :: Bool
  , ccs_allow_call_self   :: Bool
  , ccs_allow_top_apply   :: Bool
  , ccs_allow_guard       :: Bool
  , ccs_check_kind_public :: Bool
  , ccs_fixed_kinds       :: [Type]
  , ccs_witness_root      :: Maybe DataCon
  }

defaultCoreSpec :: (TyCon -> Bool) -> CoreCompileSpec
defaultCoreSpec public = CoreCompileSpec
  { ccs_public = public, ccs_helper = const Nothing, ccs_predicate = const Nothing
  , ccs_bindings = [], ccs_spec_worker = Nothing, ccs_partial_root = False
  , ccs_allow_build = False, ccs_allow_zero_cases = False, ccs_allow_empty_root = False
  , ccs_allow_tail_self = False, ccs_allow_accum_self = False, ccs_allow_sct_self = False
  , ccs_allow_call_self = False, ccs_allow_top_apply = False
  , ccs_allow_guard = False, ccs_check_kind_public = True, ccs_fixed_kinds = [], ccs_witness_root = Nothing }

isPatVar :: CorePat -> Bool
isPatVar PatVar{} = True
isPatVar _        = False

isPatCon :: CorePat -> Bool
isPatCon PatCon{} = True
isPatCon _        = False

hasPatVar :: Id -> CorePat -> Bool
hasPatVar v (PatVar u)          = v == u
hasPatVar v (PatCon _ _ fields) = any (hasPatVar v) fields

patIds :: CorePat -> [Id]
patIds (PatVar u)          = [u]
patIds (PatCon _ _ fields) = concatMap patIds fields

anyRhs :: (CoreRhs -> Bool) -> CoreRhs -> Bool
anyRhs p r = p r || case r of
  RhsCon _ _ as      -> any (anyRhs p) as
  RhsCall _ _ as     -> any (anyRhs p) as
  RhsSelf _ as       -> any (anyRhs p) as
  RhsApply _ as      -> any (anyRhs p) as
  RhsGuard (PredicateGuard pr) n y -> anyRhs p pr || anyRhs p n || anyRhs p y
  RhsGuard _ n y     -> anyRhs p n || anyRhs p y
  RhsTupleProj _ _ i -> anyRhs p i
  _                  -> False

rhsHasSelf :: CoreRhs -> Bool
rhsHasSelf = anyRhs (\case RhsSelf{} -> True; _ -> False)

rhsHasTupleProj :: CoreRhs -> Bool
rhsHasTupleProj = anyRhs (\case RhsTupleProj{} -> True; _ -> False)

rhsHasCallbackGuard :: CoreRhs -> Bool
rhsHasCallbackGuard (RhsGuard CallbackGuard{} _ _) = True
rhsHasCallbackGuard _                              = False

rhsHasPredicateGuard :: CoreRhs -> Bool
rhsHasPredicateGuard (RhsGuard PredicateGuard{} _ _) = True
rhsHasPredicateGuard _                               = False

splitCoreSignature :: Type -> Maybe ([ForAllTyBinder], [Type], Type)
splitCoreSignature tau = do
    let (telescope, body) = splitForAllForAllTyBinders tau
        splitArrows t | Just (a, r) <- caseArrow t = let (as, o) = splitArrows r in (a : as, o) | otherwise = ([], t)
        (domains, result) = splitArrows body
    binders <- traverse ordinaryCaseBinder telescope
    (binders, domains, result) <$ guard (not (null domains))

resolveCaseAlias :: [(Id, Id)] -> Id -> Id
resolveCaseAlias aliases v = fromMaybe v (lookup v aliases)

completeCase :: TyCon -> [DataCon] -> Bool
completeCase tc actual =
    let expected = tyConDataCons tc in lengthIs actual (length expected) && mkUniqSet actual == mkUniqSet expected

data BranchState = BranchState
  { bs_scope   :: InScopeSet
  , bs_root    :: Bool
  , bs_pats    :: [CorePat]
  , bs_aliases :: [(Id, Id)]
  , bs_known   :: [(Id, (DataCon, [Type], [Id]))]
  , bs_lets    :: [(Id, Core.CoreExpr)]
  , bs_susps   :: [(Id, (Type, CoreRhs))]
  , bs_projs   :: [(Id, CoreRhs)]
  }

compileCoreEquations :: CoreCompileSpec -> Id -> Core.CoreExpr -> Maybe CompiledCoreTable
compileCoreEquations spec owner original_rhs = do
    let rhs0 = stripTicksE isSourceBreakpoint original_rhs
    guard (exprType rhs0 `eqType` varType owner)
    (declared_tele, _, _) <- splitCoreSignature (varType owner)
    (telescope, domains, result) <- splitCoreSignature (exprType rhs0)
    let has_retained_root = isJust (ccs_witness_root spec) ||
                            any (\d -> maybe False (isRetainedCaseRoot d . fst) (caseDatatype d)) domains
    guard (has_retained_root || not (hasNativeDependentExprRhs original_rhs))
    let (all_lams, raw_body) = Core.collectBinders rhs0
        (parameters, prefix_terms) = splitAt (length telescope) all_lams
    guard (parameters == binderVars telescope && all isId prefix_terms &&
           length prefix_terms <= length domains && eqTypes (map varType prefix_terms) (take (length prefix_terms) domains))
    let q_tys = mkTyVarTys parameters
        allowed_kinds = q_tys ++ ccs_fixed_kinds spec
        public tc = ccs_public spec tc || tc == listTyCon || isBoxedTupleTyCon tc
        public_kind = if ccs_check_kind_public spec then casePublicKind public allowed_kinds else const True
        public_dom ty
          | not (ccs_check_kind_public spec) = True
          | Just (i, o) <- caseArrow ty =
              let ok t = maybe (public_kind t) (\(a, b) -> public_kind a && public_kind b) (caseArrow t) in ok i && ok o
          | otherwise = public_kind ty
    guard (isJust (ccs_witness_root spec) || (public_dom result && all public_dom domains))
    (terms, init_aliases, mb_local_worker, target_loop, unboxed_pair_mode, body0) <-
      unwrapWorkerWrapper parameters prefix_terms domains result raw_body
    let eff_result = maybe result (\l -> mkTupleTy Unboxed [l, l]) unboxed_pair_mode
        init_st = BranchState (mkInScopeSet (exprsFreeVars [rhs0])) True (map PatVar terms) init_aliases [] [] [] []

        descend st raw_e
          | Just (st_nt, nt_body) <- exposeNewtypeUnwrap public public_kind st (stripTicksE isSourceBreakpoint raw_e)
          = descend st_nt nt_body
        descend st raw_e =
          let body = stripTicksE isSourceBreakpoint raw_e
              scoped v = any (hasPatVar v) (bs_pats st)
              st_inner = st { bs_root = False }
          in case body of
            Core.Let (Core.NonRec b _) rest
              | isId b, not (isUnliftedType (varType b)), not (isJoinId b)
              , not (b `elemVarSet` exprsFreeVars [rest]) -> descend st rest
            Core.Let (Core.NonRec j _) rest | isJoinId j -> descend st rest
            Core.Let (Core.NonRec b (Core.Var target)) rest
              | isId b, isId target, b /= target, not (isJoinId b), not (scoped b)
              , isNothing (lookup b (bs_aliases st)), varType b `eqType` varType target
              , let sel = resolveCaseAlias (bs_aliases st) target
              , scoped sel || isJust (lookup sel (bs_known st)) ->
                  descend (st { bs_aliases = (b, sel) : bs_aliases st }) rest
            Core.Let (Core.NonRec susp call) rest
              | isId susp, not (isJoinId susp), not (bs_root st)
              , Just (tup_tc, [l1, l2]) <- splitTyConApp_maybe (varType susp)
              , tup_tc == tupleTyCon Boxed 2, l1 `eqType` l2
              , Just raw_call <- case unboxed_pair_mode of
                  Just list_ty | list_ty `eqType` l1 -> reboxedCall list_ty call
                  Nothing -> Just call
                  _ -> Nothing
              , Just (self_rhs, False) <- checkSelfCall st raw_call ->
                  descend (st_inner { bs_susps = (susp, (l1, self_rhs)) : bs_susps st }) rest
            Core.Let (Core.NonRec alias sel_expr) rest
              | isId alias, not (isJoinId alias), not (bs_root st)
              , Just (side, list_ty, self_rhs) <- matchSuspendedProj (bs_susps st) sel_expr ->
                  descend (st_inner { bs_projs = (alias, RhsTupleProj side list_ty self_rhs) : bs_projs st }) rest
            Core.Let (Core.NonRec b bound) rest
              | isId b, not (isUnliftedType (varType b)), not (isJoinId b)
              , not (b `elemVarSet` exprsFreeVars [bound]) ->
                  descend (st { bs_lets = (b, bound) : bs_lets st }) rest

            Core.Case (Core.Var actual) _ case_res alts
              | not (bs_root st) || ccs_allow_empty_root spec, case_res `eqType` eff_result
              , let scrut = resolveCaseAlias (bs_aliases st) actual
              , scoped scrut, isJust (emptyCaseDatatype (varType scrut))
              , case alts of { [] -> True; [Core.Alt Core.DEFAULT [] _] -> True; _ -> False } ->
                  pure ([], False)
            Core.Case (Core.Var actual) binder case_res alts
              | not (bs_root st), let scrut = resolveCaseAlias (bs_aliases st) actual
              , Just (dc, _, fields) <- lookup scrut (bs_known st)
              , varType binder `eqType` varType actual, case_res `eqType` eff_result
              , Just (Core.Alt key actual_fs rhs_e) <- findAlt (Core.DataAlt dc) alts ->
                  case key of
                    Core.DEFAULT | null actual_fs ->
                      descend (st_inner { bs_aliases = (binder, scrut) : bs_aliases st }) rhs_e
                    Core.DataAlt chosen | chosen == dc, all isId actual_fs, eqTypes (map varType actual_fs) (map varType fields) ->
                      descend (st_inner { bs_aliases = (binder, scrut) : zip actual_fs fields ++ bs_aliases st }) rhs_e
                    _ -> Nothing
            Core.Case (Core.Var actual) binder case_res raw_alts
              | let scrut = resolveCaseAlias (bs_aliases st) actual
              , scoped scrut, varType binder `eqType` varType actual, case_res `eqType` eff_result -> do
                  (tc, args) <- caseDatatype (varType scrut)
                  guard (public tc && all public_kind args)
                  (in_scope', alts) <- expandCaseDefaults (bs_scope st) (not (ccs_partial_root spec)) tc args raw_alts
                  let st_br = st_inner { bs_scope = in_scope', bs_aliases = (binder, scrut) : bs_aliases st }
                  checked_alts <- traverse (checkAlt (ccs_partial_root spec && bs_root st) st_br scrut tc args) alts
                  let (dcs, row_lists, build_flags) = unzip3 checked_alts
                  guard (completeCase tc dcs)
                  pure (concat row_lists, or build_flags)

            Core.Case test g_binder case_res [Core.Alt (Core.DataAlt no_dc) [] no_e, Core.Alt (Core.DataAlt yes_dc) [] yes_e]
              | ccs_allow_guard spec, not (bs_root st), no_dc == falseDataCon, yes_dc == trueDataCon
              , case_res `eqType` eff_result, varType g_binder `eqType` boolTy
              , null (bs_susps st), null (bs_projs st) -> do
                  guard_spec <- matchGuardSpec st test
                  let st_arm = st_inner { bs_susps = [], bs_projs = [] }
                  ([CompiledCoreRow _ no_rhs], b1) <- descend st_arm no_e
                  ([CompiledCoreRow _ yes_rhs], b2) <- descend st_arm yes_e
                  guard (not (rhsHasCallbackGuard no_rhs || rhsHasCallbackGuard yes_rhs ||
                              rhsHasPredicateGuard no_rhs || rhsHasPredicateGuard yes_rhs))
                  pure ([CompiledCoreRow (bs_pats st) (RhsGuard guard_spec no_rhs yes_rhs)], b1 || b2)

            _ | not (bs_root st), isJust (matchAbsurdLeaf (bs_aliases st) scoped body) -> pure ([], False)
              | not (bs_root st) || ccs_allow_zero_cases spec -> do
                  (leaf, used_b) <- checkLeaf [] False True st eff_result body
                  guard ((null (bs_susps st) || rhsHasTupleProj leaf) &&
                         all (\(a, _) -> isNothing (lookup a (bs_lets st))) (bs_projs st))
                  let norm_leaf = case unboxed_pair_mode of
                        Just list_ty | RhsCon dc _ cs <- leaf, dc == tupleDataCon Unboxed 2 ->
                          RhsCon (tupleDataCon Boxed 2) [list_ty, list_ty] cs
                        _ -> leaf
                  pure ([CompiledCoreRow (bs_pats st) norm_leaf], used_b)
              | otherwise -> Nothing

        checkAlt partial_here st scrut tc args (Core.Alt (Core.DataAlt dc) fields alt_body) = do
          guard (dataConTyCon dc == tc && dataConUserTyVars dc == dataConUnivTyVars dc)
          let domain = mkTyConApp tc args; kinds = map varType fields
          if isRetainedCaseRoot domain tc
            then guard (all isNativeOpaqueValueId fields && Core.nativeIndexArgKinds dc args (map TermVarTy fields) &&
                        all (publicFieldKind dc allowed_kinds (map TermVarTy fields) public) kinds)
            else do
              expected <- openDataConFieldTypes dc args
              guard (all isId fields && eqTypes kinds expected && all public_kind kinds)
          let replace (PatVar v) | v == scrut = (True, PatCon dc args (map PatVar fields))
              replace pat@PatVar{} = (False, pat)
              replace (PatCon con tys cs) = let (fs, cs') = unzip (map replace cs) in (or fs, PatCon con tys cs')
              (replaced, patterns') = unzip (map replace (bs_pats st))
          guard (or replaced)
          let sub_res = descend (st { bs_pats = patterns', bs_known = (scrut, (dc, args, fields)) : bs_known st }) alt_body
          if partial_here
            then case sub_res of
              Just ([row@(CompiledCoreRow lhs _)], b) | equalLength lhs patterns', and (zipWith samePat lhs patterns') ->
                pure (dc, [row], b)
              _ -> pure (dc, [], False)
            else (\(rows, b) -> (dc, rows, b)) <$> sub_res
        checkAlt _ _ _ _ _ _ = Nothing

        matchGuardSpec st test = matchCallback <|> matchPred
          where
            matchCallback = do
              [PatVar p, PatCon dc _ [PatVar x, PatVar xs]] <- pure (bs_pats st)
              (Core.Var pred_fn, [Core.Var arg_v]) <- pure (Core.collectArgs (stripTicksE isSourceBreakpoint test))
              guard (dc == consDataCon && resolveCaseAlias (bs_aliases st) pred_fn == p &&
                     resolveCaseAlias (bs_aliases st) arg_v == x)
              pure (CallbackGuard p x xs)
            matchPred = do
              (Core.Var callee, arguments) <- pure (Core.collectArgs (stripTicksE isSourceBreakpoint test))
              table <- ccs_predicate spec callee
              let (type_args, fields) = splitAt (helperInvisibleArity table) arguments
                  st_pred = st { bs_known = [], bs_susps = [], bs_projs = [] }
              args <- coreTypeArgs type_args
              guard (all public_kind args)
              (input_kinds, result_kind) <- helperKinds table args
              guard (result_kind `eqType` boolTy && lengthIs fields (length input_kinds))
              children_pairs <- zipWithM (checkLeaf [] False False st_pred) input_kinds fields
              let (children, b_flags) = unzip children_pairs
              PredicateGuard (RhsCall table args children) <$ guard (not (or b_flags))

        checkSelfCall st expr = checkSpecCall <|> checkDirectOrLocal
          where
            patterns = bs_pats st; aliases = bs_aliases st
            checkSpecCall = do
              worker <- ccs_spec_worker spec
              [_, other] <- pure terms
              [first@(PatCon con _ [PatVar hd, PatVar tl]), second] <- pure patterns
              (Core.Var callee, Core.Type x : Core.Type y : operands) <- pure (Core.collectArgs expr)
              guard (con == consDataCon && callee == worker && eqTypes [x, y] q_tys)
              [a_hd, a_tl, a_other] <- traverse (\case Core.Var v -> Just (resolveCaseAlias aliases v); _ -> Nothing) operands
              guard (a_hd == hd && a_tl == tl)
              (True, smaller) <- recursiveOperand (other, second) a_other
              pure (RhsSelf q_tys (map patToRhs [first, smaller]), False)

            checkDirectOrLocal = do
              (Core.Var callee, raw_args) <- pure (Core.collectArgs expr)
              operands <- if callee == target_loop
                then do
                  let (kind_args, val_args) = splitAt (length q_tys) raw_args
                  tys <- coreTypeArgs kind_args
                  val_args <$ guard (eqTypes tys q_tys && equalLength val_args terms)
                else case mb_local_worker of
                  Just (w_id, carried) | callee == w_id -> do
                    guard (not (any Core.isTyCoArg raw_args) && length carried + length raw_args == length terms)
                    pure (map Core.Var carried ++ raw_args)
                  _ -> Nothing
              if ccs_allow_sct_self spec
                then do
                  (rhs_args, build_flags) <- unzip <$>
                    zipWithM (\orig arg_e -> checkLeaf [] True True st (varType orig) arg_e) terms operands
                  pure (RhsSelf q_tys rhs_args, or build_flags)
                else do
                  checked <- zipWithM checkRecArg (zip terms patterns) operands
                  let (dec_flags, rhs_args, build_flags) = unzip3 checked
                  guard (or dec_flags)
                  when (length [() | PatCon{} <- patterns] == 1 && not (ccs_allow_accum_self spec)) $
                    forM_ (zip3 terms patterns checked) $ \(orig, pat, (dec, _, _)) -> when dec $ case pat of
                      PatCon _ _ children ->
                        guard $ not $ null [v | Core.Var raw_v <- operands, let v = resolveCaseAlias aliases raw_v
                                              , any (\case PatVar c -> c == v; _ -> False) children, varType v `eqType` varType orig]
                      _ -> Nothing
                  pure (RhsSelf q_tys rhs_args, or build_flags)

            checkRecArg (orig, pat) arg_e
              | Core.Var raw_v <- stripTicksE isSourceBreakpoint arg_e = do
                  (dec, sub_pat) <- recursiveOperand (orig, pat) (resolveCaseAlias aliases raw_v)
                  pure (dec, patToRhs sub_pat, False)
              | ccs_allow_accum_self spec, PatVar _ <- pat = do
                  (acc_rhs, used_b) <- checkLeaf [] True True (st { bs_known = [] }) (varType orig) arg_e
                  pure (False, acc_rhs, used_b)
              | otherwise = Nothing

            recursiveOperand (orig, pat) field
              | field == orig = Just (False, pat)
              | PatCon _ _ cs <- pat, varType field `eqType` varType orig, any (hasPatVar field) cs = Just (True, PatVar field)
              | otherwise = Nothing
            patToRhs (PatVar v)        = RhsVar v
            patToRhs (PatCon dc ts fs) = RhsCon dc ts (map patToRhs fs)

        checkLeaf seen under_con allow_full st expected raw_expr = do
          let expr = stripTicksE isSourceBreakpoint raw_expr
              scoped v = any (hasPatVar v) (bs_pats st)
          guard (exprType expr `eqType` expected)
          case splitTyConApp_maybe expected of
            Just (tc, [el]) | tc == listTyCon, Just built <- unwrapBuildExpr el expr -> do
              (res, _) <- checkLeaf seen under_con allow_full st expected built
              pure (res, True)
            _ -> checkNonBuild seen under_con scoped expr
          where
            checkNonBuild seen_vs under_c scoped expr =
              (do Core.Cast inner_expr sym_co <- pure expr
                  (tc, args) <- caseDatatype expected
                  guard (isNewTyCon tc && public tc && all public_kind args)
                  nt_ax <- newTyConCo_maybe tc
                  [dc] <- pure (tyConDataCons tc)
                  [field_ty] <- openDataConFieldTypes dc args
                  guard (public_kind field_ty && isNewtypeWrapCo nt_ax field_ty expected sym_co)
                  (child, b_flag) <- checkLeaf seen_vs True allow_full st field_ty inner_expr
                  pure (RhsCon dc args [child], b_flag)) <|>
              (do guard under_c
                  (side, list_ty, self_rhs) <- matchSuspendedProj (bs_susps st) expr
                  (RhsTupleProj side list_ty self_rhs, False) <$ guard (list_ty `eqType` expected)) <|>
              (case Core.collectArgs expr of
                (Core.Var out_v, []) | isNothing (isDataConWorkId_maybe out_v) ->
                  let sel = resolveCaseAlias (bs_aliases st) out_v
                  in (guard (scoped sel && varType sel `eqType` expected) $> (RhsVar sel, False)) <|>
                     (guard under_c *> ((, False) <$> lookup out_v (bs_projs st))) <|>
                     ((, False) <$> reconstructKnown scoped (bs_known st) sel) <|>
                     (do guard (out_v `notElem` seen_vs)
                         bound <- lookup out_v (bs_lets st) <|>
                                  (guard (not (isExternalName (idName out_v))) *> lookup out_v (ccs_bindings spec))
                         checkLeaf (out_v : seen_vs) under_c allow_full st expected bound)

                (Core.Var worker, args)
                  | allow_full, Just list_ty <- unboxed_pair_mode, not under_c
                  , worker == dataConWorkId (tupleDataCon Unboxed 2)
                  , expected `eqType` mkTupleTy Unboxed [list_ty, list_ty]
                  , (_, [f1, f2]) <- span Core.isTypeArg args -> do
                      (c1, b1) <- checkLeaf seen_vs True allow_full st list_ty f1
                      (c2, b2) <- checkLeaf seen_vs True allow_full st list_ty f2
                      pure (RhsCon (tupleDataCon Unboxed 2) [list_ty, list_ty] [c1, c2], b1 || b2)

                (Core.Var worker, arguments) | Just dc <- isDataConWorkId_maybe worker -> do
                  let tc = dataConTyCon dc; universals = dataConUnivTyVars dc
                      (type_args, fields) = splitAt (length universals) arguments
                  guard (public tc && dataConUserTyVars dc == universals)
                  args <- coreTypeArgs type_args
                  guard (all public_kind args && mkTyConApp tc args `eqType` expected)
                  kinds <- openDataConFieldTypes dc args
                  guard (lengthIs fields (length kinds))
                  (children, b_flags) <- unzip <$> zipWithM (checkLeaf seen_vs True allow_full st) kinds fields
                  pure (RhsCon dc args children, or b_flags)

                (Core.Var callee, arguments) | Just table <- ccs_helper spec callee -> do
                  let (type_args, fields) = splitAt (helperInvisibleArity table) arguments
                  args <- coreTypeArgs type_args
                  guard (all public_kind args)
                  (input_kinds, result_kind) <- helperKinds table args
                  guard (result_kind `eqType` expected && lengthIs fields (length input_kinds))
                  (children, b_flags) <- unzip <$>
                    zipWithM (checkLeaf seen_vs (under_c || ccs_allow_call_self spec) allow_full st) input_kinds fields
                  pure (RhsCall table args children, or b_flags)

                _ ->
                  (guard (allow_full && (under_c || ccs_allow_tail_self spec)) *> checkSelfCall st expr) <|>
                  (do guard (allow_full && (under_c || ccs_allow_top_apply spec))
                      (Core.Var raw_fn, val_args@(_ : _)) <- pure (Core.collectArgs expr)
                      let fn = resolveCaseAlias (bs_aliases st) raw_fn
                      guard (scoped fn && not (any Core.isTyCoArg val_args))
                      (arg_doms, fn_res) <- peelFunArgs (length val_args) (varType fn)
                      guard (fn_res `eqType` expected)
                      (rhs_args, b_flags) <- unzip <$> zipWithM (checkLeaf seen_vs True allow_full st) arg_doms val_args
                      pure (RhsApply fn rhs_args, or b_flags)))

    (rows, used_build) <- descend init_st body0
    guard (not (null rows) || (ccs_allow_empty_root spec && any (isJust . emptyCaseDatatype) domains))
    forM_ (ccs_witness_root spec) $ \con -> do
      [CompiledCoreRow [PatCon actual _ (PatVar witness : payloads)] (RhsVar value)] <- pure rows
      guard (actual == con && value == witness && all isPatVar payloads)
    pure CompiledCoreTable
      { cct_binders    = zipWith (\(Bndr v _) (Bndr _ vis) -> Bndr v vis) telescope declared_tele
      , cct_domains    = domains
      , cct_result     = result
      , cct_rows       = rows
      , cct_used_build = used_build
      }
  where
    ($>) = flip (<$)

    isNewtypeUnwrapCo nt_ax src_ty dst_ty co@(AxiomCo (UnbranchedAxiom ax) cos) =
      ax == nt_ax && all isReflCo cos &&
      coercionLKind co `eqType` src_ty && coercionRKind co `eqType` dst_ty
    isNewtypeUnwrapCo _ _ _ _ = False

    isNewtypeWrapCo nt_ax src_ty dst_ty (SymCo co) = isNewtypeUnwrapCo nt_ax dst_ty src_ty co
    isNewtypeWrapCo _ _ _ _                        = False

    exposeNewtypeUnwrap public public_kind st expr = do
      (scrut, nt_ax, dc, args, field_ty) <- findUnwrap expr
      let f_id = uniqAway (bs_scope st) (mkSysLocal (fsLit "nt") minLocalUnique ManyTy field_ty)
          in_scope' = extendInScopeSet (bs_scope st) f_id
          replace (PatVar v) | v == scrut = (True, PatCon dc args [PatVar f_id])
          replace pat@PatVar{}            = (False, pat)
          replace (PatCon con tys cs)     = let (fs, cs') = unzip (map replace cs) in (or fs, PatCon con tys cs')
          (replaced, patterns') = unzip (map replace (bs_pats st))
      guard (or replaced)
      let rewrite (Core.Cast (Core.Var actual) co)
            | resolveCaseAlias (bs_aliases st) actual == scrut
            , isNewtypeUnwrapCo nt_ax (varType scrut) field_ty co
            = Core.Var f_id
          rewrite (Core.App a b)        = Core.App (rewrite a) (rewrite b)
          rewrite (Core.Lam b e)        = Core.Lam b (rewrite e)
          rewrite (Core.Let b e)        = Core.Let (rewriteBind b) (rewrite e)
          rewrite (Core.Case s b t as)  = Core.Case (rewrite s) b t [Core.Alt c bs (rewrite r) | Core.Alt c bs r <- as]
          rewrite (Core.Cast e co)      = Core.Cast (rewrite e) co
          rewrite (Core.Tick t e)       = Core.Tick t (rewrite e)
          rewrite other                 = other
          rewriteBind (Core.NonRec b r) = Core.NonRec b (rewrite r)
          rewriteBind (Core.Rec ps)     = Core.Rec [(b, rewrite r) | (b, r) <- ps]
          st' = st { bs_root  = False
                   , bs_scope = in_scope'
                   , bs_pats  = patterns'
                   , bs_known = (scrut, (dc, args, [f_id])) : bs_known st }
      pure (st', rewrite expr)
      where
        is_pat_var v = any (hasPatVar v) (bs_pats st) &&
                       all (\case PatCon _ _ _ -> True; _ -> True) (bs_pats st) &&
                       isNothing (lookup v (bs_known st))
        findUnwrap (Core.Cast (Core.Var actual) co)
          | let scrut = resolveCaseAlias (bs_aliases st) actual
          , is_pat_var scrut
          , Just (tc, args) <- caseDatatype (varType scrut)
          , isNewTyCon tc && public tc && all public_kind args
          , Just nt_ax <- newTyConCo_maybe tc
          , [dc] <- tyConDataCons tc
          , Just [field_ty] <- openDataConFieldTypes dc args
          , public_kind field_ty
          , isNewtypeUnwrapCo nt_ax (varType scrut) field_ty co
          = Just (scrut, nt_ax, dc, args, field_ty)
        findUnwrap (Core.App a b)       = findUnwrap a <|> findUnwrap b
        findUnwrap (Core.Lam _ e)       = findUnwrap e
        findUnwrap (Core.Let (Core.NonRec _ r) e) = findUnwrap r <|> findUnwrap e
        findUnwrap (Core.Let (Core.Rec ps) e)     = foldr (<|>) (findUnwrap e) (map (findUnwrap . snd) ps)
        findUnwrap (Core.Case s _ _ as) = findUnwrap s <|> foldr (<|>) Nothing [findUnwrap r | Core.Alt _ _ r <- as]
        findUnwrap (Core.Cast e _)      = findUnwrap e
        findUnwrap (Core.Tick _ e)      = findUnwrap e
        findUnwrap _                    = Nothing

    collectValueLams (Core.Lam v e) | isId v = let (vs, rest) = collectValueLams e in (v : vs, rest)
    collectValueLams e = ([], e)

    unwrapWorkerWrapper parameters prefix_terms domains result raw_body
      | length prefix_terms == length domains, [p, ls] <- prefix_terms
      , Just (root, [_]) <- caseDatatype (varType ls), root == listTyCon
      , result `eqType` mkTupleTy Boxed [varType ls, varType ls]
      , Just call <- reboxedCall (varType ls) raw_body
      , Just worker <- guardedPairCall (mkTyVarTys parameters) p ls call, worker /= owner
      , Just w_expr <- lookup worker (ccs_bindings spec)
      , let w_rhs = stripTicksE isSourceBreakpoint w_expr, exprType w_rhs `eqType` varType worker
      , let (w_lams, w_body) = Core.collectBinders w_rhs
            (w_tvs, w_terms) = splitAt (length parameters) w_lams
      , lengthIs w_tvs (length parameters), all isTyVar w_tvs, lengthIs w_terms 2, all isId w_terms
      , let subst_in_scope = mkInScopeSet (exprsFreeVars [w_rhs, raw_body])
            w_terms' = [setIdType v (substTyWith w_tvs (mkTyVarTys parameters) (varType v)) | v <- w_terms]
            full_sub = CoreSubst.mkOpenSubst subst_in_scope
                         ([(tv, Core.Type (mkTyVarTy p_tv)) | (tv, p_tv) <- zip w_tvs parameters] ++
                          [(old_v, Core.Var new_v) | (old_v, new_v) <- zip w_terms w_terms'])
            inst_body = stripTicksE isSourceBreakpoint (CoreSubst.substExpr full_sub w_body)
      , eqTypes (map varType w_terms') domains
      = Just (w_terms', [], Nothing, worker, Just (varType (w_terms' !! 1)), inst_body)

      | Core.Let (Core.Rec [(worker, worker_rhs)]) invoke <- raw_body
      , (Core.Var invoked, invoke_args) <- Core.collectArgs invoke, invoked == worker
      , Just supplied <- traverse (\case Core.Var v -> Just v; _ -> Nothing) invoke_args
      , let n_carried = length prefix_terms - length supplied, n_carried >= 0
      , drop n_carried prefix_terms == supplied
      , let carried = take n_carried prefix_terms; (w_params, w_body) = collectValueLams worker_rhs
      , not (null w_params), length carried + length w_params == length domains
      , eqTypes (map varType (carried ++ w_params)) domains
      = Just (carried ++ w_params, zip supplied w_params, Just (worker, carried), owner, Nothing, w_body)

      | length prefix_terms == length domains = Just (prefix_terms, [], Nothing, owner, Nothing, raw_body)
      | otherwise = Nothing

    expandCaseDefaults in_scope True tc args (Core.Alt Core.DEFAULT [] def_body : rest) | not (null (tyConDataCons tc)) = do
      let explicit = [dc | Core.Alt (Core.DataAlt dc) _ _ <- rest]
          missing = filter (`notElem` explicit) (tyConDataCons tc)
          alloc sc [] = Just (sc, [])
          alloc sc (fty : ftys) = do
            let v' = uniqAway sc (mkSysLocal (fsLit "def") minLocalUnique ManyTy fty)
            (sc'', vs) <- alloc (extendInScopeSet sc v') ftys
            pure (sc'', v' : vs)
          build sc [] = Just (sc, [])
          build sc (dc : dcs) = do
            ftys <- openDataConFieldTypes dc args
            (sc', fs) <- alloc sc ftys
            (sc'', more) <- build sc' dcs
            pure (sc'', Core.Alt (Core.DataAlt dc) fs def_body : more)
      guard (equalLength explicit rest && not (null missing))
      (in_scope', inferred) <- build in_scope missing
      pure (in_scope', mergeAlts rest inferred)
    expandCaseDefaults in_scope _ _ _ alts = Just (in_scope, alts)

    samePat (PatVar a) (PatVar b) = a == b
    samePat (PatCon c1 _ fs1) (PatCon c2 _ fs2) = c1 == c2 && equalLength fs1 fs2 && and (zipWith samePat fs1 fs2)
    samePat _ _ = False

    publicFieldKind dc parameters fields public kind
      | Just (family, index) <- splitAppTy_maybe kind, any (eqType family) parameters, any (eqType index) fields = True
      | Just dc == ccs_witness_root spec = caseKind (parameters ++ fields) kind
      | otherwise = casePublicKind public (parameters ++ fields) kind

    matchAbsurdLeaf aliases scoped expr
      | (Core.Var f, args) <- Core.collectArgs expr
      , (_ : _, [Core.Var raw_v]) <- span Core.isTypeArg args
      , Just m <- nameModule_maybe (idName f), moduleUnitId m == ghcInternalUnitId
      , occNameString (nameOccName (idName f)) == "absurd"
      , let v = resolveCaseAlias aliases raw_v
      , scoped v, isJust (emptyCaseDatatype (varType v)) = Just v
      | otherwise = Nothing

    matchSuspendedProj susp_pairs expr = do
      Core.Case (Core.Var pair_v) _ kind [Core.Alt (Core.DataAlt con) [first, second] (Core.Var sel)] <-
        pure (stripTicksE isSourceBreakpoint expr)
      (list_ty, self_rhs) <- lookup pair_v susp_pairs
      guard (con == tupleDataCon Boxed 2 && kind `eqType` list_ty)
      side <- if sel == first then Just CLeft else if sel == second then Just CRight else Nothing
      pure (side, list_ty, self_rhs)

    reconstructKnown scoped known u
      | scoped u = Just (RhsVar u)
      | Just (dc, c_args, fs) <- lookup u known = RhsCon dc c_args <$> traverse (reconstructKnown scoped known) fs
      | otherwise = Nothing

    unwrapBuildExpr el expr
      | ccs_allow_build spec
      , (Core.Var b_id, [Core.Type b_el, Core.Lam tv (Core.Lam c_id (Core.Lam n_id alg))]) <- Core.collectArgs expr
      , nameModule_maybe (idName b_id) == Just gHC_INTERNAL_BASE
      , occNameString (nameOccName (idName b_id)) == "build"
      , b_el `eqType` el, isTyVar tv, isId c_id, isId n_id = expandAlg c_id n_id (stripTicksE isSourceBreakpoint alg)
      | otherwise = Nothing
      where
        expandAlg _ n_id (Core.Var v) | v == n_id = Just (Core.mkConApp nilDataCon [Core.Type el])
        expandAlg c_id n_id e
          | (Core.Var hd_fn, [item, rest]) <- Core.collectArgs e, hd_fn == c_id =
              (\r -> Core.mkConApp consDataCon [Core.Type el, item, r]) <$> expandAlg c_id n_id (stripTicksE isSourceBreakpoint rest)
        expandAlg _ _ _ = Nothing

    helperKinds table args = do
      let (invisible, visible) = span isInvisibleTyConBinder (tyConBinders table)
          vars = binderVars invisible; inst = substTyWith vars args
      guard (lengthIs args (length vars) && not (null visible) &&
             and (zipWith eqType (map typeKind args) (map (inst . varType) vars)))
      kinds <- traverse (\case Bndr v AnonTCB -> Just (varType v); _ -> Nothing) visible
      pure (map inst kinds, inst (tyConResKind table))

    peelFunArgs 0 ty = Just ([], ty)
    peelFunArgs n ty = do
      (dom, rest) <- caseArrow ty
      (\(ds, out) -> (dom : ds, out)) <$> peelFunArgs (n - 1) rest

guardedPairCall :: [Type] -> Id -> Id -> Core.CoreExpr -> Maybe Id
guardedPairCall types predicate input value = do
    (Core.Var function, args) <- pure (Core.collectArgs value)
    let (kind_args, terms) = span Core.isTypeArg args
    [Core.Var p, Core.Var ls] <- pure terms
    function <$ guard (p == predicate && ls == input && eqTypes (map Core.exprToType kind_args) types)

reboxedCall :: Type -> Core.CoreExpr -> Maybe Core.CoreExpr
reboxedCall list expr = do
    Core.Case call _ result [Core.Alt (Core.DataAlt con) [first, second] rhs] <- pure expr
    (Core.Var worker, args) <- pure (Core.collectArgs rhs)
    (_, [Core.Var left, Core.Var right]) <- pure (span Core.isTypeArg args)
    call <$ guard (con == tupleDataCon Unboxed 2 && result `eqType` mkTupleTy Boxed [list, list] &&
                   exprType call `eqType` mkTupleTy Unboxed [list, list] &&
                   worker == dataConWorkId (tupleDataCon Boxed 2) && left == first && right == second)

guardedListProjections :: Type -> TcM (Maybe (TyCon, TyCon))
guardedListProjections list = do
    envs <- tcGetFamInstEnvs
    left <- mkTyVarTy <$> freshSysTyVar "field" list
    right <- mkTyVarTy <$> freshSysTyVar "field" list
    let inputs = [list, list, mkTyConApp (promotedTupleDataCon Boxed 2) [list, list, left, right]]
        projection occ selected = do
          projected <- lookupBootModuleIdMaybe (mkGhcInternalModule (fsLit "GHC.Internal.Data.Tuple")) (mkVarOcc occ)
          pure $ do
            table <- projected >>= sourceTableId_maybe
            guard (sourceCaseVisibleArity table == Just 1)
            reduced <- reduceTyFamApp_maybe envs Nominal table inputs
            table <$ guard (reductionReducedType reduced `eqType` selected)
    (\a b -> (,) <$> a <*> b) <$> projection "fst" left <*> projection "snd" right

publishCompiledTable :: Id -> CompiledCoreTable -> TcM TyCon
publishCompiledTable owner cct = publishCompiledWithGuard owner cct >>= \case
    Just (table, []) -> pure table
    _ -> pprPanic "publishCompiledTable: unexpected guard or invalid branches" (ppr owner)

publishCompiledWithGuard :: Id -> CompiledCoreTable -> TcM (Maybe (TyCon, [TyCon]))
publishCompiledWithGuard owner cct = do
    let telescope = cct_binders cct; domains = cct_domains cct; result = cct_result cct; rows = cct_rows cct
        term = idName owner; loc = nameSrcSpan term
    mb_projs <- case [l | CompiledCoreRow _ r <- rows, Just l <- [findProjListTy r]] of
      l : _ -> guardedListProjections l
      []    -> pure (Just (listTyCon, listTyCon))
    case mb_projs of
      Nothing -> pure Nothing
      Just projs -> do
        (renaming, quantifiers) <- freshenTyVarBndrs (binderVars telescope)
        let kind_binders = zipWith mkNamedTyConBinder (map binderFlag telescope) quantifiers
            instantiate = substTyUnchecked renaming . expandTypeSynonyms
            q_tys = mkTyVarTys quantifiers; inst_res = instantiate result
        operands <- mapM (freshSysTyVar "arg" . instantiate) domains
        mb_guard <- if any (\(CompiledCoreRow _ r) -> rhsHasCallbackGuard r) rows
          then Just . Left <$> allocateCallbackGuard term kind_binders instantiate domains
          else if any (\(CompiledCoreRow _ r) -> rhsHasPredicateGuard r) rows
          then Just . Right <$> publishBooleanSelector owner inst_res
          else pure Nothing
        built <- forM rows $ \(CompiledCoreRow patterns rhs) -> do
          (lhs_pats, scoped) <- publishPats instantiate [] patterns
          let fields = reverse scoped
          pure (quantifiers ++ map snd fields, lhs_pats, fields, rhs)
        let checkValid = all (all isTyFamFree . coAxBranchLHS)
            branchesOf tc = maybe [] (fromBranches . coAxiomBranches) (closedFamilyTyConCoAxiom_maybe tc)
            hdr = kind_binders ++ mkAnonTyConBinders operands
        case mb_guard of
          Nothing -> do
            let mk_br (vars, lhs_pats, fields, rhs) fam =
                  mkCoAxBranch vars [] [] (q_tys ++ lhs_pats) (translateRhs fam projs instantiate fields rhs)
                    (map (const Nominal) vars) loc
            table <- makeSourceFamily term hdr inst_res (map mk_br built)
            pure (if checkValid (branchesOf table) then Just (table, []) else Nothing)
          Just (Right select) -> do
            let mk_br (vars, lhs_pats, fields, rhs) fam =
                  let rhs_ty = case rhs of
                        RhsGuard (PredicateGuard pr) nr yr ->
                          mkTyConApp select (map (translateRhs fam projs instantiate fields) [pr, nr, yr])
                        _ -> translateRhs fam projs instantiate fields rhs
                  in mkCoAxBranch vars [] [] (q_tys ++ lhs_pats) rhs_ty (map (const Nominal) vars) loc
            table <- makeSourceFamily term hdr inst_res (map mk_br built)
            pure (if checkValid (branchesOf table) then Just (table, [select]) else Nothing)
          Just (Left (g_name, g_ax, g_binders)) -> do
            root_name <- newImplicitBinder term mkSourceTableOcc
            root_ax <- newImplicitBinder root_name mkInstTyCoOcc
            case built of
              [(nil_vars, nil_pats, nil_fs, nil_rhs),
               (cons_vars, cons_pats, cons_fs, RhsGuard (CallbackGuard p_id x_id xs_id) no_r yes_r)]
                | Just [p_tv, x_tv, xs_tv] <- traverse (`lookup` cons_fs) [p_id, x_id, xs_id] -> do
                    let p_ty = mkTyVarTy p_tv; x_ty = mkTyVarTy x_tv; xs_ty = mkTyVarTy xs_tv
                        g_tvs = quantifiers ++ [p_tv, x_tv, xs_tv]
                        mkG con arm _ = mkCoAxBranch g_tvs [] [] (q_tys ++ [mkTyConTy (promoteDataCon con), p_ty, x_ty, xs_ty])
                                          (translateRhs spine projs instantiate cons_fs arm) (map (const Nominal) g_tvs) loc
                        spine = buildSourceFamily root_name root_ax hdr inst_res
                                  [ \_ -> mkCoAxBranch nil_vars [] [] (q_tys ++ nil_pats)
                                            (translateRhs spine projs instantiate nil_fs nil_rhs) (map (const Nominal) nil_vars) loc
                                  , \_ -> mkCoAxBranch cons_vars [] [] (q_tys ++ cons_pats)
                                            (mkTyConApp guard_tc (q_tys ++ [mkAppTy p_ty x_ty, p_ty, x_ty, xs_ty]))
                                            (map (const Nominal) cons_vars) loc ]
                        guard_tc = buildSourceFamily g_name g_ax g_binders inst_res [mkG falseDataCon no_r, mkG trueDataCon yes_r]
                    pure (if checkValid (concatMap branchesOf [spine, guard_tc]) then Just (spine, [guard_tc]) else Nothing)
              _ -> pure Nothing
  where
    findProjListTy (RhsTupleProj _ l _)    = Just l
    findProjListTy (RhsCon _ _ args)       = listToMaybe (mapMaybe findProjListTy args)
    findProjListTy (RhsCall _ _ args)      = listToMaybe (mapMaybe findProjListTy args)
    findProjListTy (RhsApply _ args)       = listToMaybe (mapMaybe findProjListTy args)
    findProjListTy (RhsGuard _ no_r yes_r) = findProjListTy no_r <|> findProjListTy yes_r
    findProjListTy _                       = Nothing

    allocateCallbackGuard term kind_binders instantiate [pred_dom, list_dom] = do
      let pred_k = instantiate pred_dom; list_k = instantiate list_dom
          el_k = case caseDatatype list_k of { Just (_, [e]) -> e; _ -> liftedTypeKind }
      g_ops <- mapM (freshSysTyVar "arg") [boolTy, pred_k, el_k, list_k]
      g_name <- newImplicitBinder term filterGuardOcc
      g_ax <- newImplicitBinder g_name mkInstTyCoOcc
      pure (g_name, g_ax, kind_binders ++ mkAnonTyConBinders g_ops)
    allocateCallbackGuard term _ _ _ = pprPanic "allocateCallbackGuard" (ppr term)

    publishPats _ scoped [] = pure ([], scoped)
    publishPats instantiate scoped (p : ps) = do
      (t, s') <- publishPat instantiate scoped p
      (ts, s'') <- publishPats instantiate s' ps
      pure (t : ts, s'')

    publishPat instantiate scoped (PatVar ident) = do
      let kind = instantiate (varType ident)
          indices = [(TermVarTy runtime, tv) | (runtime, tv) <- scoped]
          checked_k
            | not (anyFreeVarsOfType isId kind) = kind
            | Just abstracted <- abstractNativeIndices indices kind, not (anyFreeVarsOfType isId abstracted) = abstracted
            | otherwise = panic "publishCompiledWithGuard: unchecked field kind"
          occ_str = occNameString (nameOccName (varName ident))
      field <- flip mkTyVar checked_k <$> newSysName (mkTyVarOcc (if null occ_str then "field" else occ_str))
      pure (mkTyVarTy field, (ident, field) : scoped)
    publishPat instantiate scoped (PatCon dc params children) = do
      (fs, s') <- publishPats instantiate scoped children
      pure (mkTyConApp (promoteDataCon dc) (map instantiate params ++ fs), s')

translateRhs :: TyCon -> (TyCon, TyCon) -> (Type -> Type) -> [(Id, TyVar)] -> CoreRhs -> Type
translateRhs family (fst_tc, snd_tc) instantiate fields = go
  where
    fieldTy sel = maybe (panic "translateRhs: unbound field") mkTyVarTy (lookup sel fields)
    go (RhsVar sel)               = fieldTy sel
    go (RhsCon dc args children)  = mkTyConApp (promoteDataCon dc) (map instantiate args ++ map go children)
    go (RhsCall tc args children) = mkTyConApp tc (map instantiate args ++ map go children)
    go (RhsSelf args children)    = mkTyConApp family (map instantiate args ++ map go children)
    go (RhsApply fn args)         = mkAppTys (fieldTy fn) (map go args)
    go (RhsTupleProj side l r)    = let il = instantiate l in mkTyConApp (pickLR side (fst_tc, snd_tc)) [il, il, go r]
    go RhsGuard{}                 = panic "translateRhs: nested RhsGuard"

publishBooleanSelector :: Id -> Type -> TcM TyCon
publishBooleanSelector owner result = do
    let term = idName owner; loc = nameSrcSpan term
    binders <- traverse (uncurry freshSysTyVar) [("test", boolTy), ("no", result), ("yes", result)]
    no <- freshSysTyVar "no" result; yes <- freshSysTyVar "yes" result
    name <- newImplicitBinder term filterGuardOcc
    axiom <- newImplicitBinder name mkInstTyCoOcc
    let choose con sel _ = mkCoAxBranch [no, yes] [] [] (mkTyConTy (promoteDataCon con) : mkTyVarTys [no, yes])
                             (mkTyVarTy sel) [Nominal, Nominal] loc
    pure (buildSourceFamily name axiom (mkAnonTyConBinders binders) result [choose falseDataCon no, choose trueDataCon yes])

makeSourceFamily :: Name -> [TyConBinder] -> Kind -> [TyCon -> CoAxBranch] -> TcM TyCon
makeSourceFamily fun binders res_kind branches = do
    name <- newImplicitBinder fun mkSourceTableOcc
    ax_name <- newImplicitBinder name mkInstTyCoOcc
    pure (buildSourceFamily name ax_name binders res_kind branches)

buildSourceFamily :: Name -> Name -> [TyConBinder] -> Kind -> [TyCon -> CoAxBranch] -> TyCon
buildSourceFamily name ax_name binders res_kind branches =
    let flav = case branches of
          [] -> CTF Nothing
          _  -> CTF (Just axiom)
        fam_tc = mkFamilyTyCon name (mkTyConKindWithMatchability unmatchableDataConTy binders res_kind)
                   binders 0 res_kind Nothing (ClosedTypeFamilyTyCon flav) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal ax_name fam_tc (map (tidySourceAxBranch . ($ fam_tc)) branches)
    in fam_tc

-- | Assign concise, non-colliding names to synthetic function-kinded branch
-- binders so interface headers remain readable.
tidySourceAxBranch :: CoAxBranch -> CoAxBranch
tidySourceAxBranch br
  | not (any (has_fun_ty . tyVarKind) tvs) = br
  | otherwise =
      let reserved = [occNameString (nameOccName (varName v)) | v <- tvs, not (has_fun_ty (tyVarKind v))]
          go _ _ _ _ [] = ([], [])
          go seen res old_tvs new_tys (tv : rest) =
            let k'   = substTyWith old_tvs new_tys (tyVarKind tv)
                occ0 = occNameString (nameOccName (varName tv))
                cand | not (has_fun_ty k') = occ0
                     | occ0 `elem` ho_pool && occ0 `notElem` seen = occ0
                     | otherwise = fromMaybe occ0
                         (find (\c -> c `notElem` seen && c `notElem` res) ho_pool
                          <|> find (`notElem` seen) ho_pool)
                nm'  = if cand == occ0 then varName tv else tidyNameOcc (varName tv) (mkTyVarOcc cand)
                tv'  = mkTyVar nm' k'
                ty'  = mkTyVarTy tv'
                (rest_tvs, rest_tys) = go (cand : seen) res (tv : old_tvs) (ty' : new_tys) rest
            in (tv' : rest_tvs, ty' : rest_tys)
          (tvs', tys') = go [] reserved [] [] tvs
          sub = substTyWith tvs tys'
      in mkCoAxBranch tvs' [] (coAxBranchCoVars br)
           (map sub (coAxBranchLHS br)) (sub (coAxBranchRHS br))
           (coAxBranchRoles br) (coAxBranchSpan br)
  where
    tvs = coAxBranchTyVars br
    ho_pool = ["f", "g", "h", "k", "p", "q", "r", "a", "b", "c", "d", "e", "m", "n", "rel", "step"]
    has_fun_ty t = case coreFullView t of
      FunTy{}        -> True
      DepFunTy{}     -> True
      DepDictTy _ b  -> has_fun_ty b
      ForAllTy _ _ b -> has_fun_ty b
      ExistsTy _ _ b -> has_fun_ty b
      AppTy a b      -> has_fun_ty a || has_fun_ty b
      TyConApp _ as  -> any has_fun_ty as
      CastTy a _     -> has_fun_ty a
      _              -> False

-- ---------------------------------------------------------------------------
-- Canonical CoAxBranch equivalence & post-Simplifier verification (ST8)
-- ---------------------------------------------------------------------------

matchesCompiledTable :: TyCon -> Maybe TyCon -> CompiledCoreTable -> Bool
matchesCompiledTable table mb_guard cct = isJust $ do
    exp_branches <- case famTyConFlav_maybe table of
      Just (ClosedTypeFamilyTyCon (CTF Nothing))   -> Just []
      Just (ClosedTypeFamilyTyCon (CTF (Just ax))) -> Just (fromBranches (coAxiomBranches ax))
      _                                            -> Nothing
    let rows = cct_rows cct; telescope = cct_binders cct
        projs = case mb_guard >>= sourceTableCallees of
          Just cs -> let findOcc s = find (\tc -> occNameString (nameOccName (tyConName tc)) == s) cs
                     in fromMaybe (listTyCon, listTyCon) ((,) <$> findOcc "T:fst" <*> findOcc "T:snd")
          Nothing -> (listTyCon, listTyCon)
    case mb_guard of
      Nothing -> guard (equalLength rows exp_branches) >> zipWithM_ (matchRow table projs Nothing telescope) rows exp_branches
      Just guard_tc
        | Just sel <- booleanSelector table, sel == guard_tc ->
            guard (equalLength rows exp_branches) >> zipWithM_ (matchRow table projs (Just sel) telescope) rows exp_branches
        | otherwise -> do
            [exp_no, exp_yes] <- fromBranches . coAxiomBranches <$> closedFamilyTyConCoAxiom_maybe guard_tc
            [exp_nil, exp_cons] <- pure exp_branches
            [nil_row, CompiledCoreRow cons_pats (RhsGuard (CallbackGuard p_id x_id xs_id) no_r yes_r)] <- pure rows
            matchRow table projs Nothing telescope nil_row exp_nil
            let (params, fields) = splitAt (length telescope) (coAxBranchTyVars exp_cons)
                ids = concatMap patIds cons_pats
                inst = substTyWith (binderVars telescope) (mkTyVarTys params)
                f_map = zip ids fields
            guard (equalLength params telescope && equalLength ids fields && eqTypes (map (inst . varType) ids) (map varType fields))
            [p_tv, x_tv, xs_tv] <- traverse (`lookup` f_map) [p_id, x_id, xs_id]
            let p_ty = mkTyVarTy p_tv; x_ty = mkTyVarTy x_tv; xs_ty = mkTyVarTy xs_tv
            guard (coAxBranchRHS exp_cons `eqType` mkTyConApp guard_tc (mkTyVarTys params ++ [mkAppTy p_ty x_ty, p_ty, x_ty, xs_ty]))
            matchGuardArm table projs telescope [p_id, x_id, xs_id] no_r exp_no
            matchGuardArm table projs telescope [p_id, x_id, xs_id] yes_r exp_yes
  where
    matchRow fam projs mb_sel telescope (CompiledCoreRow pats rhs) branch = do
      let (params, fields) = splitAt (length telescope) (coAxBranchTyVars branch)
          ids = concatMap patIds pats
          inst = substTyWith (binderVars telescope) (mkTyVarTys params)
          f_map = zip ids fields
          patToTy (PatVar v)          = mkTyVarTy <$> lookup v f_map
          patToTy (PatCon dc c_as cs) = mkTyConApp (promoteDataCon dc) . (map inst c_as ++) <$> traverse patToTy cs
          rhsToTy (RhsGuard (PredicateGuard pr) nr yr) | Just sel <- mb_sel =
            mkTyConApp sel (map (translateRhs fam projs inst f_map) [pr, nr, yr])
          rhsToTy r = translateRhs fam projs inst f_map r
      guard (null (coAxBranchCoVars branch) && equalLength params telescope && equalLength ids fields &&
             eqTypes (map (inst . varType) ids) (map varType fields))
      lhs_tys <- traverse patToTy pats
      guard (eqTypes (mkTyVarTys params ++ lhs_tys) (coAxBranchLHS branch) && rhsToTy rhs `eqType` coAxBranchRHS branch)

    matchGuardArm fam projs telescope ids arm_r g_branch = do
      let (g_params, g_fields) = splitAt (length telescope) (coAxBranchTyVars g_branch)
          inst = substTyWith (binderVars telescope) (mkTyVarTys g_params)
      guard (equalLength g_params telescope && equalLength ids g_fields &&
             translateRhs fam projs inst (zip ids g_fields) arm_r `eqType` coAxBranchRHS g_branch)

isValidMapperTable :: CompiledCoreTable -> Bool
isValidMapperTable cct = case cct_rows cct of
    [ CompiledCoreRow [PatVar _, PatCon nil _ []] (RhsCon r_nil _ [])
      , CompiledCoreRow [PatVar cb, PatCon cons _ [PatVar h, PatVar t]]
          (RhsCon r_cons _ [RhsApply f [RhsVar x], RhsSelf _ [RhsVar f', RhsVar xs]]) ] ->
      all (== nilDataCon) [nil, r_nil] && all (== consDataCon) [cons, r_cons] && cb == f && cb == f' && h == x && t == xs
    _ -> False

isValidAppendTable :: CompiledCoreTable -> Bool
isValidAppendTable cct = case cct_rows cct of
    [ CompiledCoreRow [PatCon nil _ [], PatVar r1] (RhsVar ret)
      , CompiledCoreRow [PatCon cons _ [PatVar h, PatVar t], PatVar r2]
          (RhsCon r_cons _ [RhsVar h', RhsSelf _ [RhsVar t', RhsVar r2']]) ] ->
      nil == nilDataCon && all (== consDataCon) [cons, r_cons] && r1 == ret && h == h' && t == t' && r2 == r2'
    _ -> False

isValidFoldTable :: CompiledCoreTable -> Bool
isValidFoldTable cct = case cct_rows cct of
    [ CompiledCoreRow [PatVar _, PatVar s1, PatCon nil _ []] (RhsVar ret)
      , CompiledCoreRow [PatVar cb, PatVar s2, PatCon cons _ [PatVar h, PatVar t]]
          (RhsApply f [RhsVar x, RhsSelf _ [RhsVar cb', RhsVar s2', RhsVar xs]]) ] ->
      nil == nilDataCon && cons == consDataCon && s1 == ret && cb == f && cb == cb' && s2 == s2' && h == x && t == xs
    _ -> False

isValidReverseWorkerTable :: CompiledCoreTable -> Bool
isValidReverseWorkerTable cct = case cct_rows cct of
    [ CompiledCoreRow [PatCon nil _ [], PatVar a1] (RhsVar ret)
      , CompiledCoreRow [PatCon cons _ [PatVar h, PatVar t], PatVar a2]
          (RhsSelf _ [RhsVar t', RhsCon p_cons _ [RhsVar h', RhsVar a2']]) ] ->
      nil == nilDataCon && all (== consDataCon) [cons, p_cons] && a1 == ret && h == h' && t == t' && a2 == a2'
    _ -> False

isValidZipTable :: CompiledCoreTable -> Bool
isValidZipTable cct = case cct_rows cct of
    [ CompiledCoreRow (_ : _) (RhsCon n1 _ [])
      , CompiledCoreRow (_ : _) (RhsCon n2 _ [])
      , CompiledCoreRow pats (RhsCon c _ [_, RhsSelf{}]) ] ->
      all (== nilDataCon) [n1, n2] && c == consDataCon && length [() | PatCon dc _ _ <- pats, dc == consDataCon] == 2
    _ -> False

directListReverseWrapper :: Id -> Core.CoreExpr -> Maybe ([ForAllTyBinder], Id, Type, Id)
directListReverseWrapper owner rhs = do
    guard (exprType rhs `eqType` varType owner)
    [Bndr _ visibility] <- listReverseWrapperType (varType owner)
    ([core_element, input], body) <- pure (Core.collectBinders (stripTicksE isSourceBreakpoint rhs))
    (Core.Var callee, [Core.Type kind, Core.Var operand, empty_e]) <- pure (Core.collectArgs body)
    (Core.Var nil, [Core.Type nil_kind]) <- pure (Core.collectArgs empty_e)
    guard (isTyVar core_element && isId input && kind `eqType` mkTyVarTy core_element && operand == input &&
           nil == dataConWorkId nilDataCon && nil_kind `eqType` kind)
    pure ([Bndr core_element visibility], input, kind, callee)

compileZipCore :: (TyCon -> Bool) -> Id -> Core.CoreExpr -> Maybe CompiledCoreTable
compileZipCore visible owner rhs = do
    let ty = varType owner
    guard (isJust (listZipWithType ty) || maybe False isTwoListCase (constructorCaseType ty))
    cct <- compileCoreEquations ((defaultCoreSpec visible) { ccs_allow_top_apply = False, ccs_check_kind_public = True }) owner rhs
    cct <$ guard (isValidZipTable cct)

compileStructuralCore :: (TyCon -> Bool) -> Id -> Core.CoreExpr -> Maybe CompiledCoreTable
compileStructuralCore visible owner rhs = do
    sig@(ConstructorCaseType _ _ _ _ pos _) <- constructorCaseType (varType owner)
    guard (isTwoStructuralCase sig || isSizeListCase sig)
    cct <- compileCoreEquations ((defaultCoreSpec visible) { ccs_allow_tail_self = isTwoStructuralCase sig }) owner rhs
    let checksOther 0 (CompiledCoreRow [_, PatCon{}] _) = True
        checksOther 1 (CompiledCoreRow [PatCon{}, _] _) = True
        checksOther _ _                                 = False
    cct <$ guard (any (checksOther pos) (cct_rows cct))

verifyFinalCoreMapper :: [Core.CoreBind] -> Id -> Bool
verifyFinalCoreMapper binds original =
    case [p | p@(v, _) <- bindings, idName v == idName original] of
      [(owner, expression)]
        | not (varType owner `eqType` varType original) -> False
        | isIdentitySourceType (varType owner) -> verifyIdentity (unitNameSet (idName owner)) owner expression
        | isJust (compositionSourceType (varType owner)), Just table <- sourceTableId_maybe original ->
            matchesSpec ((defaultCoreSpec (const True))
              { ccs_allow_zero_cases = True, ccs_allow_top_apply = True, ccs_check_kind_public = False })
              table Nothing owner expression
        | Just table <- sourceTableId_maybe original, isFinalCoreCaseTable table ->
            checkedFinalCaseTable bindings table
        | Just (_, constructors) <- indexedProjectionSource original ->
            maybe False (\(_, rec_row) -> lengthIs constructors 1 || isJust rec_row)
              (checkedIndexedProjection id owner expression)
        | otherwise -> case finalCoreSourceFamilies original of
            [table] -> case compileCoreEquations ((defaultCoreSpec (const False)) { ccs_check_kind_public = False }) owner expression of
              Just cct -> (isValidMapperTable cct || isValidAppendTable cct) && matchesCompiledTable table Nothing cct
              Nothing  -> False
            [table, guard_tc] ->
              matchesSpec ((defaultCoreSpec (const True))
                { ccs_bindings = bindings, ccs_allow_guard = True, ccs_allow_tail_self = True, ccs_check_kind_public = False })
                table (Just guard_tc) owner expression
            _ -> False
      _ -> False
  where
    bindings = Core.flattenBinds binds
    matchesSpec s t g o e = maybe False (matchesCompiledTable t g) (compileCoreEquations s o e)
    verifyIdentity seen owner (Core.Var target)
      | not (idName target `elemNameSet` seen), varType target `eqType` varType owner
      , Just rhs <- lookup target bindings = verifyIdentity (extendNameSet seen (idName target)) target rhs
    verifyIdentity _ owner expression =
      case compileCoreEquations ((defaultCoreSpec (const False)) { ccs_allow_zero_cases = True }) owner expression of
        Just cct | [CompiledCoreRow [PatVar x] (RhsVar y)] <- cct_rows cct -> x == y
        _ -> False

failedFinalListTables :: [Core.CoreBind] -> [Core.CoreBind] -> [TyCon] -> [TyCon]
failedFinalListTables original binds tables =
    [table | table <- tables, isFinalCoreListCaseTable table
           , not (tyConName table `elemNameSet` early), not (checkedFinalCaseTable (Core.flattenBinds binds) table)]
  where
    early = mkNameSet [tyConName t | bind <- original, o <- Core.bindersOf bind, SourceTableId _ t NominalSourceTable <- [idDetails o]]

checkedFinalCaseTable :: [(Id, Core.CoreExpr)] -> TyCon -> Bool
checkedFinalCaseTable bindings table = isJust $ do
    (owner, rhs) <- find (owns table . fst) bindings
    guard (sourceTableId_maybe owner == Just table)
    let base_spec = (defaultCoreSpec (const True)) { ccs_check_kind_public = False }
    case reverseWrapperWorker table of
      Just worker_tc -> do
        (_, _, _, callee) <- directListReverseWrapper owner rhs
        guard (owns worker_tc callee)
        w_rhs <- lookup callee bindings
        w_cct <- compileCoreEquations (base_spec { ccs_allow_tail_self = True, ccs_allow_accum_self = True }) callee w_rhs
        guard (isValidReverseWorkerTable w_cct && matchesCompiledTable worker_tc Nothing w_cct)
      Nothing
        | Just select <- booleanSelector table -> do
            let pred_fn c = checkedBranchPredicate (fromMaybe c (fst <$> find ((== c) . fst) bindings))
            cct <- compileCoreEquations (base_spec { ccs_predicate = pred_fn, ccs_allow_guard = True }) owner rhs
            guard (matchesCompiledTable table (Just select) cct)
        | isPartialConstructorCaseTable table -> do
            cct <- compileCoreEquations (base_spec { ccs_partial_root = True }) owner rhs
            guard (matchesCompiledTable table Nothing cct)
        | isTwoListCaseTable table -> do
            let check mb_w = compileCoreEquations (base_spec { ccs_spec_worker = mb_w }) owner rhs >>=
                             \cct -> guard (matchesCompiledTable table Nothing cct)
            check Nothing <|> (checkedSpecConstrWorker bindings table owner >>= check . Just)
        | otherwise -> do
            let calleeLookup fn = sourceTableId_maybe fn >>= \fam ->
                  fam <$ guard (isJust (sourceTableTermOcc (nameOccName (tyConName fam))))
                spec = base_spec
                  { ccs_helper           = if isFinalSourceListTable table then calleeLookup else const Nothing
                  , ccs_bindings         = bindings
                  , ccs_allow_tail_self  = isReverseWorkerTable table || isTwoStructuralCaseTable table
                  , ccs_allow_accum_self = isReverseWorkerTable table
                  , ccs_allow_call_self  = isFinalSourceListTable table
                  , ccs_allow_top_apply  = isListFoldTable table }
            cct <- compileCoreEquations spec owner rhs
            when (isListFoldTable table) (guard (isValidFoldTable cct))
            when (isListZipWithTable table) (guard (isValidZipTable cct))
            when (isReverseWorkerTable table) (guard (isValidReverseWorkerTable cct))
            guard (matchesCompiledTable table Nothing cct)
  where
    owns fam val = nameModule_maybe (idName val) == nameModule_maybe (tyConName fam) &&
                   sourceTableTermOcc (nameOccName (tyConName fam)) == Just (nameOccName (idName val))

checkedImportedSpecConstr :: RuleEnv -> Id -> TyCon -> CoreRule -> Maybe ()
checkedImportedSpecConstr env owner table rule = do
    worker <- checkSpecConstrRule owner rule
    guard (worker /= owner && nameModule_maybe (idName worker) == nameModule_maybe (idName owner) &&
           lazySpecConstrFields worker && null (activeMapperRules env worker))
    Core.CoreUnfolding { Core.uf_src = VanillaSrc, Core.uf_tmpl = rhs } <- pure (realIdUnfolding owner)
    let plain = (defaultCoreSpec (const True)) { ccs_check_kind_public = False }
    guard (isNothing (compileCoreEquations plain owner rhs))
    cct <- compileCoreEquations (plain { ccs_spec_worker = Just worker }) owner rhs
    guard (matchesCompiledTable table Nothing cct)

lazySpecConstrFields :: Id -> Bool
lazySpecConstrFields = all (not . isMarkedCbv) . take 2 . fromMaybe [] . idCbvMarks_maybe

checkedSpecConstrWorker :: [(Id, Core.CoreExpr)] -> TyCon -> Id -> Maybe Id
checkedSpecConstrWorker bindings table owner = do
    ConstructorCaseType telescope _ _ [left, right] 0 result <- constructorCaseType (varType owner)
    [Bndr a _, Bndr b _] <- pure telescope
    guard (eqTypes [left, right] (map (mkListTy . mkTyVarTy) [a, b]))
    [rule@Rule{ru_local = True}] <- pure (filter (not . isNeverActive . ruleActivation) (idCoreRules owner))
    target <- checkSpecConstrRule owner rule
    (worker, rhs) <- find ((== target) . fst) bindings
    guard (worker /= owner && not (isExternalName (idName worker)) &&
           eqType (varType target) (varType worker) && eqType (exprType rhs) (varType worker))
    (x : y : fields@[hd, tl, other], body) <- pure (Core.collectBinders (stripTicksE isSourceBreakpoint rhs))
    guard (all isTyVar [x, y] && all isId fields && eqTypes (map varType [x, y]) (map varType [a, b]))
    let instantiate = substTyWith [a, b] (mkTyVarTys [x, y])
    guard (eqTypes (map varType fields) (map instantiate [mkTyVarTy a, left, right]) &&
           maybe True null (idCbvMarks_maybe worker) && lazySpecConstrFields (tidyCbvInfoTop emptyNameSet worker rhs))
    (res_tc, res_args) <- caseDatatype (instantiate result)
    null_dc <- find (\dc -> dataConSourceArity dc == 0) (tyConDataCons res_tc)
    let list_arg = mkSysLocal (fsLit "spec_ls") minLocalUnique ManyTy (instantiate left)
        wrapper_ty = mkForAllTys (zipWith (\v (Bndr _ vis) -> (unmatchableDataConTy, Bndr v vis)) [x, y] telescope)
                       (mkVisFunTyManyU (instantiate left) (mkVisFunTyManyU (instantiate right) (instantiate result)))
        synth_case = Core.Case (Core.Var list_arg) list_arg (instantiate result)
                       [ Core.Alt (Core.DataAlt nilDataCon) [] (Core.mkConApp null_dc (map Core.Type res_args))
                       , Core.Alt (Core.DataAlt consDataCon) [hd, tl] body ]
        spec = (defaultCoreSpec (const True)) { ccs_spec_worker = Just worker, ccs_check_kind_public = False }
    cct <- compileCoreEquations spec (setIdType owner wrapper_ty) (Core.mkLams [x, y, list_arg, other] synth_case)
    axiom <- closedFamilyTyConCoAxiom_maybe table
    let isCons br = case drop (length telescope) (coAxBranchLHS br) of
          [p1, _] -> fmap fst (splitTyConApp_maybe p1) == Just (promoteDataCon consDataCon); _ -> False
        cons_rows = [r | r@(CompiledCoreRow [PatCon dc _ _, _] _) <- cct_rows cct, dc == consDataCon]
        sub_table = buildSourceFamily (tyConName table) (coAxiomName axiom) (tyConBinders table)
                      (tyConResKind table) (map const (filter isCons (fromBranches (coAxiomBranches axiom))))
    worker <$ guard (not (null cons_rows) && matchesCompiledTable sub_table Nothing (cct { cct_rows = cons_rows }))

checkSpecConstrRule :: Id -> CoreRule -> Maybe Id
checkSpecConstrRule owner Rule{ru_fn = fn, ru_origin = origin, ru_auto = True,
                               ru_bndrs = binders, ru_args = Core.Type left : Core.Type right : first : rest, ru_rhs = rhs} = do
    guard (fn == idName owner && Just origin == nameModule_maybe (idName owner))
    a <- getTyVar_maybe left; b <- getTyVar_maybe right
    (Core.Var cons, [Core.Type element, Core.Var hd, Core.Var tl]) <- pure (Core.collectArgs first)
    later <- traverse (\case Core.Var v -> Just v; _ -> Nothing) rest
    let captures = hd : tl : later
    guard (cons == dataConWorkId consDataCon && element `eqType` left && all isId captures &&
           eqTypes (map varType [hd, tl]) [left, mkListTy left] && (null later || eqTypes (map varType later) [mkListTy right]) &&
           exactRuleVars binders (a : b : captures))
    (Core.Var worker, Core.Type x : Core.Type y : arguments) <- pure (Core.collectArgs rhs)
    fields <- traverse (\case Core.Var v -> Just v; _ -> Nothing) arguments
    worker <$ guard (left `eqType` x && right `eqType` y && captures == fields &&
                     exprType (Core.mkApps (Core.Var owner) (Core.Type left : Core.Type right : first : rest)) `eqType` exprType rhs)
checkSpecConstrRule _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Nullary and recursive GADT index projections (ST6)
-- ---------------------------------------------------------------------------

data IndexedProjection = IndexedProjection [ForAllTyBinder] (Type, Type) Type DataCon TyVar Type [Type]
data IndexedRecursion = IndexedRecursion DataCon TyVar Type [Type] Kind

indexedProjectionType :: Type -> Maybe IndexedProjection
indexedProjectionType ty = do
    let (binders, body) = splitForAllForAllTyBinders ty
    telescope <- traverse ordinaryCaseBinder binders
    (left, more) <- caseArrow body; (right, result) <- caseArrow more
    (tc, args) <- splitTyConApp_maybe left
    guard (not (null telescope) && isBoxedDataTyCon tc && isLiftedAlgTyCon tc && lengthIs args (tyConArity tc))
    [(con, slot, index, parameters)] <- pure (mapMaybe (refinement args) (tyConDataCons tc))
    let quantified = binderVars telescope
    guard (slot `elem` quantified && not (anyFreeVarsOfType (== slot) right) &&
           all (not . anyFreeVarsOfType (== slot) . varType) quantified)
    pure (IndexedProjection telescope (left, right) result con slot index parameters)
  where
    refinement args con = do
      (universals, [], [equation], [], [], _) <- pure (dataConFullSig con)
      guard (null (dataConStupidTheta con) && lengthIs universals (length args))
      slot <- lookup (eqSpecTyVar equation) (zip universals args) >>= getTyVar_maybe
      let index = eqSpecType equation
      guard (noFreeVarsOfType index && isTyFamFree index && typeKind index `eqType` varType slot)
      (con, slot, index,) <$> traverse (`lookup` zip universals args) (dataConUserTyVars con)

checkedIndexedProjection :: (Core.CoreExpr -> Core.CoreExpr) -> Id -> Core.CoreExpr
                         -> Maybe (IndexedProjection, Maybe IndexedRecursion)
checkedIndexedProjection prepare owner original = do
    projection@(IndexedProjection binders _ _ con slot _ _) <- indexedProjectionType (varType owner)
    guard (exprType original `eqType` varType owner)
    let (core_types, arguments) = splitAt (length binders) (fst (Core.collectBinders (prepare original)))
    [left, right] <- pure arguments
    Core.Case (Core.Var scrutinee) _ _ alts <- pure (snd (Core.collectBinders (prepare original)))
    guard (all isTyVar core_types && left == scrutinee && isId right)
    [rhs] <- pure [br | Core.Alt (Core.DataAlt dc) [ev] br <- alts, dc == con, isCoVar ev]
    case stripTicksTopE isSourceBreakpoint rhs of
      Core.Var v | v == right -> pure ()
      Core.Cast (Core.Var v) _ | v == right -> pure ()
      _ -> Nothing
    let recursion = do
          row <- indexedRecursionType projection
          row <$ checkIndexedRecursion owner (binderVars binders) slot core_types right alts row
    pure (projection, recursion)

publishIndexedProjection :: Id -> IndexedProjection -> Maybe IndexedRecursion -> TcM (Maybe TyCon)
publishIndexedProjection owner projection@(IndexedProjection telescope (left, right) result con slot index con_args) recursion = do
    (renaming, parameters) <- freshenTyVarBndrs (binderVars telescope)
    let named = zipWith mkNamedTyConBinder (map binderFlag telescope) parameters
        instantiate = substTyUnchecked renaming . expandTypeSynonyms
        specialize = instantiate . substTyWith [slot] [index]
        hidden = map (specialize . mkTyVarTy) (binderVars telescope)
        branch_params = [v | (old, v) <- zip (binderVars telescope) parameters, old /= slot]
        constructor = mkTyConApp (promoteDataCon con) (map specialize con_args)
        term = idName owner
    families <- tcGetFamInstEnvs
    case projectionResultCoercion families (specialize right) (specialize result) of
      Just cast | typeKind constructor `eqType` specialize left -> do
        carried <- freshSysTyVar "carried" (specialize right)
        operands <- mapM (freshSysTyVar "arg" . instantiate) [left, right]
        let vars = branch_params ++ [carried]; right_pat = mkTyVarTy carried
            branch _ = mkCoAxBranch vars [] [] (hidden ++ [constructor, right_pat])
                         (mkCastTy right_pat cast) (map (const Nominal) vars) (nameSrcSpan term)
        recursive <- maybe (pure Nothing) (publishIndexedRecursion families projection parameters instantiate carried) recursion
        Just <$> makeSourceFamily term (named ++ mkAnonTyConBinders operands) (instantiate result) (branch : maybeToList recursive)
      _ -> pure Nothing

projectionResultCoercion :: FamInstEnvs -> Kind -> Kind -> Maybe Coercion
projectionResultCoercion families from to
  | from `eqType` to = Just (mkNomReflCo from)
  | otherwise = do
      (tc, left) <- splitTyConApp_maybe from
      (result_tc, right) <- splitTyConApp_maybe to
      guard (tc == result_tc && isLiftedDataOrNewTyCon tc && lengthIs left (length right))
      mkTyConAppCo Nominal tc <$> zipWithM check left right
  where
    check found expected
      | found `eqType` expected = Just (mkNomReflCo found)
      | otherwise = do
          (fam, args) <- splitTyConApp_maybe expected
          reduction <- reduceTyFamApp_maybe families Nominal fam args
          mkSymCo (reductionCoercion reduction) <$ guard (reductionReducedType reduction `eqType` found)

indexedRecursionType :: IndexedProjection -> Maybe IndexedRecursion
indexedRecursionType (IndexedProjection _ (left, _) _ nil slot _ _) = do
    (_, args) <- splitTyConApp_maybe left
    [row] <- pure (mapMaybe (refinement args) (tyConDataCons (dataConTyCon nil)))
    pure row
  where
    refinement args con = do
      (universals, [predecessor], [equation], [], fields, _) <- pure (dataConFullSig con)
      let instantiate = substTyWith universals args
      guard (null (dataConStupidTheta con) && lengthIs args (length universals) && isLazyDataConRep con &&
             all (not . isUnpacked) (dataConImplBangs con) && dataConRepArity con == length fields + 1 &&
             isTyVar predecessor && varType predecessor `eqType` varType slot)
      [head_kind, tail_kind] <- pure (map (instantiate . scaledThing) fields)
      actual_slot <- lookup (eqSpecTyVar equation) (zip universals args)
      let index = instantiate (eqSpecType equation)
      (successor, [argument]) <- splitTyConApp_maybe index
      _ <- isPromotedDataCon_maybe successor
      guard (actual_slot `eqType` mkTyVarTy slot && argument `eqType` mkTyVarTy predecessor &&
             not (anyFreeVarsOfType (\v -> v == predecessor || v == slot) head_kind) &&
             isLiftedTypeKind (typeKind head_kind) && tail_kind `eqType` substTyWith [slot] [argument] left)
      pure (IndexedRecursion con predecessor index (map (instantiate . mkTyVarTy) (dataConUserTyVars con)) head_kind)

checkIndexedRecursion :: Id -> [TyVar] -> TyVar -> [TyVar] -> Id -> [Core.CoreAlt] -> IndexedRecursion -> Maybe ()
checkIndexedRecursion owner parameters slot core_types carried alts (IndexedRecursion con _ _ _ _) = do
    [(predecessor, evidence, hd, tl, rhs)] <- pure [(p, co, x, xs, e) | Core.Alt (Core.DataAlt dc) [p, co, x, xs] e <- alts, dc == con]
    guard (isTyVar predecessor && isCoVar evidence && isId hd && isId tl)
    let value = case stripTicksTopE isSourceBreakpoint rhs of { Core.Cast b _ -> stripTicksTopE isSourceBreakpoint b; b -> b }
    (Core.Var rebuild, args) <- pure (Core.collectArgs value)
    [Core.Var first, recurse] <- pure (dropWhile Core.isTyCoArg args)
    (Core.Var self, arguments) <- pure (Core.collectArgs (stripTicksTopE isSourceBreakpoint recurse))
    let (types, terms) = splitAt (length parameters) arguments
        expected = [mkTyVarTy (if v == slot then predecessor else cv) | (v, cv) <- zip parameters core_types]
    actual <- coreTypeArgs types
    [Core.Var tail_arg, Core.Var carried_arg] <- pure terms
    guard (isDataConId_maybe rebuild == Just con && first == hd && self == owner &&
           eqTypes actual expected && tail_arg == tl && carried_arg == carried)

indexedRecursionResult :: IndexedProjection -> IndexedRecursion -> [Type] -> Type -> Maybe ([Type], [Type], Kind)
indexedRecursionResult (IndexedProjection telescope (left, _) result _ slot _ _)
                       (IndexedRecursion _ predecessor index args _) hidden pred_ty = do
    (input_tc, input_args) <- splitTyConApp_maybe left
    let parameters = binderVars telescope
        at applied = substTyWith parameters applied
        recur_args = zipWith (\v ty -> if v == slot then pred_ty else ty) parameters hidden
        match (inp, out) | getTyVar_maybe inp == Just slot = Just (Just out)
                         | at hidden inp `eqType` out = Just Nothing
                         | otherwise = Nothing
    (output_tc, output_args) <- splitTyConApp_maybe (at recur_args result)
    guard (input_tc == output_tc && lengthIs input_args (length output_args))
    [output_index] <- catMaybes <$> traverse match (zip input_args output_args)
    let inst = at hidden . substTyWith [predecessor] [output_index]
    pure (recur_args, map inst args, at (zipWith (\v ty -> if v == slot then inst index else ty) parameters hidden) left)

publishIndexedRecursion :: FamInstEnvs -> IndexedProjection -> [TyVar] -> (Type -> Type) -> TyVar
                        -> IndexedRecursion -> TcM (Maybe (TyCon -> CoAxBranch))
publishIndexedRecursion families projection@(IndexedProjection telescope (left, right) result _ slot _ _)
                        parameters instantiate carried row@(IndexedRecursion con predecessor index args head_kind) = do
    p <- freshSysTyVar "predecessor" (instantiate (varType slot))
    let p_ty = mkTyVarTy p
        specialized = instantiate . substTyWith [predecessor] [p_ty]
        hidden = [if v == slot then specialized index else instantiate (mkTyVarTy v) | v <- binderVars telescope]
        specialize = substTyWith (binderVars telescope) hidden
    hd <- freshSysTyVar "head" (specialize (specialized head_kind))
    tl <- freshSysTyVar "tail" (specialize (substTyWith [slot] [p_ty] left))
    pure $ do
      (recur_args, result_args, rebuilt_kind) <- indexedRecursionResult projection row hidden p_ty
      cast <- projectionResultCoercion families rebuilt_kind (specialize result)
      let h = mkTyVarTy hd; t = mkTyVarTy tl; ys = mkTyVarTy carried
          pat = mkTyConApp (promoteDataCon con) (map (specialize . specialized) args ++ [h, t])
          vars = [v | (old, v) <- zip (binderVars telescope) parameters, old /= slot] ++ [p, hd, tl, carried]
      guard (typeKind pat `eqType` specialize left && typeKind ys `eqType` specialize right)
      pure $ \family ->
        let rhs = mkTyConApp (promoteDataCon con) (result_args ++ [h, mkTyConApp family (recur_args ++ [t, ys])])
        in mkCoAxBranch vars [] [] (hidden ++ [pat, ys]) (mkCastTy rhs cast)
                        (map (const Nominal) vars) (nameSrcSpan (tyConName family))

indexedProjectionSource :: Id -> Maybe (TyCon, [DataCon])
indexedProjectionSource owner = do
    guard (finalCoreMapperOwner owner)
    table <- sourceTableId_maybe owner
    guard (tyConKind table `eqType` varType owner)
    projection@(IndexedProjection _ _ _ nil _ _ _) <- indexedProjectionType (varType owner)
    axiom <- closedFamilyTyConCoAxiom_maybe table
    guard (coAxiomRole axiom == Nominal)
    case fromBranches (coAxiomBranches axiom) of
      [empty_br] -> (table, [nil]) <$ checkIndexedSourceBranch projection table Nothing empty_br
      [empty_br, nonempty] -> do
        checkIndexedSourceBranch projection table Nothing empty_br
        row@(IndexedRecursion con _ _ _ _) <- indexedRecursionType projection
        (table, [nil, con]) <$ checkIndexedSourceBranch projection table (Just row) nonempty
      _ -> Nothing

checkIndexedSourceBranch :: IndexedProjection -> TyCon -> Maybe IndexedRecursion -> CoAxBranch -> Maybe ()
checkIndexedSourceBranch projection@(IndexedProjection telescope (left, right) result nil slot index con_args)
                         table recursion branch = do
    let parameters = binderVars telescope
        (hidden, operands) = splitAt (length parameters) (coAxBranchLHS branch)
        specialize = substTyWith parameters hidden
    guard (lengthIs hidden (length parameters) && null (coAxBranchCoVars branch))
    [constructor, carried] <- pure operands
    carried_var <- getTyVar_maybe carried
    [refined] <- pure [arg | (v, arg) <- zip parameters hidden, v == slot]
    hidden_vars <- traverse getTyVar_maybe [arg | (v, arg) <- zip parameters hidden, v /= slot]
    (local_vars, wanted) <- case recursion of
      Nothing -> ([] , carried) <$ guard (index `eqType` refined &&
                                          mkTyConApp (promoteDataCon nil) (map specialize con_args) `eqType` constructor)
      Just row@(IndexedRecursion con predecessor result_index args head_kind) -> do
        (_, [pred_ty]) <- splitTyConApp_maybe refined
        pred_var <- getTyVar_maybe pred_ty
        (promoted, constructor_args) <- splitTyConApp_maybe constructor
        let inst = specialize . substTyWith [predecessor] [pred_ty]
            (promoted_params, fields) = splitAt (length args) constructor_args
        [hd, tl] <- pure fields
        field_vars <- traverse getTyVar_maybe fields
        guard (promoted == promoteDataCon con && inst result_index `eqType` refined &&
               eqTypes (map inst args) promoted_params && typeKind hd `eqType` inst head_kind &&
               typeKind tl `eqType` specialize (substTyWith [slot] [pred_ty] left))
        (recur_args, result_args, _) <- indexedRecursionResult projection row hidden pred_ty
        pure (pred_var : field_vars, mkTyConApp promoted (result_args ++ [hd, mkTyConApp table (recur_args ++ [tl, carried])]))
    let rhs = coAxBranchRHS branch; vars = hidden_vars ++ local_vars ++ [carried_var]
    guard (coAxBranchTyVars branch == vars && sizeUniqSet (mkUniqSet vars) == length vars &&
           eqTypes (map typeKind hidden) (map (specialize . varType) parameters) &&
           lengthIs (coAxBranchRoles branch) (length vars) && all (== Nominal) (coAxBranchRoles branch) &&
           typeKind constructor `eqType` specialize left && typeKind carried `eqType` specialize right &&
           typeKind rhs `eqType` specialize result)
    case rhs of
      CastTy payload co -> guard (payload `eqType` wanted && coercionRole co == Nominal &&
                                  coercionLKind co `eqType` typeKind wanted && coercionRKind co `eqType` specialize result)
      _ -> guard (rhs `eqType` wanted)

-- ---------------------------------------------------------------------------
-- Partial pair builders, character cases, retained witnesses, and exports
-- ---------------------------------------------------------------------------

pairBuilderType :: Type -> Maybe ([ForAllTyBinder], [Type], Type, Type, Type)
pairBuilderType ty = do
    let (telescope, body) = splitForAllForAllTyBinders ty
        (_, arrows, result) = splitFunTys body
        domains = map scaledThing arrows
    (tc, [left, right]) <- splitTyConApp_maybe result
    guard (tc == tupleTyCon Boxed 2 && not (null domains) &&
           body `eqType` foldr mkVisFunTyManyU result domains && all (isLiftedTypeKind . typeKind) domains)
    (\bs -> (bs, domains, result, left, right)) <$> traverse liftedSourceBinder telescope

checkedPairBuilder :: Id -> Core.CoreExpr -> Maybe (TcM (Maybe (Id, TyCon), [TyCon]))
checkedPairBuilder owner rhs = do
    guard (exprType rhs `eqType` varType owner)
    (tvs, domains, result, left, right) <- pairBuilderType (exprType rhs)
    (signature, _, _, _, _) <- pairBuilderType (varType owner)
    let (types, terms) = splitAt (length tvs) (fst (Core.collectBinders rhs))
    guard (types == binderVars tvs && all isId terms && eqTypes domains (map varType terms))
    (Core.Var worker, [Core.Type l, Core.Type r, x, y]) <- pure (Core.collectArgs (snd (Core.collectBinders rhs)))
    guard (isDataConWorkId_maybe worker == Just (tupleDataCon Boxed 2) &&
           eqTypes [l, r, exprType x, exprType y] [left, right, left, right])
    let input (Core.Var v) | v `elem` terms = Just v; input _ = Nothing
        purePairField (Core.Var v) | v `elem` terms = True
        purePairField expr
          | (Core.Var w, args) <- Core.collectArgs expr, Just dc <- isDataConWorkId_maybe w
          , let (ty_as, val_as) = splitAt (length (dataConUnivTyVars dc)) args
          , Just inst_tys <- coreTypeArgs ty_as, isJust (openDataConFieldTypes dc inst_tys) = all purePairField val_as
          | otherwise = False
        complete_poly other = not (null tvs) && all (polySourceType types) (result : domains) && purePairField other
        parameters = zipWith (\v (Bndr _ vis) -> Bndr v (NamedTCB vis)) types signature
        name = idName owner
        copiedPair known side opaque_kind occ = do
          args <- forM domains (freshSysTyVar "arg")
          let binders = parameters ++ mkAnonTyConBinders args; vars = types ++ args; lhs = mkTyVarTys vars
              selected = maybe (panic "checkedPairBuilder") mkTyVarTy (lookup known (zip terms args))
          field_nm <- newImplicitBinder name occ
          let field = mkFamilyTyCon field_nm (mkTyConKindWithMatchability unmatchableDataConTy binders opaque_kind)
                        binders 0 opaque_kind Nothing (ClosedTypeFamilyTyCon (CTF Nothing)) Nothing NotInjective
              pair_fs = pickLR side ([selected, mkTyConApp field lhs], [mkTyConApp field lhs, selected])
              pair = mkTyConApp (promotedTupleDataCon Boxed 2) ([left, right] ++ pair_fs)
          table <- makeSourceFamily name binders result
                     [\_ -> mkCoAxBranch vars [] [] lhs pair (map (const Nominal) vars) (nameSrcSpan name)]
          pure (Just (owner, table), [field])
    pure $ case (input x, input y) of
      (Just v, Nothing) | not (complete_poly y) -> copiedPair v CLeft right mkSourceSndOcc
      (Nothing, Just v) | not (complete_poly x) -> copiedPair v CRight left mkSourceFstOcc
      _ -> pure (Nothing, [])

lookupObservedPairField :: LeftOrRight -> Id -> [Type] -> Kind -> TcM (Maybe TyCon)
lookupObservedPairField _ _ _ _ = pure Nothing

canObservePairSource :: Id -> TcM Bool
canObservePairSource _ = pure False

lookupPairObserver :: LeftOrRight -> Id -> Int -> TcM (Maybe Id)
lookupPairObserver _ _ _ = pure Nothing

lookupPairRecipe :: Module -> OccName -> Int -> TcM (Maybe Id, Maybe Id)
lookupPairRecipe _ _ _ = pure (Nothing, Nothing)

sourcePairBuilderField :: TyCon -> Maybe (LeftOrRight, Int)
sourcePairBuilderField table = do
    owner <- sourceTableTermOcc (nameOccName (tyConName table))
    axiom <- closedFamilyTyConCoAxiom_maybe table
    [branch] <- pure (fromBranches (coAxiomBranches axiom))
    let lhs = coAxBranchLHS branch
        inputs = drop (helperInvisibleArity table) lhs
        opaque occ ty = case splitTyConApp_maybe ty of
          Just (tc, args) -> nameModule_maybe (tyConName tc) == nameModule_maybe (tyConName table) &&
                             nameOccName (tyConName tc) == occ owner && eqTypes args lhs &&
                             case famTyConFlav_maybe tc of { Just (ClosedTypeFamilyTyCon (CTF Nothing)) -> True; _ -> False }
          _ -> False
    (pair, [_, _, left, right]) <- splitTyConApp_maybe (coAxBranchRHS branch)
    guard (pair == promotedTupleDataCon Boxed 2 && coAxiomRole axiom == Nominal &&
           eqTypes lhs (mkTyVarTys (coAxBranchTyVars branch)) && null (coAxBranchCoVars branch))
    (side, copied) <- if opaque mkSourceSndOcc right then Just (CLeft, left)
                      else if opaque mkSourceFstOcc left then Just (CRight, right) else Nothing
    (side,) <$> findIndex (eqType copied) inputs

sourceCharacterTables :: NameEnv (Id, Core.CoreExpr) -> TcM [(Id, TyCon)]
sourceCharacterTables definitions = do
    env <- getGblEnv
    let candidates = sortBy (\(l, _) (r, _) -> stableNameCmp (idName l) (idName r)) (nonDetNameEnvElts definitions)
    direct <- fmap catMaybes $ forM candidates $ \(owner, rhs) -> case charCaseResultType (varType owner) of
      Just result
        | Just old <- sourceTableOccurrence env owner >>= sourceTableId_maybe -> pure (Just (owner, old))
        | Core.Var target <- rhs -> forwardSourceCharacter env result owner target
        | otherwise -> publishCertifiedCharacter owner rhs
      Nothing -> pure Nothing
    publishCharacterAliases env candidates direct

publishCharacterAliases :: TcGblEnv -> [(Id, Core.CoreExpr)] -> [(Id, TyCon)] -> TcM [(Id, TyCon)]
publishCharacterAliases env candidates direct = do
    let root_tables = mkNameEnv $
          [ (idName v, t)
          | v <- typeEnvIds (tcg_type_env env)
          , Just t <- [sourceTableId_maybe v] ] ++
          [(idName o, t) | (o, t) <- direct]
        aliases = [ DigraphNode (o, target, res) (idName o) [idName target]
                  | (o, Core.Var target) <- candidates, not (idName o `elemNameEnv` root_tables)
                  , nameModule_maybe (idName target) == Just (tcg_semantic_mod env)
                  , Just res <- [charCaseResultType (varType o)] ]
        step (tables, pub) (AcyclicSCC (o, target, res))
          | Just t <- lookupNameEnv tables (idName target), compatibleForwarding o target res t = do
              e@(_, alias) <- publishSourceCharacter o res [] (\arg_ty -> mkTyConApp t [arg_ty])
              pure (extendNameEnv tables (idName o) alias, e : pub)
        step checked _ = pure checked
    (_, forwarded) <- foldM step (root_tables, []) (stronglyConnCompFromEdgedVerticesUniq aliases)
    pure (direct ++ reverse forwarded)

publishCertifiedCharacter :: Id -> Core.CoreExpr -> TcM (Maybe (Id, TyCon))
publishCertifiedCharacter owner rhs
  | Right c <- certify owner rhs, certOwner c == idName owner =
      Just <$> publishSourceCharacter owner (certResultType c) (certRows c) (certDefault c)
  | otherwise = pure Nothing

compatibleForwarding :: Id -> Id -> Type -> TyCon -> Bool
compatibleForwarding owner target result table =
    varType target `eqType` varType owner && tyConResKind table `eqType` result &&
    case tyConTyVars table of { [arg] -> tyVarKind arg `eqType` charTy; _ -> False }

forwardSourceCharacter :: TcGblEnv -> Type -> Id -> Id -> TcM (Maybe (Id, TyCon))
forwardSourceCharacter env result owner target
  | Just target_mod <- nameModule_maybe (idName target), target_mod /= tcg_semantic_mod env, not (isHoleModule target_mod)
  , (installed, Nothing) <- getModuleInstantiation target_mod
  , Just IsBoot /= (gwib_isBoot <$> lookupInstalledModuleEnv (imp_boot_mods (tcg_imports env)) installed)
  , Just table <- sourceTableId_maybe target, compatibleForwarding owner target result table = do
      deps <- sourceTableDependencies table
      case deps of
        Just callees -> do
          let uses = [(v, Nothing) | v <- target : callees]
          changed <- changedSourceUse uses
          if isJust changed then pure Nothing else do
            forwarded <- publishSourceCharacter owner result [] (\arg_ty -> mkTyConApp table [arg_ty])
            Just forwarded <$ publishSourceUses uses
        Nothing -> pure Nothing
  | otherwise = pure Nothing

publishSourceCharacter :: Id -> Type -> [(Char, Type)] -> (Type -> Type) -> TcM (Id, TyCon)
publishSourceCharacter owner result rows fallback = do
    let loc = nameSrcSpan (idName owner)
        literal (inp, val) _ = mkCoAxBranch [] [] [] [mkCharLitTy inp] val [] loc
    var <- freshSysTyVar "c" charTy; arg <- freshSysTyVar "x" charTy
    let ty = mkTyVarTy var
    (owner,) <$> makeSourceFamily (idName owner) [mkAnonTyConBinder arg] result
                   (map literal rows ++ [\_ -> mkCoAxBranch [var] [] [] [ty] (fallback ty) [Nominal] loc])

sourceRetainedWitnessTables :: NameEnv (Id, Core.CoreExpr) -> TcM [(Id, TyCon)]
sourceRetainedWitnessTables definitions = do
    env <- getGblEnv; rdr_env <- getGlobalRdrEnv
    let available n | nameModule_maybe n == Just (tcg_semantic_mod env) = maybe False isLocalGRE (lookupGRE_Name rdr_env n)
                    | otherwise = ordinaryImport rdr_env n
        public = visibleCaseType available
    fmap catMaybes $ forM (sortBy (\(l, _) (r, _) -> stableNameCmp (idName l) (idName r)) (nonDetNameEnvElts definitions)) $
      \(owner, rhs) -> case directRetainedWitnessType (varType owner) >>= \(con, _) ->
                            compileCoreEquations ((defaultCoreSpec public) { ccs_witness_root = Just con }) owner rhs of
        Just cct -> Just . (owner,) <$> publishCompiledTable owner cct
        Nothing  -> pure Nothing

publishRetainedRecordWitness :: DataCon -> Id -> TcM TyCon
publishRetainedRecordWitness con owner
  | Just (expected, ConstructorCaseType tele _ args domains _ result) <- retainedWitnessType (varType owner)
  , con == expected, Just witness : _ <- dataConRepArgBinders con, _ : payloads <- dataConOrigArgTys con = do
      let instantiate = substTyWith (dataConUnivTyVars con) args
          inst_witness = setIdType witness (instantiate (varType witness))
          subst_witness = CoreSubst.substTy (CoreSubst.extendSubst (CoreSubst.mkEmptySubst (mkInScopeSet (tyCoVarsOfTypes (result : domains)))) witness (Core.Type (TermVarTy inst_witness)))
      fields <- mapM (mkSysLocalM (fsLit "field") ManyTy . subst_witness . instantiate . scaledThing) payloads
      publishCompiledTable owner
        (CompiledCoreTable tele domains result [CompiledCoreRow [PatCon con args (map PatVar (inst_witness : fields))] (RhsVar inst_witness)] False)
  | otherwise = pprPanic "publishRetainedRecordWitness: invalid selector" (ppr owner)

-- | Publish a source equation family for a total single-constructor record field selector
-- when @-fwrite-source-equations@ is active.
publishPlainRecordFieldSelector :: DataCon -> Int -> Id -> TcM (Maybe TyCon)
publishPlainRecordFieldSelector con idx owner = do
  write_eqs <- goptM Opt_WriteSourceEquations
  if not write_eqs then pure Nothing else
    case plain_selector_spec of
      Nothing -> pure Nothing
      Just (binders, args, domain, result, field_tys) -> do
        fields <- mapM (mkSysLocalM (fsLit "field") ManyTy) field_tys
        let target = fields !! idx
            row    = CompiledCoreRow [PatCon con args (map PatVar fields)] (RhsVar target)
        fmap fst <$> publishCompiledWithGuard owner (CompiledCoreTable binders [domain] result [row] False)
  where
    plain_selector_spec = do
      let tc = dataConTyCon con
          (telescope, body) = splitForAllForAllTyBinders (varType owner)
      guard (isVanillaAlgTyCon tc && isLiftedDataOrNewTyCon tc && [con] == tyConDataCons tc)
      binders <- traverse ordinaryCaseBinder telescope
      (domain, result) <- caseArrow body
      (tc', args) <- caseDatatype domain
      guard (tc' == tc && all (tcIsLiftedTypeKind . varType) (binderVars binders)
             && eqTypes args (mkTyVarTys (binderVars binders)))
      let instantiate = substTyWith (dataConUnivTyVars con) args
          field_tys   = map (instantiate . scaledThing) (dataConOrigArgTys con)
      guard (idx >= 0 && idx < length field_tys
             && all (\t -> tcIsLiftedTypeKind (typeKind t) && isTauTy t) field_tys
             && result `eqType` (field_tys !! idx))
      pure (binders, args, domain, result, field_tys)

checkLocalRetainedWitnessExports :: TcGblEnv -> [Id] -> TcM ()
checkLocalRetainedWitnessExports env owners = do
    let exported = availsToNameSet (tcg_exports env)
        exported_tycons = tyConsOfTypes (exportedLargeElimInterfaceTypes exported (tcg_type_env env))
        emit span_nm owner mb_dep detail = setSrcSpan (nameSrcSpan span_nm) $
          addErr (TcRnOpenTypeEval owner mb_dep ("The dependent type needs this equation. " ++ detail ++ " to make it available."))
        detailFor (hidden, True)  = "Export the constructors of " ++ occNameString (nameOccName hidden)
        detailFor (hidden, False) = "Export " ++ occNameString (nameOccName hidden)
    forM_ owners $ \owner -> case sourceTableId_maybe owner of
      Just table
        | Just (con, _) <- directRetainedWitnessType (varType owner) <|> retainedWitnessType (varType owner)
        , let tc = dataConTyCon con, nameModule_maybe (tyConName tc) == Just (tcg_semantic_mod env)
        , not (publicCaseType env exported (const False) tc) -> do
            let name = idName owner
                dep = listToMaybe (sortBy stableNameCmp [idName c | c <- owners, table `elem` sourceFamilyTyCons [varType c]])
            emit (fromMaybe name dep) name dep ("Export the matched constructor " ++ occNameString (nameOccName (dataConName con)))
        | isNominalStructuralSource owner, isJust (booleanSelector table)
        , hidden : _ <- unexported_case_types exported table ->
            emit (idName owner) (idName owner) Nothing ("Export the constructors of " ++ occNameString (nameOccName hidden))
        | SourceTableId _ _ NominalSourceTable <- idDetails owner, isLargeElimTable table
        , let name = idName owner; exposed = table `elementOfUniqSet` exported_tycons
        , name `elemNameSet` exported || exposed ->
            case unexportedLargeElimTypes env exported table of
              pair : _ -> emit name name Nothing (detailFor pair)
              [] | exposed, not (name `elemNameSet` exported) -> emit name name Nothing ("Export " ++ occNameString (nameOccName name))
              [] -> pure ()
        | SourceTableId _ _ NominalSourceTable <- idDetails owner, not (isLargeElimTable table)
        , table `elementOfUniqSet` exported_tycons
        , pair : _ <- unexported_value_table_types exported table ->
            emit (idName owner) (idName owner) Nothing (detailFor pair)
        | RecSelId { sel_sourceTable = Just ts } <- idDetails owner, ts == table
        , isNothing (directRetainedWitnessType (varType owner))
        , isNothing (retainedWitnessType (varType owner))
        , table `elementOfUniqSet` exported_tycons
        , pair : _ <- unexported_value_table_types exported table ->
            emit (idName owner) (idName owner) Nothing (detailFor pair)
      _ -> pure ()
  where
    is_local n = nameModule_maybe n == Just (tcg_semantic_mod env)
    unexported_case_types exported table = sortBy stableNameCmp
      [tyConName tc | tc <- sourceEquationDatatypes (sourceTableWithCallees table)
                    , is_local (tyConName tc), not (publicCaseType env exported (const False) tc)]
    unexported_value_table_types exported table =
      let tables = sourceTableWithCallees table
      in map (, True) (sortBy stableNameCmp [tyConName tc | tc <- tablePromotedParents True tables
                                                          , is_local (tyConName tc), not (publicCaseType env exported (const False) tc)]) ++
         map (, False) (sortBy stableNameCmp [tyConName tc | tc <- sourceEquationDatatypes tables
                                                           , is_local (tyConName tc), not (tyConName tc `elemNameSet` exported)])

sourceTableWithCallees :: TyCon -> [TyCon]
sourceTableWithCallees root = go emptyUniqSet [root]
  where
    go _ [] = []
    go seen (t : ts)
      | t `elementOfUniqSet` seen = go seen ts
      | otherwise =
          let seen'   = addOneToUniqSet seen t
              callees = fromMaybe [] (sourceTableCallees t)
          in t : go seen' (callees ++ ts)

tablePromotedParents :: Bool -> [TyCon] -> [TyCon]
tablePromotedParents include_lhs tables =
    nonDetEltsUniqSet $ mkUniqSet [dataConTyCon dc | tc <- nonDetEltsUniqSet (tyConsOfTypes branch_tys), Just dc <- [isPromotedDataCon_maybe tc]]
  where
    branch_tys = [ty | t <- tables, ax <- maybeToList (closedFamilyTyConCoAxiom_maybe t)
                     , br <- fromBranches (coAxiomBranches ax)
                     , ty <- coAxBranchRHS br : if include_lhs then coAxBranchLHS br else []]

-- | True when a residual kind is `Type` or a pure arrow kind ending in `Type`
-- (such as `Type -> Type` for higher-kinded large eliminations).
isLargeElimResultKind :: Type -> Bool
isLargeElimResultKind ty
  | tcIsLiftedTypeKind ty = True
  | Just (FTF_T_T, _, mult, dom, res) <- splitFunTy_maybe ty
  = isManyTy mult && isLargeElimResultKind dom && isLargeElimResultKind res
  | otherwise = False

-- | Convert residual term-level `%U` arrows in a large-elimination result kind
-- to standard kind-level `%M` (`matchableDataConTy`) arrows.
toKindMatchableArrows :: Type -> Type
toKindMatchableArrows ty
  | Just (FTF_T_T, _, mult, dom, res) <- splitFunTy_maybe ty
  = FunTy FTF_T_T matchableDataConTy mult (toKindMatchableArrows dom) (toKindMatchableArrows res)
  | otherwise = ty

-- | Compare a term owner's `varType` against a certified `SourceTable` `tyConKind`,
-- accounting for residual matchable kind arrows on higher-kinded large eliminations.
matchesSourceFamilyKind :: Type -> TyCon -> Bool
matchesSourceFamilyKind term_ty fam
  | term_ty `eqType` tyConKind fam = True
  | isLargeElimTable fam =
      let (tele, tau) = splitForAllForAllTyBinders term_ty
          vis_arity = length [() | Bndr _ AnonTCB <- tyConBinders fam]
          peel 0 t = toKindMatchableArrows t
          peel n t
            | Just (FTF_T_T, ma, mult, dom, cod) <- splitFunTy_maybe t
            = FunTy FTF_T_T ma mult dom (peel (n - 1) cod)
            | otherwise = t
          norm_ty = foldr (\(ma, bndr) acc -> ForAllTy ma bndr acc) (peel vis_arity tau) tele
      in norm_ty `eqType` tyConKind fam
  | otherwise = False

isLargeElimTable :: TyCon -> Bool
isLargeElimTable table =
  hasSourceTableAxiom False (const True) table && isLargeElimResultKind (tyConResKind table)

unexportedLargeElimTypes :: TcGblEnv -> NameSet -> TyCon -> [(Name, Bool)]
unexportedLargeElimTypes env exported table =
    map (, True) (sortBy stableNameCmp hidden_domains) ++ map (, False) (sortBy stableNameCmp hidden_rhs_types)
  where
    tables = sourceTableWithCallees table
    is_local name = nameModule_maybe name == Just (tcg_semantic_mod env)
    lhs_tys t = tyConKind t : [ty | ax <- maybeToList (closedFamilyTyConCoAxiom_maybe t)
                                  , br <- fromBranches (coAxiomBranches ax)
                                  , ty <- coAxBranchLHS br ++ map tyVarKind (coAxBranchTyVars br)]
    domain_tcs = [parent | tc <- nonDetEltsUniqSet (tyConsOfTypes (concatMap lhs_tys tables)) ++ tablePromotedParents False tables
                         , let parent = maybe tc dataConTyCon (isPromotedDataCon_maybe tc), isAlgTyCon parent]
    hidden_domains = [tyConName tc | tc <- nonDetEltsUniqSet (mkUniqSet domain_tcs)
                                   , is_local (tyConName tc), not (publicCaseType env exported (const False) tc)]
    all_rhs_tys = [coAxBranchRHS br | t <- tables, ax <- maybeToList (closedFamilyTyConCoAxiom_maybe t), br <- fromBranches (coAxiomBranches ax)]
    raw_rhs_tcs = [tc | tc <- nonDetEltsUniqSet (tyConsOfTypes all_rhs_tys), isNothing (isPromotedDataCon_maybe tc), tc `notElem` tables]
    rhs_tcs = [tc | tc <- raw_rhs_tcs, isNothing (sourceTableTermOcc (nameOccName (tyConName tc)))]
    callee_owners = [ mkVarOcc (occNameString o)
                    | c <- drop 1 tables ++ raw_rhs_tcs
                    , is_local (tyConName c), Just o <- [sourceTableTermOcc (nameOccName (tyConName c))] ]
    hidden_callees = [idName v | v <- typeEnvIds (tcg_type_env env), is_local (idName v)
                               , nameOccName (idName v) `elem` callee_owners, not (idName v `elemNameSet` exported)]
    hidden_rhs_types = [tyConName tc | tc <- nonDetEltsUniqSet (mkUniqSet rhs_tcs)
                                     , is_local (tyConName tc), not (tyConName tc `elemNameSet` exported)] ++ hidden_callees

exportedLargeElimInterfaceTypes :: NameSet -> TypeEnv -> [Type]
exportedLargeElimInterfaceTypes exported = concatMap entity_types . typeEnvElts
  where
    entity_types (AnId v) | idName v `elemNameSet` exported = [varType v]
    entity_types (AConLike (RealDataCon dc)) | dataConName dc `elemNameSet` exported = [varType (dataConWrapId dc), varType (dataConWorkId dc)]
    entity_types (ATyCon tc) | tyConName tc `elemNameSet` exported = tyConKind tc : maybeToList (synTyConRhs_maybe tc)
    entity_types _ = []

sourceEquationDatatypes :: [TyCon] -> [TyCon]
sourceEquationDatatypes tables =
    [parent | tc <- nonDetEltsUniqSet (tyConsOfTypes (concatMap eq_tys tables))
            , let parent = maybe tc dataConTyCon (isPromotedDataCon_maybe tc), isAlgTyCon parent]
  where
    eq_tys tc = tyConKind tc : [ty | ax <- maybeToList (closedFamilyTyConCoAxiom_maybe tc), br <- fromBranches (coAxiomBranches ax)
                                   , ty <- coAxBranchRHS br : coAxBranchLHS br ++ map tyVarKind (coAxBranchTyVars br)]

isMonomorphicRetainedWitness :: Id -> Bool
isMonomorphicRetainedWitness = isJust . directRetainedWitnessType . varType

publicCaseType :: TcGblEnv -> NameSet -> (Name -> Bool) -> TyCon -> Bool
publicCaseType env exported imported = visibleCaseType (\n -> if nameModule_maybe n == Just (tcg_semantic_mod env) then n `elemNameSet` exported else imported n)

visibleCaseType :: (Name -> Bool) -> TyCon -> Bool
visibleCaseType available tc = all available (tyConName tc : map dataConName (tyConDataCons tc))

ordinaryImport :: GlobalRdrEnv -> Name -> Bool
ordinaryImport rdr_env name = maybe False (any ((== NotBoot) . is_isboot . is_decl) . gre_imp) (lookupGRE_Name rdr_env name)

rebuildSourceFamilyWith :: (TyCon -> TyCon) -> TyCon -> TyCon
rebuildSourceFamilyWith subst_tc table
  | Just (ClosedTypeFamilyTyCon (CTF Nothing)) <- famTyConFlav_maybe table = table
  | Just ax <- closedFamilyTyConCoAxiom_maybe table =
      buildSourceFamily (tyConName table) (coAxiomName ax) (tyConBinders table) (tyConResKind table)
        [ \fam -> mkCoAxBranch (coAxBranchTyVars br) [] (coAxBranchCoVars br)
                    (coAxBranchLHS br) (map_rhs fam (coAxBranchRHS br)) (coAxBranchRoles br) (coAxBranchSpan br)
        | br <- fromBranches (coAxiomBranches ax) ]
  | otherwise = pprPanic "rebuildSourceFamilyWith: expected closed family" (ppr table)
  where
    map_rhs fam =
      let subst t | tyConName t == tyConName table = fam | otherwise = subst_tc t
          (map_ty, _, _, _) = mapTyCo TyCoMapper
            { tcm_tyvar = pure . mkTyVarTy, tcm_termvar = pure . TermVarTy, tcm_termbinder = \v k -> k v
            , tcm_covar = pure . mkCoVarCo, tcm_hole = pprPanic "rebuildSourceFamilyWith: coercion hole" . ppr
            , tcm_tycobinder = \tv _ k -> k tv
            , tcm_tcapp_ty = \_ tc tys -> Identity (TyConApp (subst tc) tys)
            , tcm_tcapp_co = \_ role tc cos -> Identity (mkTyConAppCo role (subst tc) cos) }
      in runIdentity . map_ty

-- ---------------------------------------------------------------------------
-- Embedded-type large eliminations (`HsEmbTy`) and Core-backed stage drivers
-- ---------------------------------------------------------------------------

data EmbPat
  = EmbPatVar (Maybe Id)
  | EmbPatCon DataCon [Type] [EmbPat]

data EmbRhs
  = EmbTypeRhs Type
  | EmbAbsurdRhs Id
  | EmbCallRhs Id [Type] [EmbRhs]
  | EmbApplyRhs Id [EmbRhs]

data EmbClause = EmbClause SrcSpan EmbPat [Maybe Id] EmbRhs
data EmbSpec = EmbSpec Id Int Int [TyVar] [TcEvBinds] [EmbClause] [Type] [Id]

readEmbSpec :: HsBind GhcTc -> Maybe EmbSpec
readEmbSpec binding = do
    (fun, mono, body_tvs, evidence, matches) <- sourceMatches binding
    guard (not (null matches))
    guard (has_type_codomain False (varType fun))
    arity <- case matches of
      L _ Match{m_pats = L _ ps} : rest
        | not (null ps)
        , all (\(L _ Match{m_pats = L _ qs}) -> length qs == length ps) rest
        -> Just (length ps)
      _ -> Nothing
    let tryCol col_idx = do
          (evs, clauses) <- unzip <$> traverse (readClause mono fun body_tvs arity col_idx) matches
          let emb_tys  = concatMap (rhs_tys . clause_rhs) clauses
              call_ids = concatMap (rhs_callees . clause_rhs) clauses
          EmbSpec fun arity col_idx body_tvs (evidence ++ concat evs) clauses emb_tys call_ids
            <$ guard (any (not . is_absurd . clause_rhs) clauses)
    foldr (<|>) Nothing [tryCol i | i <- [0 .. arity - 1]]
  where
    has_type_codomain seen ty = case splitForAllTyCoVar_maybe ty of
      Just (_, _, inner) -> has_type_codomain seen inner
      Nothing -> case caseArrow ty of
        Just (_, rest) -> has_type_codomain True rest
        Nothing        -> seen && isLargeElimResultKind ty

    clause_rhs (EmbClause _ _ _ r) = r
    is_absurd EmbAbsurdRhs{} = True; is_absurd _ = False
    rhs_tys (EmbTypeRhs t)         = [t]
    rhs_tys EmbAbsurdRhs{}         = []
    rhs_tys (EmbCallRhs _ ts args) = ts ++ concatMap rhs_tys args
    rhs_tys (EmbApplyRhs _ args)   = concatMap rhs_tys args
    rhs_callees (EmbCallRhs f _ args) = f : concatMap rhs_callees args
    rhs_callees (EmbApplyRhs _ args)  = concatMap rhs_callees args
    rhs_callees _                     = []

    unParen (L _ (ParPat _ p)) = unParen p; unParen (L _ p) = p
    plainBinder p = case unParen p of { VarPat _ (L _ v) -> Just (Just v); WildPat _ -> Just Nothing; _ -> Nothing }

    readEmbPat :: LPat GhcTc -> Maybe ([TcEvBinds], EmbPat)
    readEmbPat lp = case unParen lp of
      VarPat _ (L _ v) -> Just ([], EmbPatVar (Just v))
      WildPat _        -> Just ([], EmbPatVar Nothing)
      SigPat _ inner _ -> readEmbPat inner
      XPat (ExpansionPat _ inner) -> readEmbPat (noLocA inner)
      XPat (CoPat WpHole inner _) -> readEmbPat (noLocA inner)
      ConPat (ConPatTc ps [] [] cevs WpHole []) (L _ (RealDataCon con)) details -> do
        fs <- case details of
          PrefixCon _ xs -> Just xs
          InfixCon _ x1 x2 -> Just [x1, x2]
          _ -> Nothing
        (sub_evs, sub_pats) <- unzip <$> traverse readEmbPat fs
        Just (cevs : concat sub_evs, EmbPatCon con ps sub_pats)
      TuplePat ps fs Boxed | length fs /= 1 -> do
        (sub_evs, sub_pats) <- unzip <$> traverse readEmbPat fs
        Just (concat sub_evs, EmbPatCon (tupleDataCon Boxed (length fs)) ps sub_pats)
      ListPat elem_ty fs ->
        foldr (\hd acc -> do
                 (e1, p1) <- readEmbPat hd
                 (e2, p2) <- acc
                 Just (e1 ++ e2, EmbPatCon consDataCon [elem_ty] [p1, p2]))
              (Just ([], EmbPatCon nilDataCon [elem_ty] []))
              fs
      _ -> Nothing

    patBinders :: EmbPat -> [Maybe Id]
    patBinders (EmbPatVar mb_v)     = [mb_v]
    patBinders (EmbPatCon _ _ subs) = concatMap patBinders subs

    isConEmbPat EmbPatCon{} = True
    isConEmbPat _           = False

    readClause mono fun body_tvs arity col_idx
               (L loc (Match { m_pats = L _ pats, m_grhss = GRHSs _ (L _ (GRHS _ [] (L _ rhs)) :| []) (L _ (EmptyLocalBinds _)) }))
      | length pats == arity
      , Just (before, scrut_pat, after) <- splitCaseOperand col_idx pats = do
          (evs, c_pat) <- readEmbPat scrut_pat
          guard (isConEmbPat c_pat)
          carried_l <- traverse plainBinder before
          carried_r <- traverse plainBinder after
          let carried = carried_l ++ carried_r
              local_scope = mkUniqSet (catMaybes (patBinders c_pat ++ carried))
          (evs,) . EmbClause (locA loc) c_pat carried <$> parseEmbRhs mono fun body_tvs local_scope rhs
    readClause _ _ _ _ _ _ = Nothing

    parseEmbRhs mono fun body_tvs scope = go_top
      where
        go_top (HsPar _ e) = go_top (unLoc e)
        go_top (XExpr (WrapExpr WpHole e)) = go_top e
        go_top (XExpr (ExpandedThingTc (HSE (ExprCtxt HsEmbTy{}) (L _ (XExpr (WrapExpr (WpCompose (WpTyApp t) (WpTyLam _)) _)))))) =
          Just (EmbTypeRhs t)
        go_top (HsCase _ scrut (MG { mg_alts = L _ [] })) | Just v <- hsVar (unLoc scrut) = Just (EmbAbsurdRhs v)
        go_top (HsApp _ fn arg)
          | Just f <- hsHead (unLoc fn), Just m <- nameModule_maybe (idName f), moduleUnitId m == ghcInternalUnitId
          , occNameString (nameOccName (idName f)) == "absurd", Just v <- hsVar (unLoc arg) = Just (EmbAbsurdRhs v)
        go_top (HsVar _ (L _ v))
          | v `elementOfUniqSet` scope, isLargeElimResultKind (varType v) = Just (EmbTypeRhs (TermVarTy v))
        go_top e = parse_call e

        go_arg (HsPar _ e) = go_arg (unLoc e)
        go_arg (XExpr (WrapExpr WpHole e)) = go_arg e
        go_arg (XExpr (ExpandedThingTc (HSE (ExprCtxt HsEmbTy{}) (L _ (XExpr (WrapExpr (WpCompose (WpTyApp t) (WpTyLam _)) _)))))) =
          Just (EmbTypeRhs t)
        go_arg (HsVar _ (L _ v))
          | v `elementOfUniqSet` scope = Just (EmbTypeRhs (TermVarTy v))
        go_arg e = parse_call e

        parse_call e = do
          (raw_head, ty_args, val_exprs) <- split_app_spine e []
          guard (not (null val_exprs))
          if raw_head `elementOfUniqSet` scope && null ty_args
            then EmbApplyRhs raw_head <$> traverse go_arg val_exprs
            else do
              let (target, inst_tys)
                    | raw_head == mono = (fun, if null ty_args then mkTyVarTys body_tvs else ty_args)
                    | otherwise        = (raw_head, ty_args)
              guard (isExternalName (idName target) && isNothing (isDataConId_maybe target))
              parsed_args <- traverse go_arg val_exprs
              pure (EmbCallRhs target inst_tys parsed_args)

        split_app_spine (HsPar _ inner) vals = split_app_spine (unLoc inner) vals
        split_app_spine (HsApp _ fn arg) vals = split_app_spine (unLoc fn) (unLoc arg : vals)
        split_app_spine head_e vals = do
          (f, tys) <- split_ty_head head_e []
          pure (f, tys, vals)

        split_ty_head (HsPar _ inner) tys = split_ty_head (unLoc inner) tys
        split_ty_head (HsVar _ (L _ f)) tys = Just (f, tys)
        split_ty_head (XExpr (WrapExpr wp inner)) tys = do
          wp_tys <- wrap_ty_apps wp
          split_ty_head inner (wp_tys ++ tys)
        split_ty_head _ _ = Nothing

        wrap_ty_apps WpHole = Just []
        wrap_ty_apps (WpTyApp t) = Just [t]
        wrap_ty_apps (WpCompose w1 w2) = (++) <$> wrap_ty_apps w2 <*> wrap_ty_apps w1
        wrap_ty_apps _ = Nothing

    hsVar (HsPar _ e) = hsVar (unLoc e); hsVar (XExpr (WrapExpr WpHole e)) = hsVar e
    hsVar (HsVar _ (L _ v)) = Just v; hsVar _ = Nothing

    hsHead (HsPar _ e) = hsHead (unLoc e); hsHead (HsVar _ (L _ f)) = Just f
    hsHead (HsAppType _ fn _) = hsHead (unLoc fn); hsHead (XExpr (WrapExpr _ fn)) = hsHead fn
    hsHead _ = Nothing

certifyEmbSpec :: Bool -> Bool -> TcGblEnv -> (Id -> TcM (Maybe TyCon)) -> [Id] -> EmbSpec -> TcM (Maybe (Id, TyCon))
certifyEmbSpec strict final_exports env lookup_callee emb_callees (EmbSpec fun arity col_idx body_tvs evidence clauses _ _) = do
    clean <- allM emptyEvidence evidence
    if not clean then pure Nothing else do
      ty <- liftZonkM (zonkTcType (varType fun))
      callee_tcs <- traverse (\c -> fmap (c,) <$> lookup_callee c) emb_callees
      case sequence callee_tcs >>= checkDom ty of
        Nothing -> pure Nothing
        Just (tv_flags, inst_args, inst_dom, inst_carried, res_kind, resolved_callees) -> do
          let (tvs, vis_flags) = unzip tv_flags
          vars <- forM body_tvs $ \sk -> flip mkTyVar (tyVarKind sk) <$> newSysName (nameOccName (varName sk))
          let inst_hdr = substTyWith body_tvs (mkTyVarTys vars)
              ordered_doms = placeAt (inst_hdr inst_dom) (map inst_hdr inst_carried)
          args <- zipWithM freshSysTyVar ("x" : "y" : map (("arg" ++) . show) [3 :: Int ..]) ordered_doms
          let fam_binders = zipWith mkNamedTyConBinder vis_flags vars ++ mkAnonTyConBinders args
              placeholder_fam =
                mkFamilyTyCon (varName fun)
                  (mkTyConKindWithMatchability unmatchableDataConTy fam_binders res_kind)
                  fam_binders 0 res_kind Nothing
                  (ClosedTypeFamilyTyCon CTF_Abstract) Nothing NotInjective
          mb_builders <- traverse (buildEmbClause placeholder_fam res_kind body_tvs tvs inst_args inst_dom inst_carried (varName fun) resolved_callees) clauses
          case sequence mb_builders of
            Just builders | let real_brs = catMaybes builders, not (null real_brs) -> do
              table <- case if final_exports then sourceTableOccurrence env fun >>= sourceTableId_maybe else Nothing of
                Just existing -> pure existing
                Nothing ->
                  makeSourceFamily (varName fun) fam_binders res_kind real_brs
              let exported = availsToNameSet (tcg_exports env)
              if final_exports && (not (idName fun `elemNameSet` exported) || not (null (unexportedLargeElimTypes env exported table)))
                then pure Nothing else pure (Just (setIdType fun ty, table))
            _ -> pure Nothing
  where
    placeAt x cs = let (before, after) = splitAt col_idx cs in before ++ (x : after)

    embBinder (ma, Bndr tv vis@(Invisible _))
      | ma `eqType` unmatchableDataConTy, isTyVar tv
      , tcIsLiftedTypeKind (typeKind (tyVarKind tv)) = Just (tv, vis)
    embBinder _ = Nothing

    embCarried tvs arg
      | isLargeElimResultKind arg = True
      | Just tv <- getTyVar_maybe arg = tv `elem` tvs && tcIsLiftedTypeKind (typeKind (tyVarKind tv))
      | Just (FTF_T_T, ma, mult, dom, res) <- splitFunTy_maybe arg
      = ma `eqType` unmatchableDataConTy && isManyTy mult && embCarried tvs dom && embCarried tvs res
      | tcIsLiftedTypeKind (typeKind arg), isTyFamFree arg, all (`elem` tvs) (tyCoVarsOfTypeList arg)
      , Just (tc, _) <- splitTyConApp_maybe arg = isAlgTyCon tc && isLiftedDataOrNewTyCon tc
      | otherwise = False

    peelNArrows 0 ty = Just ([], ty)
    peelNArrows n ty
      | n > 0, Just (arg, rest) <- caseArrow ty = do
          (args, res) <- peelNArrows (n - 1) rest
          pure (arg : args, res)
    peelNArrows _ _ = Nothing

    -- Verify that a constructor/variable pattern matrix is both exhaustive and
    -- pairwise disjoint so that closed CoAxiom branches are order-independent.
    checkExhaustiveDisjointEmbMatrix :: [Type] -> [[EmbPat]] -> Bool
    checkExhaustiveDisjointEmbMatrix [] rows = lengthIs rows 1
    checkExhaustiveDisjointEmbMatrix (ft : fts) rows
      | null rows = isJust (emptyCaseDatatype ft)
      | all is_var_pat (map head rows) =
          checkExhaustiveDisjointEmbMatrix fts (map tail rows)
      | otherwise = isJust $ do
          guard (not strict || all is_con_pat (map head rows))
          (tc, tc_args) <- caseDatatype ft
          guard (null (tyConStupidTheta tc))
          forM_ (tyConDataCons tc) $ \dc -> do
            guard (isVanillaDataCon dc && dataConUserTyVars dc == dataConUnivTyVars dc)
            let d_fts = map scaledThing (dataConInstOrigArgTys dc tc_args)
                spec_row (EmbPatCon dc' _ sub_ps : rest_ps)
                  | dc' == dc = [sub_ps ++ rest_ps | length sub_ps == length d_fts]
                  | otherwise = []
                spec_row (EmbPatVar _ : rest_ps) =
                  [replicate (length d_fts) (EmbPatVar Nothing) ++ rest_ps]
                spec_row [] = []
                sub_rows = concatMap spec_row rows
            guard (checkExhaustiveDisjointEmbMatrix (d_fts ++ fts) sub_rows)
      where
        is_var_pat EmbPatVar{} = True; is_var_pat _ = False
        is_con_pat EmbPatCon{} = True; is_con_pat _ = False

    checkDom ty resolved = do
      let (telescope, tau) = splitForAllForAllTyBinders ty
      guard (equalLength telescope body_tvs && arity > 0 && col_idx >= 0 && col_idx < arity)
      tv_flags <- traverse embBinder telescope
      let tvs = map fst tv_flags
      (doms, raw_res_ty) <- peelNArrows arity tau
      (before, arg_ty, after) <- splitCaseOperand col_idx doms
      let carried = before ++ after
          res_kind = toKindMatchableArrows raw_res_ty
      guard (all (embCarried tvs) carried)
      guard (isLargeElimResultKind raw_res_ty && (null carried || not strict))
      let lhs_fvs = concatMap tyCoVarsOfTypeList (arg_ty : carried); inst_body = substTyWith tvs (mkTyVarTys body_tvs)
      (tc, inst_args) <- case tvs of
        [] | Just (tc, ps) <- ordinaryDataType arg_ty -> pure (tc, ps)
           | not strict, Just (tc, as) <- caseDatatype arg_ty, polySourceType [] arg_ty -> pure (tc, as)
           | otherwise -> Nothing
        _  | Just (tc, []) <- ordinaryDataType arg_ty -> (tc, []) <$ guard (not strict && all (`elem` lhs_fvs) tvs)
           | otherwise -> do
               (tc, args) <- caseDatatype arg_ty
               params <- traverse getTyVar_maybe args
               guard (equalLength params (nubBy (==) params) && all (`elem` tvs) params && all (`elem` lhs_fvs) tvs)
               unless (tc == listTyCon) $ do
                 guard (not strict && null (tyConStupidTheta tc) && strictlyPositiveTyCon tc)
                 forM_ (tyConDataCons tc) $ \con -> do
                   guard (isVanillaDataCon con && dataConUserTyVars con == dataConUnivTyVars con)
                   openDataConFieldTypes con args >>= guard . all (caseKind args)
               pure (tc, map inst_body args)
      let inst_dom = mkTyConApp tc inst_args
          has_nested = any (\(EmbClause _ p _ _) -> case p of { EmbPatCon _ _ subs -> any (\case EmbPatCon{} -> True; _ -> False) subs; _ -> False }) clauses
      guard (not strict || not has_nested)
      guard (checkExhaustiveDisjointEmbMatrix [inst_dom] [[p] | EmbClause _ p _ _ <- clauses])
      pure (tv_flags, inst_args, inst_dom, map inst_body carried, res_kind, resolved)

    buildEmbClause placeholder_fam res_kind body_tvs _ _ dom carried_tys owner_name resolved_callees
                   (EmbClause loc c_pat carried emb_rhs) = do
      c_tys <- liftZonkM (traverse (traverse (zonkTcType . varType)) carried)
      qvars <- forM body_tvs $ \sk -> flip mkTyVar (tyVarKind sk) <$> newSysName (nameOccName (varName sk))
      let inst = substTyWith body_tvs (mkTyVarTys qvars)
          allocVar mb_v k = flip mkTyVar k <$> newSysName (maybe (mkTyVarOcc "_") (setOccNameSpace tvName . nameOccName . varName) mb_v)
          elabEmbPat :: Type -> EmbPat -> TcM (Maybe (Type, [(Maybe Id, TyVar)]))
          elabEmbPat exp_ty (EmbPatVar mb_v) = do
            mb_v_ty <- liftZonkM (traverse (zonkTcType . varType) mb_v)
            if not (maybe True (\vt -> exp_ty `eqType` inst vt) mb_v_ty)
              then pure Nothing
              else do
                tv <- allocVar mb_v exp_ty
                pure (Just (mkTyVarTy tv, [(mb_v, tv)]))
          elabEmbPat exp_ty (EmbPatCon dc pat_args sub_pats) = do
            z_args <- liftZonkM (zonkTcTypes pat_args)
            case splitTyConApp_maybe exp_ty of
              Just (exp_tc, exp_tc_args)
                | dataConTyCon dc == exp_tc
                , isVanillaDataCon dc && dataConUserTyVars dc == dataConUnivTyVars dc
                , eqTypes (map inst z_args) exp_tc_args
                , let f_tys = map scaledThing (dataConInstOrigArgTys dc exp_tc_args)
                , length sub_pats == length f_tys -> do
                    mb_subs <- zipWithM elabEmbPat f_tys sub_pats
                    pure $ do
                      subs <- sequence mb_subs
                      let (sub_lhs_tys, sub_scopes) = unzip subs
                      pure (mkTyConApp (promoteDataCon dc) (exp_tc_args ++ sub_lhs_tys), concat sub_scopes)
              _ -> pure Nothing
      mb_elab <- elabEmbPat (inst dom) c_pat
      case mb_elab of
        Nothing -> pure Nothing
        Just (lhs_con, fields_scope) -> do
          let bound_ids = catMaybes (map fst fields_scope ++ carried)
              field_tvs = map snd fields_scope
          if not (equalLength c_tys carried_tys &&
                  sizeUniqSet (mkUniqSet bound_ids) == length bound_ids &&
                  and (zipWith (\e a -> maybe True (eqType e) a) carried_tys c_tys))
            then pure Nothing
            else do
              carried_tvs <- zipWithM allocVar carried (map inst carried_tys)
              let carried_scope = zip carried carried_tvs
                  patterns = mkTyVarTys qvars ++ placeAt lhs_con (mkTyVarTys carried_tvs)
                  binders = qvars ++ field_tvs ++ carried_tvs
              case emb_rhs of
                EmbAbsurdRhs v -> pure $ do
                  tv <- lookup (Just v) (fields_scope ++ carried_scope)
                  Nothing <$ guard (all isTyFamFree patterns && inst (varType v) `eqType` tyVarKind tv &&
                                    isJust (emptyCaseDatatype (tyVarKind tv)))
                _ -> do
                  zonked_rhs <- zonk_emb_rhs emb_rhs
                  pure $ do
                    guard (all isTyFamFree patterns && clean_emb_rhs zonked_rhs)
                    _ <- rewriteEmb placeholder_fam qvars binders inst (inst dom) (mkTyVarTys qvars) fields_scope carried_scope zonked_rhs
                    pure $ Just $ \fam ->
                      let rhs_t = fromMaybe (panic "certifyEmbSpec") $
                                    rewriteEmb fam qvars binders inst (inst dom) (mkTyVarTys qvars) fields_scope carried_scope zonked_rhs
                      in mkCoAxBranch binders [] [] patterns rhs_t (map (const Nominal) binders) loc
      where
        zonk_emb_rhs (EmbTypeRhs t) = EmbTypeRhs <$> liftZonkM (zonkTcType t)
        zonk_emb_rhs (EmbCallRhs f ts args) =
          EmbCallRhs <$> (setIdType f <$> liftZonkM (zonkTcType (varType f)))
                     <*> liftZonkM (zonkTcTypes ts)
                     <*> traverse zonk_emb_rhs args
        zonk_emb_rhs (EmbApplyRhs v args) =
          EmbApplyRhs <$> (setIdType v <$> liftZonkM (zonkTcType (varType v)))
                      <*> traverse zonk_emb_rhs args
        zonk_emb_rhs r = pure r

        clean_emb_rhs (EmbTypeRhs zonked_ty) =
          not (hasCoercionHole zonked_ty) &&
          not (any isMetaTyVar (tyCoVarsOfTypeList zonked_ty)) &&
          typeKind zonked_ty `eqType` res_kind
        clean_emb_rhs (EmbCallRhs _ ts args) =
          all (\t -> not (hasCoercionHole t) && not (any isMetaTyVar (tyCoVarsOfTypeList t))) ts &&
          all clean_emb_arg args
        clean_emb_rhs (EmbApplyRhs _ args) =
          all clean_emb_arg args
        clean_emb_rhs _ = False

        clean_emb_arg (EmbTypeRhs t) =
          not (hasCoercionHole t) && not (any isMetaTyVar (tyCoVarsOfTypeList t))
        clean_emb_arg c@EmbCallRhs{}  = clean_emb_arg_call c
        clean_emb_arg c@EmbApplyRhs{} = clean_emb_arg_call c
        clean_emb_arg _ = False

        clean_emb_arg_call (EmbCallRhs _ ts args) =
          all (\t -> not (hasCoercionHole t) && not (any isMetaTyVar (tyCoVarsOfTypeList t))) ts &&
          all clean_emb_arg args
        clean_emb_arg_call (EmbApplyRhs _ args) = all clean_emb_arg args
        clean_emb_arg_call _ = False

        peel_emb_carry all_scope (ForAllTy _ (Bndr tv (Invisible InferredSpec)) inner)
          | let s = occNameString (nameOccName (varName tv))
          , Just occ <- if take 11 s == "$emb_carry:" then Just (drop 11 s) else Nothing
          = do
              (_, target_tv) <- find (\(mb_v, _) -> maybe False (\v -> occNameString (nameOccName (idName v)) == occ) mb_v) all_scope
              (rest_pairs, body) <- peel_emb_carry all_scope inner
              pure ((tv, target_tv) : rest_pairs, body)
        peel_emb_carry _ ty = pure ([], ty)

        check_app_kinds :: Kind -> [Type] -> Bool
        check_app_kinds _ [] = True
        check_app_kinds fn_k (a : as) = case splitFunTy_maybe fn_k of
          Just (FTF_T_T, _, _, dom_k, cod_k) -> typeKind a `eqType` dom_k && check_app_kinds cod_k as
          _ -> False

        rewriteEmb fam qvars binders inst inst_dom fam_args fields_scope carried_scope root_rhs = do
          let all_scope = fields_scope ++ carried_scope
              field_set = mkUniqSet (map snd fields_scope); bndr_set = mkVarSet binders
              is_self t = nameModule_maybe (tyConName t) == nameModule_maybe owner_name &&
                          nameOccName (tyConName t) == mkSourceTableOcc (nameOccName owner_name)
              is_pending t | Just (ClosedTypeFamilyTyCon CTF_Abstract) <- famTyConFlav_maybe t =
                               isJust (sourceTableTermOcc (nameOccName (tyConName t)))
                           | otherwise = False
              resolve_callee t = listToMaybe [rt | (cid, rt) <- resolved_callees
                                                 , nameModule_maybe (tyConName t) == nameModule_maybe (idName cid)
                                                 , nameOccName (tyConName t) == mkSourceTableOcc (nameOccName (idName cid))]
              resolve_callee_id cid =
                listToMaybe [rt | (c, rt) <- resolved_callees, idName c == idName cid] <|>
                (sourceTableOccurrence env cid >>= sourceTableId_maybe)
              check_self_args mapped = do
                let fam_arity = length fam_args + 1 + length carried_scope
                guard (length mapped >= fam_arity)
                let (self_mapped, extra_args) = splitAt fam_arity mapped
                    (t_args, v_args) = splitAt (length fam_args) self_mapped
                (c_before, rec_arg, c_after) <- splitCaseOperand col_idx v_args
                let rec_cs = c_before ++ c_after
                ftv <- getTyVar_maybe rec_arg
                guard (eqTypes t_args fam_args &&
                       ftv `elementOfUniqSet` field_set && tyVarKind ftv `eqType` inst_dom &&
                       and (zipWith (\rc (_, ctv) -> typeKind rc `eqType` tyVarKind ctv) rec_cs carried_scope) &&
                       (not strict || eqTypes rec_cs (mkTyVarTys (map snd carried_scope))))
                let base_app = mkTyConApp fam (fam_args ++ v_args)
                guard (check_app_kinds (typeKind base_app) extra_args)
                pure (mkAppTys base_app extra_args)
              map_with extra_sk =
                let sk_map = extra_sk ++ zip body_tvs qvars
                    (map_ty, _, _, _) = mapTyCo TyCoMapper
                      { tcm_tyvar = \tv -> pure (mkTyVarTy (fromMaybe tv (lookup tv sk_map)))
                      , tcm_termvar = \v -> lookup (Just v) all_scope >>= \tv -> mkTyVarTy tv <$ guard (inst (varType v) `eqType` tyVarKind tv)
                      , tcm_termbinder = \_ _ -> Nothing, tcm_covar = \_ -> Nothing, tcm_hole = \_ -> Nothing
                      , tcm_tycobinder = \tv _ k -> guard (tv `notElem` body_tvs) *> k tv
                      , tcm_tcapp_ty = \_ tc mapped ->
                          if is_self tc then check_self_args mapped
                          else case resolve_callee tc of
                            Just rt -> do
                              guard (length mapped >= tyConArity rt)
                              let (sat, extra) = splitAt (tyConArity rt) mapped
                                  base_app = mkTyConApp rt sat
                              guard (check_app_kinds (typeKind base_app) extra_args_ok)
                              pure (mkAppTys base_app extra)
                              where extra_args_ok = drop (tyConArity rt) mapped
                            Nothing
                              | isTypeSynonymTyCon tc, Just expanded <- coreView (mkTyConApp tc mapped)
                              , any (\t -> is_self t || is_pending t) (nonDetEltsUniqSet (tyConsOfTypes [expanded])) -> map_ty expanded
                              | otherwise -> mkTyConApp tc mapped <$ guard (not (is_pending tc))
                      , tcm_tcapp_co = \_ role tc cos -> mkTyConAppCo role tc cos <$ guard (not (is_self tc) && not (is_pending tc)) }
                in map_ty
              go_rhs (EmbTypeRhs raw_ty) = do
                (carry_pairs, inner_ty) <- peel_emb_carry all_scope raw_ty
                map_with carry_pairs inner_ty
              go_rhs (EmbApplyRhs v arg_rhss) = do
                tv <- lookup (Just v) all_scope
                guard (inst (varType v) `eqType` tyVarKind tv)
                mapped_args <- traverse go_rhs arg_rhss
                guard (check_app_kinds (tyVarKind tv) mapped_args)
                pure (mkAppTys (mkTyVarTy tv) mapped_args)
              go_rhs (EmbCallRhs target ty_args arg_rhss) = do
                mapped_tys  <- traverse (map_with []) ty_args
                mapped_args <- traverse go_rhs arg_rhss
                let mapped = mapped_tys ++ mapped_args
                if idName target == owner_name
                  then check_self_args mapped
                  else do
                    rt <- resolve_callee_id target
                    guard (matchesSourceFamilyKind (varType target) rt && length mapped >= tyConArity rt)
                    let (sat, extra) = splitAt (tyConArity rt) mapped
                        base_app = mkTyConApp rt sat
                    guard (check_app_kinds (typeKind base_app) extra)
                    pure (mkAppTys base_app extra)
              go_rhs EmbAbsurdRhs{} = Nothing
          rewritten <- go_rhs root_rhs
          rewritten <$ guard (not (hasNativeDependentExpr (Core.Type rewritten)) &&
                              all (`elemVarSet` bndr_set) (tyCoVarsOfTypeList rewritten) &&
                              typeKind rewritten `eqType` res_kind)

sourceMatches :: HsBind GhcTc -> Maybe (Id, Id, [TyVar], [TcEvBinds], [LMatch GhcTc (LHsExpr GhcTc)])
sourceMatches binding = do
    (fun, mono, body_tvs, MG { mg_ext = MatchGroupTc { mg_origin = FromSource }, mg_alts = L _ matches }, evidence) <-
      case binding of
        XHsBindsLR (AbsBinds { abs_tvs = [], abs_ev_vars = [], abs_ev_binds = evs
                             , abs_exports = [ABE { abe_poly = fun, abe_mono = mono, abe_wrap = WpHole }]
                             , abs_binds = body_binds, abs_sig = has_sig }) -> do
          (bound, body_tvs, matches) <- listToMaybe
            [ fb | L _ b <- body_binds, Just fb@(f, _, _) <- [funBind b], f == mono ]
          (fun, mono, body_tvs, matches, evs) <$ guard (bound == mono && (null body_tvs || has_sig))
        _ -> do
          (f, [], ms) <- funBind binding
          pure (f, f, [], ms, [])
    (fun, mono, body_tvs, evidence, matches) <$ guard (isExternalName (varName fun))
  where
    funBind (FunBind { fun_ext = (wrap, []), fun_id = L _ f, fun_matches = mg })
      | (tvs, WpHole) <- collectHsWrapBinders wrap
      , all (\tv -> isTyVar tv && (tcIsLiftedTypeKind (tyVarKind tv)
                                   || isJust (ordinaryDataType (tyVarKind tv))
                                   || tcIsLiftedTypeKind (typeKind (tyVarKind tv)))) tvs
      = Just (f, tvs, mg)
    funBind _ = Nothing

emptyEvidence :: TcEvBinds -> TcM Bool
emptyEvidence ev_binds = do
    env <- getGblEnv
    binds <- case ev_binds of
      EvBinds bs    -> pure ( foldr (:) [] bs )
      TcEvBinds var -> foldr (:) [] . evBindMapBinds <$> getTcEvBindsMap var
    pure (all (certified_ev_bind env) binds)
  where
    certified_ev_bind env EvBind{eb_rhs = EvExpr e} = certified_dict_expr env e
    certified_ev_bind _ _                           = False

    certified_dict_expr env expr =
      let (hd, args) = Core.collectArgs expr
      in case hd of
           Core.Var v
             | DFunId{} <- idDetails (installed_id env v)
             , let cur = installed_id env v
             , not (null (sourceMethodAliases cur))
             -> all (\a -> Core.isTypeArg a || certified_dict_expr env a) args
           _ -> False

    installed_id env v = case lookupTypeEnv (tcg_type_env env) (idName v) of
      Just (AnId cur) -> cur
      _               -> v

rewriteCertifiedDictCalls :: TcGblEnv -> Core.CoreExpr -> TcM Core.CoreExpr
rewriteCertifiedDictCalls env = go
  where
    installed_id v = case lookupTypeEnv (tcg_type_env env) (idName v) of
      Just (AnId cur) -> cur
      _               -> v

    go expr
      | (Core.Var sel, args) <- Core.collectArgs expr
      , Just cls <- isClassOpId_maybe sel
      , let n_cls = length (classTyVars cls)
      , (ty_args, Core.Var raw_dfun : rest_args) <- splitAt n_cls args
      , Just pred_tys <- coreTypeArgs ty_args
      , let dfun = installed_id raw_dfun
      , Just target_name <- sourceMethodTarget_maybe dfun sel
      = do
          mb_target <- lookupInstalledOrGlobalId env target_name
          case mb_target of
            Just target
              | isJust (sourceTableId_maybe target)
              , varType target `eqType` classMethodInstTy sel pred_tys
              -> Core.mkApps (Core.Var target) <$> traverse go rest_args
            _ -> go_default expr
    go expr = go_default expr

    go_default (Core.App f a)                 = Core.App <$> go f <*> go a
    go_default (Core.Lam b e)                 = Core.Lam b <$> go e
    go_default (Core.Let (Core.NonRec b r) e) = Core.Let <$> (Core.NonRec b <$> go r) <*> go e
    go_default (Core.Let (Core.Rec ps) e)     = Core.Let . Core.Rec <$> traverse (\(b, r) -> (b,) <$> go r) ps <*> go e
    go_default (Core.Case s b t as)           = Core.Case <$> go s <*> pure b <*> pure t <*> traverse (\(Core.Alt c bs r) -> Core.Alt c bs <$> go r) as
    go_default (Core.Cast e co)               = (`Core.Cast` co) <$> go e
    go_default (Core.Tick t e)                = Core.Tick t <$> go e
    go_default other                          = pure other

bindingCore :: LHsBind GhcTc -> Id -> TcM (Maybe Core.CoreExpr)
bindingCore bind owner = do
    let mono_to_poly = case unLoc bind of
          XHsBindsLR AbsBinds{abs_tvs = [], abs_ev_vars = [], abs_exports = exports} ->
            [(abe_mono, Core.Var abe_poly) | ABE{abe_poly = abe_poly, abe_mono = abe_mono, abe_wrap = WpHole} <- exports]
          _ -> []
    (messages, bindings) <- initDsTc (dsTopLHsBinds [bind])
    pure $ case bindings of
      Just core | not (errorsFound messages), [raw_rhs] <- [e | (i, e) <- fromOL core, idName i == idName owner] ->
        Just (unwrapMutualRec mono_to_poly raw_rhs)
      _ -> Nothing
  where
    unwrapMutualRec pairs (Core.Let (Core.Rec rec_binds) (Core.Var target))
      | not (null pairs)
      , Just body <- lookup target rec_binds
      , all (\(m, _) -> isJust (lookup m pairs)) rec_binds
      = let in_scope = mkInScopeSet (exprsFreeVars (map snd rec_binds ++ map snd pairs))
            sub      = CoreSubst.mkOpenSubst in_scope pairs
        in CoreSubst.substExpr sub body
    unwrapMutualRec _ expr = expr

unpackSourceBinds :: LHsBinds GhcTc -> [LHsBind GhcTc]
unpackSourceBinds = concatMap split_one
  where
    split_one (L loc (XHsBindsLR abs_b@AbsBinds{abs_tvs = [], abs_ev_vars = [], abs_exports = exports@(_ : _ : _)})) =
      [L loc (XHsBindsLR (abs_b { abs_exports = [abe] })) | abe <- exports]
    split_one b = [b]

hasPendingTypedSplice :: Data a => a -> Bool
hasPendingTypedSplice node = case cast node :: Maybe (HsExpr GhcTc) of
    Just HsTypedSplice{} -> True
    _ -> or (gmapQ hasPendingTypedSplice node)

{- Note [Fallback for uncertified local source functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The renamer allocates a private source-table name @T:f@ for a signed local
function @f@ before typechecking. Core certification later succeeds only when
@f@ lies in a certified structural, mapper, or closure fragment.

A valid program can still apply a total local non-structural function @f@ to
open variables in a type, for example an accumulator loop with a local
recursive @where@ helper, a function with literal pattern matches and a
@DEFAULT@ branch, or a wrapper around a guarded local helper. When @T:f@ has
no certified closed family, type elaboration falls back to open evaluation and
then to the uninterpreted closed family @U:f@.

When a typechecker plugin is active, allow any solved non-large-elimination
local function to reach @U:f@ so the plugin can inspect or refute the goal.
When no typechecker plugin is active, 'isSafeLocalUninterpretedRhs' restricts
this fallback to lazy, non-HPC, non-bottoming Core right-hand sides without
pattern-match error calls or strict single-@DEFAULT@ cases. Local recursive
@let@/@where@ helpers are permitted when their branches are exhaustive. Direct
top-level self-recursion is permitted only when the body scrutinizes literals
with a @DEFAULT@ branch, which rejects unproductive structural loops on
algebraic datatypes.
-}

-- | Drop unreferenced non-recursive 'Core.Let' bindings left behind by
-- pattern-match desugaring (such as unused @fail@ continuations calling
-- 'patErrorId').
pruneDeadNonRecLets :: Core.CoreExpr -> Core.CoreExpr
pruneDeadNonRecLets (Core.App f a)  = Core.App (pruneDeadNonRecLets f) (pruneDeadNonRecLets a)
pruneDeadNonRecLets (Core.Lam v b)  = Core.Lam v (pruneDeadNonRecLets b)
pruneDeadNonRecLets (Core.Cast b c) = Core.Cast (pruneDeadNonRecLets b) c
pruneDeadNonRecLets (Core.Tick t b) = Core.Tick t (pruneDeadNonRecLets b)
pruneDeadNonRecLets (Core.Let (Core.NonRec v r) b) =
  let b' = pruneDeadNonRecLets b
  in if v `elemVarSet` exprsFreeVars [b']
       then Core.Let (Core.NonRec v (pruneDeadNonRecLets r)) b'
       else b'
pruneDeadNonRecLets (Core.Let (Core.Rec pairs) b) =
  Core.Let (Core.Rec [(v, pruneDeadNonRecLets r) | (v, r) <- pairs]) (pruneDeadNonRecLets b)
pruneDeadNonRecLets (Core.Case s cb ty alts) =
  Core.Case (pruneDeadNonRecLets s) cb ty
    [Core.Alt con bs (pruneDeadNonRecLets r) | Core.Alt con bs r <- alts]
pruneDeadNonRecLets expr = expr

-- | Check whether an uncertified local Core right-hand side can elaborate as
-- an uninterpreted type-level application when no 'tcPlugin' is active.
-- See Note [Fallback for uncertified local source functions].
isSafeLocalUninterpretedRhs :: Bool -> Bool -> Name -> Core.CoreExpr -> Bool
isSafeLocalUninterpretedRhs strict_mode hpc_mode owner_nm raw_rhs
  | strict_mode || hpc_mode = False
  | any is_bottom_fv fvs = False
  | any (\v -> idName v == owner_nm) fvs && not (has_lit_case rhs) = False
  | otherwise = go_e rhs
  where
    rhs = pruneDeadNonRecLets raw_rhs
    fvs = exprSomeFreeVarsList isId rhs

    is_bottom_fv v =
      v `hasKey` patErrorIdKey
      || isDeadEndId v
      || case nameModule_maybe (idName v) of
           Just m | moduleUnitId m == ghcInternalUnitId ->
             occNameString (nameOccName (idName v))
               `elem` ["error", "errorWithoutStackTrace", "undefined", "divZeroError", "overflowError"]
           _ -> False

    is_lit_alt (Core.Alt Core.LitAlt{} _ _) = True
    is_lit_alt _                            = False

    is_def_alt (Core.Alt Core.DEFAULT _ _) = True
    is_def_alt _                           = False

    has_lit_case (Core.App f a) = has_lit_case f || has_lit_case a
    has_lit_case (Core.Lam _ b) = has_lit_case b
    has_lit_case (Core.Cast b _) = has_lit_case b
    has_lit_case (Core.Tick _ b) = has_lit_case b
    has_lit_case (Core.Let (Core.NonRec _ r) b) = has_lit_case r || has_lit_case b
    has_lit_case (Core.Let (Core.Rec pairs) b) = any (has_lit_case . snd) pairs || has_lit_case b
    has_lit_case (Core.Case s _ _ alts) =
      (any is_lit_alt alts && any is_def_alt alts)
      || has_lit_case s
      || any (\(Core.Alt _ _ r) -> has_lit_case r) alts
    has_lit_case _ = False

    go_e (Core.Var _)      = True
    go_e (Core.Lit _)      = True
    go_e (Core.Type _)     = True
    go_e (Core.Coercion _) = True
    go_e (Core.App f a)    = go_e f && go_e a
    go_e (Core.Lam _ b)    = go_e b
    go_e (Core.Cast b _)   = go_e b
    go_e (Core.Tick _ b)   = go_e b
    go_e (Core.Let (Core.NonRec _ r) b) = go_e r && go_e b
    go_e (Core.Let (Core.Rec pairs) b)  = all (go_e . snd) pairs && go_e b
    go_e (Core.Case s _ _ alts@(_ : _)) =
      go_e s && check_alts alts && all (\(Core.Alt _ _ r) -> go_e r) alts
    go_e _ = False

    check_alts [Core.Alt Core.DEFAULT _ _] = False
    check_alts alts
      | any is_lit_alt alts = any is_def_alt alts
      | dc0 : rest_dcs <- [dc | Core.Alt (Core.DataAlt dc) _ _ <- alts] =
          any is_def_alt alts
          || length (dc0 : rest_dcs) == length (tyConDataCons (dataConTyCon dc0))
      | otherwise = False

nominalBindPrecheck :: HsBind GhcTc -> Maybe (Id, [TyVar], [TcEvBinds])
nominalBindPrecheck binding = do
    (fun, _, body_tvs, evs, matches) <- sourceMatches binding
    guard (not (null matches) && not (hasPendingTypedSplice binding))
    (\pevs -> (fun, body_tvs, evs ++ concat pevs)) <$> traverse checkMatch matches
  where
    checkMatch (L _ Match{m_pats = L _ pats, m_grhss = GRHSs _ (L _ (GRHS _ [] _) :| []) (L _ local_binds)})
      | checkLocalBinds local_binds = concat <$> traverse checkPat pats
    checkMatch _ = Nothing
    checkLocalBinds (EmptyLocalBinds _) = True
    checkLocalBinds _                   = False
    checkPat (L _ p) = case p of
      ParPat _ inner -> checkPat inner
      SigPat _ inner _ -> checkPat inner
      AsPat _ _ inner -> checkPat inner
      VarPat{} -> Just []; WildPat{} -> Just []; LitPat{} -> Just []
      ListPat _ ps -> concat <$> traverse checkPat ps
      TuplePat _ ps Boxed | length ps /= 1 -> concat <$> traverse checkPat ps
      XPat (ExpansionPat _ inner) -> checkPat (noLocA inner)
      XPat (CoPat WpHole inner _) -> checkPat (noLocA inner)
      ConPat (ConPatTc _ [] [] cevs WpHole []) (L _ (RealDataCon _)) details ->
        (cevs :) . concat <$> traverse checkPat (hsConPatArgs details)
      _ -> Nothing

sourceTableTyCons :: Bool -> Bool -> LHsBinds GhcTc -> TcM [(Id, TyCon)]
sourceTableTyCons local final_exports raw_binds = do
    let binds = unpackSourceBinds raw_binds
    dep_types <- xoptM LangExt.DependentTypes
    strict <- xoptM LangExt.Strict
    rules <- goptM Opt_EnableRewriteRules
    dflags <- getDynFlags
    env <- getGblEnv
    let generates_code = backendGeneratesCode (backend dflags)
        produces_iface = gopt Opt_WriteInterface dflags || not (isOneShot (ghcMode dflags))
    no_code_final <- if generates_code || not final_exports || not produces_iface ||
                        not (gopt Opt_WriteSourceEquations dflags) || gopt Opt_WriteIfSimplifiedCore dflags ||
                        gopt Opt_Hpc dflags || not (null (tcg_rules env))
      then pure False else not . hasActiveLatePlugins <$> getTopEnv
    let checks_final_core = generates_code || no_code_final
        prepare = safeSimpleOptExpr (initSimpleOpts dflags)
    (visible_rules, unloaded_rules, local_rules, build_clean) <- if rules && checks_final_core
      then do
        visible <- visibleSourceMethodRules env
        unloaded <- unloadedSourceRuleHeads env
        let local_r = sourceRuleOwners (tcg_rules env)
        build_ready <- isBootModuleReady gHC_INTERNAL_BASE
        build_name <- lookupOrig gHC_INTERNAL_BASE (mkVarOcc "build")
        mb_build <- if build_ready then tcLookupIdMaybe build_name else pure Nothing
        let ok = build_ready && not (build_name `elemNameSet` local_r) &&
                 not (build_name `elemNameSet` unloaded) && maybe True (not . hasSourceMethodRule visible) mb_build
        pure (Just visible, unloaded, local_r, ok)
      else pure (Nothing, emptyNameSet, emptyNameSet, False)

    let bind_owners = concatMap (collectHsBindBinders CollNoDictBinders . unLoc) raw_binds
        local_names = mkNameSet (map idName bind_owners)
        isBuiltin v | Just m <- nameModule_maybe (idName v), moduleUnitId m == ghcInternalUnitId =
                        occNameString (nameOccName (idName v)) `elem` ["build", "absurd"]
                    | otherwise = False

    prepared <- fmap catMaybes $ forM binds $ \bind -> case readEmbSpec (unLoc bind) of
      Just emb@(EmbSpec fun _ _ _ _ _ emb_tys call_ids) -> do
        let call_name_set = mkNameSet (map idName call_ids)
            emb_calls = [ o | o <- bind_owners, o /= fun
                            , idName o `elemNameSet` call_name_set ||
                              any (\tc -> let tn = tyConName tc in
                                     maybe False (\to -> nameModule_maybe (idName o) == nameModule_maybe tn && nameOccName (idName o) == to)
                                           (sourceTableTermOcc (nameOccName tn)))
                                  (nonDetEltsUniqSet (tyConsOfTypes emb_tys)) ]
        pure (Just (fun, Left (emb, emb_calls), emb_calls))
      Nothing -> case nominalBindPrecheck (unLoc bind) of
        Nothing -> pure Nothing
        Just (fun, _, evs) -> do
          clean <- allM emptyEvidence evs
          if not clean then pure Nothing else do
            ty <- liftZonkM (zonkTcType (varType fun))
            if any isMetaTyVar (tyCoVarsOfTypeList ty) then pure Nothing else do
              zonked_bind <- if final_exports then pure bind else do
                let orig_for_zonk = case unLoc bind of
                      XHsBindsLR AbsBinds{abs_exports = [abe]} ->
                        fromMaybe bind $ find (\orig_b -> case unLoc orig_b of
                          XHsBindsLR orig_abs -> any (\e -> idName (abe_poly e) == idName (abe_poly abe)) (abs_exports orig_abs)
                          _ -> False) raw_binds
                      _ -> bind
                (_, _, [zb], _, _, _, _) <- withReadOnlyFinalZonk $ zonkTopDecls emptyBag [orig_for_zonk] [] [] [] []
                pure $ case unLoc zb of
                  XHsBindsLR zb_abs@AbsBinds{abs_exports = exports@(_ : _ : _)}
                    | Just abe' <- find (\e -> idName (abe_poly e) == idName fun) exports ->
                        L (getLoc zb) (XHsBindsLR (zb_abs { abs_exports = [abe'] }))
                  _ -> zb
              let owner = setIdType fun ty
              mb_rhs <- bindingCore zonked_bind owner
              case mb_rhs of
                Nothing -> pure Nothing
                Just rhs -> do
                  rewritten <- rewriteCertifiedDictCalls env (prepare rhs)
                  let opt_rhs = prepare rewritten
                      callees = exprSomeFreeVarsList
                        (\v -> isId v && isExternalName (idName v) && idName v /= idName owner &&
                               not (v `hasKey` patErrorIdKey) && isNothing (isDataConWorkId_maybe v) && not (isBuiltin v))
                        opt_rhs
                  pure (Just (owner, Right (rhs, opt_rhs, callees), callees))

    structural <- if local && not strict
      then fmap catMaybes $ forM prepared $ \(owner, payload, _) -> case payload of
        Right (_, opt_rhs, []) -> do
          let tryRev = do
                sig <- constructorCaseType (varType owner); guard (isBinaryListCase sig)
                cct <- compileCoreEquations ((defaultCoreSpec (const True))
                         { ccs_allow_tail_self = True, ccs_allow_accum_self = True, ccs_check_kind_public = False }) owner opt_rhs
                cct <$ guard (isValidReverseWorkerTable cct)
              tryStruct = do
                sig <- constructorCaseType (varType owner); guard (isTwoStructuralCase sig)
                cct <- compileStructuralCore (const True) owner opt_rhs
                cct <$ guard (lengthIs (cct_rows cct) 3)
          maybe (pure Nothing) (\cct -> Just . (owner,) <$> publishCompiledTable owner cct) (tryStruct <|> tryRev)
        _ -> pure Nothing
      else pure []

    let struct_names = mkNameSet (map (idName . fst) structural)
        remaining = [c | c@(f, _, _) <- prepared, not (idName f `elemNameSet` struct_names)]
        cand_names = extendNameSetList struct_names [idName f | (f, _, _) <- remaining]
        calleeClean c = not (idName c `elemNameSet` local_rules) &&
          (if finalCoreMapperOwner c
             then maybe True (\vis -> isNothing (finalCoreRuleConflict vis unloaded_rules local_rules c)) visible_rules
             else not (idName c `elemNameSet` unloaded_rules) && not (maybe False (`hasSourceMethodRule` c) visible_rules))
        lookupCallee checked actual = do
          let current = case lookupTypeEnv (tcg_type_env env) (idName actual) of
                Just (AnId fun) | fun == actual -> sourceTableId_maybe fun; _ -> Nothing
              table = if idName actual `elemNameSet` local_names then snd <$> find ((== actual) . fst) checked
                      else if nameModule_maybe (idName actual) == Just (tcg_semantic_mod env) then current
                      else sourceTableId_maybe actual
          ty <- liftZonkM (zonkTcType (varType actual))
          pure (table >>= \fam -> fam <$ guard (matchesSourceFamilyKind ty fam && tyConArity fam > 0))
        exported = availsToNameSet (tcg_exports env)
        certifyNominal callee_pairs owner raw_rhs opt_rhs = do
          let owner_clean = isDefaultInlinePragma (idInlinePragma owner) && calleeClean owner && all (calleeClean . fst) callee_pairs
              allow_build = not rules || (final_exports && checks_final_core && build_clean && owner_clean)
              spec = (defaultCoreSpec (const True))
                { ccs_helper = (`lookup` callee_pairs), ccs_predicate = (`lookup` callee_pairs)
                , ccs_allow_build = allow_build
                , ccs_allow_zero_cases = not strict, ccs_allow_empty_root = dep_types && not strict
                , ccs_allow_tail_self = dep_types && not strict, ccs_allow_sct_self = dep_types && not strict
                , ccs_allow_top_apply = dep_types && not strict, ccs_allow_guard = dep_types && not strict
                , ccs_allow_call_self = True, ccs_check_kind_public = False }
          case compileCoreEquations spec owner opt_rhs >>= checkNominalCoreShape strict of
            Just cct -> do
              mb_pub <- case if final_exports then sourceTableOccurrence env owner >>= sourceTableId_maybe else Nothing of
                Just existing -> pure (Just (existing, []))
                Nothing       -> publishCompiledWithGuard owner cct
              case mb_pub of
                Nothing -> pure Nothing
                Just (table, _)
                  | final_exports && not (publicTableTypes env exported table) -> pure Nothing
                  | otherwise ->
                      pure $ Just (if rules && cct_used_build cct
                                     then setIdDetails owner (SourceTableId (idName owner) table FinalCoreSourceTable)
                                     else owner, table)
            Nothing
              | null callee_pairs, isJust (charCaseResultType (varType owner)) -> do
                  mb_ch <- publishCertifiedCharacter owner raw_rhs
                  mb_ch' <- case mb_ch of
                    Just _  -> pure mb_ch
                    Nothing -> publishCertifiedCharacter owner opt_rhs
                  case mb_ch' of
                    Just (o, tbl) | not final_exports || publicTableTypes env exported tbl -> pure (Just (o, tbl))
                    _ -> pure Nothing
              | otherwise -> pure Nothing
        mkPlaceholderFamily owner = case splitCoreSignature (varType owner) of
          Nothing -> pure Nothing
          Just (tele, domains, result) -> do
            (renaming, quantifiers) <- freshenTyVarBndrs (binderVars tele)
            let kind_binders = zipWith mkNamedTyConBinder (map binderFlag tele) quantifiers
                instantiate  = substTyUnchecked renaming . expandTypeSynonyms
            operands <- mapM (freshSysTyVar "arg" . instantiate) domains
            fam_name <- newImplicitBinder (idName owner) mkSourceTableOcc
            let hdr      = kind_binders ++ mkAnonTyConBinders operands
                inst_res = instantiate result
                ph_tc    = mkFamilyTyCon fam_name (mkTyConKindWithMatchability unmatchableDataConTy hdr inst_res)
                             hdr 0 inst_res Nothing (ClosedTypeFamilyTyCon CTF_Abstract) Nothing NotInjective
            pure (Just ph_tc)
        certifyMutualScc checked group
          | strict || not dep_types = pure []
          | otherwise = do
              let extract_mem (o, Right (raw_r, opt_r, cs), _) = Just (o, raw_r, opt_r, cs)
                  extract_mem _                                = Nothing
              case traverse extract_mem group of
                Nothing -> pure []
                Just members -> do
                  let group_set = mkNameSet [idName o | (o, _, _, _) <- members]
                      ext_callees = nubBy (\a b -> idName a == idName b)
                                      [c | (_, _, _, cs) <- members, c <- cs, not (idName c `elemNameSet` group_set)]
                  resolved_ext <- traverse (\c -> fmap (c,) <$> lookupCallee checked c) ext_callees
                  mb_phs <- traverse (\(o, _, _, _) -> mkPlaceholderFamily o) members
                  case (sequence resolved_ext, sequence mb_phs) of
                    (Just ext_pairs, Just ph_tcs) -> do
                      let peer_pairs = [(o, ph) | ((o, _, _, _), ph) <- zip members ph_tcs]
                          all_helpers = peer_pairs ++ ext_pairs
                          owners_clean = all (\(o, _, _, _) -> isDefaultInlinePragma (idInlinePragma o) && calleeClean o) members &&
                                         all (calleeClean . fst) ext_pairs
                          allow_build = not rules || (final_exports && checks_final_core && build_clean && owners_clean)
                          spec = (defaultCoreSpec (const True))
                            { ccs_helper = (`lookup` all_helpers), ccs_allow_build = allow_build
                            , ccs_allow_zero_cases = True, ccs_allow_empty_root = True
                            , ccs_allow_tail_self = True, ccs_allow_sct_self = True
                            , ccs_allow_top_apply = True, ccs_allow_guard = False
                            , ccs_allow_call_self = True, ccs_check_kind_public = False }
                          mb_ccts = [ compileCoreEquations spec o opt_r >>= checkNominalCoreShapeWithSct False False
                                    | (o, _, opt_r, _) <- members ]
                      case sequence mb_ccts of
                        Just ccts | checkMutualSizeChangeTermination (zip ph_tcs ccts) -> do
                          mb_raw_pubs <- zipWithM (\(o, _, _, _) cct ->
                            case if final_exports then sourceTableOccurrence env o >>= sourceTableId_maybe else Nothing of
                              Just existing -> pure (Just (existing, []))
                              Nothing       -> publishCompiledWithGuard o cct) members ccts
                          case sequence mb_raw_pubs of
                            Just raw_pubs | all (null . snd) raw_pubs -> do
                              let raw_tcs   = map fst raw_pubs
                                  subst_env = mkNameEnv (zip (map tyConName ph_tcs) tied_tcs ++ zip (map tyConName raw_tcs) tied_tcs)
                                  subst_tc tc = fromMaybe tc (lookupNameEnv subst_env (tyConName tc))
                                  tied_tcs  = map (rebuildSourceFamilyWith subst_tc) raw_tcs
                              if final_exports && not (all (publicTableTypes env exported) tied_tcs)
                                then pure []
                                else pure [ (if rules && cct_used_build cct
                                               then setIdDetails o (SourceTableId (idName o) tbl FinalCoreSourceTable)
                                               else o, tbl)
                                          | ((o, _, _, _), cct, tbl) <- zip3 members ccts tied_tcs ]
                            _ -> pure []
                        _ -> pure []
                    _ -> pure []
        sccs = stronglyConnCompFromEdgedVerticesUniq
          [ DigraphNode cand (idName owner) [idName c | c <- cs, idName c `elemNameSet` cand_names]
          | cand@(owner, _, cs) <- remaining ]
        stepScc (available, checked) (AcyclicSCC (owner, payload, cs))
          | all (\f -> not (idName f `elemNameSet` cand_names) || idName f `elemNameSet` available) cs = do
              mb_tbl <- case payload of
                Left (emb, emb_calls) -> certifyEmbSpec strict final_exports env (lookupCallee checked) emb_calls emb
                Right (raw_rhs, opt_rhs, callees) -> do
                  resolved <- traverse (\c -> fmap (c,) <$> lookupCallee checked c) callees
                  maybe (pure Nothing) (\pairs -> certifyNominal pairs owner raw_rhs opt_rhs) (sequence resolved)
              pure $ case mb_tbl of
                Nothing        -> (available, checked)
                Just e@(fn, _) -> (extendNameSet available (idName fn), checked ++ [e])
          | otherwise = pure (available, checked)
        stepScc (available, checked) (CyclicSCC group)
          | all (\(_, _, cs) -> all (\f -> idName f `elemNameSet` grp_set || not (idName f `elemNameSet` cand_names) || idName f `elemNameSet` available) cs) group
          = do
              new_entries <- certifyMutualScc checked group
              if null new_entries
                then pure (available, checked)
                else pure (extendNameSetList available (map (idName . fst) new_entries), checked ++ new_entries)
          | otherwise = pure (available, checked)
          where
            grp_set = mkNameSet [idName o | (o, _, _) <- group]
    snd <$> foldM stepScc (struct_names, structural) sccs
  where
    publicTableTypes env exported table =
      let tables = sourceTableWithCallees table; rdr_env = tcg_rdr_env env; imported = ordinaryImport rdr_env
          not_boot n | nameModule_maybe n == Just (tcg_semantic_mod env) = n `elemNameSet` exported
                     | Just gre <- lookupGRE_Name rdr_env n = any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
                     | otherwise = True
          builtin tc = tc == listTyCon || isBoxedTupleTyCon tc || isKindTyCon tc
      in all (\tc -> builtin tc || publicCaseType env exported imported tc) (tablePromotedParents True tables) &&
         all (\tc -> builtin tc || not_boot (tyConName tc)) (sourceEquationDatatypes tables)

{- Note [Size-change termination for nominal source tables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Certify termination of self-recursive and mutually recursive nominal
`SourceTable` equations using the Size-Change Termination (SCT) criterion of
Lee, Jones, and Ben-Amram (POPL 2001, Theorem 4):
  1. For each call from function `f` (with pattern row `pats`) to `g` (with
     argument list `args`) in the SCC, build a `SctGraph f g` containing:
       * `i ->_strict j` when `args !! j` syntactically equals `corePatToRhs sub`
         for a strict constructor subpattern `sub` of `pats !! i`;
       * `i ->_eq j` when `args !! j` syntactically equals `corePatToRhs (pats !! i)`.
     Nested recursive calls (such as `ack m (ack (S m) n)`) emit separate
     call graphs for both the inner and outer calls.
  2. Compute the transitive closure of the call graphs under edge composition
     (retaining the stronger `SctStrict` bound when two paths connect `(i, k)`),
     bounded at 512 graphs so pathological specifications fail closed cleanly.
  3. Accept iff every idempotent self-loop graph `G` (`G ; G == G`) in the
     closure contains at least one strict diagonal edge `(i, i) -> SctStrict`.
-}

data SctRel = SctEq | SctStrict
  deriving (Eq, Ord)

data SctGraph = SctGraph !Int !Int ![((Int, Int), SctRel)]
  deriving (Eq, Ord)

corePatToRhs :: CorePat -> CoreRhs
corePatToRhs (PatVar v)        = RhsVar v
corePatToRhs (PatCon dc ts fs) = RhsCon dc ts (map corePatToRhs fs)

strictCoreSubPats :: CorePat -> [CorePat]
strictCoreSubPats (PatVar _)      = []
strictCoreSubPats (PatCon _ _ fs) = fs ++ concatMap strictCoreSubPats fs

sameCoreRhs :: CoreRhs -> CoreRhs -> Bool
sameCoreRhs (RhsVar a) (RhsVar b) = a == b
sameCoreRhs (RhsCon dc1 ts1 as1) (RhsCon dc2 ts2 as2) =
  dc1 == dc2 && equalLength ts1 ts2 && eqTypes ts1 ts2 &&
  equalLength as1 as2 && and (zipWith sameCoreRhs as1 as2)
sameCoreRhs (RhsCall tc1 ts1 as1) (RhsCall tc2 ts2 as2) =
  tyConName tc1 == tyConName tc2 && equalLength ts1 ts2 && eqTypes ts1 ts2 &&
  equalLength as1 as2 && and (zipWith sameCoreRhs as1 as2)
sameCoreRhs (RhsSelf ts1 as1) (RhsSelf ts2 as2) =
  equalLength ts1 ts2 && eqTypes ts1 ts2 &&
  equalLength as1 as2 && and (zipWith sameCoreRhs as1 as2)
sameCoreRhs (RhsApply f1 as1) (RhsApply f2 as2) =
  f1 == f2 && equalLength as1 as2 && and (zipWith sameCoreRhs as1 as2)
sameCoreRhs (RhsTupleProj s1 l1 r1) (RhsTupleProj s2 l2 r2) =
  (case (s1, s2) of { (CLeft, CLeft) -> True; (CRight, CRight) -> True; _ -> False }) &&
  l1 `eqType` l2 && sameCoreRhs r1 r2
sameCoreRhs _ _ = False

mkSctGraph :: Int -> Int -> [((Int, Int), SctRel)] -> SctGraph
mkSctGraph src dst raw =
  SctGraph src dst (foldr insertEdge [] (sortBy (\(k1, _) (k2, _) -> compare k1 k2) raw))
  where
    insertEdge (k, r) ((k', r') : rest) | k == k' = (k, max r r') : rest
    insertEdge e rest                             = e : rest

composeSct :: SctGraph -> SctGraph -> Maybe SctGraph
composeSct (SctGraph f1 g1 es1) (SctGraph g2 h2 es2)
  | g1 /= g2  = Nothing
  | otherwise = Just $ mkSctGraph f1 h2
      [ ((i, k), comp r1 r2)
      | ((i, j1), r1) <- es1
      , ((j2, k), r2) <- es2
      , j1 == j2 ]
  where
    comp SctStrict _       = SctStrict
    comp _       SctStrict = SctStrict
    comp SctEq   SctEq     = SctEq

buildCallSctGraph :: Int -> [CorePat] -> Int -> [CoreRhs] -> SctGraph
buildCallSctGraph src pats dst args = mkSctGraph src dst
  [ ((i, j), rel)
  | (i, pat_i) <- zip [0 ..] pats
  , let full_rhs = corePatToRhs pat_i
        sub_rhss = map corePatToRhs (strictCoreSubPats pat_i)
  , (j, arg_j) <- zip [0 ..] args
  , Just rel <- [ if any (sameCoreRhs arg_j) sub_rhss then Just SctStrict
                  else if sameCoreRhs arg_j full_rhs then Just SctEq
                  else Nothing ] ]

checkSctGraphs :: [SctGraph] -> Bool
checkSctGraphs [] = True
checkSctGraphs initial = case closeSet (Set.fromList initial) of
    Nothing      -> False
    Just closure -> all validIdempotent (Set.toList closure)
  where
    maxGraphs = 512
    closeSet cur
      | Set.size cur > maxGraphs = Nothing
      | otherwise =
          let gs   = Set.toList cur
              more = Set.fromList [ g' | g1 <- gs, g2 <- gs, Just g' <- [composeSct g1 g2] ]
              next = Set.union cur more
          in if Set.size next == Set.size cur then Just cur else closeSet next
    validIdempotent g@(SctGraph f h es)
      | f == h, composeSct g g == Just g = any (\((i, j), r) -> i == j && r == SctStrict) es
      | otherwise                        = True

checkSingleSizeChangeTermination :: CompiledCoreTable -> Bool
checkSingleSizeChangeTermination cct =
    checkSctGraphs [ buildCallSctGraph 0 pats 0 args
                   | CompiledCoreRow pats rhs <- cct_rows cct
                   , args <- collectSelfCalls rhs ]
  where
    collectSelfCalls r =
      (case r of { RhsSelf _ as -> [as]; _ -> [] }) ++
      case r of
        RhsCon _ _ as                    -> concatMap collectSelfCalls as
        RhsCall _ _ as                   -> concatMap collectSelfCalls as
        RhsSelf _ as                     -> concatMap collectSelfCalls as
        RhsApply _ as                    -> concatMap collectSelfCalls as
        RhsGuard (PredicateGuard pr) n y -> collectSelfCalls pr ++ collectSelfCalls n ++ collectSelfCalls y
        RhsGuard _ n y                   -> collectSelfCalls n ++ collectSelfCalls y
        RhsTupleProj _ _ inner           -> collectSelfCalls inner
        RhsVar _                         -> []

checkMutualSizeChangeTermination :: [(TyCon, CompiledCoreTable)] -> Bool
checkMutualSizeChangeTermination members =
    checkSctGraphs [ buildCallSctGraph src_idx pats dst_idx args
                   | (src_idx, (_, cct)) <- zip [0 ..] members
                   , CompiledCoreRow pats rhs <- cct_rows cct
                   , (dst_idx, args) <- collectPeerCalls src_idx rhs ]
  where
    peer_index tc = findIndex (\(ph, _) -> tyConName ph == tyConName tc) members
    collectPeerCalls self_idx r =
      (case r of
         RhsSelf _ as    -> [(self_idx, as)]
         RhsCall tc _ as | Just j <- peer_index tc -> [(j, as)]
         _               -> []) ++
      case r of
        RhsCon _ _ as                    -> concatMap (collectPeerCalls self_idx) as
        RhsCall _ _ as                   -> concatMap (collectPeerCalls self_idx) as
        RhsSelf _ as                     -> concatMap (collectPeerCalls self_idx) as
        RhsApply _ as                    -> concatMap (collectPeerCalls self_idx) as
        RhsGuard (PredicateGuard pr) n y -> collectPeerCalls self_idx pr ++ collectPeerCalls self_idx n ++ collectPeerCalls self_idx y
        RhsGuard _ n y                   -> collectPeerCalls self_idx n ++ collectPeerCalls self_idx y
        RhsTupleProj _ _ inner           -> collectPeerCalls self_idx inner
        RhsVar _                         -> []

checkNominalCoreShape :: Bool -> CompiledCoreTable -> Maybe CompiledCoreTable
checkNominalCoreShape strict = checkNominalCoreShapeWithSct strict True

checkNominalCoreShapeWithSct :: Bool -> Bool -> CompiledCoreTable -> Maybe CompiledCoreTable
checkNominalCoreShapeWithSct strict run_sct cct = do
  let tele = cct_binders cct; domains = cct_domains cct; result = cct_result cct; rows = cct_rows cct; tvs = binderVars tele
      validNominalBinder (Bndr tv (Invisible _)) =
        isTyVar tv && (tcIsLiftedTypeKind (tyVarKind tv)
                       || isJust (ordinaryDataType (tyVarKind tv))
                       || tcIsLiftedTypeKind (typeKind (tyVarKind tv)))
      validNominalBinder _ = False
      allowedNominalType ty
        | polySourceArgType tvs ty = True
        | Just (d, r) <- caseArrow ty = allowedNominalType d && allowedNominalType r
        | otherwise = False
  guard (all validNominalBinder tele)
  case rows of
    [] -> do
      guard (not strict &&
             all (`elem` concatMap tyCoVarsOfTypeList (result : domains)) tvs &&
             any (isJust . emptyCaseDatatype) domains &&
             all (\d -> isJust (emptyCaseDatatype d) || allowedNominalType d) domains &&
             allowedNominalType result)
      pure cct
    [CompiledCoreRow pats rhs] | all isPatVar pats -> do
      let is_prod RhsCon{} = True; is_prod RhsCall{} = True; is_prod _ = False
      guard (all (`elem` concatMap tyCoVarsOfTypeList domains) tvs)
      cct <$ guard (not strict && is_prod rhs && not (rhsHasSelf rhs) &&
                    all allowedNominalType domains && allowedNominalType result)
    CompiledCoreRow pats0 _ : _ -> do
      guard (all (`elem` concatMap tyCoVarsOfTypeList domains) tvs)
      guard (all (\(CompiledCoreRow ps _) -> equalLength ps domains) rows)
      guard (any (\(CompiledCoreRow ps _) -> any isPatCon ps) rows)
      if strict
        then do
          col_idx <- findIndex isPatCon pats0
          guard ((col_idx == 0 || (col_idx == 1 && lengthIs domains 2)) &&
                 all (\(CompiledCoreRow ps _) -> isPatCon (ps !! col_idx) &&
                                                 all (\(i, p) -> i == col_idx || isPatVar p) (zip [0 ..] ps)) rows)
          let has_nested = any (\(CompiledCoreRow ps _) -> case ps !! col_idx of { PatCon _ _ cs -> any isPatCon cs; _ -> False }) rows
              arg_ty     = domains !! col_idx
              carried    = [d | (i, d) <- zip [0 ..] domains, i /= col_idx]
          guard (null carried && not has_nested)
          case tvs of
            [] -> () <$ (ordinaryDataType arg_ty >> ordinaryDataType result >>= \(_, as) -> guard (null as))
            _  -> do
              (tc, args) <- caseDatatype arg_ty
              params <- traverse getTyVar_maybe args
              guard (tc == listTyCon && equalLength params tvs && mkUniqSet params == mkUniqSet tvs && result `eqType` arg_ty)
          when run_sct (guard (checkSingleSizeChangeTermination cct))
          pure cct
        else do
          let colHasCon i = any (\(CompiledCoreRow ps _) -> isPatCon (ps !! i)) rows
          forM_ (zip [0 ..] domains) $ \(i, dom) ->
            if colHasCon i
              then guard (polySourceType tvs dom || isJust (emptyCaseDatatype dom))
              else guard (allowedNominalType dom)
          guard (allowedNominalType result)
          when run_sct (guard (checkSingleSizeChangeTermination cct))
          pure cct

-- | Identify single-column nominal equations that stay ahead of final-Core
-- case certification when publishing module exports.
isSimpleNominalTable :: Id -> TyCon -> Bool
isSimpleNominalTable owner table
  | finalCoreMapperOwner owner = False
  | Just cs <- sourceTableCallees table
  , any isSourceFilterGuard cs = False
  | Just ax <- closedFamilyTyConCoAxiom_maybe table
  , let brs = fromBranches (coAxiomBranches ax)
        (invis, vis) = span isInvisibleTyConBinder (tyConBinders table)
        n_invis = length invis
        n_vis   = length vis
  , not (null brs), n_vis > 0
  , all (noAppTy . coAxBranchRHS) brs
  = case brs of
      [br] | let pats = drop n_invis (coAxBranchLHS br)
           , all isTyVarTy pats ->
        let rhs = coAxBranchRHS br
        in not (isTyVarTy rhs) && not (table `elementOfUniqSet` tyConsOfTypes [rhs])
      br0 : _ -> isJust $ do
        let pats0 = drop n_invis (coAxBranchLHS br0)
        col_idx <- findIndex isPromotedConPat pats0
        guard (col_idx == 0 || (col_idx == 1 && n_vis == 2))
        let has_self = any (\b -> table `elementOfUniqSet` tyConsOfTypes [coAxBranchRHS b]) brs
        forM_ brs $ \br -> do
          let pats = drop n_invis (coAxBranchLHS br)
          guard (length pats == n_vis)
          guard (all (\(i, p) -> if i == col_idx then isPromotedConPat p else isTyVarTy p) (zip [0 ..] pats))
          when has_self $ do
            field_tvs <- directConFields (pats !! col_idx)
            guard (validSelfCalls n_invis col_idx field_tvs (coAxBranchRHS br))
  | otherwise = False
  where
    isPromotedConPat ty
      | Just (tc, _) <- splitTyConApp_maybe ty = isPromotedDataCon tc
      | otherwise = False
    directConFields ty
      | Just (tc, args) <- splitTyConApp_maybe ty
      , isPromotedDataCon tc
      , let n_fields = length (filter isVisibleTyConBinder (tyConBinders tc))
            fields   = drop (length args - n_fields) args
      = traverse getTyVar_maybe fields
      | otherwise = Nothing
    noAppTy ty = case coreFullView ty of
      AppTy{}         -> False
      TyConApp _ as   -> all noAppTy as
      FunTy _ m w a r -> all noAppTy [m, w, a, r]
      ForAllTy m b r  -> noAppTy m && noAppTy (binderType b) && noAppTy r
      CastTy t _      -> noAppTy t
      _               -> True
    validSelfCalls n_invis col_idx field_tvs ty = case coreFullView ty of
      TyConApp tc as
        | tc == table ->
            (case drop (n_invis + col_idx) as of
               arg : _ | Just tv <- getTyVar_maybe arg -> tv `elem` field_tvs
               _ -> False) &&
            all (validSelfCalls n_invis col_idx field_tvs) as
        | otherwise -> all (validSelfCalls n_invis col_idx field_tvs) as
      AppTy f a       -> validSelfCalls n_invis col_idx field_tvs f && validSelfCalls n_invis col_idx field_tvs a
      FunTy _ m w a r -> all (validSelfCalls n_invis col_idx field_tvs) [m, w, a, r]
      ForAllTy m b r  -> all (validSelfCalls n_invis col_idx field_tvs) [m, binderType b, r]
      CastTy t _      -> validSelfCalls n_invis col_idx field_tvs t
      _               -> True

-- | Certify ground unary and binary instance method workers (including stock
-- @deriving Eq@ tag-based @==@ workers) from desugared local instance bindings.
certifyLocalInstanceMethods
  :: TcGblEnv -> [(Id, Core.CoreExpr)]
  -> TcM ([LHsBind GhcTc], [(Id, TyCon)], [(Id, [(Id, Id)])])
certifyLocalInstanceMethods env0 pairs
  | null dfun_candidates = pure ([], [], [])
  | otherwise = do
      strict <- xoptM LangExt.Strict
      dflags <- getDynFlags
      mb_and_tc <- do
        mb_and <- lookupBootModuleIdMaybe gHC_CLASSES (mkVarOcc "&&")
        pure (mb_and >>= sourceTableId_maybe)
      let prepare = safeSimpleOptExpr (initSimpleOpts dflags)
          local_rules = sourceRuleOwners (tcg_rules env0)
          owners = mkNameEnv [(idName v, (v, rhs)) | (v, rhs) <- pairs]
          step env eq_tables acc_binds acc_tables acc_dfuns [] =
            pure (reverse acc_binds, reverse acc_tables, reverse acc_dfuns)
          step env eq_tables acc_binds acc_tables acc_dfuns pending = do
            results <- forM pending $ \cand@(dfun, cls, inst_ty, slots) ->
              if idName dfun `elemNameSet` local_rules
                then pure (Left cand)
                else do
                  slot_res <- fmap catMaybes $ forM slots $ \(sel, field) ->
                    case stripTicksTopE isSourceBreakpoint field of
                      Core.Var worker
                        | not (idName sel `elemNameSet` local_rules)
                        , Just (meth_id, raw_rhs) <- lookupNameEnv owners (idName worker)
                        , varType meth_id `eqType` classMethodInstTy sel [inst_ty] -> do
                            rewritten <- rewriteCertifiedDictCalls env (prepare raw_rhs)
                            let clean_rhs = prepare rewritten
                                spec = (defaultCoreSpec (const True))
                                  { ccs_allow_call_self = True, ccs_check_kind_public = False }
                                mb_struct =
                                  (guard (isJust (sourceBinarySelector sel)) *>
                                     compileStructuralCore (const True) meth_id clean_rhs) <|>
                                  (guard (not strict) *>
                                     (compileCoreEquations spec meth_id clean_rhs >>= checkNominalCoreShape strict))
                            mb_cct <- case mb_struct of
                              Just cct -> pure (Just cct)
                              Nothing  -> compileDerivedEqCoreM mb_and_tc eq_tables cls sel inst_ty dfun clean_rhs
                            case mb_cct of
                              Nothing -> pure Nothing
                              Just cct -> do
                                (target_id, mb_bind) <- if isExternalName (idName meth_id)
                                  then pure (meth_id, Nothing)
                                  else do
                                    ext_name <- newImplicitBinder (idName dfun) $ \df_occ ->
                                      mkVarOcc (occNameString df_occ ++ "_" ++ occNameString (nameOccName (idName meth_id)))
                                    let synth = setIdExported (mkLocalId ext_name manyDataConTy (varType meth_id))
                                        wrap  = mkWpEvApps [EvExpr (Core.Var dfun)] <.> mkWpTyApps [inst_ty]
                                        rhs_tc = mkLHsWrap wrap (noLocA (HsVar noExtField (noLocA sel)))
                                    pure (synth, Just (mkVarBind synth rhs_tc))
                                table <- publishCompiledTable target_id cct
                                let certified_id = setIdDetails target_id (SourceTableId (idName target_id) table NominalSourceTable)
                                pure (Just (sel, certified_id, table, mb_bind))
                      _ -> pure Nothing
                  if null slot_res
                    then pure (Left cand)
                    else pure (Right (dfun, cls, inst_ty, slot_res))
            let progressed = [ r | Right r <- results ]
                remaining  = [ c | Left c <- results ]
            if null progressed
              then pure (reverse acc_binds, reverse acc_tables, reverse acc_dfuns)
              else do
                let new_binds  = [ b | (_, _, _, ss) <- progressed, (_, _, _, Just b) <- ss ]
                    new_tables = [ (cid, tbl) | (_, _, _, ss) <- progressed, (_, cid, tbl, _) <- ss ]
                    new_dfuns  = [ (dfun, [(sel, cid) | (sel, cid, _, _) <- ss]) | (dfun, _, _, ss) <- progressed ]
                    new_eqs    = [ (inst_ty, tbl)
                                 | (_, cls, inst_ty, ss) <- progressed, cls `hasKey` eqClassKey
                                 , (sel, _, tbl, _) <- ss, nameOccName (idName sel) == eqClassOpOcc ]
                    upd_dfun_ids = [ let cur = case lookupTypeEnv (tcg_type_env env) (idName dfun) of
                                           Just (AnId d) -> d
                                           _             -> dfun
                                     in addSourceMethods cur m_pairs
                                   | (dfun, m_pairs) <- new_dfuns ]
                    env' = env { tcg_type_env = extendTypeEnvWithIds (tcg_type_env env)
                                                  (map fst new_tables ++ upd_dfun_ids) }
                step env' (new_eqs ++ eq_tables)
                     (reverse new_binds ++ acc_binds)
                     (reverse new_tables ++ acc_tables)
                     (reverse new_dfuns ++ acc_dfuns)
                     remaining
      step env0 [] [] [] [] dfun_candidates
  where
    dfun_candidates =
      [ (dfun, cls, inst_ty, [s | s@(sel, _) <- slots, sel `elem` classMethods cls])
      | (dfun, rhs) <- pairs
      , DFunId{} <- [idDetails dfun]
      , let (bndrs, body) = Core.collectBinders rhs
      , null bndrs
      , exprType rhs `eqType` varType dfun
      , Just (cls, [inst_ty]) <- [getClassPredTys_maybe (varType dfun)]
      , noFreeVarsOfType inst_ty, isTyFamFree inst_ty
      , Just slots <- [dictionarySlotsMulti cls [inst_ty] body] ]

    compileDerivedEqCoreM mb_and_tc eq_tables cls sel inst_ty dfun rhs
      | cls `hasKey` eqClassKey
      , nameOccName (idName sel) == eqClassOpOcc
      , Just (tc, tc_args) <- splitTyConApp_maybe inst_ty
      , isBoxedDataTyCon tc, not (isTypeDataTyCon tc)
      , let dcs = tyConDataCons tc
      , not (null dcs)
      , all (\dc -> dataConUserTyVars dc == dataConUnivTyVars dc && isJust (openDataConFieldTypes dc tc_args)) dcs
      , let all_ids = exprSomeFreeVarsList isId rhs
            has_tag_op = any (\v -> maybe False (\op -> op == DataToTagSmallOp || op == DataToTagLargeOp) (isPrimOpId_maybe v)) all_ids
            ext_ids = [ v | v <- all_ids, isExternalName (idName v), isNothing (isDataConWorkId_maybe v) ]
            ok_ext v = v == dfun || v == sel || v `hasKey` patErrorIdKey
                    || maybe False (\m -> moduleUnitId m == ghcInternalUnitId) (nameModule_maybe (idName v))
                    || isCertifiedFieldEqDFun v
      , lengthIs dcs 1 || has_tag_op
      , all ok_ext ext_ids = do
          mb_rows <- sequence [ build_eq_row tc_args dc1 dc2 | dc1 <- dcs, dc2 <- dcs ]
          pure (CompiledCoreTable [] [inst_ty, inst_ty] boolTy <$> sequence mb_rows <*> pure False)
      | otherwise = pure Nothing
      where
        isCertifiedFieldEqDFun v
          | DFunId{} <- idDetails v
          , Just (c, [fty]) <- getClassPredTys_maybe (varType v)
          , c `hasKey` eqClassKey = any (eqType fty . fst) eq_tables
          | otherwise = False

        lookupEqTable fty = snd <$> find (eqType fty . fst) eq_tables

        build_eq_row tc_args dc1 dc2
          | Just ftys1 <- openDataConFieldTypes dc1 tc_args
          , Just ftys2 <- openDataConFieldTypes dc2 tc_args = do
              xs <- mapM (mkSysLocalM (fsLit "eq_x") manyDataConTy) ftys1
              ys <- mapM (mkSysLocalM (fsLit "eq_y") manyDataConTy) ftys2
              let lhs = [PatCon dc1 tc_args (map PatVar xs), PatCon dc2 tc_args (map PatVar ys)]
              if dc1 /= dc2
                then pure (Just (CompiledCoreRow lhs (RhsCon falseDataCon [] [])))
                else case zip3 ftys1 xs ys of
                  [] -> pure (Just (CompiledCoreRow lhs (RhsCon trueDataCon [] [])))
                  [(fty, x, y)] -> pure (CompiledCoreRow lhs <$> field_cmp fty x y)
                  triples
                    | Just and_tc <- mb_and_tc ->
                        pure $ do
                          c0 : cs <- traverse (\(fty, x, y) -> field_cmp fty x y) triples
                          pure (CompiledCoreRow lhs (foldr (\a b -> RhsCall and_tc [] [a, b]) c0 cs))
                    | otherwise -> pure Nothing
          | otherwise = pure Nothing

        field_cmp fty x y
          | fty `eqType` inst_ty = Just (RhsSelf [] [RhsVar x, RhsVar y])
          | Just eq_tc <- lookupEqTable fty = Just (RhsCall eq_tc [] [RhsVar x, RhsVar y])
          | otherwise = Nothing

compileBooleanGuardCase :: (TyCon -> Bool) -> (Id -> Maybe TyCon) -> Id -> Core.CoreExpr -> TcM (Maybe ((Id, TyCon), TyCon))
compileBooleanGuardCase public predicate owner body
  | Just cct <- compileCoreEquations ((defaultCoreSpec public) { ccs_predicate = predicate, ccs_allow_guard = True }) owner body
  , any (\(CompiledCoreRow _ r) -> rhsHasPredicateGuard r) (cct_rows cct) = do
      mb_pub <- publishCompiledWithGuard owner cct
      pure $ case mb_pub of { Just (table, [sel]) -> Just ((owner, table), sel); _ -> Nothing }
  | otherwise = pure Nothing

sourceCaseTables :: NameSet -> LHsBinds GhcTc -> TcM ([(Id, TyCon)], [TyCon])
sourceCaseTables nominal_owners binds = do
  env <- getGblEnv
  char_tables <- xoptM LangExt.DependentTypes
  dflags <- getDynFlags
  rdr_env <- getGlobalRdrEnv
  let generates_code = backendGeneratesCode (backend dflags)
      produces_iface = gopt Opt_WriteInterface dflags || not (isOneShot (ghcMode dflags))
  no_code_structural <- if generates_code || gopt Opt_WriteIfSimplifiedCore dflags || not (null (tcg_rules env))
    then pure False else not . hasActiveLatePlugins <$> getTopEnv
  let checks_final_core = generates_code || (no_code_structural && produces_iface &&
                                             gopt Opt_WriteSourceEquations dflags && not (gopt Opt_Hpc dflags))
      can_struct = generates_code || no_code_structural
      exported = availsToNameSet (tcg_exports env); ruled = sourceRuleOwners (tcg_rules env)
      imported = ordinaryImport rdr_env
      public = publicCaseType env exported imported
      eligible_case owner
        | Just kinds <- monomorphicEnumCaseKinds (varType owner) =
            all (maybe False (public . fst) . caseDatatype) kinds
        | Just (con, _) <- monomorphicRetainedWitnessType (varType owner) = public (dataConTyCon con)
        | Just sig@(ConstructorCaseType _ tc _ domains pos _) <- constructorCaseType (varType owner)
        , not (isBinaryListCase sig || isTwoStructuralCase sig || isSizeListCase sig) =
            (pos == 1 && (tc == listTyCon || public tc)) || (isPermutingConstructorCase sig && public tc) ||
            (nameModule_maybe (tyConName tc) == Just (tcg_semantic_mod env) && publicCaseType env exported (const False) tc) ||
            (maybe False (\(_, dom, _) -> isRetainedCaseRoot dom tc) (splitCaseOperand pos domains) && public tc)
        | otherwise = False
      list_case_position o
        | checks_final_core, let ty = varType o
        , Just (ConstructorCaseType _ tc _ _ pos _) <- constructorCaseType ty <|> monomorphicGuardedListCaseType ty
        , tc == listTyCon = Just pos
        | otherwise = Nothing
      indexed_cand o = checks_final_core && maybe False (\(IndexedProjection _ _ _ c _ _ _) -> public (dataConTyCon c)) (indexedProjectionType (varType o))
      struct_cand o = can_struct && isNothing (sourceTableOccurrence env o >>= sourceTableId_maybe) &&
                      maybe False (\s -> isTwoStructuralCase s || isSizeListCase s) (constructorCaseType (varType o))
      partial_cand o = can_struct && not (idName o `elemNameSet` nominal_owners) &&
                       isNothing (sourceTableOccurrence env o >>= sourceTableId_maybe) && isJust (partialConstructorCaseType (varType o))
      final_cand o = isJust (list_case_position o) || indexed_cand o || struct_cand o || partial_cand o
      bin_list_cand o = not (idName o `elemNameSet` nominal_owners) && list_case_position o == Just 0
      fold_cand o = can_struct && isJust (listFoldType (varType o))
      zip_cand o = checks_final_core && isJust (listZipWithType (varType o))
      rev_wrap_cand o = checks_final_core && isJust (listReverseWrapperType (varType o))
      id_cand o = checks_final_core && isIdentitySourceType (varType o)
      comp_cand o = checks_final_core && isJust (compositionSourceType (varType o))
      is_exported o = idName o `elemNameSet` exported && not (idName o `elemNameSet` nominal_owners)
      eligible o = is_exported o && (final_cand o || not (idName o `elemNameSet` ruled)) &&
                   ((char_tables && isJust (charCaseResultType (varType o))) || isJust (pairCaseType (varType o)) ||
                    isJust (pairBuilderType (varType o)) || eligible_case o)
      owners = [(b, o) | b <- binds, o <- collectHsBindBinders CollNoDictBinders (unLoc b)]
      candidates = [(b, o) | (b, o) <- owners, eligible o ||
                    (is_exported o && (id_cand o || comp_cand o || final_cand o || rev_wrap_cand o || fold_cand o || zip_cand o))]
  if isHsBootOrSig (tcg_src env) || null candidates then pure ([], []) else do
    visible <- visibleSourceMethodRules env
    unloaded <- unloadedSourceRuleHeads env
    let changed o = idName o `elemNameSet` ruled || idName o `elemNameSet` unloaded || hasSourceMethodRule visible o
        prepare = safeSimpleOptExpr (initSimpleOpts dflags)
        finalOwner o t = setIdDetails o (SourceTableId (idName o) t FinalCoreSourceTable)
    all_bind_pairs <- fmap catMaybes $ forM owners $ \(b, o) -> fmap (o,) <$> bindingCore b o
    prepared <- fmap catMaybes $ forM candidates $ \(bind, owner) -> do
      let ruled_owner = changed owner
      if ruled_owner && not (final_cand owner || id_cand owner || fold_cand owner || zip_cand owner || rev_wrap_cand owner)
        then pure Nothing
        else do
          mb_body <- maybe (bindingCore bind owner) (pure . Just) (lookup owner all_bind_pairs)
          pure $ flip fmap mb_body $ \body ->
            let guarded_cct
                  | isJust (list_case_position owner), let ty = varType owner
                  , Just (ConstructorCaseType _ tc [el] [pred_ty, inp] 1 res) <- constructorCaseType ty <|> monomorphicGuardedListCaseType ty
                  , tc == listTyCon, pred_ty `eqType` mkVisFunTyManyU el boolTy
                  , res `eqType` inp || res `eqType` mkTupleTy Boxed [inp, inp]
                  , let g_spec = (defaultCoreSpec public)
                          { ccs_bindings = all_bind_pairs, ccs_allow_guard = True, ccs_allow_tail_self = True, ccs_check_kind_public = False }
                  , Just cct <- compileCoreEquations g_spec owner (prepare body)
                  , any (\(CompiledCoreRow _ r) -> rhsHasCallbackGuard r) (cct_rows cct) = Just cct
                  | otherwise = Nothing
            in (owner, body, ruled_owner, guarded_cct)

    let root_cores = [(o, body) | (o, body, False, Nothing) <- prepared, eligible o]
        tryFinal o raw_b opt_b ruled_o =
          (guard ruled_o *> compileCoreEquations ((defaultCoreSpec (const False)) { ccs_check_kind_public = False }) o raw_b >>=
             \c -> c <$ guard (isValidMapperTable c)) <|>
          (do guard (bin_list_cand o)
              let s_app = (defaultCoreSpec (const False)) { ccs_check_kind_public = False }
                  s_rev = s_app { ccs_allow_tail_self = True, ccs_allow_accum_self = True }
              (compileCoreEquations s_app o raw_b >>= \c -> c <$ guard (isValidAppendTable c)) <|>
                (compileCoreEquations s_rev o opt_b >>= \c -> c <$ guard (isValidReverseWorkerTable c)) <|>
                (do sig@(ConstructorCaseType _ _ _ ds _ _) <- constructorCaseType (varType o); guard (isTwoListCase sig)
                    let el_ks = [el | d <- ds, Just (_, [el]) <- [caseDatatype d]]
                    compileCoreEquations ((defaultCoreSpec public) { ccs_fixed_kinds = el_ks }) o opt_b <|>
                      compileZipCore public o opt_b)) <|>
          (guard (struct_cand o) *> compileStructuralCore public o opt_b) <|>
          (do guard (partial_cand o)
              ConstructorCaseType _ root _ _ _ _ <- partialConstructorCaseType (varType o)
              cct <- compileCoreEquations ((defaultCoreSpec public) { ccs_partial_root = True }) o opt_b
              cct <$ guard (length (cct_rows cct) < length (tyConDataCons root))) <|>
          (guard (fold_cand o) *>
             compileCoreEquations ((defaultCoreSpec (const True)) { ccs_allow_top_apply = True, ccs_check_kind_public = False }) o opt_b >>=
             \c -> c <$ guard (isValidFoldTable c)) <|>
          (guard (zip_cand o) *> compileZipCore public o opt_b) <|>
          (do guard (comp_cand o)
              cct <- compileCoreEquations ((defaultCoreSpec (const True))
                       { ccs_allow_zero_cases = True, ccs_allow_top_apply = True, ccs_check_kind_public = False }) o raw_b
              case cct_rows cct of
                [CompiledCoreRow [PatVar f, PatVar g, PatVar x] (RhsApply f' [RhsApply g' [RhsVar x']])]
                  | f == f', g == g', x == x' -> pure cct
                _ -> Nothing)

    final <- forM prepared $ \(owner, body, ruled_owner, mb_guarded) -> case mb_guarded of
      Just cct -> publishCompiledWithGuard owner cct >>= \case
        Just (table, gs) -> pure (Just (finalOwner owner table, table), gs)
        Nothing          -> pure (Nothing, [])
      Nothing
        | Just cct <- tryFinal owner body (prepare body) ruled_owner -> do
            table <- publishCompiledTable owner cct
            pure (Just (finalOwner owner table, table), [])
        | indexed_cand owner, Just (proj, rec_r) <- checkedIndexedProjection prepare owner body -> do
            checked <- publishIndexedProjection owner proj rec_r
            pure (fmap (\t -> (finalOwner owner t, t)) checked, [])
        | otherwise -> pure (Nothing, [])

    let final_entries = catMaybes (map fst final)
        checked_preds = mkNameEnv [(idName v, v) | (v, _) <- final_entries, isFinalStructuralSource v]
        predicate c
          | nameModule_maybe (idName c) == Just (tcg_semantic_mod env)
          , not (idName c `elemNameSet` exported) = Nothing
          | otherwise = checkedBranchPredicate (fromMaybe c (lookupNameEnv checked_preds (idName c)))
    boolean_cases <- fmap catMaybes $ forM [(o, prepare b) | (o, b, _, Nothing) <- prepared, partial_cand o] $
      \(o, b) -> fmap (\((o', t), sel) -> ((finalOwner o' t, t), sel)) <$> compileBooleanGuardCase public predicate o b

    let reverse_workers = mkNameEnv [(idName w, t) | (Just (w, t), _) <- final, isReverseWorkerTable t]
    reverse_wrappers <- forM
      [ (o, bs, op, el, w_tc) | (o, body, _, _) <- prepared, rev_wrap_cand o
      , Just (bs, op, el, callee) <- [directListReverseWrapper o (prepare body)]
      , Just w_tc <- [lookupNameEnv reverse_workers (idName callee)] ] $ \(o, bs, op, el, w_tc) -> do
        let dom = varType op
            row = CompiledCoreRow [PatVar op] (RhsCall w_tc [el] [RhsVar op, RhsCon nilDataCon [el] []])
        table <- publishCompiledTable o (CompiledCoreTable bs [dom] dom [row] False)
        pure (finalOwner o table, table)

    let references = map idName . exprSomeFreeVarsList (\v -> isId v && nameModule_maybe (idName v) == Just (tcg_semantic_mod env))
        local_owners = mkNameEnv [(idName o, (b, o)) | (b, o) <- owners]
        helperCore name
          | Just (b, o) <- lookupNameEnv local_owners name, isExternalName name, not (eligible o)
          , not (changed o), isJust (constructorHelperType (varType o)) = fmap (o,) <$> bindingCore b o
          | otherwise = pure Nothing
        collectHelpers _ [] = pure []
        collectHelpers seen (nm : rest)
          | nm `elemNameSet` seen = collectHelpers seen rest
          | otherwise = helperCore nm >>= \case
              Just (o, rhs) -> let direct = references rhs in (DigraphNode (o, rhs) nm direct :) <$> collectHelpers (extendNameSet seen nm) (direct ++ rest)
              Nothing -> collectHelpers (extendNameSet seen nm) rest
        lookupHelper helpers actual
          | nameModule_maybe (idName actual) == Just (tcg_semantic_mod env) = lookup actual helpers
          | imported (idName actual) = sourceTableId_maybe actual
          | otherwise = Nothing
        buildHelper done (AcyclicSCC (o, rhs))
          | Just cct <- compileCoreEquations ((defaultCoreSpec public) { ccs_helper = lookupHelper done, ccs_allow_zero_cases = True }) o rhs
          , [CompiledCoreRow pats _] <- cct_rows cct, all isPatVar pats = (\t -> (o, t) : done) <$> publishCompiledTable o cct
          | Just sig <- constructorCaseType (varType o)
          , not (isBinaryListCase sig || isTwoStructuralCase sig || isSizeListCase sig)
          , Just cct <- compileOrdinaryCase public (lookupHelper done) sig o rhs = (\t -> (o, t) : done) <$> publishCompiledTable o cct
        buildHelper done _ = pure done

    helper_cores <- collectHelpers emptyNameSet (concatMap (references . snd) (filter (eligible_case . fst) root_cores))
    helpers <- foldM buildHelper [] (stronglyConnCompFromEdgedVerticesUniq helper_cores)
    let isEnumBinRow (CompiledCoreRow [PatCon{}, PatVar{}] r) = case r of { RhsVar{} -> True; RhsCon _ _ [] -> True; _ -> False }
        isEnumBinRow _ = False
        certifyRoot (o, rhs)
          | Just build <- checkedPairBuilder o rhs = build
          | otherwise = (, []) <$> case rhs of
              Core.Var target | Just res <- charCaseResultType (varType o) -> forwardSourceCharacter env res o target
              _ | isJust (pairCaseType (varType o)), Just cct <- compilePairCase o rhs ->
                    Just . (o,) <$> publishCompiledTable o cct
                | let enum_arity = monomorphicEnumCaseArity (varType o)
                , eligible_case o
                , Just (con_mb, sig) <- ((\(c, s) -> (Just c, s)) <$> directRetainedWitnessType (varType o)) <|>
                                        ((Nothing,) <$> constructorCaseType (varType o))
                , Just cct <- compileOrdinaryCaseWithWitness con_mb public (lookupHelper helpers) sig o rhs
                , enum_arity /= Just 2 || all isEnumBinRow (cct_rows cct) -> do
                    table <- publishCompiledTable o cct
                    pure $ Just (if isJust enum_arity then setIdDetails o (SourceTableId (idName o) table ExecutableSourceTable) else o, table)
                | otherwise -> publishCertifiedCharacter o rhs

    (root_entries, fields) <- unzip <$> traverse certifyRoot root_cores
    roots <- publishCharacterAliases env root_cores (catMaybes root_entries)
    let helper_tables = mkNameEnv [(tyConName t, t) | (_, t) <- helpers]
        closeH seen [] = seen
        closeH seen (nm : rest)
          | nm `elemNameSet` seen = closeH seen rest
          | Just t <- lookupNameEnv helper_tables nm = closeH (extendNameSet seen nm) (maybe [] (map tyConName) (sourceTableCallees t) ++ rest)
          | otherwise = closeH seen rest
        used = closeH emptyNameSet (concatMap (maybe [] (map tyConName) . sourceTableCallees . snd) roots)
        used_helpers = filter (\(_, t) -> tyConName t `elemNameSet` used) helpers
        helper_owners = mkNameSet (map (idName . fst) used_helpers)
    identities <- fmap catMaybes $ forM
      [ (o, cct) | (o, body, _, _) <- prepared, id_cand o, not (idName o `elemNameSet` helper_owners)
      , Just cct <- [compileCoreEquations ((defaultCoreSpec (const False)) { ccs_allow_zero_cases = True }) o body]
      , [CompiledCoreRow [PatVar x] (RhsVar y)] <- [cct_rows cct], x == y
      ] $ \(o, cct) -> Just . (\t -> (finalOwner o t, t)) <$> publishCompiledTable o cct

    let (boolean_tables, boolean_selectors) = unzip boolean_cases
        boolean_owners = mkNameSet (map (idName . fst) boolean_tables)
        other_final = filter (\(v, _) -> not (idName v `elemNameSet` boolean_owners)) final_entries
    pure (other_final ++ boolean_tables ++ reverse_wrappers ++ identities ++ roots ++ used_helpers,
          concatMap snd final ++ boolean_selectors ++ concat fields)

compilePairCase :: Id -> Core.CoreExpr -> Maybe CompiledCoreTable
compilePairCase owner rhs = do
    _ <- pairCaseType (varType owner)
    cct <- compileCoreEquations (defaultCoreSpec (const False)) owner rhs
    case cct_rows cct of
      [CompiledCoreRow [PatCon dc _ [PatVar a, PatVar b]] r]
        | dc == tupleDataCon Boxed 2
        , let ok (RhsVar v) = v == a || v == b; ok _ = False
        , ok r || case r of { RhsCon dc' _ [f1, f2] -> dc' == dc && ok f1 && ok f2; _ -> False } -> pure cct
      _ -> Nothing

compileOrdinaryCase :: (TyCon -> Bool) -> (Id -> Maybe TyCon) -> ConstructorCaseType -> Id -> Core.CoreExpr
                    -> Maybe CompiledCoreTable
compileOrdinaryCase = compileOrdinaryCaseWithWitness Nothing

compileOrdinaryCaseWithWitness :: Maybe DataCon -> (TyCon -> Bool) -> (Id -> Maybe TyCon)
                               -> ConstructorCaseType -> Id -> Core.CoreExpr -> Maybe CompiledCoreTable
compileOrdinaryCaseWithWitness mb_con public helper sig@(ConstructorCaseType _ _ _ _ pos _) owner rhs = do
    let spec = (defaultCoreSpec public) { ccs_helper = helper, ccs_fixed_kinds = fixedMapTypes sig, ccs_witness_root = mb_con }
    cct <- compileCoreEquations spec owner rhs
    let multi = isTwoListCase sig || isTwoStructuralCase sig || isSizeListCase sig
    cct <$ guard (all (\(CompiledCoreRow ps _) -> pos < length ps && isPatCon (ps !! pos) &&
                        (multi || all (\(i, p) -> i == pos || isPatVar p) (zip [0 ..] ps))) (cct_rows cct))

sourceMapperTable :: Id -> Core.CoreExpr -> TcM (Maybe (Id, TyCon))
sourceMapperTable owner rhs = do
    env <- getGblEnv
    let public = visibleCaseType (\n -> nameModule_maybe n == Just (tcg_semantic_mod env) || ordinaryImport (tcg_rdr_env env) n)
        mb_cct = (constructorCaseType (varType owner) >>= \sig -> compileOrdinaryCase public (const Nothing) sig owner rhs) <|>
                 (guard (isPairPermutationType (varType owner)) *> compilePairCase owner rhs)
    maybe (pure Nothing) (\cct -> Just . (owner,) <$> publishCompiledTable owner cct) mb_cct

safeSimpleOptExpr :: SimpleOpts -> Core.CoreExpr -> Core.CoreExpr
safeSimpleOptExpr opts expr
  | hasExistentialExpr expr || hasNativeDependentExpr expr = expr
  | otherwise                                              = simpleOptExpr opts expr

sourceLocalBooleanCases :: NameEnv (Id, Core.CoreExpr) -> NameEnv (Id, Core.CoreExpr) -> TcM ([(Id, TyCon)], [TyCon])
sourceLocalBooleanCases roots definitions = do
    env <- getGblEnv; rdr_env <- getGlobalRdrEnv; dflags <- getDynFlags
    let public = visibleCaseType (\n -> if nameModule_maybe n == Just (tcg_semantic_mod env)
                                          then maybe False isLocalGRE (lookupGRE_Name rdr_env n)
                                          else ordinaryImport rdr_env n)
        prepare = safeSimpleOptExpr (initSimpleOpts dflags)
        fresh o = isNothing (sourceTableOccurrence env o >>= sourceTableId_maybe)
        ordered = sortBy (\a b -> stableNameCmp (idName (fst a)) (idName (fst b))) . nonDetNameEnvElts
    predicates <- forM
      [ (o, cct) | (o, body) <- ordered definitions, fresh o
      , Just sig@(ConstructorCaseType _ _ _ _ _ res) <- [constructorCaseType (varType o)]
      , isTwoStructuralCase sig, res `eqType` boolTy
      , Just cct <- [compileStructuralCore public o (prepare body)]
      ] $ \(o, cct) -> (o,) <$> publishCompiledTable o cct
    let local_map = mkNameEnv [(idName o, t) | (o, t) <- predicates]
        predicate c = lookupNameEnv local_map (idName c) <|> (sourceTableOccurrence env c >>= checkedBranchPredicate)
    pairs <- fmap catMaybes $ forM [(o, prepare b) | (o, b) <- ordered roots, fresh o, isJust (partialConstructorCaseType (varType o))] $
      uncurry (compileBooleanGuardCase public predicate)
    let (cases, selectors) = unzip pairs
        used = mkNameSet [tyConName c | (_, t) <- cases, c <- fromMaybe [] (sourceTableCallees t)]
    pure (filter ((`elemNameSet` used) . tyConName . snd) predicates ++ cases, selectors)

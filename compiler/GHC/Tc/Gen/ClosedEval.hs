{-# LANGUAGE TypeFamilies #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | Compile-time evaluation of closed term applications in types.
module GHC.Tc.Gen.ClosedEval
  ( tcRunClosedCall
  , tcPrepareClosedApplication
  , tcPrepareClosedFunctionValue
  , tcPrepareClosedResult
  , tcPrepareClosedObservation
  , tcCheckClosedCallContext
  , tcPreflightClosedCall
  , tcDecodeClosedCallOperand
  , tcHasClosedCallExecutable
  , tcHasClosedValueShape
  , rewriteSameModuleClosedExpr
  , desugarStagedExpr
  , tcTopSpliceExprWith
  ) where

import GHC.Prelude

import GHC.Driver.Config.Diagnostic (initDiagOpts, initDsMessageOpts)
import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Driver.Errors (printMessages)
import GHC.Driver.Main (hscCompileCoreExpr)
import GHC.Driver.Main.Passes (hscCompileCoreExprWithOptionalName, hscLoadSameModuleClosedBcos)
import GHC.Driver.Ppr (showPpr, showSDocForUser)
import GHC.Driver.Session (packageTrustOn, safeInferOn, safeLanguageOn)

import GHC.Hs

import GHC.Tc.Errors.Types
import GHC.Tc.Gen.Expr (tcCheckMonoExpr)
import GHC.Tc.Gen.OpenTypeEval (readOpenDefinition, readOpenMethodWorker)
import GHC.Tc.Gen.SourceTable (captureSourceMethodProof, closedSourceProofArgs)
import GHC.Tc.Solver (simplifyTopNoDefaulting)
import GHC.Tc.Types.Constraint (WantedConstraints)
import GHC.Tc.Types.Evidence (EvBind, TcEvBinds(..))
import GHC.Tc.Utils.Env (tcGetInstEnvs, tcLookupClass, tcLookupId, tcLookupIdMaybe, tcLookupTyCon)
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.Type (zonkTopLExpr)

import qualified GHC.Core as Core
import GHC.Core.Class (classMethods, className)
import GHC.Core.Coercion
  ( coercionLKind, coercionRKind, coercionRole, liftCoSubstWith
  , mkCoVarCo, mkSelCo, mkSubCo, mkSymCo, mkUnbranchedAxInstCo )
import GHC.Core.Coercion.Axiom (Role(Nominal, Representational))
import GHC.Core.DataCon as DataCon
import GHC.Core.FVs (exprSomeFreeVarsList)
import GHC.Core.InstEnv (instanceDFunId, lookupUniqueInstEnv)
import GHC.Core.Make
  ( mkCharExpr, mkConsExpr, mkCoreApp, mkCoreApps, mkCoreConWrapApps
  , mkListExpr, mkNaturalExpr )
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Rep as TyCoRep
import GHC.Core.TyCon
import GHC.Core.Type as Type
import GHC.Core.Utils (dataConRepInstPat, exprType, mkCast, mkLamType)

import GHC.Builtin.KnownOccs (eitherTyConOcc, returnIOIdOcc)
import GHC.Builtin.Modules (gHC_INTERNAL_BASE, gHC_TYPES, mkGhcInternalModule, mkInteractiveModule)
import GHC.Builtin.WiredIn.Types

import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDsTc)
import GHC.Iface.Env (lookupOrig)
import GHC.Iface.Load (loadInterfaceForName)
import GHC.Linker.Loader (executableDependenciesAvailable)
import GHC.Rename.Env (DeprecationWarnings(..), addUsedGREs)
import GHC.Runtime.Context (extendInteractiveContextWithIds)
import GHC.Runtime.Interpreter (Interp, evalString)
import GHC.Types.SafeHaskell (SafeHaskellMode(..), getSafeMode)
import GHC.Unit.Home (isHomeModule)
import GHC.Unit.Module (Module, isInteractiveModule, moduleUnit)
import GHC.Unit.Module.Deps (dep_trusted_pkgs, imp_mods)
import GHC.Unit.Module.Imported (imv_is_safe, importedByUser)
import GHC.Unit.Module.ModIface (ModIface, mi_deps, mi_hsc_src, mi_trust, mi_trust_pkg)
import GHC.Unit.State (isUnitIdTrusted, isUnitTrusted)

import GHC.Types.Error (errorsOrFatalWarningsFound, emptyMessages)
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Name.Set
import GHC.Types.SourceFile (isHsBootOrSig)
import GHC.Types.SrcLoc
import GHC.Types.Unique.DFM (emptyUDFM, plusUDFM)
import GHC.Types.Unique.Supply (getUniquesM)
import GHC.Types.Var
import GHC.Types.Var.Env (emptyInScopeSet, emptyVarEnv, extendVarEnv, lookupVarEnv)
import GHC.Types.Var.Set

import GHC.Data.Bag (Bag)
import GHC.Data.FastString (fsLit)
import GHC.Data.Maybe (expectJust)
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Exception (Exception(..), SomeAsyncException, fromException, throwIO)
import GHC.Utils.Logger (getLogger)
import GHC.Utils.Misc (equalLength, lengthIs)
import GHC.Utils.Outputable

import Control.Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import Data.List (find, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- | Check a staged expression and attach the evidence from the supplied solver.
tcTopSpliceExprWith :: (WantedConstraints -> TcM (Bag EvBind))
                    -> SpliceType -> TcM (LHsExpr GhcTc) -> TcM (LHsExpr GhcTc)
tcTopSpliceExprWith solve isTypedSplice tc_action
  = checkNoErrs $
    setThLevel (Splice isTypedSplice Comp) $
    do { (mb_expr', wanted) <- tryCaptureConstraints tc_action
       ; const_binds <- solve wanted
       ; case mb_expr' of
           Nothing    -> failM
           Just expr' -> return $ mkHsDictLet (EvBinds const_binds) expr' }

-- | Desugar code that runs during compilation and report diagnostics before evaluation.
desugarStagedExpr :: LHsExpr GhcTc -> TcM Core.CoreExpr
desugarStagedExpr expr = do
  (messages, result) <- initDsTc (dsLExpr expr)
  logger <- getLogger
  flags <- getDynFlags
  liftIO $ printMessages logger (initDsMessageOpts flags) (initDiagOpts flags) messages
  when (errorsOrFatalWarningsFound messages) failM
  maybe failM pure result

-- | Reconstruct a runtime operand from a checked ground literal type.
tcDecodeClosedCallOperand :: Name -> Type -> TcM (Maybe Core.CoreExpr)
tcDecodeClosedCallOperand owner operand = do
  platform <- targetPlatform <$> getDynFlags
  rdr_env <- tcg_rdr_env <$> getGblEnv
  case decode platform rdr_env operand of
    Left UnsupportedClosedShape -> return Nothing
    Left err -> reportClosedShapeError owner err
    Right (value, gres)
      | exprType value `tcEqType` typeKind operand -> do
          addUsedGREs NoDeprecationWarnings (filter isImportedGRE (nonDetNameEnvElts gres))
          return (Just value)
      | otherwise -> return Nothing
  where
    decode platform rdr_env ty
      | Just atom <- closedScalar (typeKind ty) =
          maybe (Left UnsupportedClosedShape) (\value -> Right (value, emptyNameEnv))
                (decodeScalar platform atom ty)
      | otherwise = do
          let kind = typeKind ty
          (tc, kinds) <- maybe (Left UnsupportedClosedShape) Right (splitTyConApp_maybe kind)
          (promoted, args) <- maybe (Left UnsupportedClosedShape) Right (splitTyConApp_maybe ty)
          con <- maybe (Left UnsupportedClosedShape) Right (isPromotedDataCon_maybe promoted)
          ensure (dataConTyCon con == tc)
          own_gres <- if closedBuiltinData tc
            then return emptyNameEnv
            else do
              ensure (ordinaryClosedTyCon tc kinds)
              case closedImportedGRE rdr_env con of
                Just gre -> return (unitNameEnv (dataConName con) gre)
                Nothing -> Left (InaccessibleClosedOperand tc)
          let n_ma = promotedDataConMatchabilityArity con
              (ma_args, rest_args) = splitAt n_ma args
          ensure (length ma_args == n_ma && all (`tcEqType` unmatchableDataConTy) ma_args)
          let (type_args, values) = splitAt (length (dataConUserTyVars con)) rest_args
          (ClosedConstructorPlan source_args _, fields) <-
            closedConstructorPlan (Just type_args) kind kinds con
          ensure (closedConstructorArgumentsMatch fields values)
          found <- traverse (decode platform rdr_env) values
          return (mkCoreConWrapApps con (map Core.Type source_args ++ map fst found),
                  plusNameEnvList (own_gres : map snd found))

    ensure ok = unless ok (Left UnsupportedClosedShape)

    decodeScalar platform ClosedNatural ty = do
      number <- isNumLitTy ty
      guard (number >= 0)
      return (mkNaturalExpr platform number)
    decodeScalar _ ClosedChar ty = mkCharExpr <$> isCharLitTy ty
    decodeScalar _ ClosedBool ty = do
      (tc, []) <- splitTyConApp_maybe ty
      con <- isPromotedDataCon_maybe tc
      guard (dataConTyCon con == boolTyCon)
      pure (Core.Var (dataConWorkId con))

-- | Scalar types with a checked runtime and promoted representation.
data ClosedCallScalar = ClosedNatural | ClosedChar | ClosedBool

-- | Value shapes supported in both directions by the closed-call evaluator.
data ClosedCallShape
  = ClosedScalar ClosedCallScalar
  | ClosedScalarList ClosedCallScalar
  | ClosedData TyCon [Kind] [ClosedConstructor]
  | ClosedRef Kind

-- | Check source arity and field kinds before applying the constructor.
closedConstructorArgumentsMatch :: [Kind] -> [Type] -> Bool
closedConstructorArgumentsMatch fields values =
  equalLength fields values && and (zipWith tcEqType fields (map typeKind values))

-- | An independently quantified datatype whose one index changes in recursion.
data ClosedOpenShape = ClosedOpenShape TyVar TyCon [Type] [ClosedOpenAlternative]

-- | Nothing denotes the one recursive field at the case-bound predecessor.
data ClosedOpenAlternative = ClosedOpenAlternative DataCon ClosedOpenIndex [Maybe ClosedWitnessField]

-- | The promoted constructor that fixes the index or extends a predecessor.
data ClosedOpenIndex
  = ClosedFixedIndex { closedIndexCon :: DataCon }
  | ClosedSuccessor  { closedIndexCon :: DataCon }

-- | A finite graph of codecs keyed by their exact instantiated kinds.
data ClosedCodec = ClosedCodec ClosedCallShape [(Kind, ClosedCallShape)]

lookupCodecNode :: Kind -> [(Kind, a)] -> Maybe a
lookupCodecNode kind nodes = snd <$> find (tcEqType kind . fst) nodes

-- | The wire tag (absent for a product), constructor, and field codecs.
data ClosedConstructor
  = ClosedConstructor (Maybe String) DataCon ClosedConstructorFields

-- | Field codecs use static indices or recover indices from constructor fields.
data ClosedConstructorFields
  = ClosedStaticFields ClosedConstructorPlan [ClosedCallShape]
  | ClosedWitnessedFields [TyVar] [ClosedWitnessField]

-- | An indexed field determines one index from its constructors.
data ClosedWitnessField
  = AtWitnessIndex ClosedOpenShape
  | AtGroundType ClosedCallShape

-- | Source type arguments and nominal paths for the case-bound existentials.
data ClosedConstructorPlan = ClosedConstructorPlan [Type] [ClosedIndex]

-- | Recover the kind at a leaf of the result.
closedScalarKind :: ClosedCallScalar -> Kind
closedScalarKind ClosedNatural = naturalTy
closedScalarKind ClosedChar = charTy
closedScalarKind ClosedBool = boolTy

-- | Build the kind used for a case binder or a promoted constructor.
closedShapeKind :: ClosedCallShape -> Kind
closedShapeKind (ClosedScalar scalar) = closedScalarKind scalar
closedShapeKind (ClosedScalarList scalar) = mkListTy (closedScalarKind scalar)
closedShapeKind (ClosedData tc kinds _) = mkTyConApp tc kinds
closedShapeKind (ClosedRef kind) = kind

-- | A shape can be unsupported, inaccessible, or recursively expanding.
data ClosedShapeError
  = UnsupportedClosedShape
  | InaccessibleClosedData TyCon Bool
  | InaccessibleClosedOperand TyCon
  | NonDecreasingClosedShape Kind Kind
  | UnrecoverableClosedIndex
  | UnsupportedClosedPayload DataCon Int [TyVar]

-- | Classify a checked result and record the constructors inspected by its codec.
-- See Note [Constructor visibility in closed calls].
tcClosedCallValueShape :: Name -> Kind -> TcM (Maybe ClosedCodec)
tcClosedCallValueShape owner kind = do
  env <- getGblEnv
  case closedCallValueShape (tcg_rdr_env env) kind of
    Right codec@(ClosedCodec _ nodes) -> do
      let datatypes = concatMap dependencies (map snd nodes)
          dependencies (ClosedData tc _ alternatives)
            | not (closedBuiltinData tc) =
                (tc, [con | ClosedConstructor _ con _ <- alternatives]) :
                concatMap witness_dependencies alternatives
          dependencies _ = []
          witness_dependencies (ClosedConstructor _ _ (ClosedWitnessedFields _ fields)) =
            concatMap witness_field_dependencies fields
          witness_dependencies _ = []
          witness_field_dependencies (AtWitnessIndex field) = open_dependencies field
          witness_field_dependencies (AtGroundType _) = []
          open_dependencies (ClosedOpenShape _ tc _ alternatives) =
            (tc, [con | ClosedOpenAlternative con _ _ <- alternatives]) :
            [(dataConTyCon con, [con]) | ClosedOpenAlternative _ index _ <- alternatives
                                     , let con = closedIndexCon index] ++
            concatMap witness_field_dependencies
              [field | ClosedOpenAlternative _ _ fields <- alternatives, Just field <- fields]
          this_mod = tcg_semantic_mod env
          used = [name | (tc, constructors) <- datatypes
                       , name <- tyConName tc : map dataConName constructors
                       , not (nameIsLocalOrFrom this_mod name)]
      addUsedGREs NoDeprecationWarnings
        [gre | (_, constructors) <- datatypes, con <- constructors
             , Just gre <- [closedImportedGRE (tcg_rdr_env env) con]
             , isImportedGRE gre]
      updTcRef (tcg_source_check env) (\s -> s { scs_evaluated_uses = extendNameSetList (scs_evaluated_uses s) used })
      return (Just codec)
    Left err -> reportClosedShapeError owner err

-- | Check whether a ground type has a supported closed value representation.
tcHasClosedValueShape :: Name -> Kind -> TcM Bool
tcHasClosedValueShape owner kind = isJust <$> tcClosedCallValueShape owner kind

-- | Report inaccessible constructors without naming them.
reportClosedShapeError :: Name -> ClosedShapeError -> TcM (Maybe a)
reportClosedShapeError owner err = case err of
  UnsupportedClosedShape -> return Nothing
  InaccessibleClosedData tc indexed -> do
      flags <- getDynFlags
      ctx <- getNamePprCtx
      units <- hsc_units <$> getTopEnv
      let shape = if isEnumerationTyCon tc then "an enumeration" else "an algebraic datatype"
          constructors | indexed = "constructor that can produce the result index"
                       | otherwise = "constructor"
      failWithTc (TcRnClosedTypeEval owner
        ("Cannot inspect the constructors of " ++ showSDocForUser flags units ctx (ppr tc) ++
         ". To evaluate " ++ shape ++ " in a type, bring every " ++ constructors ++
         " into scope from a non-SOURCE import."))
  InaccessibleClosedOperand tc -> do
      flags <- getDynFlags
      ctx <- getNamePprCtx
      units <- hsc_units <$> getTopEnv
      failWithTc (TcRnClosedTypeEval owner
        ("A selected constructor of " ++ showSDocForUser flags units ctx (ppr tc) ++
         " is not in scope from a non-SOURCE import."))
  UnrecoverableClosedIndex ->
    failWithTc (TcRnClosedTypeEval owner
      "Cannot recover a constructor-local type argument from the checked result type.")
  UnsupportedClosedPayload con position indices -> do
      flags <- getDynFlags
      ctx <- getNamePprCtx
      units <- hsc_units <$> getTopEnv
      let render = showSDocForUser flags units ctx
          constructor = render (quotes (ppr con))
          explanation = case indices of
            _:_:_ -> ". The field type depends on multiple hidden indices " ++
                     render (parens (pprWithCommas ppr indices)) ++
                     ". Its constructors must determine one index. " ++
                     "A field of the enclosing constructor must determine each remaining index."
            _ -> " at the hidden index. The field's constructors must determine that index."
      failWithTc (TcRnClosedTypeEval owner
        ("Cannot reify field " ++ show position ++ " of " ++ constructor ++ explanation))
  NonDecreasingClosedShape previous current -> do
      flags <- getDynFlags
      ctx <- getNamePprCtx
      units <- hsc_units <$> getTopEnv
      let show_type ty = showSDocForUser flags units ctx (ppr ty)
      failWithTc (TcRnClosedTypeEval owner
        ("Cannot plan the recursive constructor fields of " ++ show_type current ++
         ". The recursive type must repeat an earlier type or be a proper subterm of " ++
         show_type previous ++ ", or differ only in one index that is a proper subterm of its earlier value."))

-- | Classify a checked value and intern recursive constructor shapes.
closedCallValueShape :: GlobalRdrEnv -> Kind -> Either ClosedShapeError ClosedCodec
closedCallValueShape rdr_env kind = do
  (root, nodes) <- State.runStateT (shape [] kind) []
  return (ClosedCodec root nodes)
  where
    shape active current = case splitTyConApp_maybe current of
      Just (tc, [arg]) | tc == listTyCon -> case closedScalar arg of
        Just atom -> return (ClosedScalarList atom)
        Nothing -> do
          let here = mkTyConApp tc [arg]
          nodes <- State.get
          if isJust (lookupCodecNode here nodes)
            then return (ClosedRef here)
            else do
              elem_shape <- shape active arg
              node <- Trans.lift $ boxed tc [arg]
                [ (Just "0;", nilDataCon, [])
                , (Just "1;", consDataCon, [elem_shape, ClosedRef here]) ]
              State.modify' ((here, node) :)
              return (ClosedRef here)
      Just (tc, kinds)
        | Just specs <- builtin_constructors tc kinds -> do
            alts <- forM specs $ \(tag, con, field_tys) -> do
              fields <- traverse (shape active) field_tys
              return (tag, con, fields)
            Trans.lift (boxed tc kinds alts)
      _ | Just atom <- closedScalar current -> return (ClosedScalar atom)
      Just (tc, args)
        | ordinaryClosedTyCon tc args
        , let constructors = tyConDataCons tc
        , not (null constructors)
        -> do
          let here = mkTyConApp tc args
          nodes <- State.get
          if any (tcEqType here . snd) active || isJust (lookupCodecNode here nodes)
            then return (ClosedRef here)
            else do
              forM_ (find (\(head_tc, old) -> head_tc == tc && not (proper_subterm here old || smaller_index tc args old)) active) $
                \(_, old) -> Trans.lift (Left (NonDecreasingClosedShape old here))
              let possible = filter (not . dataConCannotMatch args) constructors
                  visible = all (isJust . closedImportedGRE rdr_env) possible
                  inaccessible = InaccessibleClosedData tc (not (equalLength possible constructors))
                  next = (tc, here) : active
                  plan con
                    | Just fields <- closedWitnessedFields here args con = Right (Right fields)
                    | otherwise = Left <$> closedConstructorPlan Nothing here args con
              when (null possible) $ Trans.lift (Left UnsupportedClosedShape)
              plans <- Trans.lift $ case traverse plan possible of
                Left UnrecoverableClosedIndex | not visible -> Left inaccessible
                checked -> checked
              unless visible $ Trans.lift (Left inaccessible)
              alternatives <- forM (zip possible plans) $ \(con, fields) -> do
                codecs <- case fields of
                  Left (static, ground) -> ClosedStaticFields static <$> traverse (shape next) ground
                  Right (indices, arguments) -> do
                    let numbered = zip [1..] arguments
                        used ty = field_indices indices ty
                        unsupported pos ty = UnsupportedClosedPayload con pos (used ty)
                    witness_fields <- forM numbered $ \(pos, ty) ->
                      witness_shape (unsupported pos ty) next indices ty
                    let recovered = mkVarSet [v | AtWitnessIndex (ClosedOpenShape v _ _ _) <- witness_fields]
                    forM_ numbered $ \(pos, ty) ->
                      unless (all (`elemVarSet` recovered) (used ty)) $
                        Trans.lift (Left (unsupported pos ty))
                    return (ClosedWitnessedFields indices witness_fields)
                return (ClosedConstructor (Just (closedConstructorTag con)) con codecs)
              State.modify' ((here, ClosedData tc args alternatives) :)
              return (ClosedRef here)
      Just (tc, _)
        | isVanillaAlgTyCon tc, isLiftedAlgTyCon tc, isAbstractTyCon tc
        -> Trans.lift (Left (InaccessibleClosedData tc False))
      _ -> Trans.lift (Left UnsupportedClosedShape)

    builtin_constructors tc [arg]
      | tc == maybeTyCon =
          Just [(Just "0;", nothingDataCon, []), (Just "1;", justDataCon, [arg])]
    builtin_constructors tc [a, b]
      | closedEitherTyCon tc, [left_con, right_con] <- tyConDataCons tc =
          Just [(Just "0;", left_con, [a]), (Just "1;", right_con, [b])]
    builtin_constructors tc args
      | isBoxedTupleTyCon tc, length args == tyConArity tc, length args /= 1 =
          Just [(Nothing, tupleDataCon Boxed (length args), args)]
    builtin_constructors _ _ = Nothing

    field_indices indices ty = filter (`elemVarSet` tyCoVarsOfType ty) indices

    witness_shape unsupported active indices ty
      | closedGroundField ty = AtGroundType <$> shape active ty
      | [(index, plan)] <- [(v, plan) | v <- field_indices indices ty,
                                        Just plan <- [closedOpenAlternatives (mkVarSet indices) v ty]] =
          AtWitnessIndex <$> open_shape unsupported active indices index ty plan
      | otherwise = Trans.lift (Left unsupported)

    open_shape unsupported active indices index field (tc, args, alternatives) = do
      let constructors = [con | (con, _, _) <- alternatives]
      unless (all (isJust . closedImportedGRE rdr_env) constructors) $
        Trans.lift (Left (InaccessibleClosedData tc False))
      plans <- forM alternatives $ \(con, result_index, fields) -> do
        let index_con = closedIndexCon result_index
        unless (isJust (closedImportedGRE rdr_env index_con)) $
          Trans.lift (Left (InaccessibleClosedData (dataConTyCon index_con) False))
        let payload ty = do
              unless (closedGroundField ty || proper_subterm ty field) $
                Trans.lift (Left unsupported)
              witness_shape unsupported active indices ty
        codecs <- traverse (traverse payload) fields
        return (ClosedOpenAlternative con result_index codecs)
      return (ClosedOpenShape index tc args plans)

    proper_subterm child parent
      | Just (_, args) <- splitTyConApp_maybe parent =
          any (\arg -> tcEqType child arg || proper_subterm child arg) args
      | otherwise = False

    smaller_index tc current old
      | Just (old_tc, previous) <- splitTyConApp_maybe old
      , tc == old_tc
      , let changed = [(tv, new, orig) | (tv, (new, orig)) <- zip (tyConTyVars tc) (zip current previous)
                                       , not (new `tcEqType` orig)]
      , [(tv, new, orig)] <- changed
      = not (tcIsLiftedTypeKind (tyVarKind tv)) && proper_subterm new orig
      | otherwise = False

    boxed tc kinds alternatives
      | all (constructor_ok tc kinds) alternatives
      = Right (ClosedData tc kinds
          [ClosedConstructor tag con (ClosedStaticFields (ClosedConstructorPlan kinds []) fields)
          | (tag, con, fields) <- alternatives])
      | otherwise
      = Left UnsupportedClosedShape

    constructor_ok tc kinds (_, con, fields)
      | dataConTyCon con == tc, isVanillaDataCon con
      , null (dataConRepArgBinders con), null (dataConTheta con)
      , equalLength (dataConUnivTyVars con) kinds
      , dataConRepArity con == length fields
      , let args = dataConInstArgTys con kinds
      = equalLength args fields &&
        and (zipWith (\arg field -> scaledThing arg `tcEqType` closedShapeKind field)
                     args fields)
      | otherwise = False

-- | Check the field representation before planning a runtime witness.
closedWitnessedFields :: Kind -> [Type] -> DataCon -> Maybe ([TyVar], [Type])
closedWitnessedFields here args con = do
  (universals, indices, [], [], fields, result) <- pure (dataConFullSig con)
  guard (not (null indices) && all (\v -> isTyVar v && closedGroundField (tyVarKind v)) indices)
  let variables = universals ++ indices
  guard (closedOpenRepresentation con && equalLength universals args &&
         equalLength variables (dataConUserTyVars con) &&
         all (`elem` variables) (dataConUserTyVars con))
  let instantiate = substTyWith universals args
      rep = map (instantiate . scaledThing) (dataConRepArgTys con)
      source = map (instantiate . scaledThing) fields
  guard (instantiate result `tcEqType` here &&
         equalLength rep source && and (zipWith tcEqType rep source))
  guard (mkVarSet indices `subVarSet` tyCoVarsOfTypes source)
  return (indices, source)

-- | Check that worker values have the source fields' boxed representation.
closedOpenRepresentation :: DataCon -> Bool
closedOpenRepresentation con =
  null (dataConRepArgBinders con) && not (any isUnpacked (dataConImplBangs con))

-- | Validate the open datatype before generating its polymorphic encoder.
-- See Note [Closed results with an existential witness].
closedOpenAlternatives :: TyVarSet -> TyVar -> Type
                       -> Maybe (TyCon, [Type], [(DataCon, ClosedOpenIndex, [Maybe Type])])
closedOpenAlternatives indices index field = do
  (tc, args) <- splitTyConApp_maybe field
  guard (isVanillaAlgTyCon tc && isBoxedDataTyCon tc && isLiftedAlgTyCon tc &&
         null (tyConStupidTheta tc) && lengthIs args (tyConArity tc))
  let positions = [i | (i, arg) <- zip [0 :: Int ..] args, arg `tcEqType` mkTyVarTy index]
  [position] <- pure positions
  guard (all fixed_argument [arg | (i, arg) <- zip [0 ..] args, i /= position])
  guard (not (null (tyConDataCons tc)))
  alternatives <- traverse (check position args) (tyConDataCons tc)
  return (tc, args, alternatives)
  where
    fixed_argument arg =
      tyCoVarsOfType arg `subVarSet` indices && isTyFamFree arg && isTauTy arg

    check position args con = do
      (universals, locals, [equation], [], fields, result) <- pure (dataConFullSig con)
      let (refined, rhs) = eqSpecPair equation
          source_vars = filter (/= refined) universals ++ locals
      guard (closedOpenRepresentation con && equalLength universals args &&
             listToMaybe (drop position universals) == Just refined &&
             equalLength source_vars (dataConUserTyVars con) &&
             all (`elem` source_vars) (dataConUserTyVars con))
      (tc, _) <- splitTyConApp_maybe field
      let instantiate = substTyWith universals args
          here_index = instantiate (mkTyVarTy refined)
          constructor_index = instantiate rhs
          at_index value = mkTyConApp tc (atStructuralIndex index value id args)
          result_type = instantiate result
          field_types = map (instantiate . scaledThing) fields
          rep = map (instantiate . scaledThing) (dataConRepArgTys con)
          expected_evidence = map instantiate (eqSpecPreds [equation])
      (result_index, recursive_type) <- case locals of
        [] | Just (base, []) <- splitTyConApp_maybe constructor_index
           , Just base_con <- isPromotedDataCon_maybe base, tyConArity base == 0 ->
          return (ClosedFixedIndex base_con, Nothing)
        [local] | isTyVar local, tyVarKind local `tcEqType` tyVarKind index -> do
          (successor, [arg]) <- splitTyConApp_maybe constructor_index
          successor_con <- isPromotedDataCon_maybe successor
          guard (nominalClosedIndexTyCon successor &&
                 tyConArity successor == 1 && arg `tcEqType` mkTyVarTy local)
          return (ClosedSuccessor successor_con,
                  Just (at_index (mkTyVarTy local)))
        _ -> Nothing
      let typed_fields = [if maybe False (tcEqType arg) recursive_type then Nothing else Just arg
                         | arg <- field_types]
      guard (maybe True (const (lengthIs (filter isNothing typed_fields) 1)) recursive_type &&
             result_type `tcEqType` at_index constructor_index &&
             here_index `tcEqType` mkTyVarTy index &&
             dataConRepArity con == length rep &&
             equalLength rep (expected_evidence ++ field_types) &&
             and (zipWith tcEqType rep (expected_evidence ++ field_types)))
      return (con, result_index, typed_fields)

-- | Substitute the structural index and the fixed arguments from the enclosing constructor.
closedOpenArgs :: Subst -> ClosedOpenShape -> Type -> [Type]
closedOpenArgs actuals (ClosedOpenShape index _ args _) val =
  atStructuralIndex index val (substTy actuals) args

-- | Replace the structural argument and keep its enclosing binder in other arguments.
atStructuralIndex :: TyVar -> Type -> (Type -> Type) -> [Type] -> [Type]
atStructuralIndex index val instantiate = map argument
  where
    argument arg
      | arg `tcEqType` mkTyVarTy index = val
      | otherwise = instantiate arg

-- | Get the type consumed by a polymorphic renderer at this index.
closedOpenType :: Subst -> ClosedOpenShape -> Type -> Type
closedOpenType actuals shape@(ClosedOpenShape _ tc _ _) val =
  mkTyConApp tc (closedOpenArgs actuals shape val)

-- | A terminated tag distinguishes each constructor on the wire.
closedConstructorTag :: DataCon -> String
closedConstructorTag con = show (dataConTagZ con) ++ ";"

-- | Canonical fixed datatypes do not require client constructor imports.
closedBuiltinData :: TyCon -> Bool
closedBuiltinData tc =
  tc == listTyCon || tc == maybeTyCon || (isBoxedTupleTyCon tc && tyConArity tc /= 1) || closedEitherTyCon tc

closedEitherTyCon :: TyCon -> Bool
closedEitherTyCon tc =
  let name = tyConName tc
  in nameOccName name == eitherTyConOcc &&
     nameModule_maybe name == Just (mkGhcInternalModule (fsLit "GHC.Internal.Data.Either"))

closedScalar :: Type -> Maybe ClosedCallScalar
closedScalar ty
  | ty `tcEqType` naturalTy = Just ClosedNatural
  | ty `tcEqType` charTy = Just ClosedChar
  | ty `tcEqType` boolTy = Just ClosedBool
  | otherwise = Nothing

ordinaryClosedTyCon :: TyCon -> [Type] -> Bool
ordinaryClosedTyCon tc args =
  isVanillaAlgTyCon tc && isLiftedDataOrNewTyCon tc &&
  lengthIs args (tyConArity tc) && null (tyConStupidTheta tc) &&
  all closedGroundType args &&
  all (closedGroundField . instantiate . tyVarKind) tvs
  where
    tvs = tyConTyVars tc
    instantiate = substTyWith tvs args

-- | Check that a kind or type has no unresolved information.
closedGroundType :: Type -> Bool
closedGroundType ty = noFreeVarsOfType ty && isTyFamFree ty && isTauTy ty

closedGroundField :: Type -> Bool
closedGroundField ty = tcIsLiftedTypeKind (typeKind ty) && closedGroundType ty

-- | A ground existential and the path to its nominal equality in a case arm.
data ClosedIndex = ClosedIndex Type Int [Int]

-- | Solve erased indices only through a constructor's nominal EqSpec.
-- See Note [Recovering constructor-local indices in closed calls].
closedConstructorIndices :: [TyVar] -> [(Type, Type)] -> Maybe [ClosedIndex]
closedConstructorIndices locals equations = do
  matches <- traverse (\(i, (ground, template)) -> match i [] ground template)
                      (zip [0 ..] equations)
  traverse (find_solution (concat matches)) locals
  where
    match evidence path ground template
      | Just tv <- getTyVar_maybe template
      , tv `elem` locals
      , closedGroundType ground
      , typeKind ground `tcEqType` tyVarKind tv
      = Just [(tv, ClosedIndex ground evidence (reverse path))]
      | ground `tcEqType` template = Just []
      | Just (head_ground, args_ground) <- splitTyConApp_maybe ground
      , Just (head_template, args_template) <- splitTyConApp_maybe template
      , head_ground == head_template, nominalClosedIndexTyCon head_ground
      , lengthIs args_ground (tyConArity head_ground)
      , equalLength args_ground args_template
      = concat <$> traverse (\(n, (g, t)) -> match evidence (n : path) g t)
                            (zip [0 ..] (zip args_ground args_template))
      | otherwise = Nothing

    find_solution matches tv = case [solution | (v, solution) <- matches, tv == v] of
      first@(ClosedIndex ground _ _) : rest
        | all (\(ClosedIndex other _ _) -> ground `tcEqType` other) rest -> Just first
      _ -> Nothing

-- | Check the constructor class used for both matching and projection.
nominalClosedIndexTyCon :: TyCon -> Bool
nominalClosedIndexTyCon tc =
  (isPromotedDataCon tc || isVanillaAlgTyCon tc) && isInjectiveTyCon tc Nominal

-- | Instantiate the source fields, erased arguments, and worker evidence.
-- See Note [Recovering constructor-local indices in closed calls].
closedConstructorPlan :: Maybe [Type] -> Kind -> [Type] -> DataCon
                      -> Either ClosedShapeError (ClosedConstructorPlan, [Type])
closedConstructorPlan checked_args here args con
  | (universals, existentials, specs, [], _, _) <- dataConFullSig con
  , null (dataConRepArgBinders con)
  , equalLength universals args
  , all isTyVar existentials, all (closedGroundType . tyVarKind) existentials
  , let bangs = dataConImplBangs con
  , not (any isUnpacked bangs)
  , not (null specs) || not (null existentials) || isVanillaDataCon con
  , not (null specs) || not (null existentials) || isNothing (dataConWrapId_maybe con) ||
      any isBanged bangs || dataConUserTyVars con /= universals || isNewTyCon (dataConTyCon con)
  , let user_vars = dataConUserTyVars con
        refined = map (fst . eqSpecPair) specs
        allowed = filter (`notElem` refined) universals ++ existentials
  , equalLength user_vars allowed, all (`elem` allowed) user_vars
  = do
      let instantiate_universals = substTyWith universals args
          equations = [(instantiate_universals (mkTyVarTy tv), instantiate_universals rhs)
                      | spec <- specs, let (tv, rhs) = eqSpecPair spec]
          recovery_error | null existentials = UnsupportedClosedShape
                         | otherwise = UnrecoverableClosedIndex
      (local_args, indices) <- case checked_args of
        Just provided
          | equalLength provided user_vars, all closedGroundType provided -> do
            locals <- maybe (Left UnsupportedClosedShape) Right $
              traverse (`lookup` zip user_vars provided) existentials
            return (locals, [])
          | otherwise -> Left UnsupportedClosedShape
        Nothing -> do
          inferred <- maybe (Left recovery_error) Right
                        (closedConstructorIndices existentials equations)
          return ([ty | ClosedIndex ty _ _ <- inferred], inferred)
      let variables = universals ++ existentials
          arguments = args ++ local_args
          instantiate = substTyWith variables arguments
          fields = map (instantiate . scaledThing) (dataConOrigArgTys con)
          rep = map (instantiate . scaledThing) (dataConRepArgTys con)
          (scope_binders, proof_preds) =
            fromMaybe ([], eqSpecPreds specs) (dataConGadtProof_maybe con)
          (evidence, rep_fields) = splitAt (length proof_preds) rep
          expected_evidence = map instantiate proof_preds
      unless (instantiate (dataConOrigResTy con) `tcEqType` here &&
              all (\(lhs, rhs) -> instantiate lhs `tcEqType` instantiate rhs) equations &&
              all closedGroundField fields &&
              dataConRepArity con == length scope_binders + length rep &&
              equalLength evidence expected_evidence && and (zipWith tcEqType evidence expected_evidence) &&
              equalLength fields rep_fields && and (zipWith tcEqType fields rep_fields)) $
        Left UnsupportedClosedShape
      source_args <- maybe (Left UnsupportedClosedShape) Right
                       (traverse (`lookup` zip variables arguments) user_vars)
      unless (and (zipWith (\tv arg -> instantiate (tyVarKind tv) `tcEqType` typeKind arg)
                          user_vars source_args) &&
              maybe True (and . zipWith tcEqType source_args) checked_args) $
        Left UnsupportedClosedShape
      return (ClosedConstructorPlan source_args indices, fields)
  | otherwise = Left UnsupportedClosedShape

-- | Project the actual case evidence and orient it from the local index to ground.
closedIndexCoercion :: [Coercion] -> TyVar -> ClosedIndex -> Maybe Coercion
closedIndexCoercion evidence tv (ClosedIndex ground evidence_pos path) = do
  co <- listToMaybe (drop evidence_pos evidence)
  guard (coercionRole co == Nominal)
  projected <- foldM project co path
  guard (coercionLKind projected `tcEqType` ground &&
         coercionRKind projected `tcEqType` mkTyVarTy tv)
  return (mkSymCo projected)
  where
    project co i = do
      (left, ls) <- splitTyConApp_maybe (coercionLKind co)
      (right, rs) <- splitTyConApp_maybe (coercionRKind co)
      guard (left == right && nominalClosedIndexTyCon left &&
             lengthIs ls (tyConArity left) && equalLength ls rs &&
             i < length ls)
      return (mkSelCo (SelTyCon i Nominal) co)

-- | Find a local or non-SOURCE imported GRE for this exact constructor.
closedImportedGRE :: GlobalRdrEnv -> DataCon -> Maybe GlobalRdrElt
closedImportedGRE rdr_env con = do
  gre <- lookupGRE_Name rdr_env (dataConName con)
  guard (isLocalGRE gre || any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre))
  return gre

-- | Execute a checked closed call through the target interpreter.
-- See Note [Closed literal type evaluation].
tcRunClosedCall :: Id -> Core.CoreExpr -> Type -> TcM Type
tcRunClosedCall fun argument result_kind = do
  apply <- tcPrepareClosedApplication fun [exprType argument] result_kind []
  reify <- tcPrepareClosedResult (idName fun) result_kind
  apply [argument] >>= reify

-- | Typecheck the ordinary runtime application without observing its value.
tcPrepareClosedApplication :: Id -> [Type] -> Kind -> [Bool] -> TcM ([Core.CoreExpr] -> TcM Core.CoreExpr)
tcPrepareClosedApplication fun domains result_kind rigid_args = do
  let name = idName fun
      bad = failWithTc . TcRnClosedTypeEval name
      rigidOperand True arg = Just arg
      rigidOperand False _ = Nothing
      expected = foldr mkVisFunTyManyU result_kind domains
  (function, method) <- prepareClosedFunction fun expected
  loc <- getSrcSpanM
  return $ \arguments -> setSrcSpan loc $ do
    unless (length arguments == length domains &&
            and (zipWith tcEqType (map exprType arguments) domains)) $
      bad "The runtime operands do not have the expected types."
    forM_ method $ \(dfun, worker) -> do
      let (dictionary, body) = case function of
            Core.Let (Core.NonRec binder (Core.Var selected)) body
              | selected == dfun -> (binder, body)
            _ -> (dfun, function)
          checked = case Core.collectArgs body of
            (Core.Var selector, args)
              | fun == selector
              , [Core.Var actual_dict] <- filter (not . Core.isTypeArg) args
              , actual_dict == dictionary ->
                  closedSourceProofArgs (zipWith rigidOperand rigid_args arguments)
            _ -> []
      refined <- if null checked then pure ([], []) else do
        let leaves = concatMap (exprSomeFreeVarsList nonConstructorTerm) arguments
        stable <- and <$> traverse closedRuleLeaf (worker : leaves)
        if stable then do
          env <- getGblEnv
          updTcRef (tcg_source_check env) (\s -> s { scs_evaluated_uses = extendNameSetList (scs_evaluated_uses s) (map idName (worker : leaves)) })
          pure (checked, leaves)
        else pure ([], [])
      captureSourceMethodProof dfun fun worker refined
    return (mkCoreApps function arguments)

{- Note [Closed rule argument shapes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor argument supplies a rule mismatch only when the provider
wrote that constructor. Evaluation can turn an opaque call into a
constructor, while a rewrite rule can change the result of that opaque call.

Inlining the selected worker or a callback can expose another call to the
selector. Check the imported worker and each non-constructor free term
of the arguments. Accept only available unfoldings with no free term
references other than data constructor workers. Retain the coarse rule
dependency of each such argument term so a new rule cannot replace it.
-}

-- | Check the imported definition used by an argument or method worker.
closedRuleLeaf :: Id -> TcM Bool
closedRuleLeaf var = do
  body <- readOpenDefinition var
  pure (either (const False) (null . exprSomeFreeVarsList nonConstructorTerm) body)

-- | Find term references that a constructor cannot account for.
nonConstructorTerm :: Var -> Bool
nonConstructorTerm var = isId var && not (isDataConWorkId var)

-- | Prepare an imported function at its checked ordinary runtime type.
tcPrepareClosedFunctionValue :: Id -> Type -> TcM Core.CoreExpr
tcPrepareClosedFunctionValue fun expected = do
  (function, method) <- prepareClosedFunction fun expected
  forM_ method $ \(dfun, worker) -> captureSourceMethodProof dfun fun worker ([], [])
  pure function

-- | Check a specialization and identify its selected method, if known.
prepareClosedFunction :: Id -> Type -> TcM (Core.CoreExpr, Maybe (Id, Id))
prepareClosedFunction fun expected = do
  let name = idName fun
  tcPreflightClosedCall name
  failIfErrsM
  (function, method) <- if idType fun `tcEqType` expected
                          then pure (Core.Var fun, Nothing)
                          else specializeClosedFunction fun expected
  unless (exprType function `tcEqType` expected) $
    failWithTc (TcRnClosedTypeEval name
      "The closed call does not have the expected ordinary function type.")
  env <- getGblEnv
  unless (nameIsLocalOrFrom (tcg_semantic_mod env) name) $
    updTcRef (tcg_source_check env) (\s -> s { scs_evaluated_uses = extendNameSet (scs_evaluated_uses s) name })
  return (function, method)

-- | Render and interpret a complete runtime expression at a type boundary.
tcPrepareClosedResult :: Name -> Kind -> TcM (Core.CoreExpr -> TcM Type)
tcPrepareClosedResult name result_kind = do
  reify <- tcPrepareClosedResultWithOptionalName name result_kind Nothing
  pure (fmap expectJust . reify)

-- | Observe an executable entry when the interpreter can load its symbol.
tcPrepareClosedObservation :: Name -> Kind -> Name -> TcM (Core.CoreExpr -> TcM (Maybe Type))
tcPrepareClosedObservation name result_kind entry =
  tcPrepareClosedResultWithOptionalName name result_kind (Just entry)

tcPrepareClosedResultWithOptionalName
  :: Name -> Kind -> Maybe Name -> TcM (Core.CoreExpr -> TcM (Maybe Type))
tcPrepareClosedResultWithOptionalName name result_kind optional = do
  hsc_env <- getTopEnv
  checked_shape <- tcClosedCallValueShape name result_kind
  shape <- case checked_shape of
    Just shape -> return shape
    Nothing -> bad ("The function returns " ++ showPpr (hsc_dflags hsc_env) result_kind ++
                    ". Closed calls in types currently support results of type Natural, Char, Bool, " ++
                    "lists, boxed pairs, Maybe, and Either of supported results, " ++
                    "or ordinary boxed datatypes with supported fields and imported constructors.")
  interp <- checkClosedTypeEvalStage hsc_env name
  failIfErrsM
  render <- closedCallRenderer name shape
  ret_id <- lookupOrig gHC_INTERNAL_BASE returnIOIdOcc >>= tcLookupId
  io_tc <- lookupOrig gHC_TYPES (mkTcOcc "IO") >>= tcLookupTyCon
  let ret = mkCoreApp (Core.Var ret_id) (Core.Type stringTy)
      io_string = mkTyConApp io_tc [stringTy]
  unless (exprType ret `tcEqType` mkVisFunTyManyU stringTy io_string) $
    bad "Unexpected type for the interpreter response."
  loc <- getSrcSpanM
  return $ \call -> setSrcSpan loc $ do
    failIfErrsM
    unless (exprType call `tcEqType` result_kind) $
      bad "The runtime expression does not have the checked result type."
    let response = render call
    unless (exprType response `tcEqType` stringTy) $
      bad "Unexpected type for the interpreter response."
    gbl_env <- getGblEnv
    let full_expr0 = mkCoreApp ret response
        (local_bco_names, local_bco_binds, full_expr) =
          rewriteSameModuleClosedExpr (tcg_local_open_defs gbl_env) full_expr0
        local_alg_tcs = [tc | tc <- tcg_tcs gbl_env, isAlgTyCon tc]
        needs_local_itbls =
          not (null local_bco_binds) ||
          any (\v -> (isDataConWorkId v || isDataConWrapId v)
                     && nameIsLocalOrFrom (tcg_semantic_mod gbl_env) (idName v))
              (exprSomeFreeVarsList (const True) full_expr)
        hsc_env_bco =
          if null local_bco_binds
            then hsc_env
            else hsc_env { hsc_IC = extendInteractiveContextWithIds (hsc_IC hsc_env) (map fst local_bco_binds) }
    unless (null local_bco_names) $
      updTcRef (tcg_source_check gbl_env) (\s -> s { scs_evaluated_uses = extendNameSetList (scs_evaluated_uses s) local_bco_names })
    compiled <- tryMostM $ liftIO $ do
      (local_links, local_pkgs) <-
        if needs_local_itbls
          then hscLoadSameModuleClosedBcos hsc_env_bco loc local_alg_tcs local_bco_binds
          else pure ([], emptyUDFM)
      (value, links, packages) <- case optional of
        Nothing -> do
          (val, lks, pkgs) <- hscCompileCoreExpr hsc_env_bco loc full_expr
          pure (Just val, lks, pkgs)
        Just entry -> hscCompileCoreExprWithOptionalName hsc_env_bco loc entry full_expr
      pure (value, local_links ++ links, plusUDFM local_pkgs packages)
    (value, links, packages) <- either (bad . displayException) pure compiled
    recordThNeededRuntimeDeps links packages
    forM value $ \val -> do
      outcome <- tryAllM $ liftIO $ timeout 10000000 (evalString interp val)
      rendered <- case outcome of
        Left exn | Just async <- (fromException exn :: Maybe SomeAsyncException) ->
          liftIO $ throwIO async
        Left exn -> bad (displayException exn)
        Right Nothing -> bad "Compile-time evaluation exceeded the time limit."
        Right (Just rendered) -> pure rendered
      case decodeClosedCallResult shape rendered of
        Just ty | typeKind ty `tcEqType` result_kind -> return ty
        _ -> bad "The value returned by the interpreter does not match the checked result type."
  where
    bad :: String -> TcM a
    bad = failWithTc . TcRnClosedTypeEval name

-- | Build the renderer before any nested operand executes.
closedCallRenderer :: Name -> ClosedCodec -> TcM (Core.CoreExpr -> Core.CoreExpr)
closedCallRenderer name (ClosedCodec shape nodes) = do
  show_class <- lookupOrig (mkGhcInternalModule (fsLit "GHC.Internal.Show"))
                           (mkTcOcc "Show") >>= tcLookupClass
  inst_env <- tcGetInstEnvs
  let empty_string = mkListExpr charTy []
      show_s = mkVisFunTyManyU stringTy stringTy
      builtinShow scalar is_list = do
        let dictionary_kind = closedScalarKind scalar
            method = if is_list then "showList" else "show"
            domain = if is_list then mkListTy dictionary_kind else dictionary_kind
            range = if is_list then show_s else stringTy
        case (find ((== method) . getOccString) (classMethods show_class),
              lookupUniqueInstEnv inst_env show_class [dictionary_kind]) of
          (Just show_id, Right (inst, []))
            | let dfun = instanceDFunId inst
            , nameModule_maybe (idName dfun) == nameModule_maybe (className show_class)
            , idType dfun `tcEqType` mkClassPred show_class [dictionary_kind] -> do
                let render = mkCoreApps (Core.Var show_id)
                               [Core.Type dictionary_kind, Core.Var dfun]
                unless (exprType render `tcEqType` mkVisFunTyManyU domain range) $
                  failWithTc (TcRnClosedTypeEval name "Unexpected type for the interpreter response.")
                return render
          _ -> failWithTc (TcRnClosedTypeEval name
                 "Cannot resolve the built-in Show dictionary for the result type.")
      renderScalar scalar = mkCoreApp <$> builtinShow scalar False
      renderList scalar = do
        render <- builtinShow scalar True
        return $ \value -> mkCoreApps render [value, empty_string]
      renderToken quote render value suffix = mkCoreApps quote [render value, suffix]
      cast_field existentials cos field var
        | varType var `tcEqType` closedShapeKind field = Just (Core.Var var)
        | otherwise = do
            let lifted = liftCoSubstWith Nominal existentials cos (varType var)
            guard (coercionLKind lifted `tcEqType` varType var &&
                   coercionRKind lifted `tcEqType` closedShapeKind field)
            return (mkCast (Core.Var var) (mkSubCo lifted))
      prependTag Nothing body = body
      prependTag (Just s) body = foldr (\c r -> mkConsExpr charTy (mkCharExpr c) r) body s
      unexpected_rep :: TcM a
      unexpected_rep = failWithTc (TcRnClosedTypeEval name "Unexpected indexed constructor representation.")
      renderWitnessField refs quote actuals field value = case field of
        AtWitnessIndex indexed@(ClosedOpenShape template _ _ _)
          | let idx = substTy actuals (mkTyVarTy template)
          , varType value `tcEqType` closedOpenType actuals indexed idx -> do
              render <- renderOpen refs quote actuals indexed
              return (render idx (Core.Var value))
        AtGroundType ground
          | varType value `tcEqType` closedShapeKind ground -> do
              render <- renderFields refs quote ground
              return (render (Core.Var value))
        _ -> unexpected_rep
      renderOpen refs quote actuals open_shape@(ClosedOpenShape template _ _ alternatives) = do
        unique <- newUnique
        let index = mkTyVar (mkSysTvName unique (fsLit "closed_index")) (tyVarKind template)
            index_ty = mkTyVarTy index
            kind = closedOpenType actuals open_shape index_ty
        loop <- mkSysLocalM (fsLit "closed_indexed") ManyTy
                  (mkLamType index (mkVisFunTyManyU kind show_s))
        current <- mkSysLocalM (fsLit "closed_indexed_value") ManyTy kind
        suffix <- mkSysLocalM (fsLit "closed_indexed_suffix") ManyTy stringTy
        binder <- mkSysLocalM (fsLit "closed_indexed_arm") ManyTy kind
        alts <- forM alternatives $ \(ClosedOpenAlternative con result_index fields) -> do
          uniques <- getUniquesM
          let (locals, binders) = dataConRepInstPat uniques ManyTy con (closedOpenArgs actuals open_shape index_ty)
          case binders of
            evidence : values | isCoVar evidence, equalLength fields values -> do
              (target, previous) <- case (result_index, locals) of
                (ClosedFixedIndex base, []) -> return (mkTyConTy (promoteDataCon base), Nothing)
                (ClosedSuccessor successor, [local])
                  | isTyVar local, tyVarKind local `tcEqType` tyVarKind index ->
                  let previous = mkTyVarTy local
                  in return (mkTyConApp (promoteDataCon successor) [previous], Just previous)
                _ -> unexpected_rep
              let proof = mkCoVarCo evidence
              unless (coercionRole proof == Nominal &&
                      coercionLKind proof `tcEqType` index_ty &&
                      coercionRKind proof `tcEqType` target) unexpected_rep
              renderers <- forM (zip fields values) $ \(field, var) -> case field of
                Just field_shape -> renderWitnessField refs quote actuals field_shape var
                Nothing | Just child <- previous, varType var `tcEqType` closedOpenType actuals open_shape child ->
                  return (\rest -> mkCoreApps (Core.Var loop) [Core.Type child, Core.Var var, rest])
                _ -> unexpected_rep
              let encoded = foldr ($) (Core.Var suffix) renderers
              return (Core.Alt (Core.DataAlt con) (locals ++ binders)
                        (prependTag (Just (closedConstructorTag con)) encoded))
            _ -> unexpected_rep
        let body = Core.Case (Core.Var current) binder stringTy alts
            binding = Core.Rec [(loop, Core.Lam index (Core.Lam current (Core.Lam suffix body)))]
        return $ \idx value rest ->
          Core.Let binding (mkCoreApps (Core.Var loop) [Core.Type idx, value, rest])
      renderFields refs quote current = case current of
        ClosedRef kind
          | Just renderer <- lookupCodecNode kind refs ->
              return $ \value suffix -> mkCoreApps (Core.Var renderer) [value, suffix]
          | otherwise -> failWithTc (TcRnClosedTypeEval name "Missing recursive constructor renderer.")
        ClosedScalar scalar -> renderToken quote <$> renderScalar scalar
        ClosedScalarList scalar -> renderToken quote <$> renderList scalar
        ClosedData tc kinds [ClosedConstructor tag _ (ClosedStaticFields _ [codec])]
          | isNewTyCon tc
          , Just nt_ax <- newTyConCo_maybe tc -> do
              let co = mkUnbranchedAxInstCo Representational nt_ax kinds []
              if coercionLKind co `tcEqType` closedShapeKind current
                 && coercionRKind co `tcEqType` closedShapeKind codec
                then do
                  render_inner <- renderFields refs quote codec
                  return $ \value suffix ->
                    prependTag tag (render_inner (mkCast value co) suffix)
                else failWithTc (TcRnClosedTypeEval name "Unexpected newtype representation.")
        ClosedData _ kinds alternatives -> do
          binder <- mkSysLocalM (fsLit "closed_data") ManyTy (closedShapeKind current)
          render_alts <- forM alternatives $ \(ClosedConstructor tag con fields) -> do
            uniques <- getUniquesM
            let (worker_binders, binders) = dataConRepInstPat uniques ManyTy con kinds
                (existentials, scope_evidence) = span isTyVar worker_binders
            render_body <- case fields of
              ClosedStaticFields (ClosedConstructorPlan _ indices) codecs -> do
                let proof_fields = length (dataConTheta con) - length scope_evidence
                    (evidence, value_binders) = splitAt proof_fields binders
                    checked = do
                      guard (equalLength indices existentials && all isCoVar scope_evidence &&
                             all isCoVar evidence && equalLength codecs value_binders)
                      source_evidence <- if null scope_evidence
                        then return (map mkCoVarCo evidence)
                        else dataConGadtSourceCoercions_maybe con (kinds ++ mkTyVarTys existentials)
                               (scope_evidence ++ evidence)
                      cos <- zipWithM (closedIndexCoercion source_evidence) existentials indices
                      zipWithM (cast_field existentials cos) codecs value_binders
                values <- maybe (failWithTc (TcRnClosedTypeEval name "Unexpected constructor representation."))
                                return checked
                renderers <- mapM (renderFields refs quote) codecs
                return (foldr (.) id (zipWith ($) renderers values))
              ClosedWitnessedFields templates codecs
                | null scope_evidence, equalLength templates existentials, all isTyVar existentials
                , and (zipWith (\t v -> tyVarKind t `tcEqType` tyVarKind v) templates existentials)
                , equalLength codecs binders -> do
                    let actuals = zipTvSubst templates (map mkTyVarTy existentials)
                    renderers <- zipWithM (renderWitnessField refs quote actuals) codecs binders
                    return (foldr (.) id renderers)
                | otherwise -> unexpected_rep
            return $ \suffix ->
              Core.Alt (Core.DataAlt con) (worker_binders ++ binders)
                (prependTag tag (render_body suffix))
          let select value suffix = Core.Case value binder stringTy (map ($ suffix) render_alts)
          case render_alts of
            [_] -> return select
            _ -> do
              rest <- mkSysLocalM (fsLit "closed_suffix") ManyTy stringTy
              return $ \value suffix -> Core.Let (Core.NonRec rest suffix)
                                           (select value (Core.Var rest))
  case shape of
    ClosedScalar scalar -> renderScalar scalar
    ClosedScalarList scalar -> renderList scalar
    _ -> do
      quote <- builtinShow ClosedChar True
      refs <- forM nodes $ \(kind, _) -> do
        f <- mkSysLocalM (fsLit "closed_constructor") ManyTy (mkVisFunTyManyU kind show_s)
        return (kind, f)
      bindings <- forM (zip nodes refs) $ \((kind, node), (_, f)) -> do
        value <- mkSysLocalM (fsLit "closed_value") ManyTy kind
        suffix <- mkSysLocalM (fsLit "closed_suffix") ManyTy stringTy
        render <- renderFields refs quote node
        return (f, Core.Lam value (Core.Lam suffix (render (Core.Var value) (Core.Var suffix))))
      render <- renderFields refs quote shape
      let with_bindings = if null bindings then id else Core.Let (Core.Rec bindings)
      return $ \value -> with_bindings (render value empty_string)

-- | Use ordinary term checking to supply type and dictionary arguments.
specializeClosedFunction :: Id -> Type -> TcM (Core.CoreExpr, Maybe (Id, Id))
specializeClosedFunction fun expected = do
  loc <- getSrcSpanM
  let name = idName fun
      occurrence = L (noAnnSrcSpan loc) (mkHsVar (L (noAnnSrcSpan loc) name))
  typed <- unsetGOptM Opt_DeferTypeErrors $
           unsetGOptM Opt_DeferTypedHoles $
           unsetGOptM Opt_DeferOutOfScopeVariables $
           tcTopSpliceExprWith simplifyTopNoDefaulting Untyped $
           tcCheckMonoExpr occurrence expected
  zonked <- zonkTopLExpr typed
  core <- desugarStagedExpr zonked
  env <- getGblEnv
  let this_mod = tcg_semantic_mod env
      local_defs = tcg_local_open_defs env
      free = exprSomeFreeVarsList (const True) core
      local_ok v = idName v `elemNameEnv` local_defs
                   || isDataConWorkId v || isDataConWrapId v
      unavailable v = (isLocalVar v && not (idName v `elemNameEnv` local_defs))
                      || (nameIsLocalOrFrom this_mod (getName v) && not (local_ok v))
  when (any unavailable free) $
    failWithTc (TcRnClosedTypeEval name
      "The specialization uses an unfinished local binding or dictionary.")
  let external_free =
        [ getName v
        | v <- free, isGlobalId v
        , not (nameIsLocalOrFrom this_mod (getName v)) ]
  hsc_env <- getTopEnv
  mapM_ (checkClosedTypeEvalStage hsc_env) external_free
  updTcRef (tcg_source_check env) $ \s ->
    s { scs_evaluated_uses = extendNameSetList (scs_evaluated_uses s) external_free }
  method <- closedMethodProof env fun free
  pure (core, method)

-- | Rewrite references to solved same-module bindings in @tcg_local_open_defs@
-- to deterministic interactive BCO GlobalIds.
-- See Note [Same-module closed term evaluation] in GHC.Tc.Gen.HsType.
rewriteSameModuleClosedExpr
  :: NameEnv (Id, Core.CoreExpr)
  -> Core.CoreExpr
  -> ([Name], [(Id, Core.CoreExpr)], Core.CoreExpr)
rewriteSameModuleClosedExpr locals expr
  | isEmptyNameEnv locals = ([], [], expr)
  | null roots            = ([], [], expr)
  | otherwise             = (local_bco_names, rewritten_binds, rewrite emptyVarSet expr)
  where
    free_local_names e =
      [ idName v
      | v <- exprSomeFreeVarsList (const True) e
      , idName v `elemNameEnv` locals ]

    roots = free_local_names expr

    closure :: NameSet -> [Name] -> [(Id, Core.CoreExpr)]
    closure _ [] = []
    closure seen (n : ns)
      | n `elemNameSet` seen = closure seen ns
      | Just entry@(_, rhs) <- lookupNameEnv locals n
      = entry : closure (extendNameSet seen n) (free_local_names rhs ++ ns)
      | otherwise = closure seen ns

    mk_bco_id :: Id -> Id
    mk_bco_id owner =
      let u = idUnique owner
          bco_mod = mkInteractiveModule ("SameModClosed_" ++ show u)
          bco_name = mkExternalName u bco_mod (nameOccName (idName owner)) (nameSrcSpan (idName owner))
      in mkVanillaGlobal bco_name (idType owner)

    needed_bcos =
      [ (owner, mk_bco_id owner, rhs)
      | (owner, rhs) <- closure emptyNameSet roots ]

    bco_env = mkNameEnv [ (idName owner, bco) | (owner, bco, _) <- needed_bcos ]

    local_bco_names = [ idName owner | (owner, _, _) <- needed_bcos ]

    rewritten_binds =
      [ (bco, rewrite emptyVarSet rhs)
      | (_, bco, rhs) <- needed_bcos ]

    rewrite :: VarSet -> Core.CoreExpr -> Core.CoreExpr
    rewrite bound (Core.Var v)
      | not (v `elemVarSet` bound)
      , Just bco <- lookupNameEnv bco_env (idName v)
      = Core.Var bco
      | otherwise = Core.Var v
    rewrite _ e@Core.Lit{} = e
    rewrite bound (Core.App f a) = Core.App (rewrite bound f) (rewrite bound a)
    rewrite bound (Core.Lam b body) = Core.Lam b (rewrite (extendVarSet bound b) body)
    rewrite bound (Core.Let (Core.NonRec b rhs) body) =
      Core.Let (Core.NonRec b (rewrite bound rhs)) (rewrite (extendVarSet bound b) body)
    rewrite bound (Core.Let (Core.Rec prs) body) =
      let bound' = extendVarSetList bound (map fst prs)
      in Core.Let (Core.Rec [(b, rewrite bound' r) | (b, r) <- prs]) (rewrite bound' body)
    rewrite bound (Core.Case scrut b ty alts) =
      let bound_scrut = extendVarSet bound b
          rewrite_alt (Core.Alt con bs rhs) =
            Core.Alt con bs (rewrite (extendVarSetList bound_scrut bs) rhs)
      in Core.Case (rewrite bound scrut) b ty (map rewrite_alt alts)
    rewrite bound (Core.Cast e co) = Core.Cast (rewrite bound e) co
    rewrite bound (Core.Tick t e)  = Core.Tick t (rewrite bound e)
    rewrite bound (Core.Pack ex_ty wit_ty e) =
      Core.Pack ex_ty wit_ty (rewrite bound e)
    rewrite bound (Core.Unpack tv pv scrut body) =
      Core.Unpack tv pv (rewrite bound scrut)
        (rewrite (extendVarSetList bound [tv, pv]) body)
    rewrite _ e@Core.Type{}        = e
    rewrite _ e@Core.Coercion{}    = e

-- | Find the concrete method selection made by ordinary specialization.
closedMethodProof :: TcGblEnv -> Id -> [Var] -> TcM (Maybe (Id, Id))
closedMethodProof env selector free
  | Just cls <- isClassOpId_maybe selector
  , [dfun] <- [v | v <- free, isDFunId v
                 , Just (actual, [_]) <- [getClassPredTys_maybe (idType v)]
                 , cls == actual]
  = do
      state <- scs_rule_uses <$> readTcRef (tcg_source_check env)
      case sourceProofSignature state of
        Nothing -> pure Nothing
        Just _ -> do
          worker <- case sourceMethodTarget_maybe dfun selector of
            Just name -> tcLookupIdMaybe name
            Nothing -> either (const Nothing) Just <$> readOpenMethodWorker dfun selector
          return ((dfun,) <$> worker)
  | otherwise = pure Nothing

-- | Decode the canonical response from the interpreter.
decodeClosedCallResult :: ClosedCodec -> String -> Maybe Type
decodeClosedCallResult (ClosedCodec shape nodes) rendered = case shape of
  ClosedScalar scalar -> decodeScalar scalar False rendered
  ClosedScalarList scalar -> decodeScalar scalar True rendered
  _ -> do
    (value, rest) <- decodeFields shape rendered
    guard (null rest)
    return value
  where
    decodeFields current input = case current of
      ClosedRef kind -> lookupCodecNode kind nodes >>= \node -> decodeFields node input
      ClosedScalar scalar -> decodeToken scalar False input
      ClosedScalarList scalar -> decodeToken scalar True input
      ClosedData _ kinds alternatives -> do
        (ClosedConstructor _ con fields, encoded) <- selectConstructor alternatives input
        let kind = closedShapeKind current
        case fields of
          ClosedStaticFields (ClosedConstructorPlan source_args _) codecs -> do
            (values, rest) <- decodeArguments decodeFields codecs encoded
            let ma_args = replicate (promotedDataConMatchabilityArity con) unmatchableDataConTy
                value   = mkTyConApp (promoteDataCon con) (ma_args ++ source_args ++ values)
            guard (typeKind value `tcEqType` kind)
            return (value, rest)
          ClosedWitnessedFields templates codecs -> do
            let field (AtGroundType field_shape) inp = do
                  (value, rest) <- Trans.lift (decodeFields field_shape inp)
                  return (const (Just value), rest)
                field (AtWitnessIndex open_shape@(ClosedOpenShape template _ _ _)) inp = State.StateT $ \known -> do
                  (index, build, rest) <- decodeOpen known open_shape (lookupVarEnv known template) inp
                  return ((build, rest), extendVarEnv known template index)
            ((builders, rest), known) <- State.runStateT (decodeArguments field codecs encoded) emptyVarEnv
            indices <- traverse (lookupVarEnv known) templates
            values <- traverse ($ known) builders
            value <- packConstructor kind kinds con indices values
            return (value, rest)

    decodeOpen known open_shape@(ClosedOpenShape _ result_tc _ alternatives) expected input = do
      (ClosedOpenAlternative con result_index fields, payload) <-
        stripTaggedAlt (\(ClosedOpenAlternative ctor _ _) -> Just (closedConstructorTag ctor)) alternatives input
      expected_child <- case (expected, result_index) of
        (Nothing, _) -> return Nothing
        (Just index, ClosedFixedIndex fixed) -> do
          guard (index `tcEqType` mkTyConTy (promoteDataCon fixed))
          return Nothing
        (Just index, ClosedSuccessor successor) -> do
          (tc, [child]) <- splitTyConApp_maybe index
          guard (tc == promoteDataCon successor)
          return (Just child)
      (children, values, rest) <- decodeOpenFields known open_shape expected_child fields payload
      index <- case (result_index, children) of
        (ClosedFixedIndex fixed, []) -> return (mkTyConTy (promoteDataCon fixed))
        (ClosedSuccessor successor, [child]) -> return (mkTyConApp (promoteDataCon successor) [child])
        _ -> Nothing
      let build complete = do
            let actuals = mkTvSubst emptyInScopeSet complete
                args = closedOpenArgs actuals open_shape index
            packed <- traverse ($ complete) values
            packConstructor (mkTyConApp result_tc args) args con children packed
      return (index, build, rest)

    decodeOpenFields _ _ _ [] input = Just ([], [], input)
    decodeOpenFields known open_shape expected (field : fields) input = do
      (indices, value, after_field) <- case field of
        Just (AtGroundType field_shape) -> do
          (value, rest) <- decodeFields field_shape input
          return ([], const (Just value), rest)
        Just (AtWitnessIndex nested@(ClosedOpenShape template _ _ _)) -> do
          (index, build, rest) <- decodeOpen known nested (lookupVarEnv known template) input
          let checked complete = do
                wanted <- lookupVarEnv complete template
                guard (index `tcEqType` wanted)
                build complete
          return ([], checked, rest)
        Nothing -> do
          (index, build, rest) <- decodeOpen known open_shape expected input
          return ([index], build, rest)
      (other_indices, values, rest) <- decodeOpenFields known open_shape expected fields after_field
      return (indices ++ other_indices, value : values, rest)

    packConstructor kind kinds con indices values = do
      let variables = dataConUnivTyVars con ++ dataConExTyCoVars con
          arguments = kinds ++ indices
      guard (equalLength variables arguments)
      source_args <- traverse (`lookup` zip variables arguments) (dataConUserTyVars con)
      (ClosedConstructorPlan _ _, fields) <-
        either (const Nothing) Just (closedConstructorPlan (Just source_args) kind kinds con)
      guard (closedConstructorArgumentsMatch fields values)
      let ma_args  = replicate (promotedDataConMatchabilityArity con) unmatchableDataConTy
          promoted = mkTyConApp (promoteDataCon con) (ma_args ++ source_args ++ values)
      guard (typeKind promoted `tcEqType` kind)
      return promoted

    selectConstructor [constructor@(ClosedConstructor Nothing _ _)] input =
      Just (constructor, input)
    selectConstructor alternatives input =
      stripTaggedAlt (\(ClosedConstructor tag _ _) -> tag) alternatives input

    stripTaggedAlt tag_of alts input = listToMaybe
      [(alt, rest) | alt <- alts, Just tag <- [tag_of alt], Just rest <- [stripPrefix tag input]]

    decodeArguments _ [] input = return ([], input)
    decodeArguments decode (field : fields) input = do
      (value, rest) <- decode field input
      (values, residual) <- decodeArguments decode fields rest
      return (value : values, residual)

    decodeToken scalar is_list input = do
      [(token, _)] <- pure (reads @String input)
      rest <- stripPrefix (show token) input
      value <- decodeScalar scalar is_list token
      return (value, rest)

    decodeScalar scalar is_list = case scalar of
      ClosedNatural -> decodeLiteral scalar is_list (mkNumLitTy . toInteger @Natural)
      ClosedChar -> decodeLiteral scalar is_list mkCharLitTy
      ClosedBool -> decodeLiteral scalar is_list promoteBool

    promoteBool b = mkTyConTy (if b then promotedTrueDataCon else promotedFalseDataCon)

    decodeLiteral :: (Read a, Show a) => ClosedCallScalar -> Bool -> (a -> Type)
                  -> String -> Maybe Type
    decodeLiteral scalar is_list promote
      | is_list = decodeCanonical (mkPromotedListTy (closedScalarKind scalar) . map promote)
      | otherwise = decodeCanonical promote

    decodeCanonical :: (Read a, Show a) => (a -> Type) -> String -> Maybe Type
    decodeCanonical promote input = do
      value <- readMaybe input
      guard (show value == input)
      return (promote value)

{- Note [Closed results with an existential witness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor can bind erased indices shared by several fields. Read its
fields in source order. Use each existential binder as the key for its
recovered index and check later uses at that index.

Preflight checks the recursive schema of each indexed field. A base
constructor fixes its index to a nullary promoted constructor. A recursive
constructor refines its index to an injective promoted application of its
case-bound predecessor and carries one recursive field at that predecessor.
Other fields use the ground codec. Non-recursive indexed fields must be
proper subterms of the surrounding type so that codec planning terminates.

Each indexed field uses a polymorphic Core worker that abstracts its
structural index and keeps other case-bound indices in scope. Each recursive
case checks its nominal equality against the planned index pattern. The host
decoder recovers each index on first use, verifies subsequent uses at that
assignment, and supplies the indices to 'packConstructor' in declaration order.
-}

{- Note [Recovering constructor-local indices in closed calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Plan against the checked ground result type. A GADT constructor binds erased
indices with nominal equalities in its 'EqSpec'. Match only saturated,
nominally injective constructors along the 'EqSpec' path. Never invert a type
family or recover an existential without such a path.

At rendering time, 'dataConRepInstPat' supplies local binders. Project the
nominal equality down the same path, check its endpoints, and reverse it to
cast each field from its local index to the planned codec type. Lift through
the field at 'Nominal' before downgrading the field coercion to
'Representational'. A checked promoted operand already carries its erased
source arguments, so operand decoding checks the index equations directly.
-}

{- Note [Constructor visibility in closed calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Use 'DataKinds' constructor identities in both directions. An interface can
contain constructors hidden from the client. To inspect an unknown result,
require and record non-SOURCE imports for every constructor that the renderer
may inspect, and record the parent datatype in 'scs_evaluated_uses'. For
a known operand, require and record only the selected constructors.

Plan the result as a finite graph of instantiated datatypes. Exact repeats
form 'ClosedRef' edges implemented by mutually recursive Core bindings. Any
distinct instantiation of the same datatype on the active path must be a
proper subterm or strictly decrease one ground index.
-}

{- Note [Closed literal type evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an application in a type has no certified open equation (see Eisenberg
and Stolarek, "Promoting Functions to Type Families in Haskell", Haskell
Symposium 2014) and all operands are closed, GHC compiles an ordinary Core
application at the checked function type and evaluates it in the configured
target interpreter.

Use the built-in 'Show' instance for scalar types and 'returnIO' to construct
an @IO String@ for 'evalString'. Resolve 'IO', 'returnIO', and 'Show' by their
defining modules so local rebinding cannot alter the wire format. Scalar lists
use 'showList' from the scalar dictionary, preserving the @String@ fast path.
Compound lists, 'Maybe', 'Either', pairs, and user algebraic datatypes share a
single constructor-tagged S-expression codec over 'ClosedData' and 'ClosedRef'.

For an overloaded producer, specialize the occurrence with the splice solver
with defaulting disabled and local dictionary references rejected. Record
evaluated global names in 'scs_evaluated_uses' before simplification and
record loaded linkables through 'recordThNeededRuntimeDeps' (see Note [Object
File Dependencies] in "GHC.HsToCore.Usage").
-}

-- | Check the outer call before entering a nested operand.
tcPreflightClosedCall :: Name -> TcM ()
tcPreflightClosedCall name = do
  hsc_env <- getTopEnv
  _ <- checkClosedTypeEvalStage hsc_env name
  return ()

-- | Check the execution context before loading code for a type.
checkClosedTypeEvalStage :: HscEnv -> Name -> TcM Interp
checkClosedTypeEvalStage hsc_env name = do
  tcCheckClosedCallContext name
  let reject :: String -> TcM a
      reject = failWithTc . TcRnClosedTypeEval name
  dflags <- getDynFlags
  gbl_env <- getGblEnv
  if nameIsLocalOrFrom (tcg_semantic_mod gbl_env) name
     || maybe False isInteractiveModule (nameModule_maybe name)
    then do
      boot <- tcIsHsBootOrSig
      when boot $
        reject "The defining interface is a boot file or a module signature and provides no executable definition."
    else do
      iface <- loadInterfaceForName (text "closed call in a type") name
      when (isHsBootOrSig (mi_hsc_src iface)) $
        reject "The defining interface is a boot file or a module signature and provides no executable definition."
      case nameModule_maybe name of
        Just m
          | let user_imps = importedByUser (Map.findWithDefault [] m (imp_mods (tcg_imports gbl_env)))
          , any imv_is_safe user_imps
          , not (isTrustedSafeImport hsc_env dflags m iface) ->
              reject "Safe import disallows executing code from an untrusted module in types."
        _ -> pure ()
  when (safeInferOn dflags) $
    recordUnsafeInfer emptyMessages
  case hsc_interp hsc_env of
    Just interp -> return interp
    Nothing -> reject "Compile-time evaluation requires a GHC interpreter for the target."

isTrustedSafeImport :: HscEnv -> DynFlags -> Module -> ModIface -> Bool
isTrustedSafeImport hsc_env dflags m iface =
  let trust = getSafeMode (mi_trust iface)
      trust_own_pkg = mi_trust_pkg iface
      unit_state = hsc_units hsc_env
      home_unit = hsc_home_unit hsc_env
      safe_mod = trust `elem` [Sf_Safe, Sf_SafeInferred, Sf_Trustworthy]
      safe_pkg = case trust of
        Sf_None   -> False
        Sf_Ignore -> False
        Sf_Unsafe -> False
        _ | not (packageTrustOn dflags) -> True
        Sf_Safe | not trust_own_pkg -> True
        Sf_SafeInferred | not trust_own_pkg -> True
        _ | isHomeModule home_unit m -> True
        _ -> isUnitTrusted unit_state (moduleUnit m)
      deps_ok = not (packageTrustOn dflags)
                || all (isUnitIdTrusted unit_state) (Set.toList (dep_trusted_pkgs (mi_deps iface)))
  in safe_mod && safe_pkg && deps_ok

-- | Test the execution route without preflighting, linking, or running code.
tcHasClosedCallExecutable :: Name -> TcM Bool
tcHasClosedCallExecutable name = do
  env <- getTopEnv
  gbl_env <- getGblEnv
  dflags <- getDynFlags
  level <- getThLevel
  boot <- tcIsHsBootOrSig
  explicit <- xoptM LangExt.ExplicitLevelImports
  case (nameModule_maybe name, hsc_interp env, level) of
    (Just mod, Just interp, Comp)
      | not boot, not explicit, not (safeLanguageOn dflags) ->
          if mod == tcg_semantic_mod gbl_env
            then pure (name `elemNameEnv` tcg_local_open_defs gbl_env)
            else if isInteractiveModule mod
              then pure True
              else do
                loc <- getSrcSpanM
                liftIO $ executableDependenciesAvailable interp env loc mod
    (Nothing, Just _, Comp)
      | not boot, not explicit, not (safeLanguageOn dflags)
      , isInteractiveModule (tcg_semantic_mod gbl_env)
      , name `elemNameEnv` tcg_type_env gbl_env
      -> pure True
    _ -> pure False

-- | Check policy at the location where the call occurs.
tcCheckClosedCallContext :: Name -> TcM ()
tcCheckClosedCallContext name = do
  dflags <- getDynFlags
  let reject = failWithTc . TcRnClosedTypeEval name
  unlessXOptM LangExt.DependentTypes $
    reject "Compile-time evaluation in types requires DependentTypes."
  when (safeLanguageOn dflags) $
    reject "Safe Haskell disallows executing imported functions in types."
  level <- getThLevel
  case level of
    Comp -> return ()
    _    -> reject "Closed calls in types are only supported outside quotations and splices."
  whenXOptM LangExt.ExplicitLevelImports $
    reject "Closed calls in types do not yet support ExplicitLevelImports."
  boot <- tcIsHsBootOrSig
  when boot $
    reject "Closed calls in types are not supported in boot files or module signatures."

{- Note [Running imported code in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A closed call loads and executes target code from a compiled module or an
earlier same-module binding group. Apply the same Safe Haskell boundary as
'GHC.Tc.Gen.Annotation.tcAnnotation'; an inferred-safe module loses safe status
when it executes a closed call. Require stage 'Comp' outside quotations,
splices, boot files, and explicit level imports (GHC Proposal #509, Pickering,
Peyton Jones, and Sutherland, 2022). Certified source equations reduce in the
type family solver and never enter this path.
-}

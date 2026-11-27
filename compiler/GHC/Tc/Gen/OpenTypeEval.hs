module GHC.Tc.Gen.OpenTypeEval
  ( OpenTypeFailure(..)
  , DefinitionFailure(MissingTemplate)
  , openTypeFailureReason
  , observeOpenType
  , instantiateOpenTypeFunction
  , openTypeFunctionSignature
  , instantiateOpenMethod
  , instantiateOpenDictMethod
  , readOpenMethodWorker
  , readOpenDefinition
  ) where

import GHC.Prelude

import qualified Data.Set as Set
import Control.Monad (foldM, guard)
import Data.List (elemIndex, sortBy)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import GHC.Builtin.KnownKeys (eqClassKey)
import GHC.Builtin.WiredIn.Types
  ( boolTy, consDataCon, falseDataCon, liftedTypeKind, manyDataConTy
  , mkListTy, nilDataCon, trueDataCon, unmatchableDataConTy )
import GHC.Core.Class (Class, classAllSelIds, classMethods, classTyCon)
import GHC.Core (Alt(..), AltCon(..), CoreExpr, Expr(..), maybeUnfoldingTemplate, mkApps, mkLams)
import GHC.Core.Coercion (mkSubCo, mkSymCo, mkUnbranchedAxInstCo)
import GHC.Core.Coercion.Axiom (Role(Representational), coAxBranchLHS, coAxBranchRHS, coAxiomBranches, fromBranches)
import GHC.Core.Reduction (Reduction(..))
import GHC.Core.DataCon
  ( DataCon, classDataCon, dataConName, dataConRepArgTys, dataConTyCon, dataConUnivTyVars, dataConUserTyVars
  , dataConWorkId, dataConWrapId, isVanillaDataCon, promoteDataCon )
import GHC.Core.InstEnv (instanceDFunId, isIncoherent, lookupUniqueInstEnv)
import GHC.Core.TyCon
  ( TyCon, closedFamilyTyConCoAxiom_maybe, isBoxedDataTyCon, isNewTyCon, isPromotedDataCon_maybe
  , newTyConCo_maybe, tyConArity, tyConDataCons, tyConName )
import GHC.Core.Predicate (classMethodInstTy, getClassPredTys_maybe, isUnaryClass)
import GHC.Core.TyCo.Rep (Scaled(..), Type(AppTy, TermAppTy, TermVarTy, TyVarTy))
import GHC.Core.Type (Kind, PredType, Type, FunTyFlag(FTF_T_T), getTyVar_maybe, isLiftedDataOrNewTyCon, isLiftedTypeKind, isManyTy,
                      isNativeOpaqueValueId, mkTyConApp, mkTyVarTy, mkVisFunTyManyU,
                      splitForAllForAllTyBinders, splitFunTy_maybe, splitTyConApp_maybe,
                      substTyWith, typeKind)
import GHC.Core.Utils (exprType, mkCast)
import GHC.Core.TyCo.Subst (lookupTyVar, substTheta, substTy, zipTvSubst)
import GHC.Core.Unify (tcMatchTys)
import GHC.Data.FastString (fsLit)
import GHC.Data.Maybe (MaybeErr(..), isNothing)
import GHC.Driver.DynFlags (HasDynFlags(getDynFlags), ghcMode, isOneShot, reductionDepth)
import GHC.Driver.Env (HscEnv, hsc_HUG, hsc_all_home_unit_ids, hsc_dflags)
import GHC.Iface.Load (findAndReadIface)
import GHC.Iface.Syntax (IfaceDecl(..), IfaceExpr(..), IfaceInfoItem(..), IfaceOneShot(..), IfaceUnfolding(..))
import GHC.IfaceToCore (tcIfaceExpr)
import GHC.Rename.Env (DeprecationWarnings(NoDeprecationWarnings), addUsedGREs)
import GHC.Tc.Gen.SourceTable (groundFirstHead)
import GHC.Tc.Types (IfL)
import GHC.Tc.Utils.Env (tcGetInstEnvs)
import GHC.Tc.Utils.Monad (TcM, getGblEnv, getTopEnv, initIfaceLcl, initIfaceTcRn,
                           liftIO, newTcRef, newUnique, readTcRef, tcg_imports,
                           tcg_rdr_env, tcg_semantic_mod, tcg_local_open_defs, tcg_local_open_owners, updTcRef)
import GHC.Tc.Utils.TcType (TcTyVarDetails(SkolemTv), isTyFamFree, noFreeVarsOfType, tcEqType, tcSplitDFunTy, tcSplitSigmaTy)
import GHC.Types.Basic (mkIntWithInf)
import GHC.Types.Id (Id, idName, idType, isClassOpId_maybe, mkSysLocal, realIdUnfolding, sourceTableId_maybe)
import GHC.Types.Id.Make (mkDictSelRhs)
import GHC.Types.Name (Name, getOccString, isBuiltInSyntax, isExternalName, nameModule_maybe, stableNameCmp)
import GHC.Types.Unique (hasKey)
import GHC.Types.Name.Env (emptyNameEnv, extendNameEnv_C, lookupNameEnv, nonDetNameEnvElts)
import GHC.Types.Name.Reader (gre_imp, isLocalGRE, is_decl, is_isboot, lookupGRE_Name)
import GHC.Types.Name.Set (NameSet, emptyNameSet, isEmptyNameSet, mkNameSet, nameSetElemsStable,
                           unionNameSet, unitNameSet)
import GHC.Types.SourceFile (isHsBootOrSig)
import GHC.Types.Var (TyVar, Var, ForAllTyFlag(Invisible), VarBndr(..), isGlobalId, isTcTyVar, isTyVar,
                      tcTyVarDetails, tyVarKind, varName, varType)
import GHC.Types.Var.Env (VarEnv, emptyVarEnv, extendVarEnv, lookupVarEnv)
import GHC.Core.OpenWhnf
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Home.ModInfo (hm_iface)
import GHC.Unit.Module (GenWithIsBoot(..), InstalledModule, IsBootInterface(..), Module,
                        getModuleInstantiation, isHoleModule, lookupInstalledModuleEnv,
                        moduleUnit, toUnitId)
import GHC.Unit.Module.Deps (imp_boot_mods)
import GHC.Unit.Module.ModIface (ModIface, mi_decls, mi_hsc_src, mi_semantic_module)
import GHC.Utils.Misc (equalLength)
import GHC.Utils.Outputable (text)

-- | An open application did not produce a checked source type.
data OpenTypeFailure
  = InvalidInput
  | UnavailableDefinition Name DefinitionFailure
  | CannotReduce EvalFailure
  | RigidCase Name
  | GroundTypeCase
  | UnexpectedNeutral
  | HiddenConstructor Name

-- | A checked source argument and its lazy constructor fields.
data OpenArgument
  = OpenRigid Var [OpenArgument]
  | OpenGroundType Type
  | OpenConstructor DataCon [CoreExpr] [OpenArgument]

-- | Symbol table mapping rigid skolems to Core operand Ids and back to source types.
data OperandState = OperandState !(VarEnv Id) !(VarEnv Type)

-- | Apply the instantiated function to symbolic operands of the checked kind.
observeOpenType :: Bool -- ^ Permit opaque ground types for a checked local definition.
                -> Bool -- ^ Check known indexed constructor cases.
                -> CoreExpr -> Kind -> [Type]
                -> TcM (Either OpenTypeFailure (Type, [(Name, Maybe Name)], NameSet))
observeOpenType local_call indexed instantiated result_kind operands
  | Just args <- traverse checkedOperand operands = runExceptT $ do
      depth <- reductionDepth <$> lift getDynFlags
      let limit | indexed = min depth (mkIntWithInf 2048)
                | otherwise = depth
      (OperandState _ origins, revArgs) <- lift $
        foldM buildArgument (OperandState emptyVarEnv emptyVarEnv, []) args
      let coreArgs = reverse revArgs
          expected = foldr (mkVisFunTyManyU . exprType) result_kind coreArgs
          application = mkApps instantiated coreArgs
      if exprType instantiated `tcEqType` expected
         && exprType application `tcEqType` result_kind
        then do
          definitions <- lift (newTcRef emptyVarEnv)
          uses <- lift (newTcRef emptyNameEnv)
          env <- lift getGblEnv
          let locals = tcg_local_open_defs env
              record name first = lift $ updTcRef uses $ \seen ->
                let !entry = (name, maybe emptyNameSet unitNameSet first)
                    !seen' = extendNameEnv_C combine seen name entry
                in seen'
              combine new@(name, kinds) old@(_, old_kinds)
                | isEmptyNameSet kinds = new
                | isEmptyNameSet old_kinds = old
                | otherwise =
                    let !merged = kinds `unionNameSet` old_kinds
                    in (name, merged)
              local v = case lookupNameEnv (tcg_local_open_owners env) (idName v) of
                Nothing -> pure ()
                Just owner -> record (idName owner) Nothing
              remember v rhs = do
                lift $ updTcRef definitions $ \cache ->
                  let !cache' = extendVarEnv cache v rhs in cache'
                pure (Just rhs)
              obtain v first_type = do
                known <- lift (readTcRef definitions)
                result <- case lookupVarEnv known v of
                  Just rhs -> pure (Just rhs)
                  Nothing
                    | Just (_, rhs) <- lookupNameEnv locals (idName v) ->
                        if sameTemplateType (exprType rhs) (idType v)
                          then remember v rhs
                          else throwE (UnavailableDefinition (idName v) WrongTemplateType)
                    | Just cls <- isClassOpId_maybe v
                    , not (isUnaryClass cls)
                    , Just tmpl <- maybeUnfoldingTemplate (realIdUnfolding v)
                    , sameTemplateType (exprType tmpl) (idType v) ->
                        remember v tmpl
                    | Just cls <- isClassOpId_maybe v
                    , isUnaryClass cls -> do
                        mb_tmpl <- lift (unaryClassSelRhs cls)
                        case mb_tmpl of
                          Just tmpl | sameTemplateType (exprType tmpl) (idType v) -> remember v tmpl
                          _ -> throwE (UnavailableDefinition (idName v) WrongTemplateType)
                    | Just cls <- isClassOpId_maybe v
                    , not (isUnaryClass cls)
                    , Just val_index <- elemIndex v (classAllSelIds cls) ->
                        let tmpl = mkDictSelRhs cls val_index
                        in if sameTemplateType (exprType tmpl) (idType v)
                             then remember v tmpl
                             else throwE (UnavailableDefinition (idName v) WrongTemplateType)
                    | not (isGlobalId v) -> pure Nothing
                    | otherwise -> do
                        definition <- lift (readOpenDefinition v)
                        case definition of
                          Left failure -> throwE (UnavailableDefinition (idName v) failure)
                          Right rhs -> remember v rhs
                case result of
                  Just _ | isExternalName (idName v) ->
                    record (idName v) (first_type >>= groundFirstHead . (:[]))
                  _ -> pure ()
                pure result
          let origin v = lookupVarEnv origins v
              constructorGRE con = lookupGRE_Name (tcg_rdr_env env) (dataConName con)
              visible con = pure $ isBuiltInSyntax (dataConName con) || case constructorGRE con of
                Just gre -> isLocalGRE gre || any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
                Nothing -> False
          outcome <- openReadbackM local_call indexed limit obtain local origin visible result_kind application
          case outcome of
            Left (ReadbackEvaluation failure) -> throwE (CannotReduce failure)
            Left (ReadbackHidden con) -> throwE (HiddenConstructor (tyConName (dataConTyCon con)))
            Left (ReadbackOpaque (Neutral (CaseBlocked n _)))
              | Just actual <- neutralRigidVar n
              , Just ty <- origin actual ->
                  case ty of
                    TyVarTy skolem -> throwE (RigidCase (varName skolem))
                    TermVarTy idx  -> throwE (RigidCase (varName idx))
                    _              -> throwE GroundTypeCase
            Left (ReadbackOpaque _) -> throwE UnexpectedNeutral
            Right (result, constructors) -> do
              dependencies <- lift (readTcRef uses)
              let ordered = sortBy (\(n, _) (m, _) -> stableNameCmp n m) (nonDetNameEnvElts dependencies)
                  flatten (n, kinds)
                    | isEmptyNameSet kinds = [(n, Nothing)]
                    | otherwise = [(n, Just k) | k <- nameSetElemsStable kinds]
                  declarations = mkNameSet
                    [ name | con <- constructors, name <- [dataConName con, tyConName (dataConTyCon con)] ]
              lift (addUsedGREs NoDeprecationWarnings
                [gre | con <- constructors, Just gre <- [constructorGRE con]])
              pure (result, concatMap flatten ordered, declarations)
        else throwE InvalidInput
  where
    rigid v
      | isTyVar v
      = isTcTyVar v && (case tcTyVarDetails v of { SkolemTv{} -> True; _ -> False })
        && isLiftedTypeKind (typeKind (tyVarKind v))
      | otherwise
      = isNativeOpaqueValueId v && isLiftedTypeKind (typeKind (varType v))

    splitOpenRigid (TyVarTy tv) | rigid tv = Just (tv, [])
    splitOpenRigid (TermVarTy v) | rigid v = Just (v, [])
    splitOpenRigid (TermAppTy f a) | rigid f = Just (f, [a])
    splitOpenRigid (AppTy fun arg) = do
      (hd, args) <- splitOpenRigid fun
      pure (hd, args ++ [arg])
    splitOpenRigid _ = Nothing

    kindsMatch tys expected =
      equalLength tys expected &&
      and (zipWith (\t exp_k -> typeKind t `tcEqType` exp_k) tys expected)

    checkedOperand ty
      | Just (v, args) <- splitOpenRigid ty
      = OpenRigid v <$> checkArgs (varType v) args
      | Just (promoted, args) <- splitTyConApp_maybe ty
      , Just con <- isPromotedDataCon_maybe promoted
      , Just (parent, parameters) <- splitTyConApp_maybe (typeKind ty)
      , parent == dataConTyCon con
      , Just field_types <- openDataConFieldTypes con parameters
      , let (prefix, fields) = splitAt (length parameters) args
      , equalLength prefix parameters
      , and (zipWith tcEqType prefix parameters)
      , kindsMatch fields field_types
      = OpenConstructor con (map Type parameters) <$> traverse checkedOperand fields
      | indexed
      , Just (promoted, args) <- splitTyConApp_maybe ty
      , Just con <- isPromotedDataCon_maybe promoted
      , let (source, fields) = splitAt (length (dataConUserTyVars con)) args
      , Just (worker, proofs, field_types) <- openIndexedDataCon con (typeKind ty) source
      , kindsMatch fields field_types
      = OpenConstructor con (map Type worker ++ map Coercion proofs) <$> traverse checkedOperand fields
      | local_call, isLiftedTypeKind (typeKind ty)
      , noFreeVarsOfType ty, isTyFamFree ty
      = Just (OpenGroundType ty)
      | otherwise = Nothing

    -- Reflect the same unmatchable applications that readback can produce.
    -- See Note [Readback of a rigid callback application] in GHC.Core.OpenWhnf.
    checkArgs _ [] = Just []
    checkArgs kind (arg : rest) = do
      (FTF_T_T, ma, mult, domain, result) <- splitFunTy_maybe kind
      guard (ma `tcEqType` unmatchableDataConTy && isManyTy mult && typeKind arg `tcEqType` domain)
      (:) <$> checkedOperand arg <*> checkArgs result rest

    buildArgument (!state, args) operand = do
      (!state', arg) <- buildOperand state operand
      pure (state', arg : args)

    buildOperand state (OpenRigid v args) = do
      (state', head_expr) <- symbolFor state v
      (final, revArgs) <- foldM buildArgument (state', []) args
      pure (final, mkApps head_expr (reverse revArgs))
    -- See Note [Ground type operands of local calls].
    buildOperand (OperandState symbols origins) (OpenGroundType ty) = do
      symbol <- freshSymbol liftedTypeKind
      pure (OperandState symbols (extendVarEnv origins symbol ty), Var symbol)
    buildOperand state (OpenConstructor con erased fields) = do
      (state', revFields) <- foldM buildArgument (state, []) fields
      let fieldExprs = reverse revFields
          tc         = dataConTyCon con
      case (newTyConCo_maybe tc, traverse (\case Type t -> Just t; _ -> Nothing) erased, fieldExprs) of
        (Just nt_ax, Just tys, [fieldExpr]) | isNewTyCon tc ->
          pure (state', mkCast fieldExpr (mkSymCo (mkUnbranchedAxInstCo Representational nt_ax tys [])))
        _ ->
          pure (state', mkApps (Var (dataConWorkId con)) (erased ++ fieldExprs))

    symbolFor state@(OperandState symbols origins) v =
      case lookupVarEnv symbols v of
        Just symbol -> pure (state, Var symbol)
        Nothing -> do
          symbol <- freshSymbol (varType v)
          let src_ty = if isTyVar v then mkTyVarTy v else TermVarTy v
          pure (OperandState (extendVarEnv symbols v symbol)
                             (extendVarEnv origins symbol src_ty),
                Var symbol)

    freshSymbol kind = do
      unique <- newUnique
      pure (mkSysLocal (fsLit "open-type-operand") unique manyDataConTy kind)
observeOpenType _ _ _ _ _ = pure (Left InvalidInput)

{- Note [Ground type operands of local calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A checked local definition can project an ordinary type such as Bool.
Like a skolem of kind Type, the type is an opaque operand of the
observer. Store its checked source type as the symbol's origin.
Readback can return that exact type. Evaluation cannot inspect the
source type or treat it as a runtime constructor. A case demanding
its value remains stuck. Keep runtime literal evaluation on the
ordinary imported-code path.
-}

-- | A demanded external definition was unavailable to the observer.
data DefinitionFailure
  = UnavailableModule
  | UnavailableLocalMethod
  | CannotReadInterface
  | MissingTemplate
  | WrongTemplateType
  deriving (Eq, Show)

-- | Recover the polymorphic or monomorphic signature of an open source function.
openTypeFunctionSignature :: Bool -> Id -> Maybe ([TyVar], [Kind], Kind, CoreExpr -> CoreExpr)
openTypeFunctionSignature local_families function
  | all liftedUniversal binders
  = do (wrap, (domains, result)) <- case termKinds body of
         Just kinds -> Just (id, kinds)
         Nothing | local_families -> do
           let Reduction _ reduced = openFamilyReduction body
           kinds <- termKinds reduced
           let wrap expr =
                 let Reduction co _ = openFamilyReduction (exprType expr)
                 in mkCast expr (mkSubCo co)
           pure (wrap, kinds)
         _ -> Nothing
       guard (not (null domains))
       pure (tvs, domains, result, wrap)
  | otherwise = Nothing
  where
    (binders, body) = splitForAllForAllTyBinders (idType function)
    tvs = [v | (_, Bndr v _) <- binders]
    binder_tys = map mkTyVarTy tvs
    bound ty = any (tcEqType ty) binder_tys
    domainKind ty
      | bound ty = Just ty
      | Just (FTF_T_T, ma, mult, a, b) <- splitFunTy_maybe ty
      , ma `tcEqType` unmatchableDataConTy
      , isManyTy mult
      = mkVisFunTyManyU <$> domainKind a <*> domainKind b
      | Just (con, parameters) <- splitTyConApp_maybe ty
      , isLiftedDataOrNewTyCon con
      , tyConArity con == length parameters
      , all isVanillaDataCon (tyConDataCons con)
      = mkTyConApp con <$> traverse domainKind parameters
      | otherwise = Nothing
    termKinds ty
      | Just (FTF_T_T, matchability, multiplicity, domain, result) <- splitFunTy_maybe ty
      , matchability `tcEqType` unmatchableDataConTy
      , isManyTy multiplicity = do
          input <- domainKind domain
          (inputs, output) <- termKinds result
          pure (input : inputs, output)
      | otherwise = ([],) <$> domainKind ty

-- | Instantiate the type binders at 'Type' and recover the checked input and result kinds.
instantiateOpenTypeFunction :: Bool -> Id -> Maybe (CoreExpr, [Kind], Kind)
instantiateOpenTypeFunction local_families function = do
  (tvs, domains, result, wrap) <- openTypeFunctionSignature local_families function
  let inst = substTyWith tvs (liftedTypeKind <$ tvs)
      applied = foldl' (\expr _ -> App expr (Type liftedTypeKind)) (Var function) tvs
  pure (wrap applied, map inst domains, inst result)

-- | Instantiate a selected binary method using the kinds of both operands.
instantiateOpenMethod :: Id -> Kind -> Kind -> Maybe (CoreExpr, Kind)
instantiateOpenMethod worker left right = do
    let (binders, body) = splitForAllForAllTyBinders (idType worker)
    guard (all liftedUniversal binders)
    (FTF_T_T, _, _, callback, rest) <- splitFunTy_maybe body
    (FTF_T_T, _, _, receiver, result) <- splitFunTy_maybe rest
    subst <- tcMatchTys [callback, receiver] [left, right]
    args <- traverse (\(_, Bndr v _) -> lookupTyVar subst v) binders
    guard (all (isLiftedTypeKind . typeKind) args)
    let method = mkApps (Var worker) (map Type args)
        result_kind = substTy subst result
    guard (exprType method `tcEqType` mkVisFunTyManyU left (mkVisFunTyManyU right result_kind))
    pure (method, result_kind)

-- | Specialize a class method by recursively resolving its dictionary constraints
-- in 'InstEnv' so 'observeOpenType' can unfold the dictionary directly in Core.
instantiateOpenDictMethod :: Id -> [Type] -> [Kind] -> Kind -> TcM (Maybe CoreExpr)
instantiateOpenDictMethod fun ty_args doms result_kind
  | let (ma_tvs, theta, _) = tcSplitSigmaTy (idType fun)
        tvs                = map snd ma_tvs
  , equalLength tvs ty_args
  , not (null theta) = do
      let inst_theta = substTheta (zipTvSubst tvs ty_args) theta
          expected   = foldr mkVisFunTyManyU result_kind doms
      mb_dicts <- traverse (resolveDict 16) inst_theta
      pure $ do
        dicts <- sequence mb_dicts
        let specialized = mkApps (Var fun) (map Type ty_args ++ dicts)
        specialized <$ guard (exprType specialized `tcEqType` expected)
  | otherwise = pure Nothing
  where
    resolveDict :: Int -> PredType -> TcM (Maybe CoreExpr)
    resolveDict depth pred_ty
      | depth <= 0 = pure Nothing
      | Just (cls, tys) <- getClassPredTys_maybe pred_ty
      , all isTyFamFree tys = do
          insts <- tcGetInstEnvs
          env   <- getGblEnv
          case lookupUniqueInstEnv insts cls tys of
            Right (inst, inst_ty_args)
              | not (isIncoherent inst) -> do
                  let dfun      = instanceDFunId inst
                      dfun_name = idName dfun
                      is_local  = nameModule_maybe dfun_name == Just (tcg_semantic_mod env)
                  if is_local && isNothing (lookupNameEnv (tcg_local_open_defs env) dfun_name)
                    then pure Nothing
                    else do
                      let (d_ma_tvs, d_theta, _) = tcSplitSigmaTy (idType dfun)
                          d_tvs                  = map snd d_ma_tvs
                      if not (equalLength d_tvs inst_ty_args)
                        then pure Nothing
                        else do
                          let sub_preds = substTheta (zipTvSubst d_tvs inst_ty_args) d_theta
                          mb_subs <- traverse (resolveDict (depth - 1)) sub_preds
                          pure $ do
                            subs <- sequence mb_subs
                            let d_expr = mkApps (Var dfun) (map Type inst_ty_args ++ subs)
                            d_expr <$ guard (exprType d_expr `tcEqType` pred_ty)
            _ -> pure Nothing
      | otherwise = pure Nothing

-- | An ordinary erased parameter of lifted kind.
liftedUniversal :: (Type, VarBndr TyVar ForAllTyFlag) -> Bool
liftedUniversal (_, Bndr v (Invisible _)) =
    isTyVar v && tyVarKind v `tcEqType` liftedTypeKind
liftedUniversal _ = False

{- Note [Observing an imported method worker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The kind of a mapping operand determines the class parameter. It does
not determine the dictionary. The caller uses the solver with the
signature and lexical Givens. It must select the exact imported DFun.

Read the DFun from its full interface, which keeps the unfolding even
when the reader ignores interface pragmas. The unfolding stores the
constructor arguments in class selector order. Accept a dictionary
with no binders, and use the selector's slot only when it names an
imported worker of the specialized method type.

The caller records the selector, dictionary, and worker for rule checks
and recompilation. The observer reads the worker body on demand and
records any further calls it follows. A method can return a constructor
without inspecting its callback. A rigid callback with a type-variable origin
can also occur in a returned field. The next call can use that field as
a symbolic input. A case that needs its value remains stuck.
-}

-- | Read the direct method field of the imported ground dictionary.
-- See Note [Observing an imported method worker].
readOpenMethodWorker :: Id -> Id -> TcM (Either OpenTypeFailure Id)
readOpenMethodWorker dfun selector = do
    info <- readOpenIdInfo dfun
    case info of
      Left failure -> unavailable failure
      Right (mod, items)
        | Just (cls, [domain]) <- getClassPredTys_maybe (idType dfun)
        , let selectors = classAllSelIds cls
        , [parameter : fields] <- [ops | HsUnfold _ (IfDFunUnfold [] ops) <- items]
        , equalLength selectors fields
        , Just field@IfaceExt{} <- lookup selector (zip selectors fields)
        -> do
          args <- readOpenIfaceExpr mod (traverse tcIfaceExpr [parameter, field])
          case args of
            [Type actual, Var worker]
              | actual `tcEqType` domain, isGlobalId worker
              , idType worker `tcEqType` classMethodInstTy selector [domain] ->
                  pure (Right worker)
            _ -> unavailable WrongTemplateType
      _ -> unavailable MissingTemplate
  where
    unavailable = pure . Left . UnavailableDefinition (idName dfun)

-- | Read the checked expression for one demanded ordinary import.
-- Use a full interface because EPS may discard the optimizer's IdInfo.
-- The caller records the Id Name when it consumes the returned definition.
readOpenDefinition :: Id -> TcM (Either DefinitionFailure CoreExpr)
readOpenDefinition identifier = do
  info <- readOpenIdInfo identifier
  let fallback = do
        mb_st <- case sourceTableId_maybe identifier of
          Just table -> sourceTableCoreTemplate identifier table
          Nothing    -> pure Nothing
        case mb_st of
          Just tmpl -> pure (Right tmpl)
          Nothing   -> maybe (Left MissingTemplate) Right <$> stdEqListCoreTemplate identifier
  case info of
    Left failure -> do
      fb <- fallback
      pure $ case fb of
        Right tmpl -> Right tmpl
        Left _     -> Left failure
    Right (mod, items)
      | [template] <- [template | HsUnfold _ (IfCoreUnfold _ _ _ template) <- items] -> do
          body <- readOpenIfaceExpr mod (tcIfaceExpr template)
          pure $ if sameTemplateType (exprType body) (idType identifier)
            then Right body
            else Left WrongTemplateType
      | [IfDFunUnfold bs ops] <- [df | HsUnfold _ df@IfDFunUnfold{} <- items]
      , let (_, _, cls, _) = tcSplitDFunTy (idType identifier)
            con_name       = idName (dataConWrapId (classDataCon cls))
            iface_rhs      = foldr (\b e -> IfaceLam (b, IfaceNoOneShot) e)
                                   (foldl' IfaceApp (IfaceExt con_name) ops) bs -> do
          body <- readOpenIfaceExpr mod (tcIfaceExpr iface_rhs)
          pure $ if sameTemplateType (exprType body) (idType identifier)
            then Right body
            else Left WrongTemplateType
      | otherwise -> fallback

-- | Compare a Core template's type against its declared 'Id' type, ignoring
-- 'Inferred' versus 'Specified' binder specificity introduced by Core 'Lam'.
sameTemplateType :: Type -> Type -> Bool
sameTemplateType t1 t2
  | tcEqType t1 t2 = True
  | (b1, r1) <- splitForAllForAllTyBinders t1
  , (b2, r2) <- splitForAllForAllTyBinders t2
  , not (null b1), equalLength b1 b2
  , let vs1 = [v | (_, Bndr v _) <- b1]
        vs2 = [v | (_, Bndr v _) <- b2]
  , and (zipWith (\v1 v2 -> tcEqType (tyVarKind v1) (tyVarKind v2)) vs1 vs2)
  = sameTemplateType (substTyWith vs1 (map mkTyVarTy vs2) r1) r2
  | otherwise = False

-- | Synthesize the dictionary-unwrap selector expression for a unary class method.
unaryClassSelRhs :: Class -> TcM (Maybe CoreExpr)
unaryClassSelRhs cls
  | isUnaryClass cls
  , let tc = classTyCon cls
  , [data_con] <- tyConDataCons tc
  , [Scaled _ field_ty] <- dataConRepArgTys data_con
  = do
      u1 <- newUnique
      u2 <- newUnique
      let tvs      = dataConUnivTyVars data_con
          tv_tys   = map mkTyVarTy tvs
          pred_ty  = mkTyConApp tc tv_tys
          dict_id  = mkSysLocal (fsLit "dict") u1 manyDataConTy pred_ty
          field_id = mkSysLocal (fsLit "meth") u2 manyDataConTy field_ty
          rhs_body = case newTyConCo_maybe tc of
            Just nt_ax -> mkCast (Var dict_id) (mkUnbranchedAxInstCo Representational nt_ax tv_tys [])
            Nothing    -> Case (Var dict_id) dict_id field_ty [Alt (DataAlt data_con) [field_id] (Var field_id)]
      pure (Just (mkLams tvs (Lam dict_id rhs_body)))
  | otherwise = pure Nothing

-- | Synthesize the canonical Core equation for @GHC.Internal.Classes.$fEqList_$c==@
-- when loop-breaker pruning omits its unfolding from @Classes.hi@.
stdEqListCoreTemplate :: Id -> TcM (Maybe CoreExpr)
stdEqListCoreTemplate owner
  | getOccString (idName owner) == "$fEqList_$c=="
  , ([(_, tv)], [pred_ty], rho) <- tcSplitSigmaTy (idType owner)
  , Just (eq_cls, [tv_ty]) <- getClassPredTys_maybe pred_ty
  , eq_cls `hasKey` eqClassKey
  , tv_ty `tcEqType` mkTyVarTy tv
  , eq_op : _ <- classMethods eq_cls
  = do
      let list_ty = mkListTy tv_ty
          fresh lbl ty = (\u -> mkSysLocal (fsLit lbl) u manyDataConTy ty) <$> newUnique
      d_eq <- fresh "dEq" pred_ty
      xs   <- fresh "xs" list_ty
      ys   <- fresh "ys" list_ty
      w1   <- fresh "w1" list_ty
      w2   <- fresh "w2" list_ty
      w3   <- fresh "w3" list_ty
      wb   <- fresh "wb" boolTy
      x    <- fresh "x" tv_ty
      xt   <- fresh "xt" list_ty
      y    <- fresh "y" tv_ty
      yt   <- fresh "yt" list_ty
      y0   <- fresh "y0" tv_ty
      yt0  <- fresh "yt0" list_ty
      let f_expr   = Var (dataConWorkId falseDataCon)
          t_expr   = Var (dataConWorkId trueDataCon)
          eq_hd    = mkApps (Var eq_op) [Type tv_ty, Var d_eq, Var x, Var y]
          rec_tl   = mkApps (Var owner) [Type tv_ty, Var d_eq, Var xt, Var yt]
          nil_rhs  = Case (Var ys) w2 boolTy
                       [ Alt (DataAlt nilDataCon)  []         t_expr
                       , Alt (DataAlt consDataCon) [y0, yt0]  f_expr ]
          cons_rhs = Case (Var ys) w3 boolTy
                       [ Alt (DataAlt nilDataCon)  []       f_expr
                       , Alt (DataAlt consDataCon) [y, yt]
                           (Case eq_hd wb boolTy
                              [ Alt (DataAlt falseDataCon) [] f_expr
                              , Alt (DataAlt trueDataCon)  [] rec_tl ]) ]
          tmpl     = mkLams [tv, d_eq, xs, ys] $
                       Case (Var xs) w1 boolTy
                         [ Alt (DataAlt nilDataCon)  []      nil_rhs
                         , Alt (DataAlt consDataCon) [x, xt] cons_rhs ]
      pure (if sameTemplateType (exprType tmpl) (idType owner) && rho `tcEqType` mkVisFunTyManyU list_ty (mkVisFunTyManyU list_ty boolTy)
              then Just tmpl
              else Nothing)
  | otherwise = pure Nothing

-- | Reconstruct a pure Core template from a single-column certified 'SourceTable'
-- when an imported function omits its 'IfCoreUnfold' due to phase pragmas (e.g. '++').
sourceTableCoreTemplate :: Id -> TyCon -> TcM (Maybe CoreExpr)
sourceTableCoreTemplate owner table
  | Just axiom <- closedFamilyTyConCoAxiom_maybe table
  , let branches = fromBranches (coAxiomBranches axiom)
        (bndrs, rho) = splitForAllForAllTyBinders (idType owner)
        tvs = [tv | (_, Bndr tv (Invisible _)) <- bndrs]
  , not (null branches), equalLength tvs bndrs
  , Just (doms, res_ty) <- peelValArrows rho
  , not (null doms) = do
      arg_ids <- mapM (freshLocal "st_arg") doms
      wild_ids <- mapM (freshLocal "st_wild") doms
      let try_col col_idx
            | Just (tc, tc_args) <- splitTyConApp_maybe (doms !! col_idx)
            , isBoxedDataTyCon tc = do
                mb_alts <- traverse (build_alt col_idx tc tc_args arg_ids) branches
                pure $ do
                  alts <- sequence mb_alts
                  let sorted = sortBy (\(Alt c1 _ _) (Alt c2 _ _) -> compare c1 c2) alts
                      scrut  = arg_ids !! col_idx
                      wild   = wild_ids !! col_idx
                      tmpl   = mkLams (tvs ++ arg_ids) (Case (Var scrut) wild res_ty sorted)
                  tmpl <$ guard (sameTemplateType (exprType tmpl) (idType owner))
            | otherwise = pure Nothing
      candidates <- mapM try_col [0 .. length doms - 1]
      pure (foldr (\a b -> case a of { Just{} -> a; Nothing -> b }) Nothing candidates)
  | otherwise = pure Nothing
  where
    freshLocal :: String -> Type -> TcM Id
    freshLocal lbl ty = do
      u <- newUnique
      pure (mkSysLocal (fsLit lbl) u manyDataConTy ty)

    peelValArrows :: Type -> Maybe ([Type], Type)
    peelValArrows ty
      | Just (FTF_T_T, ma, mult, d, r) <- splitFunTy_maybe ty
      , ma `tcEqType` unmatchableDataConTy, isManyTy mult =
          case peelValArrows r of
            Just (ds, out) -> Just (d : ds, out)
            Nothing        -> Just ([d], r)
      | otherwise = Just ([], ty)

    build_alt col_idx tc tc_args arg_ids br
      | let (q_tys, lhs_pats) = splitAt (length tvs) (coAxBranchLHS br)
      , equalLength lhs_pats arg_ids
      , Just q_tvs <- traverse getTyVar_maybe q_tys
      , Just (prom_dc, dc_all_args) <- splitTyConApp_maybe (lhs_pats !! col_idx)
      , Just dc <- isPromotedDataCon_maybe prom_dc
      , dataConTyCon dc == tc
      , let (dc_univ, dc_field_pats) = splitAt (length tc_args) dc_all_args
      , equalLength dc_univ tc_args
      , Just field_tvs <- traverse getTyVar_maybe dc_field_pats
      , Just other_tvs <- sequence [ getTyVar_maybe p | (i, p) <- zip [0 ..] lhs_pats, i /= col_idx ]
      = do
          let ty_subst = zipTvSubst q_tvs (map mkTyVarTy tvs)
          case openDataConFieldTypes dc tc_args of
            Just field_tys | equalLength field_tys field_tvs -> do
              field_ids <- mapM (freshLocal "st_f") field_tys
              let other_ids = [ a | (i, a) <- zip [0 ..] arg_ids, i /= col_idx ]
                  term_env  = zip other_tvs (map Var other_ids) ++ zip field_tvs (map Var field_ids)
              pure $ Alt (DataAlt dc) field_ids <$> trans_rhs ty_subst term_env (coAxBranchRHS br)
            _ -> pure Nothing
      | otherwise = pure Nothing
      where
        (bndrs, _) = splitForAllForAllTyBinders (idType owner)
        tvs        = [tv | (_, Bndr tv (Invisible _)) <- bndrs]

    trans_rhs ty_subst term_env rhs_ty
      | Just tv <- getTyVar_maybe rhs_ty = lookup tv term_env
      | Just (prom_c, c_args) <- splitTyConApp_maybe rhs_ty
      , Just c <- isPromotedDataCon_maybe prom_c = do
          let (c_univ, c_fields) = splitAt (length (dataConUnivTyVars c)) c_args
          val_args <- traverse (trans_rhs ty_subst term_env) c_fields
          pure (mkApps (Var (dataConWorkId c)) (map (Type . substTy ty_subst) c_univ ++ val_args))
      | Just (fam, f_args) <- splitTyConApp_maybe rhs_ty
      , fam == table = do
          let n_tvs = length [tv | (_, Bndr tv (Invisible _)) <- fst (splitForAllForAllTyBinders (idType owner))]
              (f_tys, f_vals) = splitAt n_tvs f_args
          val_args <- traverse (trans_rhs ty_subst term_env) f_vals
          pure (mkApps (Var owner) (map (Type . substTy ty_subst) f_tys ++ val_args))
      | otherwise = Nothing

-- | Read an imported declaration with its optimizer information.
readOpenIdInfo :: Id -> TcM (Either DefinitionFailure (Module, [IfaceInfoItem]))
readOpenIdInfo identifier = do
  hscEnv <- getTopEnv
  tcgEnv <- getGblEnv
  let requested = idName identifier
  case nameModule_maybe requested of
    Just mod
      | mod == tcg_semantic_mod tcgEnv, Just _ <- isClassOpId_maybe identifier
      -> pure (Left UnavailableLocalMethod)
    Just mod
      | mod /= tcg_semantic_mod tcgEnv
      , not (isHoleModule mod)
      , (installed, Nothing) <- getModuleInstantiation mod
      , not (isBootImport tcgEnv installed) -> do
          readResult <- liftIO (openDefinitionIface hscEnv mod installed)
          case readResult of
            Left failure -> pure (Left failure)
            Right iface
              | isHsBootOrSig (mi_hsc_src iface) -> pure (Left UnavailableModule)
              | otherwise ->
                  case [ info
                       | (_, IfaceId { ifName = name, ifIdInfo = info }) <- mi_decls iface
                       , name == requested
                       ] of
                    [info] -> pure (Right (mi_semantic_module iface, info))
                    _ -> pure (Left MissingTemplate)
    _ -> pure (Left UnavailableModule)
  where
    isBootImport env installed =
      case lookupInstalledModuleEnv (imp_boot_mods (tcg_imports env)) installed of
        Just dep -> gwib_isBoot dep == IsBoot
        Nothing -> False

-- | Read an expression in the module that supplied its interface.
readOpenIfaceExpr :: Module -> IfL a -> TcM a
readOpenIfaceExpr mod readExpr = initIfaceTcRn $
    initIfaceLcl mod (text "open term definition") NotBoot readExpr

-- | Read a complete interface without repopulating the external package table.
-- The compilation manager already has complete interfaces for its loaded
-- home modules. Do not reopen unloaded home modules from disk in that mode.
openDefinitionIface :: HscEnv -> Module -> InstalledModule -> IO (Either DefinitionFailure ModIface)
openDefinitionIface hscEnv mod installed = do
  fromHome <- HUG.lookupHugByModule mod (hsc_HUG hscEnv)
  case fromHome of
    Just hmi -> pure (Right (hm_iface hmi))
    Nothing
      | not (isOneShot (ghcMode (hsc_dflags hscEnv)))
      , toUnitId (moduleUnit mod) `Set.member` hsc_all_home_unit_ids hscEnv ->
          pure (Left UnavailableModule)
      | otherwise -> do
          loaded <- findAndReadIface hscEnv (text "open term definition") installed mod NotBoot
          case loaded of
            Failed _ -> pure (Left CannotReadInterface)
            Succeeded (iface, _) -> pure (Right iface)

-- | Describe why a candidate cannot elaborate to a type.
openTypeFailureReason :: OpenTypeFailure -> (Maybe Name, String)
openTypeFailureReason InvalidInput =
  (Nothing, "The open evaluator could not represent an operand at the required kind. " ++
    "Use rigid variables or ordinary boxed constructors with lazy fields for symbolic inputs. " ++
    "The evaluator instantiates lifted polymorphic parameters at Type.")
openTypeFailureReason (UnavailableDefinition name failure) =
  (Just name, case failure of
    UnavailableModule -> "A referenced definition does not come from an ordinary imported module."
    UnavailableLocalMethod -> "Evaluation needs a local class method whose definition is unavailable at this call."
    CannotReadInterface -> "The interface for a referenced definition could not be read."
    MissingTemplate -> "The interface for a referenced definition contains no usable Core unfolding. " ++
      "Use an interface that exposes the unfolding or provides a checked equation for this call."
    WrongTemplateType -> "The type of a referenced unfolding disagrees with its declared type.")
openTypeFailureReason (CannotReduce failure) =
  (Nothing, case failure of
    Exhausted -> "Reduction exceeded the open evaluation step limit (-freduction-depth)."
    UnsupportedCore -> "Reduction reached an unsupported Core expression."
    MissingAlternative -> "Reduction reached a case without a matching alternative.")
openTypeFailureReason (RigidCase scrutinee) =
  (Nothing, "Reduction stopped at a case on the rigid argument " ++ getOccString scrutinee ++
    ". A checked equation can retain this case until the argument's constructor is known.")
openTypeFailureReason GroundTypeCase =
  (Nothing, "Reduction cannot inspect the value of a type operand. A type operand supplies no runtime value.")
openTypeFailureReason (HiddenConstructor name) =
  (Nothing, "A result constructor of " ++ getOccString name ++ " is not in scope through an ordinary import.")
openTypeFailureReason UnexpectedNeutral =
  (Nothing, "Reduction did not produce a supplied rigid variable, an application of a supplied type-level callback, or a finite constructor value with supported fields.")

{- Note [Open source calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a type expression applies a term function to open operands (rigid type
variables or constructors with symbolic fields), 'observeOpenType' evaluates
the instantiated Core unfolding symbolically to weak head normal form and
reads back a 'Type' (see Eisenberg and Stolarek, "Promoting Functions to Type
Families in Haskell", Haskell Symposium 2014).

Admit a lifted invisible @forall@ telescope and saturated unrestricted arrows,
instantiating each parameter at 'Type'. Construct a symbolic pair only when the
source supplies a pair constructor; a bare variable of pair kind stays neutral
so projections on it block. Each distinct skolem receives a fresh symbolic 'Id'
in the observer and reifies back by identity without emitting residual Core.

Read unfoldings directly from full module interfaces so observation remains
independent of @-fignore-interface-pragmas@ at @-O0@. Record only definitions
unfolded during evaluation for rewrite-rule validation and recompilation
tracking, and bound evaluation steps by @-freduction-depth@.
-}

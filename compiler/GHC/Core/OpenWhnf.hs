-- | Demand-driven call-by-need weak-head normalizer and finite value readback
-- for open Core expressions with rigid variables and GADT equalities.
--
-- See Note [Observing open Core] and Note [Observing indexed constructor cases].
module GHC.Core.OpenWhnf
  ( EvalFailure(..)
  , Whnf(..)
  , Neutral(..)
  , Suspension
  , CaseContinuation(..)
  , openWhnfM, openWhnfWithLocalM, openWhnfTraceM, CaseStep(..)
  , checkedMultiplicityCast
  , openFamilyReduction
  , openDataConFieldTypes, openIndexedDataCon, openReadbackM, OpenReadbackFailure(..)
  , neutralRigidVar
  ) where

import GHC.Prelude

import Control.Monad (foldM, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, mapStateT, put)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isJust, isNothing)
import GHC.Builtin.PrimOps (PrimOp(..))
import GHC.Builtin.WiredIn.Types
  (charDataCon, mkPromotedPairTy, pairTyCon, tupleDataCon, unboxedUnitDataCon, unboxedUnitTy, unmatchableDataConTy)
import GHC.Core.Coercion (coercionLKind, coercionRKind, coercionType, isReflCo, mkNomReflCo)
import GHC.Core.FamInstEnv (emptyFamInstEnvs, reduceTyFamApp_maybe)
import GHC.Core.Coercion.Axiom (CoAxiomRule(UnbranchedAxiom), Role(Nominal), coAxiomTyCon)
import GHC.Core.Reduction (Reduction(..), isReflRedn, mkFunRedn, mkReflRedn)
import GHC.Core.DataCon
  ( DataCon, dataConWorkId, dataConTyCon, dataConFullSig, dataConRepArity
  , dataConRepArgTys, dataConExTyCoVars, dataConTagZ, eqSpecPair
  , dataConRepArgBinders, dataConStupidTheta, dataConImplBangs
  , dataConWrapId_maybe, dataConUserTyVars, dataConUnivTyVars, promoteDataCon
  , isBanged, isTypeDataCon )
import GHC.Core (Alt(..), AltCon(..), Bind(..), CoreAlt, CoreExpr, Expr(..), collectArgs, isTyCoArg)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.Opt.Arity (etaExpand)
import GHC.Core.Subst (Subst, extendCvSubst, extendTvSubst, mkEmptySubst, substExpr, substTy)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCon
  ( closedFamilyTyConCoAxiom_maybe, isBoxedDataTyCon
  , isNewTyCon, isUnaryClassTyCon, newTyConCo_maybe, tyConDataCons, tyConResKind, tyConSingleDataCon_maybe )
import GHC.Core.TyCo.Rep
  ( Coercion(..), MCoercion(MRefl), Type(TermVarTy), UnivCoProvenance(SubMultProv)
  , UnivCoDeps(NoUnivCoDeps), scaledThing )
import GHC.Core.Type
  ( FunTyFlag(FTF_T_T), Kind, Type, isConstraintLikeKind, isLiftedTypeKind, isManyTy, isOneTy
  , mkAppTy, mkTyConApp, mkTyVarTy
  , repGetTyVar_maybe, splitForAllForAllTyBinders, splitFunTy_maybe, splitTyConApp_maybe
  , substTyWith, tyCoVarsOfType, typeKind )
import GHC.Core.Utils (exprType, findAlt)
import GHC.Types.Basic (Boxity(Boxed), IntWithInf, intGtLimit)
import GHC.Types.Id (dataConWrapUnfolding_maybe, isDataConWorkId_maybe, isDataConWrapId_maybe, isJoinId, isPrimOpId_maybe)
import GHC.Types.Literal (LitNumType(LitNumInt), Literal(LitChar, LitNumber), mkLitIntUnchecked)
import GHC.Types.Var (Var, VarBndr(..), eqForAllVis, isCoVar, isId, isTyVar, varType)
import GHC.Types.Var.Env (VarEnv, emptyVarEnv, extendVarEnv, extendVarEnvList, lookupVarEnv,
                          mkInScopeSet)
import GHC.Types.Var.Set (VarSet, emptyVarSet, elemVarSet, unionVarSet)

-- | A bounded observation could not produce weak head form.
data EvalFailure
  = Exhausted
  | UnsupportedCore
  | MissingAlternative
  deriving (Eq, Show)

type Env = VarEnv Suspension

-- | Save the expression, the environment at its point of origin, and an
-- optional thunk key for memoized local bindings.
data Suspension = Suspension Env CoreExpr !(Maybe Int)

mkSuspension :: Env -> CoreExpr -> Suspension
mkSuspension env expr = Suspension env expr Nothing

data EvalState = EvalState !Int !Int !(IntMap Whnf)

initEvalState :: EvalState
initEvalState = EvalState 1 1 IntMap.empty

-- | Preserve a case and its environment while its scrutinee is unknown.
data CaseContinuation = CaseContinuation Env Var Type [CoreAlt]

-- | A weak head observation of a typed expression.
data Whnf
  = Lambda Var Suspension
  | PrimDataToTag
  | Literal Literal
  | BoxedChar Char
  | BoxedPair Suspension Suspension
  | BoxedData DataCon [Type] [Suspension]
  | Neutral Neutral
  | IndexedData DataCon [Type] [Type] [Coercion] [Suspension]

-- | A weak head result cannot be returned as a checked source type.
data OpenReadbackFailure
  = ReadbackEvaluation EvalFailure
  | ReadbackOpaque Whnf
  | ReadbackHidden DataCon

-- | An unknown head and its suspended eliminations.
data Neutral
  = Rigid Var
  | Unsolved Var
  | Applied Neutral Suspension
  | NewtypeUnwrap Neutral DataCon [Type]
  | PrimBlocked Neutral
  | CaseBlocked Neutral CaseContinuation

-- | Recover the underlying rigid variable of a neutral head or newtype projection.
neutralRigidVar :: Neutral -> Maybe Var
neutralRigidVar (Rigid v)             = Just v
neutralRigidVar (NewtypeUnwrap n _ _) = neutralRigidVar n
neutralRigidVar (PrimBlocked n)       = neutralRigidVar n
neutralRigidVar _                     = Nothing

-- | A known elimination performed on the observation path.
-- The caller must supply typed Core. A step is not a Core coercion.
data CaseStep
  = SelectBoxedChar Char AltCon
  | SelectBoxedPair AltCon
  | SelectBoxedData DataCon AltCon
  | SelectLiteral Literal AltCon
  | SelectLambdaDefault
  | ContinueWithUnit
  deriving (Eq)

-- | Observe weak head form with a demand-driven definition provider.
-- See Note [Observing open Core].
openWhnfM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> VarSet -> CoreExpr
          -> m (Either EvalFailure Whnf)
openWhnfM limit obtain = openWhnfWithLocalM limit (\v _ -> obtain v) (const (pure ()))

-- | Provide demanded definitions with their first syntactic type argument.
-- Report lexical variables separately when their suspended bindings are demanded.
openWhnfWithLocalM :: Monad m => IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr)) -> (Var -> m ())
                  -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfWithLocalM limit obtain local =
  openWhnfTraceWithLocalM limit obtain local (const (pure ()))

-- | Observe an expression and report case selections and the unboxed unit
-- continuation step in the order in which the observer performs them.
openWhnfTraceM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr))
              -> (CaseStep -> m ()) -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfTraceM limit obtain = openWhnfTraceWithLocalM limit (\v _ -> obtain v) (const (pure ()))

openWhnfTraceWithLocalM :: Monad m => IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr)) -> (Var -> m ())
                       -> (CaseStep -> m ()) -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfTraceWithLocalM limit obtain local record unsolved expr =
  runExceptT $ evalStateT (evaluate False False limit obtain local record unsolved Nothing (mkSuspension emptyVarEnv expr)) initEvalState

-- | Reflect a finite ordinary constructor value or a supplied rigid symbol.
-- The first observation and every demanded field share one reduction budget.
openReadbackM :: Monad m => Bool -> Bool -> IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr))
              -> (Var -> m ()) -> (Var -> Maybe Type) -> (DataCon -> m Bool)
              -> Kind -> CoreExpr
              -> m (Either OpenReadbackFailure (Type, [DataCon]))
openReadbackM local_families indexed limit obtain local origin visible expected expr =
  runExceptT $ evalStateT (readback [] expected (mkSuspension emptyVarEnv expr)) initEvalState
  where
    observe = mapStateT (withExceptT ReadbackEvaluation)
            . evaluate local_families indexed limit obtain local (const (pure ())) emptyVarSet Nothing
    readback used kind suspension = do
      whnf <- observe suspension
      case whnf of
        Neutral (Rigid v)
          | Just ty <- origin v, typeKind ty `eqType` kind -> pure (ty, used)
        Neutral app@Applied{} -> do
          (ty, used') <- readApplication used app
          if typeKind ty `eqType` kind
            then pure (ty, used')
            else lift (throwE (ReadbackOpaque whnf))
        BoxedPair first@(Suspension _ x _) second@(Suspension _ y _)
          | Just (con, [ka, kb]) <- splitTyConApp_maybe kind, con == pairTyCon
          , exprType x `eqType` ka, exprType y `eqType` kb -> do
              (a, used') <- readback used ka first
              (b, used'') <- readback used' kb second
              pure (mkPromotedPairTy ka kb a b, used'')
        BoxedData con arguments fields
          | mkTyConApp (dataConTyCon con) arguments `eqType` kind
          , dataConUserTyVars con == dataConUnivTyVars con
          , Just field_kinds <- openDataConFieldTypes con arguments
          -> readData used kind whnf con arguments field_kinds fields
        IndexedData con arguments _ _ fields
          | Just (_, _, field_kinds) <- openIndexedDataCon con kind arguments
          -> readData used kind whnf con arguments field_kinds fields
        _ -> lift (throwE (ReadbackOpaque whnf))
    readData used kind whnf con arguments field_kinds fields
      | length fields == length field_kinds = do
          in_scope <- lift (lift (visible con))
          if not in_scope then lift (throwE (ReadbackHidden con)) else do
            (rev_tys, used') <- foldM readField ([], con : used) (zip field_kinds fields)
            let result = mkTyConApp (promoteDataCon con) (arguments ++ reverse rev_tys)
            if typeKind result `eqType` kind
              then pure (result, used')
              else lift (throwE (ReadbackOpaque whnf))
      | otherwise = lift (throwE (ReadbackOpaque whnf))
    -- See Note [Readback of a rigid callback application].
    readApplication used neutral = case neutral of
      Rigid v
        | Just f <- origin v
        , isJust (repGetTyVar_maybe f) || isTermVarOrigin f
        , varType v `eqType` typeKind f -> pure (f, used)
      Applied head_neutral field@(Suspension _ operand _) -> do
        (f, used') <- readApplication used head_neutral
        case splitFunTy_maybe (typeKind f) of
          Just (FTF_T_T, ma, mult, domain, _)
            | ma `eqType` unmatchableDataConTy, isManyTy mult
            , not (isTyCoArg operand), exprType operand `eqType` domain -> do
                (arg, used'') <- readback used' domain field
                pure (mkAppTy f arg, used'')
          _ -> opaque
      _ -> opaque
      where
        opaque = lift (throwE (ReadbackOpaque (Neutral neutral)))
        isTermVarOrigin (TermVarTy _) = True
        isTermVarOrigin _             = False

    readField (rev_tys, used) (kind, field) = do
      (ty, used') <- readback used kind field
      pure (ty : rev_tys, used')

{- Note [Readback of a rigid callback application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a source signature quantifies (f :: a %U -> b), the open evaluator
uses a fresh term Id to stand for f. The origin map relates that Id to
the original type variable. If a compiled worker returns Just (f x),
the application has a rigid head. Recover that variable and check each
argument at the domain of its arrow. The result is the ordinary type
application f x. Its head stays unmatchable. This also handles a curried
callback or an argument that is itself a neutral application.

Reading a constructor field to build a source type uses the same
reduction budget and constructor visibility checks as the rest of the
observation. It adds no evaluation to the runtime program. A term binder
has no type-variable origin. In particular, this path cannot construct
a TermAppTy for a lazy runtime call. A case blocked on the result of f
cannot choose a branch.
-}

-- | Reduce at most one closed family at each function argument or result.
-- See Note [Local open family conversion].
openFamilyReduction :: Type -> Reduction
openFamilyReduction ty
  | Just (flag, ma, mult, arg, res) <- splitFunTy_maybe ty =
      mkFunRedn Nominal flag (mkReflRedn Nominal ma) (mkReflRedn Nominal mult)
        (openFamilyReduction arg) (openFamilyReduction res)
  | Just (tc, args) <- splitTyConApp_maybe ty
  , Just _ <- closedFamilyTyConCoAxiom_maybe tc
  , Just reduction <- reduceTyFamApp_maybe emptyFamInstEnvs Nominal tc args =
      reduction
  | otherwise = mkReflRedn Nominal ty

{- Note [Local open family conversion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A checked local definition can have an input or result type such as
Ready a, where the closed family Ready reduces directly to a. The
typechecker retains a real coercion when it adapts that function to
symbolic operands. The worker may also cast its argument before returning
it. A local observation may follow a cast only if independently
normalizing the two endpoints gives the same nominal type. Do not use
open instances or assume that representationally equal types have the
same promoted constructor. Other observations leave casts unsupported.
-}

evaluate :: Monad m => Bool -> Bool -> IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr)) -> (Var -> m ())
         -> (CaseStep -> m ()) -> VarSet -> Maybe Type -> Suspension
         -> StateT EvalState (ExceptT EvalFailure m) Whnf
evaluate local_families indexed limit obtain local record unsolved first_type (Suspension env expr mb_tid) = do
  EvalState steps next_tid memo <- get
  if intGtLimit steps limit
    then lift (throwE Exhausted)
    else put (EvalState (steps + 1) next_tid memo)
  case mb_tid of
    Just tid | isNothing first_type, Just cached <- IntMap.lookup tid memo -> pure cached
    _ -> do
      whnf <- eval_step
      case mb_tid of
        Just tid | isNothing first_type -> do
          EvalState s' n' m' <- get
          put (EvalState s' n' (IntMap.insert tid whnf m'))
        _ -> pure ()
      pure whnf
  where
    eval_step = case expr of
      _ | App (App (Var prim_fn) (Type _lev)) (Type scrut_ty) <- expr
        , Just op <- isPrimOpId_maybe prim_fn
        , op == DataToTagSmallOp || op == DataToTagLargeOp
        , isLiftedTypeKind (typeKind scrut_ty) ->
            pure PrimDataToTag
      _ | Just (con, arguments, fields, missing) <- constructorApplication expr
        , missing > 0 || con /= pairCon ->
            if missing > 0
            then recur (mkSuspension env (etaExpand missing expr))
            else pure (BoxedData con arguments (map (mkSuspension env) fields))
      _ | indexed
        , Just (con, arguments, locals, proofs, fields) <- indexedConstructorApplication expr ->
            pure (IndexedData con arguments locals proofs (map (mkSuspension env) fields))
      Var v
        | Just binding <- lookupVarEnv env v -> do
            lift (lift (local v))
            recur binding
        | indexed || maybe False (isNewTyCon . dataConTyCon) (isDataConWrapId_maybe v)
        , Just wrapper <- dataConWrapUnfolding_maybe v
        , sameWrapperType (exprType wrapper) (varType v) ->
            recur (mkSuspension emptyVarEnv wrapper)
        | otherwise -> do
            definition <- lift (lift (obtain v first_type))
            case definition of
              Just rhs@(Var target)
                | varType target `eqType` varType v ->
                    recurHead first_type (mkSuspension emptyVarEnv rhs)
              Just rhs -> recur (mkSuspension emptyVarEnv rhs)
              Nothing
                | v `elemVarSet` unsolved -> pure (Neutral (Unsolved v))
                | otherwise -> pure (Neutral (Rigid v))
      Cast body co
        | TransCo c1 c2 <- co ->
            recurHead first_type (mkSuspension env (Cast (Cast body c1) c2))
        | Just (con, tys, field_ty) <- matchNewtypeWrapCo co
        , exprType body `eqType` field_ty -> do
            whnf <- recur (mkSuspension env body)
            case whnf of
              Neutral (NewtypeUnwrap inner_n con' tys')
                | con' == con, eqTypes tys' tys ->
                    pure (Neutral inner_n)
              _ -> do
                EvalState s n m <- get
                put (EvalState s (n + 1) (IntMap.insert n whnf m))
                pure (BoxedData con tys [Suspension env body (Just n)])
        | Just (con, tys, field_ty) <- matchNewtypeUnwrapCo co
        , exprType body `eqType` mkTyConApp (dataConTyCon con) tys
        , coercionRKind co `eqType` field_ty -> do
            whnf <- recur (mkSuspension env body)
            case whnf of
              BoxedData con' tys' [field_susp]
                | con' == con, eqTypes tys' tys ->
                    recurHead first_type field_susp
              Neutral inner_n ->
                pure (Neutral (NewtypeUnwrap inner_n con tys))
              _ -> lift (throwE UnsupportedCore)
        | indexed
        , coercionLKind co `eqType` coercionRKind co || checkedMultiplicityCast co ->
            recurHead first_type (mkSuspension env body)
        | local_families
        , let lhs = openFamilyReduction (coercionLKind co)
              rhs = openFamilyReduction (coercionRKind co)
        , not (isReflRedn lhs && isReflRedn rhs)
        , reductionReducedType lhs `eqType` reductionReducedType rhs ->
            recurHead first_type (mkSuspension env body)
      Tick _ body -> recurHead first_type (mkSuspension env body)
      Lit lit -> pure (Literal lit)
      Lam binder body -> pure (Lambda binder (mkSuspension env body))
      App (Var worker) arg
        | worker == dataConWorkId charDataCon -> do
            payload <- recur (mkSuspension env arg)
            case payload of
              Literal (LitChar c) -> pure (BoxedChar c)
              _ -> lift (throwE UnsupportedCore)
      App (App (Var prim_fn) (Type res_ty)) arg
        | Just TagToEnumOp <- isPrimOpId_maybe prim_fn
        , Just (tc, tc_args) <- splitTyConApp_maybe res_ty
        , isBoxedDataTyCon tc -> do
            tag_whnf <- recur (mkSuspension env arg)
            case tag_whnf of
              Neutral n -> pure (Neutral (PrimBlocked n))
              Literal (LitNumber LitNumInt tag) ->
                case [dc | dc <- tyConDataCons tc
                         , null (dataConRepArgTys dc)
                         , toInteger (dataConTagZ dc) == tag] of
                  [dc] -> pure (BoxedData dc tc_args [])
                  _    -> lift (throwE UnsupportedCore)
              _ -> lift (throwE UnsupportedCore)
      App (App (App (Var prim_fn) (Type _lev)) (Type _scrut_ty)) arg
        | Just op <- isPrimOpId_maybe prim_fn
        , op == DataToTagSmallOp || op == DataToTagLargeOp -> do
            val_whnf <- recur (mkSuspension env arg)
            case val_whnf of
              Neutral n -> pure (Neutral (PrimBlocked n))
              BoxedData dc _ _ -> pure (Literal (mkLitIntUnchecked (toInteger (dataConTagZ dc))))
              IndexedData dc _ _ _ _ -> pure (Literal (mkLitIntUnchecked (toInteger (dataConTagZ dc))))
              BoxedPair _ _ -> pure (Literal (mkLitIntUnchecked 0))
              _ -> lift (throwE UnsupportedCore)
      App (App (Var prim_fn) lhs_arg) rhs_arg
        | Just op <- isPrimOpId_maybe prim_fn
        , Just cmp <- intCmpPrimOp op -> do
            lhs_whnf <- recur (mkSuspension env lhs_arg)
            case lhs_whnf of
              Neutral n -> pure (Neutral (PrimBlocked n))
              Literal (LitNumber LitNumInt i) -> do
                rhs_whnf <- recur (mkSuspension env rhs_arg)
                case rhs_whnf of
                  Neutral n -> pure (Neutral (PrimBlocked n))
                  Literal (LitNumber LitNumInt j) ->
                    pure (Literal (mkLitIntUnchecked (if cmp i j then 1 else 0)))
                  _ -> lift (throwE UnsupportedCore)
              _ -> lift (throwE UnsupportedCore)
      App (App (App (App (Var worker) (Type a)) (Type b)) x) y
        | worker == pairWorker
        , not (isTyCoArg x), not (isTyCoArg y)
        , isLiftedTypeKind (typeKind a), isLiftedTypeKind (typeKind b)
        , exprType x `eqType` a, exprType y `eqType` b ->
            pure (BoxedPair (mkSuspension env x) (mkSuspension env y))
      App fun arg -> do
        let head_type = case arg of
              Type ty -> Just ty
              _ -> Nothing
        headValue <- recurHead head_type (mkSuspension env fun)
        case headValue of
          Lambda binder (Suspension closure body _)
            | Type argTy <- arg, isTyVar binder ->
                let inScope = mkInScopeSet (exprFreeVars body `unionVarSet` tyCoVarsOfType argTy)
                    subst = extendTvSubst (mkEmptySubst inScope) binder argTy
                in recur (mkSuspension closure (substExpr subst body))
            | liftedOrJoinBinder binder ->
                recur (mkSuspension (extendVarEnv closure binder (mkSuspension env arg)) body)
            | isId binder
            , varType binder `eqType` unboxedUnitTy
            , Var worker <- arg
            , worker == dataConWorkId unboxedUnitDataCon
            , not (binder `elemVarSet` exprFreeVars body) -> do
                -- See Note [Observing open Core].
                lift (lift (record ContinueWithUnit))
                recur (mkSuspension closure body)
          PrimDataToTag
            | not (isTyCoArg arg) -> do
                val_whnf <- recur (mkSuspension env arg)
                case val_whnf of
                  Neutral n -> pure (Neutral (PrimBlocked n))
                  BoxedData dc _ _ -> pure (Literal (mkLitIntUnchecked (toInteger (dataConTagZ dc))))
                  IndexedData dc _ _ _ _ -> pure (Literal (mkLitIntUnchecked (toInteger (dataConTagZ dc))))
                  BoxedPair _ _ -> pure (Literal (mkLitIntUnchecked 0))
                  _ -> lift (throwE UnsupportedCore)
          Neutral neutral -> pure (Neutral (Applied neutral (mkSuspension env arg)))
          _ -> lift (throwE UnsupportedCore)
      Let (NonRec binder rhs) body
        | liftedOrJoinBinder binder -> do
            EvalState s n m <- get
            put (EvalState s (n + 1) m)
            recur (mkSuspension (extendVarEnv env binder (Suspension env rhs (Just n))) body)
        | indexed, isCoVar binder, Coercion co <- rhs
        , varType binder `eqType` coercionType co ->
            let inScope = mkInScopeSet (exprFreeVars body `unionVarSet` exprFreeVars rhs)
                subst = extendCvSubst (mkEmptySubst inScope) binder co
            in recur (mkSuspension env (substExpr subst body))
      Let (Rec bindings) body
        | all (liftedOrJoinBinder . fst) bindings ->
            let recEnv = extendVarEnvList env [(binder, mkSuspension recEnv rhs) | (binder, rhs) <- bindings]
            in recur (mkSuspension recEnv body)
      Case scrutinee binder resultType alts -> do
        let suspended = mkSuspension env scrutinee
        headValue <- recur suspended
        case headValue of
          Neutral neutral ->
            pure (Neutral (CaseBlocked neutral (CaseContinuation env binder resultType alts)))
          BoxedChar c
            | Just (Alt (DataAlt con) [field] body) <- findAlt (DataAlt charDataCon) alts
            , con == charDataCon -> do
                lift (lift (record (SelectBoxedChar c (DataAlt con))))
                let payload = mkSuspension emptyVarEnv (Lit (LitChar c))
                    bodyEnv = extendVarEnvList env [(binder, suspended), (field, payload)]
                recur (mkSuspension bodyEnv body)
            | Just (Alt DEFAULT [] body) <- findAlt (DataAlt charDataCon) alts -> do
                lift (lift (record (SelectBoxedChar c DEFAULT)))
                recur (mkSuspension (extendVarEnv env binder suspended) body)
            | otherwise -> lift (throwE MissingAlternative)
          BoxedPair first second ->
            selectData SelectBoxedPair pairCon [first, second] binder suspended alts
          BoxedData con _ fields ->
            selectData (SelectBoxedData con) con fields binder suspended alts
          IndexedData con _ local_tys proofs fields
            | Just (Alt key binders body) <- findAlt (DataAlt con) alts ->
                case key of
                  DEFAULT | null binders ->
                    recur (mkSuspension (extendVarEnv env binder suspended) body)
                  DataAlt chosen | chosen == con
                    , Just (subst, bindings) <- indexedAltBindings body binders local_tys proofs fields ->
                      recur (mkSuspension (extendVarEnvList env ((binder, suspended) : bindings))
                                          (substExpr subst body))
                  _ -> lift (throwE UnsupportedCore)
          Literal lit
            | Just (Alt key [] body) <- findAlt (LitAlt lit) alts -> do
                lift (lift (record (SelectLiteral lit key)))
                let bodyEnv = extendVarEnv env binder (mkSuspension emptyVarEnv (Lit lit))
                recur (mkSuspension bodyEnv body)
            | otherwise -> lift (throwE MissingAlternative)
          Lambda{}
            | Just (Alt DEFAULT [] body) <- findAlt DEFAULT alts -> do
                lift (lift (record SelectLambdaDefault))
                recur (mkSuspension (extendVarEnv env binder suspended) body)
          _ -> lift (throwE UnsupportedCore)
      _ -> lift (throwE UnsupportedCore)
    recurHead = evaluate local_families indexed limit obtain local record unsolved
    recur = recurHead Nothing
    pairCon = tupleDataCon Boxed 2
    pairWorker = dataConWorkId pairCon
    selectData step con fields binder suspended alts
      | Just (Alt key binders body) <- findAlt (DataAlt con) alts
      , Just bindings <- altBindings key con binders fields = do
          lift (lift (record (step key)))
          recur (mkSuspension (extendVarEnvList env ((binder, suspended) : bindings)) body)
      | otherwise = lift (throwE MissingAlternative)

    altBindings (DataAlt chosen) con binders fields
      | chosen == con, length binders == length fields
      , and (zipWith matches binders fields) = Just (zip binders fields)
    altBindings DEFAULT _ [] _ = Just []
    altBindings _ _ _ _ = Nothing

    matches field (Suspension _ body _) =
      liftedBinder field && varType field `eqType` exprType body

    intCmpPrimOp :: PrimOp -> Maybe (Integer -> Integer -> Bool)
    intCmpPrimOp IntEqOp = Just (==)
    intCmpPrimOp IntNeOp = Just (/=)
    intCmpPrimOp IntLtOp = Just (<)
    intCmpPrimOp IntLeOp = Just (<=)
    intCmpPrimOp IntGtOp = Just (>)
    intCmpPrimOp IntGeOp = Just (>=)
    intCmpPrimOp _       = Nothing

-- | Check a compiler-generated weakening of an otherwise unchanged arrow.
-- See Note [Observing indexed constructor cases].
checkedMultiplicityCast :: Coercion -> Bool
checkedMultiplicityCast (SubCo co) = checkedMultiplicityCast co
checkedMultiplicityCast ForAllCo{ fco_tcv = tv, fco_visL = left, fco_visR = right
                                  , fco_ma = ma, fco_kind = MRefl, fco_body = body }
  = isTyVar tv && eqForAllVis left right && isReflCo ma && checkedMultiplicityCast body
checkedMultiplicityCast (UnivCo SubMultProv _ lhs rhs NoUnivCoDeps)
  | Just (FTF_T_T, ma, mult, arg, res) <- splitFunTy_maybe lhs
  , Just (FTF_T_T, ma', mult', arg', res') <- splitFunTy_maybe rhs
  = isOneTy mult && isManyTy mult' &&
    ma `eqType` ma' && arg `eqType` arg' && res `eqType` res'
checkedMultiplicityCast _ = False

-- | Instantiate only the checked existential and proof arguments of a selected branch.
-- See Note [Observing indexed constructor cases].
indexedAltBindings :: CoreExpr -> [Var] -> [Type] -> [Coercion] -> [Suspension]
                   -> Maybe (Subst, [(Var, Suspension)])
indexedAltBindings body binders local_tys proofs fields = do
    let (tvs, after_types) = splitAt (length local_tys) binders
        (cvs, ids) = splitAt (length proofs) after_types
        free = foldl' unionVarSet (exprFreeVars body)
                 (map (exprFreeVars . Type) local_tys ++ map (exprFreeVars . Coercion) proofs)
        bind = mkEmptySubst (mkInScopeSet free)
        types = foldl' (\s (v, t) -> extendTvSubst s v t) bind (zip tvs local_tys)
        subst = foldl' (\s (v, c) -> extendCvSubst s v c) types (zip cvs proofs)
    guard (length tvs == length local_tys && all isTyVar tvs)
    guard (and (zipWith (\tv ty -> substTy types (varType tv) `eqType` typeKind ty) tvs local_tys))
    guard (length cvs == length proofs && all isCoVar cvs)
    guard (and (zipWith (\cv co -> substTy types (varType cv) `eqType` coercionType co) cvs proofs))
    guard (length ids == length fields && all liftedBinder ids)
    guard (and (zipWith (\v (Suspension _ e _) -> substTy subst (varType v) `eqType` exprType e) ids fields))
    pure (subst, zip ids fields)

-- | Read a fully applied ordinary constructor worker and its suspended fields.
constructorApplication :: CoreExpr -> Maybe (DataCon, [Type], [CoreExpr], Int)
constructorApplication expr
  | (Var worker, args) <- collectArgs expr
  , Just con <- isDataConWorkId_maybe worker
  , let (types, fields) = span isTyCoArg args
  , Just parameters <- traverse typeArgument types
  , Just expected <- openDataConFieldTypes con parameters
  , let missing = length expected - length fields
  , missing >= 0
  , all (not . isTyCoArg) fields
  , and (zipWith eqType expected (map exprType fields))
  = Just (con, parameters, fields, missing)
  | otherwise = Nothing
  where
    typeArgument (Type ty) = Just ty
    typeArgument _ = Nothing

-- | Recover a saturated indexed worker and retain its actual equality proofs.
indexedConstructorApplication :: CoreExpr
                              -> Maybe (DataCon, [Type], [Type], [Coercion], [CoreExpr])
indexedConstructorApplication expr = do
    (Var worker, args) <- pure (collectArgs expr)
    con <- isDataConWorkId_maybe worker
    let (type_args, rest) = span isTypeArg args
        (proof_args, fields) = span isTyCoArg rest
        variables = dataConUnivTyVars con ++ dataConExTyCoVars con
    tys <- traverse (\case Type ty -> Just ty; _ -> Nothing) type_args
    proofs <- traverse (\case Coercion co -> Just co; _ -> Nothing) proof_args
    guard (length tys == length variables)
    let (universals, local_tys) = splitAt (length (dataConUnivTyVars con)) tys
        kind = mkTyConApp (dataConTyCon con) universals
    source_args <- traverse (`lookup` zip variables tys) (dataConUserTyVars con)
    (checked, evidence, field_kinds) <- openIndexedDataCon con kind source_args
    guard (and (zipWith eqType checked tys))
    guard (length evidence == length proofs &&
           and (zipWith eqType (map coercionType evidence) (map coercionType proofs)))
    guard (length field_kinds == length fields && all (not . isTyCoArg) fields &&
           and (zipWith eqType field_kinds (map exprType fields)))
    pure (con, source_args, local_tys, proofs, fields)
  where
    isTypeArg Type{} = True
    isTypeArg _ = False

-- | Instantiate a promoted ordinary GADT constructor at its checked kind.
-- See Note [Observing indexed constructor cases].
openIndexedDataCon :: DataCon -> Kind -> [Type] -> Maybe ([Type], [Coercion], [Type])
openIndexedDataCon con kind source_args = do
    (tc, universals) <- splitTyConApp_maybe kind
    (univ, exs, specs, [], fields, result) <- pure (dataConFullSig con)
    let user_vars = dataConUserTyVars con
        variables = univ ++ exs
    guard (tc == dataConTyCon con && isBoxedDataTyCon tc && not (isTypeDataCon con) &&
           null (dataConRepArgBinders con) && null (dataConStupidTheta con) &&
           all (not . isBanged) (dataConImplBangs con))
    guard ((not (null exs && null specs) || user_vars /= univ) &&
           length univ == length universals &&
           all isTyVar exs && length user_vars == length source_args)
    local_tys <- traverse (`lookup` zip user_vars source_args) exs
    let tys = universals ++ local_tys
        instantiate = substTyWith variables tys
        field_kinds = map (instantiate . scaledThing) fields
        rep_kinds = map (instantiate . scaledThing) (dataConRepArgTys con)
    guard (instantiate result `eqType` kind && isLiftedTypeKind (typeKind kind) &&
           all isLiftedTypeKind (map typeKind field_kinds))
    guard (and (zipWith (\v ty -> instantiate (varType v) `eqType` typeKind ty)
                       variables tys))
    guard (and (zipWith (\v ty -> instantiate (mkTyVarTy v) `eqType` ty)
                       user_vars source_args))
    proofs <- traverse (\spec ->
        let (tv, rhs) = eqSpecPair spec
            left = instantiate (mkTyVarTy tv)
        in mkNomReflCo left <$ guard (left `eqType` instantiate rhs)) specs
    let expected = map coercionType proofs ++ field_kinds
    guard (length expected == dataConRepArity con && length expected == length rep_kinds &&
           and (zipWith eqType expected rep_kinds))
    pure (tys, proofs, field_kinds)

{- Note [Observing indexed constructor cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A promoted constructor can determine the branch of an imported GADT
match. The operand decoder uses the constructor's checked result kind
and source type arguments to instantiate its worker. A constructor can
carry existential types without refining its result type. Read these
types from the source arguments and check them against their binders.
Accept a proof argument only where the instantiated nominal EqSpec endpoints agree.
Check the complete worker representation against those proof and field
types. Leave a constructor with a strict field or other evidence out of
this observer.

When a case chooses that worker, check the alternative's binder kinds.
Substitute the worker's actual existential arguments and proofs into the
chosen body together. Save the value fields with their original
environments. A cast whose endpoints still differ blocks observation.
A neutral scrutinee cannot choose a branch. For example, a substitution
worker can apply its callback to a known bound variable, but it cannot
inspect an unknown term (Weirich et al., ICFP 2017; Cockx and Abel, ICFP 2018;
GHC Proposal #378, §4.6 and #509).

When a worker builds a GADT constructor through its wrapper, use the
wrapper unfolding reconstructed from the constructor interface. It is
an implicit binding. Check the resulting worker through the same plan
as the worker for a promoted operand.

The retained simplifier can keep a SubMultProv coercion on a constructor
used as a callback. For example, the substitution extension passes the
successor of a typed lookup to renaming. Accept the compiler's weakening
from One to Many when the argument and result types agree. Reflexive
type binders and a downgrade from nominal to representational preserve
this condition. The target arrow makes a weaker guarantee about how the
function uses its argument. The callback still runs the same code.
-}

-- | Recognize lifted, lazy constructor fields with the ordinary representation.
-- Both the source operand decoder and the Core observer use this check.
openDataConFieldTypes :: DataCon -> [Type] -> Maybe [Type]
openDataConFieldTypes con parameters
  | let tc = dataConTyCon con
  , (isBoxedDataTyCon tc && isNothing (dataConWrapId_maybe con))
    || ((isNewTyCon tc || isUnaryClassTyCon tc) && isLiftedOrConstraintKind (tyConResKind tc))
  , not (isTypeDataCon con)
  , null (dataConRepArgBinders con), null (dataConStupidTheta con)
  , all (not . isBanged) (dataConImplBangs con)
  , (vars, [], [], [], fields, result) <- dataConFullSig con
  , length vars == length parameters
  , length fields == dataConRepArity con
  , let instantiate = substTyWith vars parameters
        field_types = map (instantiate . scaledThing) fields
  -- The kind of a parameter may refer to earlier constructor parameters.
  , and (zipWith eqType (map (instantiate . varType) vars) (map typeKind parameters))
  , isLiftedOrConstraintKind (typeKind (instantiate result))
  , all (isLiftedOrConstraintKind . typeKind) field_types
  = Just field_types
  | otherwise = Nothing

isLiftedOrConstraintKind :: Kind -> Bool
isLiftedOrConstraintKind k = isLiftedTypeKind k || isConstraintLikeKind k

matchNewtypeUnwrapCo :: Coercion -> Maybe (DataCon, [Type], Type)
matchNewtypeUnwrapCo co@(AxiomCo (UnbranchedAxiom nt_ax) cos)
  | all (\c -> isReflCo c || coercionLKind c `eqType` coercionRKind c) cos
  , let tc = coAxiomTyCon nt_ax
        tys = map coercionLKind cos
  , isNewTyCon tc && isLiftedOrConstraintKind (tyConResKind tc)
  , Just nt_ax == newTyConCo_maybe tc
  , Just con <- tyConSingleDataCon_maybe tc
  , Just [field_ty] <- openDataConFieldTypes con tys
  , coercionLKind co `eqType` mkTyConApp tc tys
  , coercionRKind co `eqType` field_ty
  = Just (con, tys, field_ty)
matchNewtypeUnwrapCo _ = Nothing

matchNewtypeWrapCo :: Coercion -> Maybe (DataCon, [Type], Type)
matchNewtypeWrapCo (SymCo co) = matchNewtypeUnwrapCo co
matchNewtypeWrapCo _          = Nothing

-- | Compare a constructor wrapper unfolding's type against its 'Id' type,
-- ignoring 'Inferred' versus 'Specified' specificity introduced by Core 'Lam'.
sameWrapperType :: Type -> Type -> Bool
sameWrapperType t1 t2
  | t1 `eqType` t2 = True
  | (b1, r1) <- splitForAllForAllTyBinders t1
  , (b2, r2) <- splitForAllForAllTyBinders t2
  , not (null b1), length b1 == length b2
  , let vs1 = [v | (_, Bndr v _) <- b1]
        vs2 = [v | (_, Bndr v _) <- b2]
  , and (zipWith (\v1 v2 -> varType v1 `eqType` varType v2) vs1 vs2)
  = sameWrapperType (substTyWith vs1 (map mkTyVarTy vs2) r1) r2
  | otherwise = False

-- | Lifted term and dictionary binders (and join points in local lets) can enter the lazy environment.
liftedBinder :: Var -> Bool
liftedBinder binder =
  isId binder && not (isCoVar binder) && isLiftedOrConstraintKind (typeKind (varType binder))

liftedOrJoinBinder :: Var -> Bool
liftedOrJoinBinder binder = liftedBinder binder || isJoinId binder

{- Note [Observing open Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Save each operand with its lexical environment when forming an application.
Extend the function's saved environment when entering its body. This keeps
an ignored lifted operand lazy. Use Core's capture-avoiding substitution
for type arguments.

A definition lookup receives the first syntactic argument when it is a
type. Follow the application spine, transparent ticks and same-typed
variable aliases to find it. Crossing a suspension, entering any other
body, or inspecting a case starts a new observation. A type argument
from the caller cannot justify a specialization of a recursive call.

A saturated worker for an ordinary lazy boxed constructor is in weak
head normal form. Preserve its fields as independent suspensions. Use
the actual DataCon and typed alternative binders to select and bind its
fields. A neutral scrutinee still blocks case selection. The ordinary
source operand decoder also checks this worker shape. It does not
bypass a strict field or a representation-changing wrapper.

An ordinary function can also receive the worker as a callback. For
example, build passes (:) to the list producer before either field is
available. Once all type arguments are present, eta-expand the missing
value arguments. The resulting lambda keeps the supplied fields in their
original environment. Its body reaches the same saturated constructor
check when the caller supplies the remaining fields.

Keep the dedicated boxed pair and Char trace steps for existing clients.

Readback needs a finite type. It asks for each suspended field at its
original kind. Only original rigid symbols and checked constructors can
be read back. All of these field observations share the existing budget.
Ordinary case observation continues to leave unselected fields suspended.

A case must inspect its scrutinee even when it has only a DEFAULT branch.
If the scrutinee is neutral, retain the alternatives, the binder and their
environment. A rigid input stays neutral. An unsolved input can be observed
again with a new definition (Norell, 2007; Schmidt-Schauß, Sabel, and
Machkasova, LMCS 2015). See Note [Open source calls] in GHC.Tc.Gen.OpenTypeEval.

GHCi inserts Breakpoint ticks into ordinary imported function bodies.
They identify positions for the runtime debugger. When observing a type,
inspect the expression beneath the tick. The interpreter keeps the
original annotated body for runtime execution.

GHC.HsToCore.Utils.mkFailurePair passes the wired-in nullary unboxed
unit constructor directly to a failure continuation. The observer can
enter this continuation if the binder has exactly the unboxed unit type
and does not occur free in the body. The constructor has no fields to
evaluate. An expression of the same type can still require evaluation,
so the observer does not drop any other unlifted operand.
-}

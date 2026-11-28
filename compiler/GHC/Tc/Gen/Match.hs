{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE MonadComprehensions  #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures   #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typecheck some @Matches@
module GHC.Tc.Gen.Match
   ( tcFunBindMatches
   , tcCaseMatches, tcExprCaseMatches, tcIndexedCaseMatches
   , tcRetainedCase
   , tcRetainedCallCase, tcEmptyRetainedScrutinee
   , tcLambdaMatches
   , tcMatches
   , tcOpenImplicitPackage
   , tcOpenImplicitPackageAt
   , implicitPayloadType
   , matchesImplicitPayloadPat
   , tcGRHSNE
   , tcGRHSsPat
   , TcStmtChecker
   , TcExprStmtChecker
   , TcCmdStmtChecker
   , tcStmts
   , tcStmtsAndThen
   , tcDoStmts
   , tcBody
   , tcScopedRuntimeBody, tcCloseRuntimeScope
   , tcDoStmt
   , tcGuardStmt
   , checkArgCounts
   )
where

import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcSyntaxOp, tcInferRho, tcInferRhoFRRNC
                                       , tcMonoLExprNC, tcExpr
                                       , tcCheckMonoExpr, tcCheckMonoExprNC
                                       , tcCheckPolyExpr, tcPolyLExpr )

import GHC.Rename.Utils ( bindLocalNames, wrapGenSpan, wrapGenSpan'
                        , genLHsVar, genHsApps, genSimpleFunBind, genHsLet )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Solver ( tcCheckGivens, tcNormalise, tcRewriteFamilyAxioms )
import GHC.Tc.Solver.InertSet ( emptyInertSet )
import GHC.Tc.Solver.Monad ( runTcSEqualities )
import GHC.Tc.Solver.Rewrite ( rewriteFamilyAxioms )
import GHC.Tc.Zonk.TcType ( zonkTcType )
import GHC.Tc.Gen.Pat
import GHC.Tc.Gen.Head( tcCheckId )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Gen.Bind
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic )
import GHC.Tc.Utils.Unify
import GHC.Tc.Types.Origin
import GHC.Tc.Types.ErrCtxt( UserTypeCtxt( GenSigCtxt ), pprUserTypeCtxt )
import GHC.Tc.Types.Evidence
import GHC.Rename.Env ( irrefutableConLikeTc )

import GHC.Core.Multiplicity
import GHC.Core.UsageEnv
import GHC.Core.TyCon
import GHC.Core ( Expr(Var, App, Type), exprToNativeIndex, isNativeIndexedGadtCon )
import GHC.Core.Dependent ( hasNativeDependentExpr )
import GHC.Core.TyCo.Rep ( Type(DepFunTy, ExistsTy, ForAllTy, FunTy, ft_af, TermVarTy, TyConApp) )
import GHC.Core.Subst ( extendIdSubstWithClone )
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Type ( coreFullView, expandTypeSynonyms, isManyTy, splitTyConApp_maybe
                     , anyFreeVarsOfType, typeKind, isNativeIndexFunctionId, isNativeOpaqueValueId
                     , isInvisibleForAllTyFlag, isNativeIndexId, nativeIndexDataConTyArgs
                     , splitFunTy_maybe, nativeCallSpine_maybe, isInferredRetainedBinder
                     , mkDepFunTyWithVis )
import GHC.Core.Coercion ( CoercionR, MCoercion(MRefl, MCo), mkCoVarCo, mkCoCast, coercionType, isReflCo
                         , mkSymCo, mkSubCo, coercionKind, coercionLKind
                         , mkFunCo, mkFunResCo, mkHomoForAllCos
                         , mkTransCo, mkNomReflCo, mkRepReflCo, mkDepFunCoWithVis )
import GHC.Core.DataCon ( DataCon, dataConCannotMatch, dataConName, dataConSourceArity, dataConTyCon, dataConUnivTyVars, dataConWorkId )
import GHC.Types.Var ( setVarName )
import GHC.Core.ConLike ( ConLike(RealDataCon) )
import GHC.Core.Predicate ( getEqPredTys_maybe, mkNomEqPred )
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Utils ( applyTypeToArgs, exprType, isEmptyTy )
-- Create chunkified tuple types for monad comprehensions
import GHC.Core.Make.BigTuple

import GHC.Hs

import GHC.Driver.Config.HsToCore.Ticks ( initTicksConfig )
import GHC.Driver.Session ( getDynFlags, sccProfilingEnabled )
import GHC.HsToCore.Ticks ( TicksConfig(ticks_passes), TickishType(Breakpoints) )

import GHC.Builtin.WiredIn.Types
import GHC.Builtin.WiredIn.Prim

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Types.Name
import GHC.Types.Name.Env (anyNameEnv)
import GHC.Types.Name.Reader
import GHC.Types.Name.Set
import GHC.Types.Unique.Set (sizeUniqSet)
import GHC.Types.Id
import GHC.Types.Var ( binderFlag, EvVar, DepFunTyFlag(..), ExistsTyFlag(ImplicitExists), Specificity(SpecifiedSpec), VarBndr(Bndr), varType, tyVarKind, isCoVar, isTyVar, isVisibleFunArg, isInferredForAllTyFlag )
import GHC.Types.Var.Env ( mkInScopeSetList, unitVarEnv )
import GHC.Types.Var.Set ( mkVarSet )
import GHC.Types.SrcLoc
import GHC.Types.Basic( VisArity, RecFlag(NonRecursive), TypeOrKind(TypeLevel), Origin(Generated), GenReason(OtherExpansion), DoPmc(SkipPmc) )
import GHC.Data.Bag ( listToBag, unionBags )
import GHC.Data.FastString ( fsLit )
import GHC.Data.Pair ( pSnd )
import GHC.Data.Maybe ( MaybeT, catMaybes, firstJustsM, fromMaybe, isJust, isNothing, liftMaybeT, runMaybeT )

import qualified GHC.Data.List.NonEmpty as NE
import Data.Data ( Data, cast, gmapQ, gmapT )

import Control.Monad
import Control.Arrow ( second )
import Data.Foldable (toList)
import Data.List (find, nub, nubBy, partition, transpose, zipWith4)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, mapMaybe)

import qualified GHC.LanguageExtensions as LangExt


{-
************************************************************************
*                                                                      *
\subsection{tcFunBindMatches, tcCaseMatches}
*                                                                      *
************************************************************************

`tcFunBindMatches` typechecks a `[Match]` list which occurs in a
`FunBind`.  The second argument is the name of the function, which
is used in error messages.  It checks that all the equations have the
same number of arguments before using `tcMatches` to do the work.
-}

tcFunBindMatches :: UserTypeCtxt
                 -> Name            -- Function name
                 -> Mult            -- The multiplicity of the binder
                 -> MatchGroup GhcRn (LHsExpr GhcRn)
                 -> [ExpPatType]    -- Scoped skolemised binders
                 -> ExpRhoType      -- Expected type of function; caller
                                    -- has skolemised any outer forall's
                 -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
-- See Note [Skolemisation overview] in GHC.Tc.Utils.Unify
tcFunBindMatches ctxt fun_name mult raw_matches invis_pat_tys source_exp_ty
  = assertPpr (funBindPrecondition raw_matches) (pprMatches raw_matches) $
    do  { source_matches <- desugarExistsPackMatchGroup raw_matches
        ; -- Check that they all have the same no of arguments
          source_arity <- checkArgCounts source_matches
        ; let (visibility_wrap, matches, exp_ty, arity)
                | Check ty <- source_exp_ty
                , not (rhs_has_invis_lam source_matches)
                , (indices@(_:_), co) <- exposeInvisibleRetained ty
                = ( mkWpCastR co
                  , source_matches { mg_alts = fmap (map (alignInvisibleRetainedPatterns True invis_pat_tys indices)) (mg_alts source_matches) }
                  , mkCheckExpType (coercionLKind co)
                  , source_arity + length indices )
                | otherwise = (idHsWrapper, source_matches, source_exp_ty, source_arity)
              herald = ExpectedFunTyMatches (NameThing fun_name) source_matches
              tc_matches pat_tys rhs_ty = tcScalingUsage mult $
                do { traceTc "tcFunBindMatches 2" $
                     vcat [ text "ctxt:" <+> pprUserTypeCtxt ctxt
                          , text "arity:" <+> ppr arity
                          , text "invis_pat_tys:" <+> ppr invis_pat_tys
                          , text "pat_tys:" <+> ppr pat_tys
                          , text "rhs_ty:" <+> ppr rhs_ty ]
                   ; tcFunMatchesWithImplicitPackages mctxt (invis_pat_tys ++ pat_tys) rhs_ty matches }
        ; traceTc "tcFunBindMatches 1" (ppr fun_name $$ ppr mult $$ ppr exp_ty $$ ppr arity)

        ; (wrap_fun, r)
             <- case exp_ty of
                  Check ty | not (rhs_has_invis_lam source_matches)
                           , hasRetainedFunTy ty
                    -> tcScalingUsage mult $
                       tcRetainedMatches herald ctxt mctxt arity invis_pat_tys ty matches
                  _ -> matchExpectedFunTys herald ctxt arity exp_ty tc_matches

        ; return (visibility_wrap <.> wrap_fun, r) }
  where
    mctxt  = mkPrefixFunRhs (noLocA fun_name) noAnn

    rhs_has_invis_lam (MG { mg_alts = L _ [L _ Match { m_pats = L _ []
                                                     , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] body) :| [] } }] })
      | L _ (HsLam _ _ (MG { mg_alts = L _ (L _ Match { m_pats = L _ (L _ InvisPat{} : _) } : _) }))
          <- stripParensLHsExpr body
      = True
    rhs_has_invis_lam _ = False

-- | Expose leading implicit runtime binders and preserve their scope in the cast.
exposeInvisibleRetained :: Type -> ([Id], CoercionR)
exposeInvisibleRetained ty
  | DepFunTy DepFunInvisible binder body <- coreFullView ty
  = let (binders, co) = exposeInvisibleRetained body
    in ( binder : binders
       , mkDepFunCoWithVis DepFunVisible DepFunInvisible binder MRefl co )
  | otherwise = ([], mkRepReflCo ty)

-- | Align written @\@pat@ abstractions with 'SpecifiedSpec' invisible retained
-- binders and insert implicit runtime patterns for omitted or 'InferredSpec'
-- binders. See Note [Invisible retained definition patterns].
alignInvisibleRetainedPatterns :: Bool -> [ExpPatType] -> [Id]
                               -> LMatch GhcRn body -> LMatch GhcRn body
alignInvisibleRetainedPatterns from_fun_bind invis_pat_tys indices
                               (L loc match@Match { m_pats = L pat_loc pats })
  = L loc (match { m_pats = L pat_loc (erased_invis ++ retained_pats ++ leftover_invis ++ values) })
  where
    (leading_invis, values) = span (not . isVisArgLPat) pats
    (erased_invis, rem_invis) = split_erased invis_pat_tys leading_invis
    (retained_pats, leftover_invis) = build_retained indices rem_invis

    split_erased [] ps = ([], ps)
    split_erased (ExpForAllPatTy _ (Bndr _ (Invisible spec)) : ptys)
                 (p@(L _ (InvisPat pat_spec _)) : ps)
      | pat_spec == spec
      = let (taken, rest) = split_erased ptys ps
        in (p : taken, rest)
    split_erased (_ : ptys) ps = split_erased ptys ps

    implicit_pat b
      | from_fun_bind = noLocA (VarPat noExtField (noLocA (idName b)))
      | otherwise     = noLocA (WildPat noExtField)

    build_retained [] ps = ([], ps)
    build_retained (b : bs) ps
      | isInferredRetainedBinder b
      = let (more, rest) = build_retained bs ps
        in (implicit_pat b : more, rest)
      | L ploc (InvisPat SpecifiedSpec tp) : ps' <- ps
      , Just pat <- tyPatToRetainedPat ploc tp
      = let (more, rest) = build_retained bs ps'
        in (pat : more, rest)
      | otherwise
      = let (more, rest) = build_retained bs ps
        in (implicit_pat b : more, rest)

-- | Desugar single-equation explicit @Pack@ patterns in function or lambda
-- argument columns into nested @case@ expressions checked by 'tcExistsCase'.
desugarExistsPackMatchGroup
  :: MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcRn (LHsExpr GhcRn))
desugarExistsPackMatchGroup group@MG { mg_alts = L loc [L mloc match@Match
  { m_pats = L ploc pats
  , m_grhss = GRHSs { grhssExt = g_ext0
                    , grhssGRHSs = L gloc (GRHS gext [] rhs) :| []
                    , grhssLocalBinds = where_binds } }] }
  = do { flags <- mapM is_pack_pat pats
       ; if not (or flags)
         then return group
         else do
           { let base_rhs = case unLoc where_binds of
                   EmptyLocalBinds{} -> rhs
                   _ -> L (getLoc rhs) (HsLet noExtField where_binds rhs)
           ; (pats', wrapped_rhs) <- foldrM wrap_pack ([], base_rhs) (zip flags pats)
           ; let empty_binds = fmap (const emptyLocalBinds) where_binds
                 grhss' = GRHSs g_ext0 (L gloc (GRHS gext [] wrapped_rhs) :| []) empty_binds
                 match' = match { m_pats = L ploc pats', m_grhss = grhss' }
           ; return (group { mg_alts = L loc [L mloc match'] }) } }
  where
    is_pack_pat :: LPat GhcRn -> TcM Bool
    is_pack_pat (L _ (ParPat _ p)) = is_pack_pat p
    is_pack_pat (L _ (ConPat { pat_con = L _ (WithUserRdr _ con) })) = isExistsPackName con
    is_pack_pat _ = return False

    wrap_pack (False, p) (ps, body) = return (p : ps, body)
    wrap_pack (True, p) (ps, body) = do
      arg_nm <- newSysName (mkVarOcc "pack_arg")
      let arg_pat = L (getLoc p) (VarPat noExtField (noLocA arg_nm))
          alt = mkSimpleMatch CaseAlt (noLocA [p]) body
          mg  = MG { mg_ext = (Generated OtherExpansion SkipPmc, snd (mg_ext group))
                   , mg_alts = noLocA [alt] }
          case_e = L (getLoc p) (HsCase CaseAlt (nlHsVar arg_nm) mg)
      return (arg_pat : ps, case_e)
desugarExistsPackMatchGroup group = return group

-- | Check the original equations beneath lazy openings of known package columns.
-- See Note [Implicit existential function patterns].
tcFunMatchesWithImplicitPackages
  :: HsMatchContextRn -> [ExpPatType] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcFunMatchesWithImplicitPackages ctxt pat_tys exp_ty group
  | Check result <- exp_ty
  , any is_dot_arg pat_tys
  , let columns = transpose (map (filter isVisArgLPat . hsLMatchPats) (unLoc (mg_alts group)))
        plan = select pat_tys columns
  , any (isJust . snd) plan
  = do { args <- mapM new_arg [scaled | ExpFunPatTy scaled <- pat_tys]
       ; body <- go result plan args [] []
       ; strict <- xoptM LangExt.Strict
       ; let arg_types = map (\arg -> Scaled (idMult arg) (idType arg)) args
             arg_pat arg = let var = noLocA (VarPat noExtField (noLocA arg))
                           in if strict then noLocA (LazyPat noExtField var) else var
             equation = mkSimpleMatch ctxt (noLocA (map arg_pat args)) (noLocA body)
       ; return (MG { mg_alts = noLocA [equation]
                    , mg_ext = MatchGroupTc arg_types result (Generated OtherExpansion SkipPmc) }) }
  | otherwise
  = tcExprMatchesWithWhereFrom id ctxt pat_tys exp_ty group
  where
    is_dot_arg (ExpFunPatTy (Scaled _ (Check ty)))
      | ExistsTy ImplicitExists _ _ <- coreFullView ty = True
    is_dot_arg _ = False

    select [] _ = []
    select (pat_ty:rest) columns
      | isVisibleExpPatType pat_ty, pats:later <- columns
      = (pat_ty, source_span pat_ty pats) : select rest later
      | otherwise
      = (pat_ty, Nothing) : select rest columns

    source_span (ExpFunPatTy (Scaled _ (Check ty))) pats =
      getLocA <$> find (matchesImplicitPayloadPat ty) pats
    source_span _ _ = Nothing

    new_arg (Scaled mult ty) = do { ty' <- readExpType ty
                                 ; newSysLocalId (fsLit "package_arg") mult ty' }

    go result ((ExpFunPatTy _, open):rest) (arg:args) checked_tys operands =
      case open of
        Just loc -> tcOpenImplicitPackage loc (idType arg) (nlHsVar arg) result $ \payload ->
          go result rest args (payload_pat payload : checked_tys) (nlHsVar payload : operands)
        Nothing -> go result rest args (payload_pat arg : checked_tys) (nlHsVar arg : operands)
      where
        payload_pat p = mkCheckExpFunPatTy (Scaled (idMult arg) (idType p))
    go result ((pat_ty@ExpForAllPatTy{}, _):rest) args checked_tys operands =
      go result rest args (pat_ty : checked_tys) operands
    go _ [] [] checked_tys operands =
      do { inner <- tcExprMatchesWithWhereFrom id ctxt (reverse checked_tys) exp_ty group
         ; return $ case operands of
             [operand] -> HsCase ctxt operand inner
             _ -> unLoc (mkHsApps (L (getLoc (mg_alts inner)) (HsLam noAnn LamCases inner))
                                  (reverse operands)) }
    go _ _ _ _ _ = panic "tcFunMatchesWithImplicitPackages"

{- Note [Implicit existential function patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A data constructor of a dot package's payload selects elimination in that
argument column. Check the original equations and guards together beneath
one opening for each selected argument. The normal matcher uses the original
arrow multiplicities. Its result type is outside every witness scope.

The outer equations bind only generated arguments. Apply the checked inner
match group to their payloads and to the other ordinary arguments. Unpack is
lazy, so the source match group still determines clause order and demand.
With Strict, an outer lazy pattern prevents a generated argument from
becoming strict before any source pattern runs. The inner matches retain
their FunRhs context for coverage checks and match failures.
-}

-- | Open checked dot binders with the result type outside every scope.
tcOpenImplicitPackage :: SrcSpan -> TcType -> LHsExpr GhcTc -> TcRhoType
                      -> (Id -> TcM (HsExpr GhcTc)) -> TcM (HsExpr GhcTc)
tcOpenImplicitPackage = tcOpenImplicitPackageAt ManyTy

-- | Open checked dot binders with the multiplicity of the payload pattern.
-- See Note [Type synonyms in implicit existential openings].
tcOpenImplicitPackageAt :: Mult -> SrcSpan -> TcType -> LHsExpr GhcTc -> TcRhoType
                        -> (Id -> TcM (HsExpr GhcTc)) -> TcM (HsExpr GhcTc)
tcOpenImplicitPackageAt mult loc schema checked result_ty check_body = open schema checked
  where
    open schema checked =
      case coreFullView schema of
        ExistsTy ImplicitExists binder payload_ty ->
          do { let origin = UnifyForAllSkol schema
             ; skol <- mkSkolemInfo origin
             ; uniq <- newUnique
             ; let name = mkSystemNameAt uniq (getOccName binder) loc
             ; witness <- pushTcLevelM_ $ newSkolemTyVar skol name (tyVarKind binder)
             ; let body_ty = substTyWith [binder] [mkTyVarTy witness]
                                         (expandTypeSynonyms payload_ty)
             ; payload <- newSysLocalId (fsLit "unpacked") mult body_ty
             ; (ev_binds, body) <- setSrcSpan loc $
                 checkConstraints origin [witness] [] $
                   case coreFullView body_ty of
                     ExistsTy ImplicitExists _ _ -> open body_ty (noLocA (mkHsVar (noLocA payload)))
                     _ -> check_body payload
             ; return (XExpr (NativeExistsUnpack witness payload result_ty checked
                                 (mkHsDictLet ev_binds (noLocA body)))) }
        _ -> failWithTc (TcRnExistsPack (ExistsPackInvalidSchema schema))

{- Note [Type synonyms in implicit existential openings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The result type of Unpack is outside the witness's scope. Suppose a
package contains Tile (Constant shape) shape, where Constant is a synonym
for Bool. A reader can infer Constant shape as its result element type.
The synonym is equal to Bool, but its argument still refers to shape in
Core. Expand synonyms in the payload before checking the reader. This
keeps discarded arguments out of the inferred result. Any genuine use of
the witness stays in scope for checking the reader.
-}

{- Note [Invisible retained definition patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Leading invisible @foreach@ binders in a function signature enter scope across
the definition (see Note [Invisible retained definitions] in GHC.Rename.Bind
and GHC Proposal #281). After outer skolemisation, 'alignInvisibleRetainedPatterns'
places written erased @\@a@ patterns first, inserts runtime variable or
@\@pat@ patterns for invisible retained binders against the exposed visible
endpoints of the telescope, and casts the checked value back to its declared
visibility without adding extra lambdas or demand.
-}

-- | Check source patterns against an expected retained function telescope.
-- See Note [Checked retained lambdas].
tcRetainedMatches :: ExpectedFunTyCtxt -> UserTypeCtxt -> HsMatchContextRn
                  -> VisArity -> [ExpPatType] -> Type
                  -> MatchGroup GhcRn (LHsExpr GhcRn)
                  -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedMatches herald ctxt mctxt arity invis_pat_tys ty matches
  | arity == 1
  , DepFunTy DepFunVisible binder _ <- coreFullView ty
  , not (isNativeIndexId binder)
  , Just (tc, _) <- splitTyConApp_maybe (coreFullView (varType binder))
  , let constructors = tyConDataCons tc
  , any isNativeIndexedGadtCon constructors
  , let names = mkNameSet (map dataConName constructors)
  , L loc rows@(_ : _) <- mg_alts matches
  , Just alternatives <- indexed_rows names rows
  = do { name <- newSysName (getOccName binder)
       ; let pat = L loc (VarPat noExtField (noLocA name))
             scrutinee = L loc (HsVar noExtField (noLocA (noUserRdr name)))
             outer = mkSimpleMatch (LamAlt LamSingle) (L loc [pat]) scrutinee
             check_case _ result = do
               index <- tcLookupId name
               tcEmitBindingUsage (singleUsageUE index)
               cases <- tcIndexedCaseMatches index CaseAlt (Scaled (idMult index) (varType index))
                          (matches { mg_alts = L loc alternatives }) result
               return (L loc (HsCase CaseAlt (nlHsVar index) cases))
       ; tcRetainedVarMatches herald ctxt ty [pat] $ \pat_tys rhs_ty _ _ ->
           tcMatches mctxt check_case (invis_pat_tys ++ pat_tys) rhs_ty
             (matches { mg_alts = L loc [outer] }) }
  | arity >= 1
  , not (has_function_binder ty)
  , plain_visible_arrows arity ty
  , has_nested_retained_con_matches (retained_or_gadt_columns 0 ty) matches
    || has_multi_arg_indexed_gadt_matches matches
  , not (retainedCaseHasQuote matches)
  = do { mb_desugared <- desugarDepClauseTree mctxt arity ty matches
       ; case mb_desugared of
           Just (arg_pats, single_group) ->
             tcRetainedVarMatches herald ctxt ty arg_pats $ \pat_tys rhs_ty boundary names ->
               let L g_loc [L m_loc m0@(Match { m_pats = L p_loc ps0 })] = mg_alts single_group
                   m1 = m0 { m_pats = L p_loc (bindRetainedWildcards names ps0) }
                   prepared = single_group { mg_alts = L g_loc [L m_loc m1] }
               in tcExprMatchesWithWhereFrom id mctxt (invis_pat_tys ++ pat_tys) rhs_ty
                    (maybe prepared (\(prefix, indices) -> splitRetainedSuffix prefix indices prepared) boundary)
           Nothing -> failWithTc (TcRnRetainedQuantificationError RetainedConstructorArguments) }
  | [source_column] <- filter (`elem` constructor_columns) (retained_columns 0 ty)
  , source_column > 0
  = do { (wrap, (pat_tys, plan, rhs_ty)) <- tcRetainedConArgs herald ctxt arity ty
       ; let visible = filter (isRetainedSourceArg . snd) (zip [0..] plan)
       ; case drop source_column visible of
           (column, RetainedConIndex index) : _
             | isNativeIndexId index
             -> do { checked <- tcRetainedLaterMatches (column, index) invis_pat_tys
                                 pat_tys plan rhs_ty matches
                   ; return (wrap, checked) }
           _ -> function_pattern_error }
  | DepFunTy DepFunVisible callback _ <- coreFullView ty
  , isNativeIndexFunctionId callback
  , cols@(_ : _ : _) <- constructor_columns
  = do { (wrap, (pat_tys, plan, rhs_ty)) <- tcRetainedConArgs herald ctxt arity ty
       ; let visible = filter (isRetainedSourceArg . snd) (zip [0..] plan)
             lookup_col c = case drop c visible of
               (idx_col, RetainedConIndex idx) : _ | isNativeIndexId idx -> Just (idx_col, idx)
               _ -> Nothing
       ; case traverse lookup_col cols of
           Just mat_cols@(_ : _ : _) ->
             do { checked <- tcRetainedMatrixMatches False mat_cols
                               invis_pat_tys pat_tys plan rhs_ty matches
                ; return (wrap, checked) }
           _ -> function_pattern_error }
  | has_function_binder ty
  , not (one_variable_equation matches)
  = function_pattern_error
  | otherwise
  = case unLoc (mg_alts matches) of
      [L loc match@(Match { m_pats = L pat_loc pats })]
        | not (first_retained_constructor pats) ->
          tcRetainedVarMatches herald ctxt ty pats $ \pat_tys rhs_ty boundary names ->
            let match' = match { m_pats = L pat_loc (bindRetainedWildcards names pats) }
                prepared = matches { mg_alts = L (getLoc (mg_alts matches)) [L loc match'] }
            in tcExprMatchesWithWhereFrom (const match) mctxt (invis_pat_tys ++ pat_tys) rhs_ty
                 (maybe prepared (\(prefix, indices) -> splitRetainedSuffix prefix indices prepared) boundary)
      rows@(_ : _ : _)
        | plain_visible_arrows arity ty
        , not (null constructor_columns)
        , null (filter (`elem` constructor_columns) (retained_columns 0 ty))
        , all (variable_retained_patterns ty . filter isVisArgLPat . hsLMatchPats) rows ->
          tcRetainedVarGroupMatches herald ctxt mctxt arity invis_pat_tys ty matches
      _ | DepFunTy DepFunVisible _ _ <- coreFullView ty
        , arity >= 1 ->
          do { (wrap, (pat_tys, plan, rhs_ty)) <- tcRetainedConArgs herald ctxt arity ty
             ; result <- tcRetainedConEntry invis_pat_tys pat_tys plan rhs_ty matches
             ; return (wrap, result) }
        | otherwise -> failWithTc $
            TcRnRetainedQuantificationError RetainedConstructorCoverage
  where
    -- See Note [Equations over an indexed retained argument].
    indexed_rows names rs
      | Just con_rows <- traverse (indexed_row names) rs
      = Just con_rows
      | not (null (init rs))
      , Just init_rows <- traverse (indexed_row names) (init rs)
      , L loc row@Match { m_ctxt = FunRhs{}, m_pats = L pats_loc [source] } <- last rs
      , let pat = stripParPat source
      , isRetainedVarPat pat
      = Just (init_rows ++ [L loc (row { m_pats = L pats_loc [pat] })])
      | otherwise = Nothing

    indexed_row names (L loc row@Match { m_ctxt = FunRhs{}, m_pats = L pats_loc [source] })
      | pat@(L _ ConPat { pat_con = L _ con, pat_args = PrefixCon{} }) <- stripParPat source
      , unwrapUserRdr con `elemNameSet` names
      = Just (L loc (row { m_pats = L pats_loc [pat] }))
    indexed_row _ _ = Nothing

    plain_visible_arrows 0 _ = True
    plain_visible_arrows n sigma = case coreFullView sigma of
      DepFunTy DepFunVisible _ body -> plain_visible_arrows (n - 1) body
      FunTy af _ mult _ body
        | isVisibleFunArg af, isManyTy mult -> plain_visible_arrows (n - 1) body
      _ -> False

    unguarded_no_invis (L _ Match { m_pats = L _ ps
                                  , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } })
      = all isVisArgLPat ps
    unguarded_no_invis _ = False

    has_nested_retained_con_matches :: [Int] -> MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
    has_nested_retained_con_matches ret_cols (MG { mg_alts = L _ rows })
      = all unguarded_no_invis rows
        && any (\r -> any isNestedRetainedConPat
                        [p | (col, p) <- zip [0 :: Int ..] (hsLMatchPats r), col `elem` ret_cols])
               rows

    has_multi_arg_indexed_gadt_matches :: MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
    has_multi_arg_indexed_gadt_matches (MG { mg_alts = L _ rows })
      = arity >= 2
        && all unguarded_no_invis rows
        && not (null constructor_columns)
        && any (`elem` gadt_cols) constructor_columns
        && all (`elem` all_ret_cols) constructor_columns
        && not first_col_is_vanilla_retained
      where
        gadt_cols = indexed_gadt_columns 0 ty
        all_ret_cols = retained_or_gadt_columns 0 ty
        first_col_is_vanilla_retained = case coreFullView ty of
          DepFunTy DepFunVisible b _ -> isNativeIndexId b && (0 `elem` constructor_columns)
          _ -> False

    function_pattern_error = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionPatterns

    constructor_columns =
      [ column
      | (column, pats) <- zip [0..] (transpose visible_rows)
      , any isRetainedConPat pats ]
    visible_rows = map (filter isVisArgLPat . hsLMatchPats) (unLoc (mg_alts matches))

    is_indexed_gadt_binder :: Id -> Bool
    is_indexed_gadt_binder binder
      | not (isNativeIndexId binder)
      , Just (tc, _) <- splitTyConApp_maybe (coreFullView (varType binder))
      = any isNativeIndexedGadtCon (tyConDataCons tc)
      | otherwise = False

    matching_dep_columns :: (Id -> Bool) -> Int -> Type -> [Int]
    matching_dep_columns keep column sigma = case coreFullView sigma of
      DepFunTy DepFunVisible binder body
        | keep binder -> column : matching_dep_columns keep (column + 1) body
        | otherwise   -> matching_dep_columns keep (column + 1) body
      ForAllTy _ (Bndr _ Required) body -> matching_dep_columns keep (column + 1) body
      ForAllTy _ _ body                 -> matching_dep_columns keep column body
      FunTy af _ _ _ body
        | isVisibleFunArg af -> matching_dep_columns keep (column + 1) body
        | otherwise          -> matching_dep_columns keep column body
      _ -> []

    retained_columns     = matching_dep_columns isNativeIndexId
    indexed_gadt_columns = matching_dep_columns is_indexed_gadt_binder
    retained_or_gadt_columns =
      matching_dep_columns (\b -> isNativeIndexId b || is_indexed_gadt_binder b)

    -- See Note [Function-valued retained binders] in GHC.Core.Type.
    has_function_binder :: Type -> Bool
    has_function_binder ty = case coreFullView ty of
      DepFunTy _ binder body -> isNativeIndexFunctionId binder || has_function_binder body
      ForAllTy _ _ body -> has_function_binder body
      FunTy _ _ _ _ body -> has_function_binder body
      _ -> False

    one_variable_equation :: MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
    one_variable_equation (MG { mg_alts = L _ [L _ (Match { m_pats = L _ pats })] })
      = variable_retained_patterns ty (filter isVisArgLPat pats)
    one_variable_equation _ = False

    variable_retained_patterns :: Type -> [LPat GhcRn] -> Bool
    variable_retained_patterns _ [] = True
    variable_retained_patterns ty (pat : pats) = case coreFullView ty of
      DepFunTy DepFunVisible _ body ->
        isRetainedVarPat pat && variable_retained_patterns body pats
      ForAllTy _ (Bndr _ Required) body -> variable_retained_patterns body pats
      FunTy af _ _ _ body
        | isVisibleFunArg af -> variable_retained_patterns body pats
        | otherwise -> variable_retained_patterns body (pat : pats)
      ForAllTy _ binder body
        | isInvisibleForAllTyFlag (binderFlag binder) ->
            variable_retained_patterns body (pat : pats)
      _ -> False

    first_retained_constructor pats
      | DepFunTy DepFunVisible _ _ <- coreFullView ty
      = maybe False isRetainedConPat (listToMaybe (filter isVisArgLPat pats))
      | otherwise = False

{- Note [Equations over an indexed retained argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a single-argument function matches prefix constructors of an indexed GADT
retained binder, 'tcRetainedMatches' binds one shared runtime variable and
checks the equations via 'tcIndexedCaseMatches', preserving guard order and
source error contexts. Multi-argument unguarded clauses over indexed GADT
retained binders lower via 'desugarDepClauseTree' to nested @case@ expressions
that likewise dispatch to 'tcIndexedCaseMatches'.
-}

{- Note [Checked retained lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'tcRetainedMatches' checks both signed function bindings and checked lambdas
against an expected dependent @Pi@ telescope (GHC Proposal #281; Goguen,
McBride & McKinna 2006):
- Preserve the source lambda variant, visible arity, and outer skolemised
  binders ('invis_pat_tys'). Usage scaling stays at the caller.
- Select the constructor-matrix path only when the leading expected argument is
  a visible 'DepFunTy'; ordinary leading constructor patterns before a retained
  result use 'tcRetainedVarMatches' without eta-expanding the result.
- Keep leading required @forall@ wrappers outside any runtime value pattern.
-}

-- | Find a retained binder after ordinary arrows and erased foralls.
hasRetainedFunTy :: Type -> Bool
hasRetainedFunTy ty = case coreFullView ty of
  DepFunTy DepFunVisible _ _ -> True
  ForAllTy _ binder result
    | isInferredForAllTyFlag (binderFlag binder)
    , DepFunTy DepFunInvisible _ _ <- coreFullView result -> True
    | otherwise -> hasRetainedFunTy result
  FunTy _ _ _ _ result -> hasRetainedFunTy result
  _ -> False

-- | Lexical bindings installed inside each branch of a motive-checked @case@:
-- an optional @(scrutinee_names, motive_id)@ pair plus @(capture_names, shared_name)@
-- aliases resolved against the pattern-bound @Id@s of that branch.
type MotiveLexEnv = (Maybe ([Name], Id), [([Name], Name)])

noMotiveLexEnv :: MotiveLexEnv
noMotiveLexEnv = (Nothing, [])

data DepCaseSpec
  = DepVarScrut  Bool Id
  | DepCallScrut Type Id [Type] Type (LHsExpr GhcTc)

-- | Elaborate a checked dependent @case@ on a local variable through a shared
-- motive telescope. See Note [Source retained case refinement].
tcRetainedCase :: Bool -> Id -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
               -> TcM (Maybe (HsExpr GhcTc))
tcRetainedCase dependent index = tcDependentCaseWithMotive (DepVarScrut dependent index)

-- | Elaborate a checked dependent @case@ on an opaque call through a strict
-- alias and shared motive telescope. See Note [Source opaque call refinement].
tcRetainedCallCase :: Type -> LHsExpr GhcTc
                   -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
                   -> TcM (Maybe (HsExpr GhcTc))
tcRetainedCallCase call_ty@(nativeCallSpine_maybe -> Just (fun, indices, domain)) scrut matches exp_ty
  = tcDependentCaseWithMotive (DepCallScrut call_ty fun indices domain scrut) matches exp_ty
tcRetainedCallCase _ _ _ _ = pure Nothing

-- | Unified motive-abstracting dependent @case@ checker for variable and
-- opaque-call scrutinees, using lexical 'TcLclEnv' bindings, 'tcl_source_let_views',
-- and nominal constructor Givens instead of SYB branch-AST rewriting.
-- See Note [Source retained case refinement] and Note [Source opaque call refinement].
tcDependentCaseWithMotive
  :: DepCaseSpec -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
  -> TcM (Maybe (HsExpr GhcTc))
tcDependentCaseWithMotive _ (MG { mg_alts = L _ [] }) _ = pure Nothing
tcDependentCaseWithMotive spec matches@(MG { mg_alts = L loc alternatives }) (Check result_ty) =
  case spec of
    DepVarScrut dependent index -> do
      let domain  = varType index
          depends = anyFreeVarsOfType (== index)
          dep_givens = filter (depends . varType) <$> getNativeGivens
          lookup_case_names = do
            let nms = nameSetElemsStable (retainedCaseNames matches)
            catMaybes <$> forM nms (\nm -> do
              thing <- tcLookupLcl_maybe nm
              pure $ case thing of
                Just (ATcId { tct_id = v }) | isLocalId v, not (isCoVar v) -> Just (nm, v)
                _ -> Nothing)
          dep_captures gs = do
            resolved <- lookup_case_names
            let named = nub [v | (_, v) <- resolved, depends (varType v)]
                types = result_ty : map varType (gs ++ named)
                extra = [ v | v <- tyCoVarsOfTypesList types, isId v, not (isCoVar v)
                        , isLocalId v, depends (varType v), v `notElem` named ]
            pure (resolved, scopedSort (named ++ extra))
          use_application = do
            has_dep <- if dependent then pure True else tcHasDepImports
            if not has_dep || not (dependent || isNativeIndexId index)
              then pure False
              else do
                let retained_worker (ATcId { tct_id = w })
                      | DepFunTy DepFunVisible b body <- coreFullView (varType w)
                      , varType b `eqType` domain = anyFreeVarsOfType (== b) body
                    retained_worker _ = False
                    known_local_app (HsApp _ fn arg)
                      | HsVar _ (L _ (WithUserRdr _ w_nm)) <- unLoc (stripParensLHsExpr fn)
                      , HsVar _ (L _ (WithUserRdr _ a_nm)) <- unLoc (stripParensLHsExpr arg) = do
                          mb_a <- tcLookupLcl_maybe a_nm
                          case mb_a of
                            Just (ATcId { tct_id = av }) | av == index ->
                              maybe False retained_worker <$> tcLookupLcl_maybe w_nm
                            _ -> pure False
                    known_local_app _ = pure False
                worker_in_scope <- anyNameEnv retained_worker <$> getLclTypeEnv
                if not worker_in_scope
                  then pure False
                  else do
                    complete <- completeRetainedCaseRows domain alternatives
                    known <- if complete then hasRetainedApplication known_local_app matches else pure False
                    if not known || retainedCaseHasQuote matches
                      then pure False
                      else if dependent then pure True
                           else (&&) <$> (null . snd <$> dep_captures []) <*> (null <$> dep_givens)
      givens <- if dependent then dep_givens else pure []
      (resolved_nms0, captures) <- if dependent then dep_captures givens else pure ([], [])
      let explicit = dependent && (not (null captures) || not (null givens) || depends result_ty)
      application <- if explicit || not (isTauTy result_ty)
                        || not (isLiftedTypeKind (typeKind result_ty))
                        || anyFreeVarsOfType (const True) result_ty
                     then pure False
                     else use_application
      if not (explicit || application)
        then pure Nothing
        else setSrcSpanA loc $ do
          check_common_preconditions
          mb_nested <- if application then pure Nothing else desugarDepCaseAlts False index alternatives
          case mb_nested of
            Just (plug_alts, Just (fb_rhs, uses)) | uses >= 2 -> do
              let dep_vars = (True, index) :
                    [ (any (anyFreeVarsOfType (== c)) (result_ty : map varType (captures ++ givens)), c)
                    | c <- captures ]
              (join_binds, join_call) <- mkDepJoinBinds dep_vars (map varType givens) result_ty fb_rhs
              (binds_tc, mb_case_tc) <- tcLocalBinds join_binds $
                tcDependentCaseWithMotive spec (matches { mg_alts = L loc (plug_alts join_call) }) (Check result_ty)
              pure (HsLet noExtField (noLocA binds_tc) . noLocA <$> mb_case_tc)
            Just (plug_alts, mb_fb) ->
              let fb_leaf = maybe (panic "desugarDepCaseAlts: unused fallback") fst mb_fb
              in tcDependentCaseWithMotive spec (matches { mg_alts = L loc (plug_alts fb_leaf) }) (Check result_ty)
            Nothing -> do
              resolved_nms <- if dependent then pure resolved_nms0 else lookup_case_names
              constructors <- resolve_case_rows domain (not application) True
              motive_idx   <- newSysLocalId (occNameFS (getOccName index)) ManyTy domain
              let in_scope = mkInScopeSetList
                    (motive_idx : tyCoVarsOfTypesList (result_ty : map varType (index : givens ++ captures)))
                  subst0   = Subst.mkIdSubst in_scope (unitVarEnv index (Var motive_idx))
                  used_as_idx v = any (anyFreeVarsOfType (== v)) (result_ty : map varType (captures ++ givens))
                  bind_cap scope v = do
                    nm <- newSysName (nameOccName (idName v))
                    let sh = mkLocalId nm ManyTy (Subst.substTy scope (varType v))
                    pure (extendIdSubstWithClone scope v sh, sh)
              (cap_subst, shared) <- mapAccumLM bind_cap subst0 captures
              let generalize = Subst.substTy cap_subst
              dictionaries <- newEvVars (map (generalize . varType) givens)
              let shared_names = map idName shared
                  aliases_of v = nub (idName v : [nm | (nm, u) <- resolved_nms, u == v])
                  lex_env      = ( Just (aliases_of index, motive_idx)
                                 , [(aliases_of c, sh_nm) | (c, sh_nm) <- zip captures shared_names] )
                  matches'     = prepare_motive_alts Nothing application shared_names constructors
                  arg_tys      = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) (motive_idx : shared ++ dictionaries)
                  cap_plan v b = if used_as_idx v then RetainedConIndex b else RetainedConValue
                  plan         = RetainedConIndex motive_idx
                                 : zipWith cap_plan captures shared
                                 ++ map RetainedConDict dictionaries
              checked <- tcRetainedConEntryWithEnv lex_env [] arg_tys plan
                           (mkCheckExpType (generalize result_ty)) matches'
              mapM_ (tcEmitBindingUsage . scaleUE ManyTy . singleUsageUE) captures
              let fun     = HsLam noAnn LamSingle checked
                  idx_arg = noLocA (HsVar noExtField (noLocA index))
                  args    = map (noLocA . HsVar noExtField . noLocA) captures
                  applied = foldl' (\f a -> HsApp noExtField (noLocA f) a)
                                   (HsApp noExtField (noLocA fun) idx_arg) args
              pure (Just (mkHsWrap (mkWpEvVarApps givens) applied))

    DepCallScrut call_ty fun indices domain scrut -> do
      let call_ids = filter (\v -> isId v && not (isCoVar v)) (tyCoVarsOfTypeList call_ty)
          family_view ty = do
            ty' <- liftZonkM (zonkTcType ty)
            ct_loc <- getCtLocM PatCheckOrigin Nothing
            runTcSEqualities (rewriteFamilyAxioms ct_loc ty')
          depends ty
            | retainedCallOccurs call_ty ty = pure True
            | all (\v -> anyFreeVarsOfType (== v) ty) call_ids
            = retainedCallOccurs call_ty . (\(Reduction _ v) -> v) <$> family_view ty
            | otherwise = pure False
      resolved_caps <- fmap catMaybes $ forM (nameSetElemsStable (retainedCaseNames matches)) $ \nm -> do
        thing <- tcLookupLcl_maybe nm
        case thing of
          Just (ATcId { tct_id = v }) | isLocalId v, not (isCoVar v) -> do
            dep <- depends (varType v)
            if not dep then pure Nothing else do
              unless (isManyTy (idMult v)) $
                failWithTc (TcRnRetainedQuantificationError (RetainedPatternType (varType v)))
              pure (Just (nm, v))
          _ -> pure Nothing
      let captures = nub (map snd resolved_caps)
      givens <- getNativeGivens >>= filterM (depends . varType)
      result_depends <- depends result_ty
      let existing_trigger = not (null captures) || not (null givens)
                             || result_depends || hasCallTypeAnnotation fun indices matches
      metadata_trigger <- if existing_trigger || retainedCaseHasQuote matches
                          then pure False
                          else do
                            complete <- completeRetainedCaseRows domain alternatives
                            if complete
                              then hasRetainedApplication (namedCallWorkerApp domain depends) matches
                              else pure False
      if not (existing_trigger || metadata_trigger)
        then pure Nothing
        else setSrcSpanA loc $ do
          check_common_preconditions
          constructors <- resolve_case_rows domain False False
          dflags <- getDynFlags
          unless (all (== Breakpoints) (ticks_passes (initTicksConfig dflags))
                  && not (sccProfilingEnabled dflags)) $
            failWithTc (TcRnRetainedQuantificationError RetainedCallInstrumentation)
          alias <- newSysLocalId (fsLit "call_index") ManyTy domain
          proof <- newEvVar (mkNomEqPred (TermVarTy alias) call_ty)
          let alias_co = mkSymCo (mkCoVarCo proof)
              transport ty = do
                Reduction fam_co fam_ty <- family_view ty
                mkTransCo fam_co <$> tcRetainedCallCoercion call_ty alias_co fam_ty
          result_co   <- transport result_ty
          capture_cos <- mapM (transport . varType) captures
          given_cos   <- mapM (transport . varType) givens
          motive_idx  <- newSysLocalId (fsLit "call_motive") ManyTy domain
          let in_scope = mkInScopeSetList
                (alias : motive_idx : tyCoVarsOfTypesList
                  (call_ty : result_ty : map varType (givens ++ captures)))
              subst0     = Subst.mkIdSubst in_scope (unitVarEnv alias (Var motive_idx))
              generalize = Subst.substTy subst0 . pSnd . coercionKind
          motive_proof     <- newEvVar (mkNomEqPred (TermVarTy motive_idx) call_ty)
          old_dictionaries <- newEvVars (map generalize given_cos)
          shared_names     <- mapM (newSysName . nameOccName . idName) captures
          let dictionaries = motive_proof : old_dictionaries
              aliases_of c = nub (idName c : [nm | (nm, u) <- resolved_caps, u == c])
              lex_env      = (Nothing, [(aliases_of c, sh_nm) | (c, sh_nm) <- zip captures shared_names])
              matches'     = prepare_motive_alts (Just (fun, indices)) False shared_names constructors
              pat_tys      = map (mkCheckExpFunPatTy . Scaled ManyTy)
                               (domain : map varType dictionaries ++ map generalize capture_cos)
              plan         = RetainedConIndex motive_idx
                             : map RetainedConDict dictionaries
                             ++ map (const RetainedConValue) captures
          checked <- tcRetainedConEntryWithEnv lex_env [] pat_tys plan
                       (mkCheckExpType (generalize result_co)) matches'
          strict <- xoptM LangExt.Strict
          mapM_ (tcEmitBindingUsage . singleUsageUE) captures
          let protect_row (L rloc row@(Match { m_pats = L ploc (first : rest) }))
                = L rloc (row { m_pats = L ploc (first : map (noLocA . LazyPat noExtField) rest) })
              protect_row row = row
              checked' | strict, MG { mg_alts = L gloc rows } <- checked
                       = checked { mg_alts = L gloc (map protect_row rows) }
                       | otherwise = checked
              motive     = HsLam noAnn LamSingle checked'
              alias_arg  = noLocA (HsVar noExtField (noLocA alias))
              ev_args    = zipWith (\g co -> if isCoVar g
                                             then evCoercion (mkCoCast (mkCoVarCo g) (mkSubCo co))
                                             else evCast (Var g) (mkSubCo co))
                                   givens given_cos
              with_ev    = mkHsWrap (mkWpEvApps (evCoercion (mkCoVarCo proof) : ev_args))
                             (HsApp noExtField (noLocA motive) alias_arg)
              cap_args   = zipWith (\v co -> mkLHsWrapCo co (noLocA (HsVar noExtField (noLocA v))))
                                   captures capture_cos
              applied    = foldl' (\f a -> HsApp noExtField (noLocA f) a) with_ev cap_args
              transported = mkHsWrapCo (mkSymCo result_co) applied
          pure (Just (XExpr (NativeCaseAlias scrut alias proof result_ty (noLocA transported))))
  where
    case_error :: TcM a
    case_error = failWithTc (TcRnRetainedQuantificationError RetainedCasePatterns)

    check_common_preconditions = do
      when (retainedCaseHasQuote matches) $
        failWithTc (TcRnRetainedQuantificationError RetainedCaseQuotation)
      unless (isLiftedTypeKind (typeKind result_ty)) $
        failWithTc (TcRnRetainedQuantificationError (RetainedResultKind result_ty))

    resolve_case_rows domain allow_fb allow_empty_dom = do
      let domain_cons = maybe [] (tyConDataCons . fst) (splitTyConApp_maybe (coreFullView domain))
          has_fb
            | allow_fb, not (null (init alternatives))
            , L _ (Match { m_pats = L _ [fb_p]
                         , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }) <- last alternatives
            = isRetainedVarPat (stripParPat fb_p)
            | otherwise = False
          resolve_alt (L mloc (Match { m_pats = L _ [pat]
                                     , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
            = setSrcSpanA mloc $ do
                unless (is_simple_con_pat pat) case_error
                tcRetainedCon domain pat >>= maybe case_error pure
          resolve_alt (L mloc _) = setSrcSpanA mloc case_error
          is_simple_con_pat pat = case unLoc (retainedConPat pat) of
            ParPat _ inner -> is_simple_con_pat inner
            ConPat { pat_args = PrefixCon _ fs } -> all isRetainedVarPat fs
            _ -> False
      constructors <- if has_fb
                      then (\cs -> map Just cs ++ [Nothing]) <$> mapM resolve_alt (init alternatives)
                      else map Just <$> mapM resolve_alt alternatives
      let cons = [c | Just (c, _) <- constructors]
      if has_fb
        then unless (length cons == length (nub cons)
                     && length cons < length domain_cons
                     && all (`elem` domain_cons) cons) case_error
        else unless ((allow_empty_dom || not (null cons))
                     && cons `equalLength` domain_cons
                     && all (`elem` domain_cons) cons) case_error
      pure constructors

    prepare_motive_alts mb_call_spec keep_case_ctxt shared_names constructors =
      matches { mg_alts = L loc (zipWith add_args alternatives constructors) }
      where
        extra = map (noLocA . VarPat noExtField . noLocA) shared_names
        add_args (L mloc m@(Match { m_pats = L ploc pats })) mb_con =
          let subject_pats = maybe (map stripParPat pats)
                                   (\(_, fs) -> map (bindRetainedConFields fs) pats) mb_con
              ctxt = if keep_case_ctxt then m_ctxt m else LamAlt LamSingle
              m1   = case (mb_call_spec, mb_con) of
                       (Just (c_fun, c_idxs), Just (con, fs)) ->
                         rewriteCallTypeAnnotation c_fun c_idxs con fs m
                       _ -> m
          in L mloc (m1 { m_ctxt = ctxt, m_pats = L ploc (subject_pats ++ extra) })
tcDependentCaseWithMotive _ _ _ = pure Nothing

-- | Detect whether a renamed AST contains a type annotation spelling the exact
-- checked opaque call @fun indices@.
hasCallTypeAnnotation :: Data a => Id -> [Type] -> a -> Bool
hasCallTypeAnnotation fun indices = go
  where
    go :: Data b => b -> Bool
    go thing
      | Just HsTypedBracket{}   <- (cast thing :: Maybe (HsExpr GhcRn)) = False
      | Just HsUntypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = False
      | Just ty <- (cast thing :: Maybe (HsType GhcRn))
      , matchExactCallType fun indices ty                               = True
      | Just _ <- (cast thing :: Maybe Name)    = False
      | Just _ <- (cast thing :: Maybe NameSet) = False
      | Just _ <- (cast thing :: Maybe Type)    = False
      | otherwise = or (gmapQ go thing)

-- | Rewrite exact @fun indices@ type annotations in a branch to the branch's
-- promoted constructor application so local signatures refine with the branch.
rewriteCallTypeAnnotation :: Data a => Id -> [Type] -> DataCon -> [Id] -> a -> a
rewriteCallTypeAnnotation fun indices con fields = go
  where
    call_names = idName fun : [idName v | idx <- indices, v <- tyCoVarsOfTypeList idx, isId v]
    con_type :: HsType GhcRn
    con_type = foldl' (\f a -> HsAppTy noExtField (noLocA f) a)
      (HsTyVar noAnn IsPromoted (noLocA (noUserRdr (dataConName con))))
      (map (noLocA . HsTyVar noAnn NotPromoted . noLocA . noUserRdr . idName) fields)
    go :: Data b => b -> b
    go thing
      | Just ty <- (cast thing :: Maybe (HsType GhcRn))
      , matchExactCallType fun indices ty
      = fromMaybe thing (cast con_type)
      | Just names <- (cast thing :: Maybe NameSet)
      , all (`elemNameSet` names) call_names
      = fromMaybe thing (cast (names `unionNameSet` mkNameSet (dataConName con : map idName fields)))
      | Just _ <- (cast thing :: Maybe Name) = thing
      | Just _ <- (cast thing :: Maybe Type) = thing
      | otherwise = gmapT go thing

matchExactCallType :: Id -> [Type] -> HsType GhcRn -> Bool
matchExactCallType fun indices ty =
  let (head_ty, args) = split_apps [] ty
  in is_name (idName fun) head_ty && indices `equalLength` args
     && and (zipWith is_index indices args)
  where

    is_name nm (HsParTy _ ty) = is_name nm (unLoc ty)
    is_name nm (HsTyVar _ NotPromoted (L _ (WithUserRdr _ actual))) = nm == actual
    is_name _ _ = False

    is_index idx (HsParTy _ ty) = is_index idx (unLoc ty)
    is_index (TermVarTy var) ty = is_name (idName var) ty
    is_index (TyConApp tc args) ty
      | Just con <- isPromotedDataCon_maybe tc
      , (HsTyVar _ _ (L _ (WithUserRdr _ nm)), src_args) <- split_apps [] ty
      , let idx_args = drop (length (dataConUnivTyVars con)) args
      = nm == dataConName con && idx_args `equalLength` src_args
        && and (zipWith is_index idx_args src_args)
    is_index _ _ = False

    split_apps args (HsParTy _ ty)        = split_apps args (unLoc ty)
    split_apps args (HsAppTy _ fn arg)    = split_apps (unLoc arg : args) (unLoc fn)
    split_apps args (HsOpTy _ l op r)     = split_apps (unLoc l : unLoc r : args) (unLoc op)
    split_apps args (HsExplicitListTy _ _ [])
      = (HsTyVar noAnn IsPromoted (noLocA (noUserRdr (dataConName nilDataCon))), args)
    split_apps args ty = (ty, args)

-- | Metadata probe checking whether a local retained worker application in a
-- branch produces a residual type that mentions the scrutinized opaque call.
namedCallWorkerApp :: Type -> (Type -> TcM Bool) -> HsExpr GhcRn -> TcM Bool
namedCallWorkerApp _ depends expr
  | Just (name, args) <- spine [] expr = do
      thing <- tcLookupLcl_maybe name
      case thing of
        Just (ATcId { tct_id = worker }) -> consume False (varType worker) args
        _ -> pure False
  | otherwise = pure False
  where
    spine args (HsPar _ e)          = spine args (unLoc e)
    spine args (HsApp _ fn arg)     = spine (arg : args) (unLoc fn)
    spine args (OpApp _ l op r)     = spine (l : r : args) (unLoc op)
    spine args (ExplicitList _ [])  = Just (dataConName nilDataCon, args)
    spine args (HsVar _ (L _ (WithUserRdr _ nm))) = Just (nm, args)
    spine _ _ = Nothing

    consume _ ty _ | DepFunTy DepFunInvisible _ _ <- coreFullView ty = pure False
    consume consumed ty args
      | DepFunTy DepFunVisible binder _ <- coreFullView ty
      = case args of
          arg : rest -> do
            actual <- if isNativeIndexFunctionId binder
                      then do
                        mb_v <- local_var (unLoc arg)
                        pure $ case mb_v of
                          Just v | isNativeIndexFunctionId v, varType v `eqType` varType binder -> Just (Var v)
                          _ -> Nothing
                      else scalar_arg (varType binder) (unLoc arg)
            maybe (pure False) (\val -> consume True (applyTypeToArgs ty [val]) rest) actual
          [] -> pure False
      | otherwise = if consumed && isTauTy ty then depends ty else pure False

    scalar_arg expected (HsPar _ arg) = scalar_arg expected (unLoc arg)
    scalar_arg expected e
      | Just (nm, args) <- spine [] e, isDataOcc (nameOccName nm) = do
          cl <- tcLookupConLike (noUserRdr nm)
          case cl of
            RealDataCon con
              | Just ty_args <- nativeIndexDataConTyArgs expected con
              , args `lengthIs` dataConSourceArity con
              -> scalar_fields expected (foldl' App (Var (dataConWorkId con)) (map Type ty_args)) args
            _ -> pure Nothing
      | HsVar{} <- e = do
          mb_v <- local_var e
          pure $ case mb_v of
            Just v | isNativeOpaqueValueId v, varType v `eqType` expected -> Just (Var v)
            _ -> Nothing
      | otherwise = pure Nothing

    scalar_fields expected worker [] = pure $ do
      idx <- exprToNativeIndex worker
      guard (typeKind idx `eqType` expected)
      pure worker
    scalar_fields expected worker (arg : args)
      | let sigma = exprType worker
      , Just dom <- case coreFullView sigma of
          DepFunTy DepFunVisible b _ -> Just (varType b)
          _ -> (\(_, _, _, inp, _) -> inp) <$> splitFunTy_maybe sigma
      = scalar_arg dom (unLoc arg) >>= maybe (pure Nothing) (\v -> scalar_fields expected (App worker v) args)
      | otherwise = pure Nothing

    local_var (HsPar _ e) = local_var (unLoc e)
    local_var (HsVar _ (L _ (WithUserRdr _ nm))) = do
      thing <- tcLookupLcl_maybe nm
      pure $ case thing of
        Just (ATcId { tct_id = v }) | isLocalId v, not (isExternalName (idName v)) -> Just v
        _ -> Nothing
    local_var _ = pure Nothing

-- | Decline unsupported ordinary cases without adding a new diagnostic.
completeRetainedCaseRows :: Type -> [LMatch GhcRn (LHsExpr GhcRn)] -> TcM Bool
completeRetainedCaseRows domain alternatives
  = do { rows <- mapM row_constructor alternatives
       ; let expected = maybe [] (tyConDataCons . fst) (splitTyConApp_maybe (coreFullView domain))
       ; pure $ case sequence rows of
           Just cons -> not (null cons) && cons `equalLength` expected && all (`elem` cons) expected
           Nothing   -> False }
  where
    row_constructor (L _ (Match { m_pats = L _ [pat]
                                , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = pattern_constructor pat
    row_constructor _ = pure Nothing
    pattern_constructor pat = case unLoc (retainedConPat pat) of
      ParPat _ inner -> pattern_constructor inner
      ConPat { pat_con = L _ name, pat_args = PrefixCon _ fields }
        | all isRetainedVarPat fields -> do
            cl <- tcLookupConLike name
            pure $ case cl of
              RealDataCon con
                | Just _ <- nativeIndexDataConTyArgs domain con
                , fields `lengthIs` dataConSourceArity con -> Just con
              _ -> Nothing
      _ -> pure Nothing

-- | Inspect existing checked worker types without elaborating source values.
hasRetainedApplication :: Data a => (HsExpr GhcRn -> TcM Bool) -> a -> TcM Bool
hasRetainedApplication check thing
  | Just expr <- (cast thing :: Maybe (HsExpr GhcRn))
  = case expr of
      HsTypedBracket{}   -> pure False
      HsUntypedBracket{} -> pure False
      HsTypedSplice{}    -> pure False
      HsUntypedSplice{}  -> pure False
      XExpr{}            -> pure False
      HsApp{}            -> do { ok <- check expr; if ok then pure True else children }
      _                  -> children
  | Just _ <- (cast thing :: Maybe Name)    = pure False
  | Just _ <- (cast thing :: Maybe NameSet) = pure False
  | Just _ <- (cast thing :: Maybe Type)    = pure False
  | otherwise = children
  where
    children = anyM id (gmapQ (hasRetainedApplication check) thing)

-- | Collect resolved names outside quotations and stored free-name sets.
retainedCaseNames :: Data a => a -> NameSet
retainedCaseNames thing
  | Just HsTypedBracket{}   <- (cast thing :: Maybe (HsExpr GhcRn)) = emptyNameSet
  | Just HsUntypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = emptyNameSet
  | Just name <- cast thing                                         = unitNameSet name
  | Just _ <- (cast thing :: Maybe NameSet)                         = emptyNameSet
  | Just _ <- (cast thing :: Maybe Type)                            = emptyNameSet
  | otherwise = unionNameSets (gmapQ retainedCaseNames thing)

-- | Detect quotations before lexical index or variable aliasing enters scope.
retainedCaseHasQuote :: Data a => a -> Bool
retainedCaseHasQuote = isJust . retainedMatchQuotation

-- | Rename resolved row Names and free-name sets after checking quotations.
renameRetainedMatch :: Data a => [(Name, Name)] -> a -> a
renameRetainedMatch [] thing = thing
renameRetainedMatch names thing0 = go thing0
  where
    rename name = fromMaybe name (lookup name names)
    go :: Data b => b -> b
    go thing
      | Just name <- (cast thing :: Maybe Name)
      = fromMaybe thing (cast (rename name))
      | Just free <- (cast thing :: Maybe NameSet)
      = fromMaybe thing (cast (mkNameSet (map rename (nameSetElemsStable free))))
      | Just _ <- (cast thing :: Maybe Type) = thing
      | otherwise = gmapT go thing

-- | Find a quotation's source span without traversing compiler type graphs.
retainedMatchQuotation :: Data a => a -> Maybe SrcSpan
retainedMatchQuotation thing
  | Just (L loc expr) <- (cast thing :: Maybe (LHsExpr GhcRn))
  , is_quote expr = Just (locA loc)
  | Just _ <- (cast thing :: Maybe Name)    = Nothing
  | Just _ <- (cast thing :: Maybe NameSet) = Nothing
  | Just _ <- (cast thing :: Maybe Type)    = Nothing
  | otherwise = msum (gmapQ retainedMatchQuotation thing)
  where
    is_quote :: HsExpr GhcRn -> Bool
    is_quote HsTypedBracket{}   = True
    is_quote HsUntypedBracket{} = True
    is_quote _                  = False

{- Note [Source retained case refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'tcDependentCaseWithMotive' elaborates a dependent @case@ on a local variable
@x@ by abstracting the expected result type, dependent local captures (sorted
in dependency order), and dependent lexical Givens over a fresh motive index
@m@ (Goguen, McBride & McKinna 2006; GHC Proposal #281).

Rather than rewriting branch ASTs with SYB, 'checkRetainedRows' installs a
lexical 'MotiveLexEnv' around each branch's 'tcGRHSs':
- @(idName x, m)@ binds in 'TcLclEnv' under 'tcCheckUsage', while
  'tcWithSourceLetView' records @m -> 'K fields@ for 'GHC.Tc.Gen.App.tcSourceLetHead'
  and 'withRetainedRowGivens' scopes @cv :: TermVarTy m ~# 'K fields@ over the
  branch ('tcRetainedDeclaredTypeView', 'tcRetainedResultCoercion', and
  'GHC.Tc.Gen.HsType.tcRetainedAmbientIndices').
- Each captured local's source 'Name' aliases its branch-refined pattern 'Id'
  in 'TcLclEnv', preserving 'bind_fvs' and source spans without AST mutation.
The checked motive lambda is then applied to @x@, the captured locals, and the
enclosing Given evidence.
-}

{- Note [Source opaque call refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a checked opaque call scrutinee @f xs@ ('DepCallScrut'),
'tcDependentCaseWithMotive' introduces a strict case alias @r@ with nominal
proof @proof :: TermVarTy r ~# f xs@ ('NativeCaseAlias'), generalizes exact
occurrences of @f xs@ (after family reduction where all call dependencies are
present) across the result, captures, and incoming Givens via
'tcRetainedCallCoercion', and passes @motive_proof :: TermVarTy m ~# f xs@ into
the shared motive telescope.

Inside each branch @K fields@, 'withRetainedRowGivens' transports @motive_proof@
along @cv :: TermVarTy m ~# 'K fields@ to provide @'K fields ~# f xs@ as a
lexical Given across local bindings, type signatures, and the RHS, while
captured locals alias their pattern-bound @Id@s lexically via 'MotiveLexEnv'.
Under @-XStrict@, generated evidence and capture patterns are wrapped in
'LazyPat' so only the constructor pattern forces the alias.
-}

-- | Introduce runtime index identities before checking the remaining patterns.
-- See Note [Erased suffixes in retained matches].
tcRetainedVarMatches :: ExpectedFunTyCtxt -> UserTypeCtxt -> Type
                     -> [LPat GhcRn]
                     -> ([ExpPatType] -> ExpRhoType -> Maybe (Int, [Id]) -> [Maybe Name] -> TcM a)
                     -> TcM (HsWrapper, a)
tcRetainedVarMatches herald ctxt initial_ty source_pats = go [] initial_ty initial_pats
  where
    initial_pats = filter isVisArgLPat source_pats
    trailing_erased = not (null initial_pats) && not (isVisArgLPat (last source_pats))

    finish acc rhs_ty boundary thing_inside =
      do { let args = reverse acc
         ; result <- thing_inside (map fst args) rhs_ty boundary
                       [name | (pat_ty, name) <- args, isVisibleExpPatType pat_ty]
         ; return (idHsWrapper, result) }

    go acc ty pats thing_inside
      | (theta, _) <- tcSplitPhiTy ty, not (null theta)
      = split_suffix acc ty pats RetainedContextPattern thing_inside
      | ForAllTy{} <- coreFullView ty
      , not (leading_required ty acc)
      , not (null pats) || trailing_erased
      = split_suffix acc ty pats RetainedResultWrapper thing_inside
    go acc ty [] thing_inside = finish acc (mkCheckExpType ty) Nothing thing_inside
    go acc ty (pat : pats) thing_inside
      | DepFunTy DepFunVisible binder _ <- coreFullView ty
      = setSrcSpanA (getLoc pat) $
        do { name <- case unLoc pat of
               VarPat _ (L _ name) -> return name
               WildPat{} -> newSysName (getOccName binder)
               _ -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
           ; let index = mkLocalId name (idMult binder) (varType binder)
                 pat_ty = mkCheckExpFunPatTy (Scaled (idMult binder) (varType binder))
           ; (wrap, result) <- tcExtendTermScopeBinders [index] $
                               go ((pat_ty, Just name) : acc)
                                  (applyTypeToArgs ty [Var index]) pats thing_inside
           ; return (liftRetainedLambdaWrap index wrap, result) }
      | FunTy { ft_af = af } <- coreFullView ty
      = do { (ma_co, arg_ty, rhs_ty) <- setSrcSpanA (getLoc pat) $
                  matchRetainedLambdaArrow herald (length initial_pats, initial_ty) ty
           ; (wrap, result) <- go ((mkCheckExpFunPatTy arg_ty, Nothing) : acc) rhs_ty pats thing_inside
           ; wrap' <- liftOrdinaryLambdaWrap af ma_co arg_ty rhs_ty wrap
           ; return (wrap', result) }
      | otherwise
      = do { (wrap, (rest_wrap, result)) <- matchExpectedFunTys herald ctxt 1 (mkCheckExpType ty) $
               \pat_tys rhs_ty ->
                 do { rhs_ty <- readExpType rhs_ty
                    ; go (map (, Nothing) (reverse pat_tys) ++ acc) rhs_ty pats thing_inside }
           ; unless (isIdHsWrapper wrap || leading_required ty acc) $
               failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
           ; return (wrap <.> rest_wrap, result) }

    leading_required ty acc
      | ForAllTy _ (Bndr _ Required) _ <- coreFullView ty
      = not (any (isExpFunPatType . fst) acc)
      | otherwise = False

    split_suffix acc ty pats err thing_inside
      = do { strict <- xoptM LangExt.Strict
           ; let visible = filter isVisibleExpPatType (map fst (reverse acc))
                 can_split ty pat = not (isExpFunPatType ty) ||
                                    (not strict && nonforcing_pattern pat)
           ; unless (null pats || and (zipWith can_split visible initial_pats)) $
               failWithTc (TcRnRetainedQuantificationError err)
           ; let (indices, co) = expose_suffix ty
           ; (wrap, result) <- finish acc (mkCheckExpType (coercionLKind co))
                                 (Just (length visible, indices)) thing_inside
           ; return (mkWpCastR co <.> wrap, result) }

    expose_suffix ty
      | (indices@(_:_), co) <- exposeInvisibleRetained ty
      = (indices, co)
      | ForAllTy ma binder body <- coreFullView ty
      , isInvisibleForAllTyFlag (binderFlag binder)
      , (indices@(_:_), co) <- expose_suffix body
      = (indices, mkHomoForAllCos [(mkNomReflCo ma, binder)] co)
      | otherwise = ([], mkRepReflCo ty)

    nonforcing_pattern (L _ pat) = case pat of
      VarPat{} -> True
      WildPat{} -> True
      LazyPat{} -> True
      ParPat _ inner -> nonforcing_pattern inner
      AsPat _ _ inner -> nonforcing_pattern inner
      SigPat _ inner _ -> nonforcing_pattern inner
      _ -> False

-- | Check an ordinary arrow consumed by a new runtime lambda.
-- See Note [Ordinary arrows in retained matches].
matchRetainedLambdaArrow :: ExpectedFunTyCtxt -> (VisArity, Type) -> Type
                         -> TcM (TcCoercionN, Scaled TcSigmaTypeFRR, TcSigmaType)
matchRetainedLambdaArrow herald arity ty
  = do { (arg_co, ma, arg_ty, rhs_ty) <- matchActualFunTy herald Nothing arity ty
       ; unless (isReflCo arg_co) $
           failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
       ; ma_co <- unifyType Nothing unmatchableDataConTy ma
       ; return (ma_co, arg_ty, rhs_ty) }

-- | Lift a result cast through a retained runtime binder.
liftRetainedLambdaWrap :: Id -> HsWrapper -> HsWrapper
liftRetainedLambdaWrap index (WpCast co) =
  mkWpCastR (mkDepFunCoWithVis DepFunVisible DepFunVisible index MRefl co)
liftRetainedLambdaWrap _ wrap = wrap

-- | Build the cast for an ordinary runtime lambda and its result.
liftOrdinaryLambdaWrap :: FunTyFlag -> TcCoercionN -> Scaled TcSigmaTypeFRR
                       -> TcSigmaType -> HsWrapper -> TcM HsWrapper
liftOrdinaryLambdaWrap af ma_co (Scaled mult arg_ty) res_ty wrap
  | isReflCo ma_co, isIdHsWrapper wrap = return idHsWrapper
  | Just res_co <- hsWrapperCast_maybe wrap =
      let body_co = case res_co of
            MRefl -> mkRepReflCo res_ty
            MCo co -> co
          co = mkFunCo Representational af ma_co (mkNomReflCo mult)
                 (mkRepReflCo arg_ty) body_co
      in return (mkWpCastR co)
  | otherwise = failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)

{- Note [Ordinary arrows in retained matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Core Lam introduces an ordinary arrow with unmatchable metadata. Check
that metadata when a value pattern consumes an ordinary arrow in a
retained telescope. This uses the same equality as matchExpectedFunTys.
Let the solver resolve the equality in its original scope. A matchability
variable introduced at the call can be untouchable inside an erased forall.

Require reflexive argument representation. Carry the matchability proof in a
result cast. Lift it through the preceding binders without introducing another
lambda or moving any pattern match. A preceding erased binder uses its existing
wrapper outside this cast. In a constructor group, build the cast while
collecting shared arguments, before entering the scope of any pattern evidence.
A Given introduced by a pattern cannot justify the arrow for that pattern.
An arrow with no corresponding pattern keeps its complete type, so an
existing matchable function can pass through as the result.
-}

-- | Give each retained wildcard its column's internal Name.
-- See Note [Wildcard retained binders].
bindRetainedWildcards :: [Maybe Name] -> [LPat GhcRn] -> [LPat GhcRn]
bindRetainedWildcards (name : names) (pat : pats)
  | isVisArgLPat pat = maybe pat (`bindRetainedWildcard` pat) name
                       : bindRetainedWildcards names pats
  | otherwise = pat : bindRetainedWildcards (name : names) pats
bindRetainedWildcards _ pats = pats

{- Note [Wildcard retained binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an equation discards a retained parameter with @_@, later argument and
result types still refer to that runtime argument. 'tcRetainedVarMatches'
allocates a fresh system 'Name' (preserving the signature binder's 'OccName')
and 'bindRetainedWildcards' rewrites the 'WildPat' to a 'VarPat' so
'selectMatchVar' shares the exact runtime 'Id' without exposing a user binding
in error contexts. Multi-equation constructor rows likewise bind column
wildcards to that column's shared index 'Name'.
-}

-- | Check an erased suffix as an expression inside its runtime prefix.
-- See Note [Erased suffixes in retained matches].
splitRetainedSuffix :: Int -> [Id] -> MatchGroup GhcRn (LHsExpr GhcRn)
                     -> MatchGroup GhcRn (LHsExpr GhcRn)
splitRetainedSuffix prefix indices group@(MG { mg_alts = L loc [L match_loc match]
                                     , mg_ext = ext })
  = group { mg_alts = L loc [L match_loc outer] }
  where
    Match { m_pats = L pats_loc pats } = match
    (before, after) = split_prefix prefix pats
    inner = match { m_ctxt = LamAlt LamSingle, m_pats = L pats_loc after }
    lambda = L match_loc (HsLam noAnn LamSingle
               (MG { mg_alts = L loc [alignInvisibleRetainedPatterns True [] indices (L match_loc inner)]
                   , mg_ext = ext }))
    outer = match { m_pats = L pats_loc before
                  , m_grhss = unguardedGRHSs (locA match_loc) lambda noAnn }

    split_prefix 0 ps = ([], ps)
    split_prefix n (pat : ps)
      = let (before, after) = split_prefix (if isVisArgLPat pat then n-1 else n) ps
        in (pat : before, after)
    split_prefix n [] = pprPanic "splitRetainedSuffix: missing prefix" (ppr n)
splitRetainedSuffix _ _ _ = panic "splitRetainedSuffix: multiple equations"

{- Note [Erased suffixes in retained matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because 'MatchGroupTc' cannot interleave type or evidence lambdas between
runtime value arguments, 'tcRetainedVarMatches' splits a single-equation match
at an interior @forall@ or constraint context into an inner 'HsLam' checked at
the suffix polytype, exposing any trailing invisible retained @Type@ binders
via 'alignInvisibleRetainedPatterns'. The split is permitted only when no
remaining value patterns follow or when all preceding patterns are non-forcing
(and @-XStrict@ is disabled), preserving Haskell argument demand (GHC Proposal
#281).
-}

-- | Distinguish transported payloads from shared evidence and runtime indices.
data RetainedConArg
  = RetainedConValue
  | RetainedConDict EvVar
  | RetainedConIndex Id

-- | Test whether the argument occupies a written column.
isRetainedSourceArg :: RetainedConArg -> Bool
isRetainedSourceArg RetainedConDict{} = False
isRetainedSourceArg _ = True

-- | Collect shared binders in telescope order without counting evidence as values.
tcRetainedConArgs :: ExpectedFunTyCtxt -> UserTypeCtxt -> Int -> Type
                  -> TcM (HsWrapper, ([ExpPatType], [RetainedConArg], ExpRhoType))
tcRetainedConArgs herald ctxt = go [] []
  where
    go acc argument_plan n ty
      | (theta, tau) <- tcSplitPhiTy ty, not (null theta)
      = do { givens <- newEvVars theta
           ; let pat_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) givens
           ; (wrap, result) <- go (reverse pat_tys ++ acc)
                (reverse (map RetainedConDict givens) ++ argument_plan) n tau
           ; let wrap' = case wrap of
                   WpCast co -> mkWpCastR (foldr (mkFunResCo Representational unmatchableDataConTy) co givens)
                   _ -> wrap
           ; return (wrap', result) }
      | n == 0
      = return (idHsWrapper, (reverse acc, reverse argument_plan, mkCheckExpType ty))
      | ForAllTy _ binder _ <- coreFullView ty
      , isInvisibleForAllTyFlag (binderFlag binder)
      = failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
      | DepFunTy DepFunVisible binder _ <- coreFullView ty
      = do { name <- newSysName (nameOccName (idName binder))
           ; let index = mkLocalId name (idMult binder) (varType binder)
                 pat_ty = mkCheckExpFunPatTy (Scaled (idMult binder) (varType binder))
           ; (wrap, result) <- go (pat_ty : acc) (RetainedConIndex index : argument_plan) (n-1)
                                  (applyTypeToArgs ty [Var index])
           ; return (liftRetainedLambdaWrap index wrap, result) }
      | FunTy { ft_af = af } <- coreFullView ty
      = do { (ma_co, arg_ty, rhs_ty) <- matchRetainedLambdaArrow herald (n, ty) ty
           ; (wrap, result) <- go (mkCheckExpFunPatTy arg_ty : acc) (RetainedConValue : argument_plan) (n-1) rhs_ty
           ; wrap' <- liftOrdinaryLambdaWrap af ma_co arg_ty rhs_ty wrap
           ; return (wrap', result) }
      | otherwise
      = do { (wrap, result) <- matchExpectedFunTys herald ctxt 1 (mkCheckExpType ty) $
               \pat_tys rhs_ty ->
                 do { rhs_ty <- readExpType rhs_ty
                    ; go (reverse pat_tys ++ acc)
                         (map (const RetainedConValue) pat_tys ++ argument_plan) (n-1) rhs_ty }
           ; unless (isIdHsWrapper wrap) $
               failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
           ; return result }

{- Note [Retained argument telescopes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Retained telescopes mix visible 'DepFunTy' binders, ordinary 'FunTy' arrows,
and constraint contexts (GHC Proposal #281; Goguen, McBride & McKinna 2006).
'tcRetainedVarMatches' and 'tcRetainedConArgs' walk the telescope in source
order, instantiating each 'DepFunTy' codomain with its runtime 'Id' before
inspecting subsequent argument types, and using 'matchActualFunTy' on ordinary
arrows so trailing constraint contexts are not skolemised prematurely across
later retained binders.
-}

-- | Names shared by the visible runtime index columns of constructor equations.
retainedColumnNames :: [RetainedConArg] -> [Maybe Name]
retainedColumnNames = foldr column []
  where
    column (RetainedConIndex index) rest = Just (idName index) : rest
    column RetainedConValue rest = Nothing : rest
    column RetainedConDict{} rest = rest

-- | Bind implicit dictionaries in their original argument positions.
insertRetainedDictionaries :: [RetainedConArg] -> [LPat GhcRn] -> [LPat GhcRn]
insertRetainedDictionaries [] pats = pats
insertRetainedDictionaries (RetainedConDict dict : rest) pats
  = noLocA (VarPat noExtField (noLocA (idName dict))) : insertRetainedDictionaries rest pats
insertRetainedDictionaries plan@(_ : rest) (pat : pats)
  | isVisArgLPat pat = pat : insertRetainedDictionaries rest pats
  | otherwise = pat : insertRetainedDictionaries plan pats
insertRetainedDictionaries _ [] = panic "insertRetainedDictionaries: missing source argument"

-- | Check one equation with its constructor proofs and transported dictionaries.
withRetainedRowGivens :: SkolemInfoAnon -> [EvVar] -> [RetainedConArg] -> [TcCoercionN]
                      -> TcM a -> TcM (TcEvBinds, a)
withRetainedRowGivens origin proofs plan arg_cos action
  = do { let dictionary_cos = [(dict, co) | (RetainedConDict dict, co) <- zip plan arg_cos]
             domain_cos = [co | (RetainedConIndex _, co) <- zip plan arg_cos
                              , not (isReflCo co)]
       ; refined_givens <- newEvVars (map (pSnd . coercionKind . snd) dictionary_cos)
       ; domain_givens <- newEvVars (map coercionType domain_cos)
       ; (ev_binds, result) <-
           checkConstraints origin []
             (proofs ++ refined_givens ++ domain_givens) action
       ; let given_binds = zipWith (\given (dict, co) ->
                               mkGivenEvBind given
                                 (if isCoVar dict
                                  then evCoercion (mkCoCast (mkCoVarCo dict) (mkSubCo co))
                                  else evCast (Var dict) (mkSubCo co)))
                             refined_givens dictionary_cos
                          ++ zipWith (\given co -> mkGivenEvBind given (evCoercion co))
                                     domain_givens domain_cos
       ; branch_binds <- case ev_binds of
           TcEvBinds binds -> do { mapM_ (addTcEvBind binds) given_binds
                                 ; addNeededEvIds binds (mkVarSet (refined_givens ++ domain_givens))
                                 ; return ev_binds }
           EvBinds binds -> return (EvBinds (listToBag given_binds `unionBags` binds))
       ; return (branch_binds, result) }

-- | Select all later retained constructor columns without counting dictionaries.
retainedMatrixColumns :: [RetainedConArg]
                      -> MatchGroup GhcRn (LHsExpr GhcRn) -> [(Int, Id)]
retainedMatrixColumns plan (MG { mg_alts = L _ rows })
  = nubBy (\(c1, _) (c2, _) -> c1 == c2)
      [ (column, shared)
      | ((column, RetainedConIndex shared), pats) <-
          zip (filter (isRetainedSourceArg . snd) (zip [0..] plan))
              (transpose [drop 1 (filter isVisArgLPat (hsLMatchPats r)) | r <- rows])
      , any isRetainedConPat pats ]

-- | Check a multi-equation function or lambda whose @foreach@ columns use only
-- variable or wildcard patterns while ordinary GADT columns discriminate.
-- See Note [Refuting absurd foreach branches via GADT indices].
tcRetainedVarGroupMatches
  :: ExpectedFunTyCtxt -> UserTypeCtxt -> HsMatchContextRn
  -> VisArity -> [ExpPatType] -> Type
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedVarGroupMatches herald ctxt mctxt arity invis_pat_tys ty
                          group@(MG { mg_alts = L loc rows }) = do
  let dep_cols = collect_dep_cols 0 arity ty
      vis_rows = map (filter isVisArgLPat . hsLMatchPats) rows
      col_pats = transpose vis_rows
  shared_spec <- forM [0 .. arity - 1] $ \c ->
    case lookup c dep_cols of
      Nothing -> pure (Nothing, nlWildPatName)
      Just binder -> do
        let mb_usr = listToMaybe
                       [ (locA vloc, nameOccName nm)
                       | p <- fromMaybe [] (listToMaybe (drop c col_pats))
                       , L _ (VarPat _ (L vloc nm)) <- [stripParPat p] ]
        nm <- case mb_usr of
                Just (vspan, occ) -> newNameAt occ vspan
                Nothing           -> newSysName (getOccName binder)
        pure (Just nm, nlVarPat nm)
  let shared_names  = map fst shared_spec
      template_pats = map snd shared_spec
  prepared_rows <- forM rows $ \(L rloc rm) -> do
    let Match { m_pats = L _ rpats } = rm
        vis_rpats = filter isVisArgLPat rpats
        renames = [ (old_nm, shared_nm)
                  | (Just shared_nm, p) <- zip shared_names vis_rpats
                  , L _ (VarPat _ (L _ old_nm)) <- [stripParPat p]
                  , old_nm /= shared_nm ]
    unless (null renames) $
      case retainedMatchQuotation rm of
        Nothing -> pure ()
        Just sp -> setSrcSpan sp $
          failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
    let rm1@(Match { m_pats = L ploc1 rpats1 }) = renameRetainedMatch renames rm
        rpats2 = rewrite_vis shared_names rpats1
    pure (L rloc (rm1 { m_pats = L ploc1 rpats2 }))
  tcRetainedVarMatches herald ctxt ty template_pats $ \pat_tys rhs_ty _ _ ->
    tcMatchesFromWithIndex Nothing (const (map unLoc rows)) mctxt tcBody
      (invis_pat_tys ++ pat_tys) rhs_ty (group { mg_alts = L loc prepared_rows })
  where
    collect_dep_cols _ 0 _ = []
    collect_dep_cols c n sigma = case coreFullView sigma of
      DepFunTy DepFunVisible b body -> (c, b) : collect_dep_cols (c + 1) (n - 1) body
      FunTy af _ mult _ body
        | isVisibleFunArg af, isManyTy mult -> collect_dep_cols (c + 1) (n - 1) body
      _ -> []

    rewrite_vis [] ps = ps
    rewrite_vis spec (p : ps)
      | not (isVisArgLPat p) = p : rewrite_vis spec ps
      | Just shared_nm : rest <- spec =
          let p' = case stripParPat p of
                L vloc (VarPat x (L nloc _)) -> L vloc (VarPat x (L nloc shared_nm))
                L wloc _                     -> L wloc (VarPat noExtField (noLocA shared_nm))
          in p' : rewrite_vis rest ps
      | Nothing : rest <- spec = p : rewrite_vis rest ps
    rewrite_vis _ [] = []

-- | Verify that every omitted @foreach@ constructor tuple is refuted either by
-- declared equality constraints or by an uninhabited GADT/equality value
-- column in the telescope. When an inspected value column becomes uninhabited,
-- synthesize an absurd @Match@ whose RHS is an empty @case@ on that argument.
-- See Note [Refuting absurd foreach branches via GADT indices].
synthAbsurdMatrixRows
  :: Bool
  -> [(Int, Id)]
  -> [[DataCon]]
  -> ([DataCon] -> TcM (Maybe ([(Int, Id, (DataCon, [Id]))], LMatch GhcRn (LHsExpr GhcRn))))
  -> [ExpPatType]
  -> [RetainedConArg]
  -> SrcSpanAnnA
  -> MatchGroupAnn
  -> [LMatch GhcRn (LHsExpr GhcRn)]
  -> TcM [([(Int, Id, (DataCon, [Id]))], LMatch GhcRn (LHsExpr GhcRn))]
synthAbsurdMatrixRows _ _ [] _ _ _ _ _ _ = pure []
synthAbsurdMatrixRows require_witness cols missing_tups on_missing pat_tys plan loc ext_ann matches = do
  let con_pat dc = nlConPatName (dataConName dc)
                     (replicate (dataConSourceArity dc) nlWildPatName)
      col_ids = map snd cols
      mentions_any = anyFreeVarsOfType (`elem` col_ids)
      predicates = [varType d | RetainedConDict d <- plan, isCoverageEquality (varType d)]
      has_dep_preds = any mentions_any predicates
  zero_cache <- sequence
    [ do { mb <- tcRetainedConScaled (Scaled (idMult idx_id) (varType idx_id)) (con_pat dc)
         ; pure ((col_i, dc), mb) }
    | (k, (col_i, idx_id)) <- zip [0..] cols
    , dc <- nub [tup !! k | tup <- missing_tups]
    , dataConSourceArity dc == 0 ]
  level <- getTcLevel
  ct_loc <- getCtLocM PatCheckOrigin Nothing
  ambient <- if has_dep_preds
             then filter (isCoverageEquality . varType) <$> getLexicalGivens
             else pure []
  let satisfiable tys = do
        givens <- newEvVars tys
        tcCheckGivens (emptyInertSet level) (listToBag (ambient ++ givens))
  mb_outer <- if has_dep_preds then satisfiable predicates else pure Nothing
  mb_inspected <- case matches of
    [] -> pure Nothing
    L _ first_match : _ -> do
      mb_arg_tys <- fmap sequence (mapM read_pat_ty pat_tys)
      case mb_arg_tys of
        Nothing -> pure Nothing
        Just arg_tys -> do
          let vis_columns =
                zip [0 :: Int ..]
                  [ (col_i, arg, sty)
                  | (col_i, (arg, sty)) <- zip [0..] (zip plan arg_tys)
                  , isRetainedSourceArg arg ]
              vis_rows = [filter isVisArgLPat (hsLMatchPats m) | m <- matches]
          if not (all (`equalLength` vis_columns) vis_rows)
            then pure Nothing
            else do
              let inspected =
                    [ (vis_idx, sty)
                    | ((vis_idx, (_, RetainedConValue, Scaled _ sty)), col_pats) <-
                        zip vis_columns (transpose vis_rows)
                    , any isRetainedConPat col_pats ]
              non_empty_orig <- case mb_outer of
                Nothing -> pure []
                Just outer -> filterM (\(_, sty) -> not . isEmptyTy <$> tcNormalise outer sty) inspected
              pure (Just (m_ctxt first_match, vis_columns, inspected, non_empty_orig))
  let resolve_col (col_i, idx_id, dc)
        | dataConSourceArity dc == 0
        = pure ((col_i, idx_id,) <$> join (lookup (col_i, dc) zero_cache))
        | otherwise
        = fmap (col_i, idx_id,) <$>
            tcRetainedConScaled (Scaled (idMult idx_id) (varType idx_id)) (con_pat dc)
      check_one tup = do
        mb_resolved <- fmap sequence $
          mapM resolve_col [(col_i, idx_id, dc) | ((col_i, idx_id), dc) <- zip cols tup]
        case mb_resolved of
          Nothing -> on_missing tup
          Just inst_cols -> do
            cvs <- forM inst_cols $ \(_, idx_id, (dc, fields)) -> do
              cv <- newEvVar (mkNomEqPred (TermVarTy idx_id)
                                          (retainedConIndex (varType idx_id) dc fields))
              pure (idx_id, mkCoVarCo cv)
            let transport_ty ty = foldM step (mkNomReflCo ty) cvs
                  where
                    step acc (idx_id, co) =
                      let cur = pSnd (coercionKind acc)
                      in if anyFreeVarsOfType (== idx_id) cur
                         then mkTransCo acc <$> tcRetainedCoercion idx_id co cur
                         else pure acc
                find_direct inspected = firstJustsM
                  [ do { co <- transport_ty sty
                       ; Reduction _ dom <- runTcSEqualities
                           (rewriteFamilyAxioms ct_loc (pSnd (coercionKind co)))
                       ; pure (if isEmptyTy dom then Just vis_idx else Nothing) }
                  | (vis_idx, sty) <- inspected
                  , mentions_any sty ]
                find_under_local local non_empty_orig = firstJustsM
                  [ do { spec <- pSnd . coercionKind <$> transport_ty sty
                       ; ref  <- tcNormalise local spec
                       ; pure (if isEmptyTy ref then Just vis_idx else Nothing) }
                  | (vis_idx, sty) <- non_empty_orig ]
                mk_row first_ctxt vis_columns witness_vis_idx = do
                  imp_name <- newSysName (mkVarOcc "impossible")
                  let inst_map = [(col_i, dc) | (col_i, _, (dc, _)) <- inst_cols]
                  vis_pats <- forM vis_columns $ \(vis_idx, (col_i, _, Scaled mult _)) ->
                    case lookup col_i inst_map of
                      Just dc -> pure (con_pat dc)
                      Nothing
                        | vis_idx == witness_vis_idx -> pure (nlVarPat imp_name)
                        | isManyTy mult              -> pure nlWildPatName
                        | otherwise                  -> nlVarPat <$> newSysName (mkVarOcc "unused")
                  let empty = MG (Generated OtherExpansion SkipPmc, ext_ann) (L loc [])
                      rhs   = noLocA (HsCase CaseAlt (nlHsVar imp_name) empty)
                  pure (Just (inst_cols, mkSimpleMatch first_ctxt (noLocA vis_pats) rhs))
            if require_witness
              then case mb_inspected of
                Nothing -> on_missing tup
                Just (first_ctxt, vis_columns, inspected, non_empty_orig) -> do
                  direct <- find_direct inspected
                  witness <- case (direct, mb_outer) of
                    (Just _, _) -> pure direct
                    (Nothing, Just _) | not (null non_empty_orig) -> do
                      specialised <- mapM (fmap (pSnd . coercionKind) . transport_ty) predicates
                      mb_local <- satisfiable specialised
                      maybe (pure Nothing) (`find_under_local` non_empty_orig) mb_local
                    _ -> pure Nothing
                  maybe (on_missing tup) (mk_row first_ctxt vis_columns) witness
              else do
                dict_res <- if has_dep_preds
                  then case mb_outer of
                    Nothing -> pure (Left False)
                    Just _ -> do
                      specialised <- mapM (fmap (pSnd . coercionKind) . transport_ty) predicates
                      maybe (Left True) (Right . Just) <$> satisfiable specialised
                  else pure (Right Nothing)
                case dict_res of
                  Left True  -> pure Nothing
                  Left False -> on_missing tup
                  Right mb_local -> case mb_inspected of
                    Nothing -> on_missing tup
                    Just (first_ctxt, vis_columns, inspected, non_empty_orig) -> do
                      direct <- find_direct inspected
                      witness <- case (direct, mb_local) of
                        (Just _, _)       -> pure direct
                        (Nothing, Just l) -> find_under_local l non_empty_orig
                        (Nothing, Nothing) -> pure Nothing
                      maybe (on_missing tup) (mk_row first_ctxt vis_columns) witness
  catMaybes <$> mapM check_one missing_tups
  where
    read_pat_ty (ExpFunPatTy et) = do
      Scaled m t <- readScaledExpType et
      Just . Scaled m <$> liftZonkM (zonkTcType t)
    read_pat_ty _ = pure Nothing

{- Note [Refuting absurd foreach branches via GADT indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In single-column and N-column @foreach@ pattern matrices (GHC Proposal #281;
Goguen, McBride & McKinna 2006; Cockx, Devriese & Piessens, ICFP 2014 / JFP
2016), an omitted constructor tuple such as @('Zero, 'Succ k)@ can make a later
GADT or propositional equality parameter uninhabited (for example @Fin 'Zero@
or @'Zero :~: 'Succ k@), either directly after family reduction or under
declared equality Givens such as @(n ~ sourceLength xs) => Fin n %1 -> a@.

'synthAbsurdMatrixRows' hoists per-matrix invariants (inspected value columns,
lexical equality Givens, outer inert sets, and zero-arity constructor lookups)
across all omitted tuples. For each omitted tuple, it transports inspected
value argument types along the hypothetical nominal constructor proofs, reduces
family axioms, and tests 'isEmptyTy' directly and under specialised equality
Givens. When an inspected value column becomes uninhabited, it synthesizes an
equation that binds that column (and any linear siblings) to fresh variables
and eliminates the witness with an empty @HsCase@ (@case impossible of {}@).
'checkRetainedRows' then typechecks the synthesized equation under the branch's
transported 'CoPat' types so Core receives a well-typed strict elimination and
PMC sees full @foreach@ constructor coverage.

When all @foreach@ columns use variable or wildcard patterns and only later
GADT columns discriminate, 'tcRetainedVarGroupMatches' shares one runtime 'Id'
per @foreach@ binder across equations while 'GHC.HsToCore.Pmc.Solver' closes
nominal 'TermVarTy' Givens to refute impossible GADT combinations.
-}

-- | Dispatch a leading retained-index match group into the unified N-column
-- dependent pattern matrix checker.
tcRetainedConEntry :: [ExpPatType] -> [ExpPatType] -> [RetainedConArg] -> ExpRhoType
                   -> MatchGroup GhcRn (LHsExpr GhcRn)
                   -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedConEntry = tcRetainedConEntryWithEnv noMotiveLexEnv

tcRetainedConEntryWithEnv
  :: MotiveLexEnv -> [ExpPatType] -> [ExpPatType] -> [RetainedConArg] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedConEntryWithEnv lex_env invis_pat_tys pat_tys plan@(RetainedConIndex index : rest_plan) rhs_ty group
  = tcRetainedMatrixMatchesWithEnv lex_env is_split cols invis_pat_tys pat_tys plan rhs_ty group
  where
    later    = [(col + 1, idx) | (col, idx) <- retainedMatrixColumns rest_plan group]
    is_split = lengthIs later 1 && isSplitFinal rest_plan (unLoc (mg_alts group))
    cols | is_split  = [(0, index)]
         | otherwise = (0, index) : later
tcRetainedConEntryWithEnv _ _ _ _ _ _ = panic "tcRetainedConEntryWithEnv: missing leading RetainedConIndex"

-- | Test whether a two-index match group has distinct simple constructors in
-- column 0 and mixes variable and simple constructor patterns in column 1.
isSplitFinal :: [RetainedConArg] -> [LMatch GhcRn (LHsExpr GhcRn)] -> Bool
isSplitFinal [RetainedConIndex _] matches
  | Just (first, second) <- unzip <$> traverse split_cols matches
  , sizeUniqSet (mkNameSet first) == length first
  = any isNothing second && any isJust second
  where
    split_cols (L _ Match { m_pats = L _ pats })
      | second : prefix <- reverse pats
      , isVisArgLPat second
      , [first] <- filter isVisArgLPat prefix
      = do { con <- simpleRetainedCon first
           ; later <- if isRetainedVarPat second
                      then Just Nothing else Just <$> simpleRetainedCon second
           ; pure (unwrapUserRdr con, later) }
      | otherwise = Nothing
isSplitFinal _ _ = False

simpleRetainedCon :: LPat GhcRn -> Maybe (WithUserRdr Name)
simpleRetainedCon pat = case unLoc (retainedConPat (stripRetainedCasePat pat)) of
  ConPat { pat_con = L _ name, pat_args = PrefixCon _ fields }
    | all isRetainedVarPat fields -> Just name
  _ -> Nothing

stripRetainedCasePat :: LPat GhcRn -> LPat GhcRn
stripRetainedCasePat (L _ (ParPat _ inner)) = stripRetainedCasePat inner
stripRetainedCasePat pat@(L _ TuplePat{})   = pat
stripRetainedCasePat pat                    = retainedConPat pat

-- | Check a 1-to-N column retained constructor matrix in left-to-right order.
-- See Note [Retained constructor matrices] and Note [Retained constructor matches].
tcRetainedMatrixMatches :: Bool -> [(Int, Id)] -> [ExpPatType] -> [ExpPatType]
                        -> [RetainedConArg] -> ExpRhoType
                        -> MatchGroup GhcRn (LHsExpr GhcRn)
                        -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedMatrixMatches = tcRetainedMatrixMatchesWithEnv noMotiveLexEnv

tcRetainedMatrixMatchesWithEnv
  :: MotiveLexEnv -> Bool -> [(Int, Id)] -> [ExpPatType] -> [ExpPatType]
  -> [RetainedConArg] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedMatrixMatchesWithEnv lex_env is_split cols invis_pat_tys pat_tys plan rhs_ty
                        group@(MG { mg_alts = L loc matches, mg_ext = (_, ext_ann) })
  = setSrcSpanA loc $
    do { rows <- zipWithM constructors [1..] matches
       ; let has_fallback   = any isNothing rows
             non_fb_matches = [m | (Just _, m) <- zip rows matches]
             one_col_cons   = [dc | Just [Just (dc, _)] <- rows]
       ; if is_one_col
         then do
           { let dom = domain_cons (snd (head cols))
           ; unless (all (`elem` dom) one_col_cons) coverage_error
           ; if has_fallback
             then when (not (any has_value_pattern non_fb_matches)
                        && (length one_col_cons /= length (nub one_col_cons)
                            || length one_col_cons == length dom)) $
                    failWithTc (TcRnRetainedQuantificationError RetainedMatrixDuplicate)
             else unless (any has_value_pattern matches
                          || length one_col_cons == length (nub one_col_cons)) coverage_error }
         else unless (any has_value_pattern matches) $
                void (foldM unique_row [] (zip rows matches))
       ; let expected
               | is_one_col, null matches, isEmptyTy (varType (snd (head cols))) = []
               | otherwise = mapM (domain_cons . snd) cols
             missing_tups
               | has_fallback = []
               | otherwise    = filter (\tup -> not (any (covers tup) rows)) expected
       ; synthesized <- synthAbsurdMatrixRows is_one_col cols missing_tups on_missing
                          pat_tys plan loc ext_ann matches
       ; let row_columns Nothing = []
             row_columns (Just cells) =
               [ (col_idx, idx_id, cell)
               | ((col_idx, idx_id), Just cell) <- zip cols cells ]
             allow_linear = is_one_col && not has_fallback
       ; checkRetainedRows lex_env allow_linear is_one_col is_split (length matches)
           invis_pat_tys pat_tys plan rhs_ty group
           (zip (map row_columns rows) matches) synthesized }
  where
    is_one_col   = lengthIs cols 1
    is_two_col   = lengthIs cols 2
    source_cols  = [length (filter isRetainedSourceArg (take c plan)) | (c, _) <- cols]
    col_indices  = map snd cols
    plan_columns = retainedColumnNames plan
    has_value_pattern (L _ (Match { m_pats = L _ pats })) =
      or [not (isRetainedVarPat pat) | (Nothing, pat) <- zip plan_columns (filter isVisArgLPat pats)]

    domain_cons index = case splitTyConApp_maybe (coreFullView (varType index)) of
      Just (tc, _) -> tyConDataCons tc
      _ -> []

    coverage_error, pattern_error :: TcM a
    coverage_error
      | is_one_col = failWithTc (TcRnRetainedQuantificationError RetainedConstructorCoverage)
      | otherwise  = failWithTc (TcRnRetainedQuantificationError RetainedMatrixCoverage)
    pattern_error
      | is_one_col = coverage_error
      | otherwise  = failWithTc (TcRnRetainedQuantificationError RetainedMatrixPattern)

    row_patterns (Match { m_pats = L _ pats }) =
      case traverse (\sc -> listToMaybe (drop sc visible)) source_cols of
        Just ps -> pure ps
        Nothing -> pattern_error
      where
        visible = filter isVisArgLPat pats

    fallback_pattern :: LPat GhcRn -> Bool
    fallback_pattern (L _ (ParPat _ p)) | not is_two_col = fallback_pattern p
    fallback_pattern (L _ VarPat{})     = True
    fallback_pattern (L _ WildPat{})    = True
    fallback_pattern _                  = False

    is_unguarded GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } = True
    is_unguarded _ = False

    resolve index pat = setSrcSpanA (getLoc pat) $
      do { con <- tcRetainedConScaled (Scaled (idMult index) (varType index)) pat
         ; maybe pattern_error pure con }

    constructors position (L match_loc match@(Match { m_grhss = grhss }))
      | is_split || is_unguarded grhss
      = setSrcSpanA match_loc $
        do { ps <- row_patterns match
           ; if is_one_col
             then case ps of
               [p] | not is_split && length matches >= 2 && position == length matches
                   , isRetainedVarPat (stripRetainedCasePat p)
                   -> pure Nothing
                   | otherwise -> (\c -> Just [Just c]) <$> resolve (head col_indices) p
               _ -> coverage_error
             else if all fallback_pattern ps
             then do { unless (position == length matches) $
                         case ps of
                           first : _ -> setSrcSpanA (getLoc first) $
                             failWithTc (TcRnRetainedQuantificationError RetainedMatrixFallback)
                           [] -> coverage_error
                     ; pure Nothing }
             else Just <$> zipWithM (\idx p -> if fallback_pattern p
                                               then pure Nothing
                                               else Just <$> resolve idx p)
                                    col_indices ps }
    constructors _ (L match_loc _) = setSrcSpanA match_loc coverage_error

    covers _ Nothing = True
    covers tup (Just cells) =
      and [maybe True ((== dc) . fst) cell | (dc, cell) <- zip tup cells]

    unique_row seen (Nothing, _) = pure seen
    unique_row seen (row@(Just cells), L match_loc match)
      = setSrcSpanA match_loc $
        do { let covered_by_row =
                   sequence [ maybe (domain_cons idx) (pure . fst) cell
                            | (idx, cell) <- zip col_indices cells ]
           ; when (not (null covered_by_row)
                   && all (\tup -> any (covers tup) seen) covered_by_row) $
               do { ps <- row_patterns match
                  ; let dup_loc
                          | [_, second_p] <- ps = getLoc second_p
                          | otherwise = maybe match_loc getLoc (find (not . fallback_pattern) (reverse ps))
                  ; setSrcSpanA dup_loc $
                      failWithTc (TcRnRetainedQuantificationError RetainedMatrixDuplicate) }
           ; pure (row : seen) }

    on_missing [one] | is_one_col =
      failWithTc (TcRnRetainedQuantificationError (RetainedMissingEquation (dataConName one)))
    on_missing [one, two] =
      failWithTc (TcRnRetainedQuantificationError
        (RetainedMissingConstructorPair (dataConName one) (dataConName two)))
    on_missing _ = coverage_error

-- | Refine one later retained column without moving the ordinary patterns.
tcRetainedLaterMatches :: (Int, Id) -> [ExpPatType] -> [ExpPatType]
                       -> [RetainedConArg] -> ExpRhoType
                       -> MatchGroup GhcRn (LHsExpr GhcRn)
                       -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedLaterMatches (column, index) invis_pat_tys pat_tys plan rhs_ty
                       group@(MG { mg_alts = L _ matches })
  = do { rows <- forM matches $ \match -> (,match) <$> constructors match
       ; checkRetainedRows noMotiveLexEnv False False False (length matches)
           invis_pat_tys pat_tys plan rhs_ty group rows [] }
  where
    source_column = length (filter isRetainedSourceArg (take column plan))
    constructors (L match_loc (Match { m_pats = L _ pats })) =
      case drop source_column (filter isVisArgLPat pats) of
        pat : _ | isRetainedVarPat pat -> pure []
        pat : _ -> setSrcSpanA (getLoc pat) $
          do { con <- tcRetainedCon (varType index) pat
             ; maybe (failWithTc (TcRnRetainedQuantificationError RetainedLaterConstructorPattern))
                     (\row -> pure [(column, index, row)]) con }
        [] -> setSrcSpanA match_loc $
          failWithTc (TcRnRetainedQuantificationError RetainedConstructorArguments)

-- | Check source and synthesized rows under their nominal constructor proofs.
-- See Note [Retained constructor matrices] and Note [Retained constructor matches].
checkRetainedRows
  :: MotiveLexEnv -> Bool -> Bool -> Bool -> Int
  -> [ExpPatType] -> [ExpPatType] -> [RetainedConArg] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> [([(Int, Id, (DataCon, [Id]))], LMatch GhcRn (LHsExpr GhcRn))]
  -> [([(Int, Id, (DataCon, [Id]))], LMatch GhcRn (LHsExpr GhcRn))]
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
checkRetainedRows (mb_motive_alias, capture_aliases) allow_linear allow_empty is_split source_count
                  invis_pat_tys pat_tys plan rhs_ty
                  group@(MG { mg_alts = L loc _, mg_ext = (origin, ext_ann) })
                  written_rows synthesized
  = tcExtendTermScopeBinders [idx | RetainedConIndex idx <- plan] $
    maybe id (\(_, mid) -> tcCheckUsage (idName mid) ManyTy) mb_motive_alias $
    do { arg_tys <- mapM argument_type pat_tys
       ; result_ty <- readExpType rhs_ty >>= liftZonkM . zonkTcType
       ; strict <- xoptM LangExt.Strict
       ; grouped_written <-
           if allow_linear && any (not . isManyTy . scaledMult) (drop 1 arg_tys)
           then group_linear_rows written_rows
           else pure [(r, m, Nothing) | (r, m) <- written_rows]
       ; let all_rows = grouped_written ++ [(r, m, Just ([], [])) | (r, m) <- synthesized]
       ; (usages, checked) <- mapAndUnzipM
           (\(row, m, ops) -> tcCollectingUsage (check_row strict ops row arg_tys result_ty m))
           all_rows
       ; tcEmitBindingUsage (supUEs usages)
       ; case (allow_empty && source_count == 0, plan, arg_tys) of
           (True, RetainedConIndex index : rest_plan, index_ty : rest_tys) ->
             empty_matches index index_ty rest_plan rest_tys result_ty
           _ -> pure (MG { mg_alts = L loc checked
                         , mg_ext  = MatchGroupTc arg_tys result_ty origin }) }
  where
    dictionaries   = [d | RetainedConDict d <- plan]
    argument_error = failWithTc (TcRnRetainedQuantificationError RetainedConstructorArguments)

    argument_type (ExpFunPatTy ty)
      = do { Scaled mult t <- readScaledExpType ty
           ; unless (allow_linear || isManyTy mult) argument_error
           ; Scaled mult <$> liftZonkM (zonkTcType t) }
    argument_type _ = argument_error

    empty_matches index index_ty rest_plan rest_tys result_ty = do
      arguments <- zipWithM (\arg (Scaled mult ty) -> case arg of
                                RetainedConValue      -> newSysLocalId (fsLit "retained_arg") mult ty
                                RetainedConDict dict  -> pure dict
                                RetainedConIndex sh   -> pure sh)
                            rest_plan rest_tys
      let empty_case = MG { mg_alts = L loc []
                          , mg_ext  = MatchGroupTc [index_ty] result_ty origin }
          rhs   = L (noAnnSrcSpan (locA loc)) (HsCase CaseAlt (nlHsVar index) empty_case)
          pats  = map (noLocA . VarPat noExtField . noLocA) (index : arguments)
          match = mkSimpleMatch (LamAlt LamSingle) (noLocA pats) rhs
      pure (MG { mg_alts = L loc [match]
               , mg_ext  = MatchGroupTc (index_ty : rest_tys) result_ty
                             (Generated OtherExpansion SkipPmc) })

    group_linear_rows [] = pure []
    group_linear_rows (cur@(row@[(_, _, (dc, fields))], L row_loc first@Match { m_pats = L pats_loc pats }) : rest)
      = do { let same_dc ([(_, _, (dc', _))], _) = dc' == dc
                 same_dc _                       = False
                 (same, others) = partition same_dc rest
           ; combined <- case same of
               [] -> pure (row, L row_loc first, Nothing)
               _ | con_pat : args@(first_arg : _) <- pats
                 , let written_args = filter isRetainedSourceArg (drop 1 plan)
                 , all (\a -> case a of { RetainedConValue -> True; _ -> False }) written_args
                 , args `equalLength` written_args
                 -> do { names <- mapM (const (newSysName (mkVarOcc "arg"))) args
                       ; let ctxt = case args of [_] -> CaseAlt; _ -> LamAlt LamCases
                       ; alts <- mapM (value_alt fields ctxt) (cur : same)
                       ; let cases = group { mg_alts = L loc alts }
                             body = case names of
                               [n] -> L (getLoc first_arg) (HsCase CaseAlt (nlHsVar n) cases)
                               _   -> L row_loc (HsLam noAnn LamCases cases)
                             outer = first { m_pats  = L pats_loc (bindRetainedConFields fields con_pat : map nlVarPat names)
                                           , m_grhss = unguardedGRHSs (locA row_loc) body noAnn }
                       ; pure (row, L row_loc outer, Just (names, map (unLoc . snd) (cur : same))) }
               _ -> setSrcSpanA row_loc argument_error
           ; (combined :) <$> group_linear_rows others }
    group_linear_rows ((row, m) : rest) = ((row, m, Nothing) :) <$> group_linear_rows rest

    value_alt shared ctxt ([(_, _, (_, fields))], L row_loc source@Match { m_pats = L pats_loc (_ : pats) })
      = do { let names = [(idName own, idName common) | (own, common) <- zipEqual fields shared, own /= common]
           ; case retainedMatchQuotation source of
               Just sp | not (null names) -> setSrcSpan sp $
                 failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
               _ -> pure ()
           ; pure (L row_loc (renameRetainedMatch names
                     (source { m_ctxt = ctxt, m_pats = L pats_loc pats }))) }
    value_alt _ _ (_, L row_loc _) = setSrcSpanA row_loc argument_error

    split_second_col [domain_co] match@(Match { m_pats = L pats_loc pats, m_grhss = grhss })
      | second : prefix <- reverse pats
      , Just cname <- simpleRetainedCon second
      = setSrcSpanA (getLoc second) $
        do { con_like <- tcLookupConLike cname
           ; domain <- liftZonkM (zonkTcType (pSnd (coercionKind domain_co)))
           ; ct_loc <- getCtLocM PatCheckOrigin Nothing
           ; Reduction _ dom <- runTcSEqualities (rewriteFamilyAxioms ct_loc domain)
           ; case (con_like, splitTyConApp_maybe (coreFullView dom)) of
               (RealDataCon con, Just (tc, args))
                 | tc == dataConTyCon con
                 , let possible = filter (not . dataConCannotMatch args) (tyConDataCons tc)
                 -> case possible of
                      [only] | only == con -> pure ()
                      _ | missing@(_ : _) <- filter (/= con) possible ->
                            failWithTc (TcRnRetainedQuantificationError
                              (RetainedUncoveredConstructors (dataConName con) (map dataConName missing)))
                        | otherwise -> failWithTc (TcRnRetainedQuantificationError RetainedMatrixCoverage)
               _ -> failWithTc (TcRnRetainedQuantificationError RetainedMatrixCoverage)
           ; vname <- newSysName (mkVarOcc "retained_case")
           ; let arg_pat = L (getLoc second) (VarPat noExtField (noLocA vname))
                 guarded = L (getLoc second) (HsMultiIf noExtField (grhssGRHSs grhss))
                 inner_grhss
                   | all unguarded (grhssGRHSs grhss) = grhss
                   | otherwise = grhss { grhssGRHSs = noLocA (GRHS noAnn [] guarded) :| [] }
                 alt = L (getLoc second)
                     (match { m_ctxt = CaseAlt, m_pats = L pats_loc [stripRetainedCasePat second]
                            , m_grhss = inner_grhss })
                 scrut = noLocA (HsVar noExtField (noLocA (noUserRdr vname)))
                 cases = MG { mg_ext  = (Generated OtherExpansion SkipPmc, ext_ann)
                            , mg_alts = L loc [alt] }
                 body = L (getLoc second) (HsCase CaseAlt scrut cases)
           ; pure (match { m_pats  = L pats_loc (reverse prefix ++ [arg_pat])
                         , m_grhss = unguardedGRHSs (getLocA second) body noAnn }) }
    split_second_col _ match = pure match

    check_row strict operands row arg_tys result_ty (L match_loc source_match)
      = setSrcSpanA match_loc $
        tcExtendTermScopeBinders [f | (_, _, (_, fs)) <- row, f <- fs] $
        do { cvs <- forM row $ \(column, index, (con, fields)) ->
               do { cv <- newEvVar (mkNomEqPred (TermVarTy index)
                                     (retainedConIndex (varType index) con fields))
                  ; pure (column, (index, cv)) }
           ; let transport ty = foldM apply (mkNomReflCo ty) cvs
                 apply co (_, (index, cv)) =
                   let cur = pSnd (coercionKind co)
                   in if anyFreeVarsOfType (== index) cur
                      then mkTransCo co <$> tcRetainedCoercion index (mkCoVarCo cv) cur
                      else pure co
           ; arg_cos <- mapM (transport . scaledThing) arg_tys
           ; result_co <- transport result_ty
           ; match@(Match { m_pats = L _ pats }) <-
               if is_split then split_second_col (drop 1 arg_cos) source_match else pure source_match
           ; names <- check_columns [column | (column, _, _) <- row] 0 plan (filter isVisArgLPat pats)
           ; when (null row || not (null names)) $
               case retainedMatchQuotation source_match of
                 Nothing -> pure ()
                 Just sp -> setSrcSpan sp $
                   failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
           ; let refined_domains = zipWith3 (\argument (Scaled mult ty) co ->
                                   Scaled mult (case argument of
                                     RetainedConValue -> pSnd (coercionKind co)
                                     _                -> ty))
                                 plan arg_tys arg_cos
                 refined_args = map mkCheckExpFunPatTy refined_domains
                 renamed@(Match { m_pats = L renamed_loc renamed_pats })
                   = renameRetainedMatch names match
                 constructor_columns =
                   [(length (filter isRetainedSourceArg (take column plan)), fields)
                   | (column, _, (_, fields)) <- row]
                 prepared = renamed
                   { m_pats = L renamed_loc
                       (insertRetainedDictionaries plan $ retainedConPats constructor_columns $
                        bindRetainedWildcards (retainedColumnNames plan) renamed_pats)
                   , m_ctxt = case operands of
                       Just (_ : _ : _, _) -> LamAlt LamSingle
                       _                   -> m_ctxt renamed }
                 check_body (L body_loc (HsLam _ LamCases cases)) expected
                   | Just (lin_names@(_ : _ : _), originals) <- operands = do
                       let domains = [dom | (RetainedConValue, dom) <- zip (drop 1 plan) (drop 1 refined_domains)]
                       checked_cases <- tcMatchesFromWithIndex Nothing (const originals)
                                          (LamAlt LamCases) tcBody (map mkCheckExpFunPatTy domains) expected cases
                       args <- zipWithM (\nm (Scaled mult ty) ->
                                  tcScalingUsage mult (tcBody (nlHsVar nm) (mkCheckExpType ty)))
                                  lin_names domains
                       pure (mkHsApps (L body_loc (HsLam noAnn LamCases checked_cases)) args)
                 check_body body expected = tcBody body expected
                 original = case operands of
                   Just (_, first : _) -> first
                   _                   -> source_match
                 wrap_grhss :: forall r. TcM r -> TcM r
                 wrap_grhss act = do
                   let lookup_lcl nm def = do
                         mb <- tcLookupLcl_maybe nm
                         pure $ case mb of
                           Just (ATcId { tct_id = v }) -> v
                           _                           -> def
                       bind_view (_, idx_id, (dc, fields)) inner_act = do
                         actual_fs <- mapM (\f -> lookup_lcl (idName f) f) fields
                         tcWithSourceLetView idx_id (retainedConIndex (varType idx_id) dc actual_fs) inner_act
                   cap_prs <- fmap concat $ forM capture_aliases $ \(orig_nms, sh_nm) -> do
                     bndr <- tcLookupId sh_nm
                     pure [(nm, bndr) | nm <- orig_nms]
                   motive_prs <- case mb_motive_alias of
                     Nothing -> pure []
                     Just (src_nms, mid) -> do
                       actual_mid <- lookup_lcl (idName mid) mid
                       pure [(nm, actual_mid) | nm <- nub (idName mid : src_nms)]
                   var_bndrs <- forM names $ \(orig_nm, sh_nm) -> do
                     bndr <- tcLookupId sh_nm
                     let restored_nm = mkInternalName (nameUnique (idName bndr)) (nameOccName orig_nm) (nameSrcSpan orig_nm)
                     pure (setVarName bndr restored_nm)
                   let all_prs   = motive_prs ++ cap_prs
                       with_ids  = if null all_prs then id else tcExtendIdEnv2 all_prs
                       with_vars = if null var_bndrs then id else tcExtendIdEnv var_bndrs
                   foldr bind_view (with_vars (with_ids act)) row
                 check = do
                   clause <- if source_count == 1
                     then prepareUnguardedWhere (mkCheckExpType (pSnd (coercionKind result_co)))
                            (L match_loc prepared)
                     else pure (L match_loc prepared)
                   tcMatchWithEnv wrap_grhss check_body (invis_pat_tys ++ refined_args)
                     (mkCheckExpType (pSnd (coercionKind result_co))) original clause
           ; (ev_binds, L checked_loc checked) <- case row of
               [] | null dictionaries -> (emptyTcEvBinds,) <$> check
                  | all unguarded (grhssGRHSs (m_grhss match)) ->
                      checkConstraints (SigTypeSkol GenSigCtxt) [] dictionaries check
                  | otherwise -> argument_error
               (_, _, (first_con, _)) : _ ->
                 withRetainedRowGivens (PatSkol (RealDataCon first_con) (m_ctxt match))
                   (map (snd . snd) cvs) plan arg_cos check
           ; let Match { m_pats = L pats_loc checked_pats, m_grhss = grhss } = checked
                 generated pat@(L l _) | strict && isJust operands = L l (LazyPat noExtField pat)
                                       | otherwise                 = pat
                 wrap_column (column, argument) co (Scaled _ ty) pat@(L pat_loc inner)
                   | Just (index, cv) <- lookup column cvs
                   , let binds | column == fst (last cvs) = ev_binds
                               | otherwise                = emptyTcEvBinds
                   = L pat_loc (AsPat noExtField (L (noAnnSrcSpan generatedSrcSpan) index)
                       (L pat_loc (addRetainedConProof cv binds inner)))
                   | RetainedConValue <- argument
                   = generated (L pat_loc (XPat (CoPat (mkWpCastN co) inner ty)))
                   | otherwise = pat
                 checked_pats' = zipWith4 wrap_column (zip [0..] plan) arg_cos arg_tys checked_pats
                 cast_rhs (L rhs_loc (GRHS x guards rhs))
                   = L rhs_loc (GRHS x guards (mkLHsWrapCo (mkSymCo result_co) rhs))
                 fallback_binds = null cvs && not (null dictionaries)
                 where_binds = grhssLocalBinds grhss
                 wrap_fallback (L rhs_loc (GRHS x guards rhs))
                   | fallback_binds =
                       let body = case unLoc where_binds of
                             EmptyLocalBinds{} -> rhs
                             _ -> L (getLoc rhs) (HsLet noExtField where_binds rhs)
                       in L rhs_loc (GRHS x guards (mkHsDictLet ev_binds body))
                   | otherwise = cast_rhs (L rhs_loc (GRHS x guards rhs))
                 locals | fallback_binds = fmap (const emptyLocalBinds) where_binds
                        | otherwise      = where_binds
           ; pure (L checked_loc (checked
               { m_pats  = L pats_loc checked_pats'
               , m_ctxt  = m_ctxt source_match
               , m_grhss = grhss { grhssGRHSs      = fmap wrap_fallback (grhssGRHSs grhss)
                                 , grhssLocalBinds = locals } })) }

    unguarded :: LGRHS GhcRn (LHsExpr GhcRn) -> Bool
    unguarded (L _ (GRHS _ guards _)) = null guards

    check_columns _ _ [] [] = pure []
    check_columns cols column (RetainedConDict _ : args) pats = check_columns cols (column+1) args pats
    check_columns cols column (argument : args) (L pat_loc inner : pats)
      | RetainedConIndex shared <- argument
      = setSrcSpanA pat_loc $
        case inner of
          VarPat _ (L _ name)
            | name /= idName shared -> ((name, idName shared) :) <$> check_columns cols (column+1) args pats
            | otherwise             -> check_columns cols (column+1) args pats
          WildPat{}           -> check_columns cols (column+1) args pats
          _ | column `elem` cols -> check_columns cols (column+1) args pats
            | otherwise          -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
      | otherwise = check_columns cols (column+1) args pats
    check_columns _ _ _ _ = argument_error

-- | Give an explicit empty case its checked domain in a retained constructor branch.
tcEmptyRetainedScrutinee :: LHsExpr GhcTc -> Type -> TcM (LHsExpr GhcTc, Type)
tcEmptyRetainedScrutinee scrut ty = do
  givens <- getLexicalGivens
  let retained cv = isCoVar cv && case getEqPredTys_maybe (varType cv) of
        Just (Nominal, TermVarTy v, _) -> isRetainedConstructorProof v (mkCoVarCo cv)
        Just (Nominal, _, TermVarTy v) -> isRetainedConstructorProof v (mkSymCo (mkCoVarCo cv))
        _ -> False
  if not (any retained givens) || isEmptyTy ty then pure (scrut, ty) else do
    level <- getTcLevel
    inerts <- tcCheckGivens (emptyInertSet level) (listToBag (filter (isCoverageEquality . varType) givens))
    case inerts of
      Just local -> do
        refined <- tcNormalise local ty
        if isEmptyTy refined then do
          co <- unifyType Nothing ty refined
          pure (mkLHsWrapCo co scrut, refined)
        else pure (scrut, ty)
      Nothing -> pure (scrut, ty)

-- | Select the equality assumptions available to the coverage check.
isCoverageEquality :: Type -> Bool
isCoverageEquality ty = isEqClassPred ty || case getEqPredTys_maybe ty of
  Just (Nominal, _, _) -> True
  _ -> False

-- | Install the fields used in the proofs for the selected constructor columns.
-- See Note [Parameterized retained constructor patterns] in GHC.Tc.Gen.Pat.
retainedConPats :: [(Int, [Id])] -> [LPat GhcRn] -> [LPat GhcRn]
retainedConPats columns = go 0
  where
    go _ [] = []
    go column (pat : pats)
      | not (isVisArgLPat pat) = pat : go column pats
      | otherwise = case lookup column columns of
          Just fields -> bindRetainedConFields fields pat : go (column+1) pats
          Nothing -> pat : go (column+1) pats

-- | Bind checked nominal constructor evidence on its actual pattern fields.
addRetainedConProof :: EvVar -> TcEvBinds -> Pat GhcTc -> Pat GhcTc
addRetainedConProof cv binds (ParPat x (L loc pat))
  = ParPat x (L loc (addRetainedConProof cv binds pat))
addRetainedConProof cv binds pat@(ConPat { pat_con_ext = ext })
  = assertPpr (null (cpt_dicts ext) && null (cpt_tvs ext) && empty_binds (cpt_binds ext)) (ppr pat) $
    pat { pat_con_ext = ext { cpt_dicts = [cv], cpt_binds = binds } }
  where
    empty_binds ev@EvBinds{} = isEmptyTcEvBinds ev
    empty_binds TcEvBinds{} = False
addRetainedConProof _ _ pat = pprPanic "addRetainedConProof: not a data constructor" (ppr pat)

{- Note [Retained constructor matrices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'tcRetainedMatrixMatches' and 'checkRetainedRows' elaborate 1-to-N column
dependent constructor pattern matrices (GHC Proposal #281; Maranget, ML 2008;
Goguen, McBride & McKinna 2006; Cockx, Devriese & Piessens, ICFP 2014 / JFP
2016) through one unified pipeline:

1. Column Proofs and Left-to-Right Transport. Each matched index column @(c, i)@
   resolves its constructor @K@ and runtime field @Id@s via 'tcRetainedConScaled'
   and introduces a nominal proof @cv_c :: TermVarTy i ~# 'K fields@. Composing
   'tcRetainedCoercion' across @cvs@ in telescope order specializes later
   ordinary value argument types ('RetainedConValue') and the RHS result type
   while keeping later 'RetainedConIndex' binders at their incoming types (with
   non-reflexive domain conversions exposed as branch Givens for on-use casts).
2. Fallbacks, Later Columns, and Split Second Columns. A trailing variable or
   wildcard catch-all row has @cvs = []@ and checks at the unrefined telescope
   types outside any constructor proof. Single-column matches in a later
   position ('tcRetainedLaterMatches') keep preceding value patterns in place.
   When two retained columns have distinct simple constructors in column 0 and
   mix variable and constructor patterns in column 1 ('isSplitFinal'),
   'split_second_col' checks exhaustiveness of the specialized second domain
   and lowers column 1 into a branch-local @HsCase@.
3. Linear Value Columns. When a single-column constructor match is followed by
   linear (@%1@) value arguments across repeated equations for the same
   constructor, 'group_linear_rows' shares one outer constructor branch and
   dispatches the linear operands once into an inner @HsCase@ or @LamCases@
   wrapped in 'LazyPat' under @-XStrict@.
-}

{- Note [Retained constructor matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every branch checked by 'checkRetainedRows' preserves the outer 'MatchGroupTc'
argument and result types required by Core desugaring ('GHC.HsToCore.Match'):

1. Pattern Evidence Placement. Each matched index pattern is wrapped in
   @AsPat index (addRetainedConProof cv binds inner)@ so 'selectMatchVar' picks
   the shared runtime 'Id' across all equations and binds @cv@ inside the
   strict Core alternative ('ConPatTc.cpt_dicts'). Branch implication bindings
   ('withRetainedRowGivens', including transported dictionaries and domain
   equalities) attach to the last matched constructor in the row, where all
   column proofs are in scope.
2. Dictionary and Payload Wrappers. Dictionary parameters ('RetainedConDict')
   bind as bare 'VarPat's at their unrefined outer predicate types while the
   branch receives transported Given copies. Ordinary value payloads
   ('RetainedConValue') receive @CoPat (mkWpCastN co)@ (kept even when @co@ is
   reflexive so type synonyms do not drop retained binders during lambda type
   recovery), and the checked RHS is cast back by @mkSymCo result_co@.
3. Empty Domains. An empty 1-column match group (@\\case {}@) on an uninhabited
   retained domain wraps an explicit empty @HsCase@ inside a single variable
   clause ('empty_matches') so the outer lambda retains its shared index 'Id'
   and dictionary parameters.
-}


funBindPrecondition :: MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
funBindPrecondition (MG { mg_alts = L _ alts })
  = not (null alts) && all is_fun_rhs alts
  where
    is_fun_rhs (L _ (Match { m_ctxt = FunRhs {} })) = True
    is_fun_rhs _                                    = False

tcLambdaMatches :: HsExpr GhcRn -> HsLamVariant
                -> MatchGroup GhcRn (LHsExpr GhcRn)
                -> [ExpPatType]  -- Already skolemised
                -> ExpSigmaType  -- NB can be a sigma-type
                -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
tcLambdaMatches e lam_variant raw_matches invis_pat_tys source_res_ty
  =  do { source_matches <- desugarExistsPackMatchGroup raw_matches
        ; source_arity <- checkArgCounts source_matches
            -- Check argument counts since this is also used for \cases
        ; let (visibility_wrap, matches, res_ty, arity)
                | Check ty <- source_res_ty
                , (indices@(_:_), co) <- exposeInvisibleRetained ty
                = ( mkWpCastR co
                  , source_matches { mg_alts = fmap (map (alignInvisibleRetainedPatterns False invis_pat_tys indices)) (mg_alts source_matches) }
                  , mkCheckExpType (coercionLKind co)
                  , source_arity + length indices )
                | otherwise = (idHsWrapper, source_matches, source_res_ty, source_arity)
              check_body
                | LamSingle <- lam_variant
                , [L _ Match { m_pats = L _ pats
                             , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }]
                    <- unLoc (mg_alts matches)
                = tcScopedRuntimeBody (collectPatsBinders CollNoDictBinders pats)
                | otherwise = tcBody

        ; (wrapper, r)
            <- case res_ty of
                 Check ty | hasRetainedFunTy ty
                   -> tcRetainedMatches herald GenSigCtxt ctxt arity invis_pat_tys ty matches
                 _ -> matchExpectedFunTys herald GenSigCtxt arity res_ty $ \ pat_tys rhs_ty ->
                        tcExprMatchesWithWhereFromWith check_body id ctxt (invis_pat_tys ++ pat_tys) rhs_ty matches

        ; return (visibility_wrap <.> wrapper, r) }
  where
    ctxt   = LamAlt lam_variant
    herald = ExpectedFunTyLam lam_variant e
             -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify

-- | Infer a result within the value scopes that may occur in its type.
-- See Note [Closing an inferred runtime index scope].
tcScopedRuntimeBody :: [Name] -> LHsExpr GhcRn -> ExpRhoType -> TcM (LHsExpr GhcTc)
tcScopedRuntimeBody bound body@(L loc expr) expected
  | not (null bound)
  , Infer ir <- expected
  = do { local <- newInferExpType (ir_inst ir)
       ; checked <- tcBody body local
       ; actual <- readExpType local
       ; Reduction co reduced <- setSrcSpanA loc $
           tcCloseRuntimeScope bound (exprCtOrigin expr) actual
       ; L loc <$> tcWrapResultMono expr (mkHsWrapCo co (unLoc checked)) reduced expected }
  | otherwise = tcBody body expected

-- | Close the result with nominal family evidence before leaving a runtime scope.
tcCloseRuntimeScope :: [Name] -> CtOrigin -> Type -> TcM Reduction
tcCloseRuntimeScope [] _ ty = return (Reduction (mkNomReflCo ty) ty)
tcCloseRuntimeScope bound origin ty
  = do { actual <- liftZonkM (zonkTcType ty)
       ; if anyFreeVarsOfType mentions_local actual
         then do { loc <- getCtLocM origin (Just TypeLevel)
                 ; reduction@(Reduction _ reduced) <- tcRewriteFamilyAxioms loc actual
                 ; case find mentions_local (tyCoVarsOfTypeList reduced) of
                     Just index -> failWithTc $ TcRnRetainedQuantificationError (RetainedIndexOutOfScope index)
                     Nothing -> return reduction }
         else return (Reduction (mkNomReflCo actual) actual) }
  where
    mentions_local var = isId var && not (isCoVar var) && getName var `elem` bound

{- Note [Closing an inferred runtime index scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An inferred result can contain a local runtime variable even when callers
only need the length of a list containing that variable. Reduce the result
with the checked family axioms while the local variable is still in scope.
Insert the nominal result cast inside the scope. For example, a result
Vec Int (sourceLength '[element]) can leave a let scope as Vec Int (S Z).
A result that still refers to element cannot leave.

Use an inner inference result so the outer hole sees only the closed type.
The conversion does not equate distinct field values or look at a binding's
RHS. A checked outer type already sends its evidence to the scoped body.
Keep the usual checking rule in that case.
-}

{-
@tcCaseMatches@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.
-}

tcCaseMatches :: (AnnoBody body, Outputable (body GhcTc))
              => HsMatchContextRn
              -> TcMatchAltChecker body    -- ^ Typecheck the alternative RHSS
              -> Scaled TcSigmaTypeFRR     -- ^ Type of scrutinee
              -> MatchGroup GhcRn (LocatedA (body GhcRn)) -- ^ The case alternatives
              -> ExpRhoType                               -- ^ Type of the whole case expression
              -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))
                -- Translated alternatives
                -- wrapper goes from MatchGroup's ty to expected ty

tcCaseMatches ctxt tc_body (Scaled scrut_mult scrut_ty) matches res_ty
  = tcMatches ctxt tc_body [ExpFunPatTy (Scaled scrut_mult (mkCheckExpType scrut_ty))] res_ty matches

-- | Check case alternatives on expressions, exposing eligible unguarded @where@ bindings.
tcExprCaseMatches :: HsMatchContextRn
                  -> TcMatchAltChecker HsExpr
                  -> Scaled TcSigmaTypeFRR
                  -> MatchGroup GhcRn (LHsExpr GhcRn)
                  -> ExpRhoType
                  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcExprCaseMatches ctxt tc_body (Scaled scrut_mult scrut_ty) matches res_ty
  = tcExprMatchesWithWhereFromWith tc_body id ctxt
      [ExpFunPatTy (Scaled scrut_mult (mkCheckExpType scrut_ty))] res_ty matches

-- | Check a dependent case on a named opaque GADT scrutinee.
tcIndexedCaseMatches :: Id -> HsMatchContextRn -> Scaled TcSigmaTypeFRR
                     -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
                     -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcIndexedCaseMatches index ctxt (Scaled mult ty) matches@(MG { mg_alts = L loc alts }) res_ty = do
  mb_nested <- desugarDepCaseAlts True index alts
  case mb_nested of
    Just (plug_alts, Just (fb_rhs, uses)) | uses >= 2 -> do
      r_ty <- expTypeToType res_ty
      let dep_vars = [(True, index) | anyFreeVarsOfType (== index) r_ty]
      (join_binds, join_call) <- mkDepJoinBinds dep_vars [] r_ty fb_rhs
      (binds_tc, inner_mg) <- tcLocalBinds join_binds $
        tcIndexedCaseMatches index ctxt (Scaled mult ty)
          (matches { mg_alts = L loc (plug_alts join_call) }) (mkCheckExpType r_ty)
      let body    = noLocA (HsLet noExtField (noLocA binds_tc)
                             (noLocA (HsCase CaseAlt (nlHsVar index) inner_mg)))
          outer_m = L loc (Match noExtField ctxt (noLocA [noLocA (WildPat ty)])
                             (unguardedGRHSs (locA loc) body noAnn))
      pure (MG { mg_alts = L loc [outer_m]
               , mg_ext  = MatchGroupTc [Scaled mult ty] r_ty (Generated OtherExpansion SkipPmc) })
    Just (plug_alts, mb_fb) ->
      let fb_leaf = maybe (panic "desugarDepCaseAlts: unused fallback") fst mb_fb
      in tcIndexedCaseMatches index ctxt (Scaled mult ty)
           (matches { mg_alts = L loc (plug_alts fb_leaf) }) res_ty
    Nothing -> do
      prepared <- prepareUnguardedWhereMatches res_ty matches
      tcMatchesFromWithIndex (Just (index, mkLHsWrapCo . mkSymCo))
        (const (map unLoc alts)) ctxt tcBody
        [ExpFunPatTy (Scaled mult (mkCheckExpType ty))] res_ty prepared

{- Note [Dependent clause-tree compilation with join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nested constructor patterns in dependent @case@ expressions and @foreach@
function clauses lower to flat constructor case trees via left-to-right column
specialization (Maranget, ML 2008, "Compiling Pattern Matching to Good Decision
Trees"; Goguen, McBride & McKinna 2006).

To prevent exponential duplication or SYB AST cloning of catch-all fallback
right-hand sides, 'compileDepClauseCols' returns a leaf continuation
@(LHsExpr GhcRn -> LHsExpr GhcRn, Int)@ counting exact fallback leaf uses:
- 0 uses (exhaustive constructor branches): the fallback is dropped.
- 1 use: the fallback RHS is plugged directly into the single leaf.
- 2+ uses: 'mkDepJoinBinds' binds the fallback RHS once in a local @let@ with
  an explicit 'IdSig' join signature and invokes that join binding at each leaf.
-}

-- | Test whether a pattern is a nested constructor pattern whose subpatterns
-- are exclusively constructor, variable, or wildcard patterns.
isNestedRetainedConPat :: LPat GhcRn -> Bool
isNestedRetainedConPat pat0
  | ConPat { pat_args = PrefixCon _ subpats } <- unLoc (stripParPat (retainedConPat pat0))
  = any (not . isRetainedVarPat . stripParPat) subpats && all isValidDepSubPat subpats
  | otherwise = False

isValidDepSubPat :: LPat GhcRn -> Bool
isValidDepSubPat p
  | isRetainedVarPat (stripParPat p) = True
  | ConPat { pat_args = PrefixCon _ subs } <- unLoc (stripParPat (retainedConPat p))
  = all isValidDepSubPat subs
  | otherwise = False

stripParPat :: LPat GhcRn -> LPat GhcRn
stripParPat (L _ (ParPat _ p)) = stripParPat p
stripParPat p                  = p

isRetainedConPat :: LPat GhcRn -> Bool
isRetainedConPat p | ConPat{} <- unLoc (retainedConPat (stripParPat p)) = True
isRetainedConPat _ = False

groupByDataCon :: [(DataCon, a)] -> [(DataCon, NonEmpty a)]
groupByDataCon [] = []
groupByDataCon ((dc, r) : rest) =
  let (same, others) = partition ((== dc) . fst) rest
  in (dc, r :| map snd same) : groupByDataCon others

parseDepClauseRow :: Int -> LMatch GhcRn (LHsExpr GhcRn) -> Maybe (SrcSpanAnnA, [LPat GhcRn], LHsExpr GhcRn)
parseDepClauseRow arity (L mloc Match { m_pats = L _ pats
                                      , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] rhs) :| []
                                                        , grhssLocalBinds = wbinds } })
  | pats `lengthIs` arity
  = let full_rhs = case unLoc wbinds of
          EmptyLocalBinds{} -> rhs
          _ -> L (getLoc rhs) (HsLet noExtField wbinds rhs)
    in Just (mloc, pats, full_rhs)
parseDepClauseRow _ _ = Nothing

-- | Bind a multi-use fallback RHS once in a local join @let@ using 'IdSig'.
-- See Note [Dependent clause-tree compilation with join points].
mkDepJoinBinds
  :: [(Bool, Id)] -> [PredType] -> Type -> LHsExpr GhcRn
  -> TcM (HsLocalBinds GhcRn, LHsExpr GhcRn)
mkDepJoinBinds dep_vars raw_preds res_ty fb_rhs = do
  let preds    = [ case getEqPredTys_maybe p of
                     Just (Nominal, l, r) -> mkTyConApp eqTyCon [typeKind l, l, r]
                     _ -> p
                 | p <- raw_preds ]
      in_scope = mkInScopeSetList (tyCoVarsOfTypesList (res_ty : preds ++ map (varType . snd) dep_vars))
      clone_one sub (is_dep, v) = do
        nm <- newSysName (nameOccName (idName v))
        let fv = mkLocalId nm ManyTy (Subst.substTy sub (varType v))
        pure (extendIdSubstWithClone sub v fv, (is_dep, fv))
  (final_sub, fresh_deps) <- mapAccumLM clone_one (Subst.mkEmptySubst in_scope) dep_vars
  join_nm <- newSysName (mkVarOcc "dep_join")
  let gen     = Subst.substTy final_sub
      preds'  = map gen preds
      res_ty' = gen res_ty
      mk_arr (True, fv)  acc = mkDepFunTyWithVis DepFunVisible fv acc
      mk_arr (False, fv) acc = mkVisFunTyManyU (varType fv) acc
      fb_rhs' = renameRetainedMatch [(idName o, idName f) | ((_, o), (_, f)) <- zip dep_vars fresh_deps] fb_rhs
      (join_sig, pats, rhs_body) = case fresh_deps of
        [] -> (tcMkPhiTy preds' (mkVisFunTyManyU unitTy res_ty'), [nlWildPatName], fb_rhs')
        d1 : ds
          | null preds' || null ds ->
              (foldr mk_arr (tcMkPhiTy preds' res_ty') fresh_deps, map (nlVarPat . idName . snd) fresh_deps, fb_rhs')
          | otherwise ->
              let inner_m = mkSimpleMatch (LamAlt LamSingle) (noLocA (map (nlVarPat . idName . snd) ds)) fb_rhs'
                  lam = wrapGenSpan (HsLam noAnn LamSingle (MG (Generated OtherExpansion SkipPmc, noAnn) (noLocA [inner_m])))
              in (mk_arr d1 (tcMkPhiTy preds' (foldr mk_arr res_ty' ds)), [nlVarPat (idName (snd d1))], lam)
      join_id = mkLocalId join_nm ManyTy join_sig
      bind    = genSimpleFunBind join_nm pats rhs_body
      binds   = HsValBinds noAnn (XValBindsLR (HsVBG [(NonRecursive, [bind])] [wrapGenSpan (XSig (IdSig join_id))]))
      args    = if null dep_vars then [genLHsVar (dataConName unitDataCon)] else [genLHsVar (idName v) | (_, v) <- dep_vars]
  pure (binds, wrapGenSpan (genHsApps join_nm args))

-- | Compile a multi-column pattern matrix into nested @case@ expressions
-- without cloning fallback ASTs.
-- See Note [Dependent clause-tree compilation with join points].
compileDepClauseCols
  :: MatchGroupAnn
  -> (LHsExpr GhcRn -> (LHsExpr GhcRn -> LHsExpr GhcRn) -> TcM (LHsExpr GhcRn))
  -> [(Name, Bool)]
  -> [(([LPat GhcRn], LHsExpr GhcRn), Bool)]
  -> Bool
  -> MaybeT TcM (LHsExpr GhcRn -> LHsExpr GhcRn, Int)
compileDepClauseCols _ _ [] (((_, rhs), _) : _) _ = pure (const rhs, 0)
compileDepClauseCols _ _ [] [] _ = mzero
compileDepClauseCols mg_ann mk_join ((v, can_alias) : vs) cur_rows has_outer_fb
  | (not (null vs) || lengthIs cur_rows 1)
  , all (isRetainedVarPat . stripParPat . head . fst . fst) cur_rows
  = let step ((p : ps, rhs), rh) = case unLoc (stripParPat p) of
          VarPat _ (L _ old) ->
            ((renameRetainedMatch [(old, v)] ps, bind_hole_alias v can_alias rh p (renameRetainedMatch [(old, v)] rhs)), rh)
          _ -> ((ps, rhs), rh)
        step (([], rhs), rh) = (([], rhs), rh)
    in compileDepClauseCols mg_ann mk_join vs (map step cur_rows) has_outer_fb
  | null vs
  = do { let pats_rhss = [(stripParPat p, bind_hole_alias v can_alias rh p rhs) | ((p : _, rhs), rh) <- cur_rows]
             has_fb    = maybe False (isRetainedVarPat . fst) (listToMaybe (reverse pats_rhss))
       ; exhausts <- liftMaybeT (pats_exhaust (map fst pats_rhss))
       ; let base_alts  = [mkSimpleMatch CaseAlt (noLocA [p]) r | (p, r) <- pats_rhss]
             need_extra = has_outer_fb && not has_fb && not exhausts
             mk_c alts  = noLocA (HsCase CaseAlt (nlHsVar v) (MG (Generated OtherExpansion SkipPmc, mg_ann) (noLocA alts)))
       ; pure $ if need_extra
                then (\leaf -> mk_c (base_alts ++ [mkSimpleMatch CaseAlt (noLocA [nlWildPatName]) leaf]), 1)
                else (const (mk_c base_alts), 0) }
  | otherwise
  = do { let (init_r, mb_local_fb) = case reverse cur_rows of
               ((ps, rhs), rh) : rev_rest
                 | not (null rev_rest), all (isRetainedVarPat . stripParPat) ps ->
                     let cols_here = (v, can_alias) : vs
                         pairs = [(old, tgt) | (p, (tgt, _)) <- zip ps cols_here, VarPat _ (L _ old) <- [unLoc (stripParPat p)]]
                         aliased = foldr (\(p, (tgt, ca)) -> bind_hole_alias tgt ca rh p) (renameRetainedMatch pairs rhs) (zip ps cols_here)
                     in (reverse rev_rest, Just aliased)
               _ -> (cur_rows, Nothing)
             child_has_fb = isJust mb_local_fb || has_outer_fb
       ; groups <- groupByDataCon <$> traverse resolve_head init_r
       ; (alt_builders, group_uses) <- mapAndUnzipM (build_group child_has_fb) groups
       ; exhausts <- liftMaybeT (cons_exhaust (map fst groups))
       ; let need_extra = child_has_fb && not exhausts
             total_uses = sum group_uses + (if need_extra then 1 else 0)
             build_case leaf =
               noLocA $ HsCase CaseAlt (nlHsVar v) $ MG (Generated OtherExpansion SkipPmc, mg_ann) $ noLocA $
                 [b leaf | b <- alt_builders] ++ [mkSimpleMatch CaseAlt (noLocA [nlWildPatName]) leaf | need_extra]
       ; case mb_local_fb of
           Nothing -> pure (build_case, total_uses)
           Just local_fb
             | total_uses <= 1 -> pure (const (build_case local_fb), 0)
             | otherwise       -> (, 0) . const <$> liftMaybeT (mk_join local_fb build_case) }
  where
    bind_hole_alias tgt ca row_has_hole pat rhs
      | isGeneratedSrcSpan (nameSrcSpan tgt), ca, row_has_hole
      , L ploc (VarPat x lnm) <- stripParPat pat
      = noLocA (HsCase CaseAlt (nlHsVar tgt)
          (MG (Generated OtherExpansion SkipPmc, mg_ann)
            (noLocA [mkSimpleMatch CaseAlt (noLocA [L ploc (VarPat x lnm)]) rhs])))
      | otherwise = rhs

    resolve_head ((p : ps, rhs), rh)
      | ConPat { pat_con = cname, pat_args = PrefixCon _ subs } <- unLoc (stripParPat (retainedConPat p))
      = do { cl <- liftMaybeT (tcLookupConLike (unLoc cname))
           ; case cl of
               RealDataCon dc | subs `lengthIs` dataConSourceArity dc -> pure (dc, (cname, subs, ps, rhs, rh))
               _ -> mzero }
    resolve_head _ = mzero

    build_group child_fb (_, (cname, subs1, rest1, rhs1, rh1) :| more)
      | null more && all (isRetainedVarPat . stripParPat) subs1
      = do { (plug_b, uses_b) <- compileDepClauseCols mg_ann mk_join vs [((rest1, rhs1), rh1)] child_fb
           ; pure (mkSimpleMatch CaseAlt (noLocA [noLocA (ConPat noExtField cname (PrefixCon noExtField subs1))]) . plug_b, uses_b) }
      | otherwise
      = do { sub_specs <- liftMaybeT $ zipWithM chooseSubColName [1 .. length subs1] (transpose (subs1 : [s | (_, s, _, _, _) <- more]))
           ; let all_r = ((subs1 ++ rest1, rhs1), rh1) : [((s ++ r, e), rh) | (_, s, r, e, rh) <- more]
           ; (plug_b, uses_b) <- compileDepClauseCols mg_ann mk_join (sub_specs ++ vs) all_r child_fb
           ; let cpat = noLocA (ConPat noExtField cname (PrefixCon noExtField (map (nlVarPat . fst) sub_specs)))
           ; pure (mkSimpleMatch CaseAlt (noLocA [cpat]) . plug_b, uses_b) }

    pats_exhaust ps = do
      cls <- mapM tcLookupConLike [ unLoc cname | p <- ps
                                  , ConPat { pat_con = cname, pat_args = PrefixCon _ subs } <- [unLoc (stripParPat (retainedConPat p))]
                                  , all (isRetainedVarPat . stripParPat) subs ]
      cons_exhaust [dc | RealDataCon dc <- cls]

    cons_exhaust []          = pure False
    cons_exhaust (dc : rest) = pure (all (`elem` (dc : rest)) (tyConDataCons (dataConTyCon dc)))

chooseSubColName :: Int -> [LPat GhcRn] -> TcM (Name, Bool)
chooseSubColName idx sub_col
  | all (isRetainedVarPat . stripParPat) sub_col
  , (first_loc, first_nm) : rest_vars <- var_locs
  , let first_occ = nameOccName first_nm
  , all ((== first_occ) . nameOccName . snd) rest_vars
  = (\u -> (mkInternalName u first_occ (locA first_loc), False)) <$> newUnique
  | otherwise
  = (\u -> (mkInternalName u fallback_occ generatedSrcSpan, False)) <$> newUnique
  where
    var_locs = [(nloc, nm) | L _ (VarPat _ (L nloc nm)) <- map stripParPat sub_col]
    fallback_occ = maybe (mkVarOcc ("sub_" ++ show idx)) (nameOccName . snd) (listToMaybe var_locs)

-- | Desugar one outer constructor layer of nested patterns in a dependent or
-- indexed GADT @case@, sharing multi-use fallbacks via 'mkDepJoinBinds'.
desugarDepCaseAlts
  :: Bool -> Id -> [LMatch GhcRn (LHsExpr GhcRn)]
  -> TcM (Maybe (LHsExpr GhcRn -> [LMatch GhcRn (LHsExpr GhcRn)], Maybe (LHsExpr GhcRn, Int)))
desugarDepCaseAlts is_indexed scrut_id alts
  | not (null alts)
  , Just parsed0 <- traverse (parseDepClauseRow 1) alts
  , let parsed = [(mloc, p, rhs) | (mloc, [p], rhs) <- parsed0]
  , any (\(_, p, _) -> isNestedRetainedConPat p) parsed
  , not (retainedCaseHasQuote alts)
  = runMaybeT $ do
      let domain_ty   = varType scrut_id
          domain_cons = maybe [] (tyConDataCons . fst) (splitTyConApp_maybe (coreFullView domain_ty))
          (con_rows0, mb_fb_row) = case reverse parsed of
            (fb_loc, fb_pat, fb_rhs) : rev_init
              | not (null rev_init), isRetainedVarPat (stripParPat fb_pat) ->
                  (reverse rev_init, Just (fb_loc, stripParPat fb_pat, fb_rhs))
            _ -> (parsed, Nothing)
      guard (not (null domain_cons) && not (null con_rows0))
      grouped <- groupByDataCon <$> traverse (resolve_con domain_ty domain_cons) con_rows0
      let mb_fb_expr = (\(_, fb_pat, fb_rhs) -> case unLoc fb_pat of
                          VarPat _ (L _ fb_v) -> renameRetainedMatch [(fb_v, idName scrut_id)] fb_rhs
                          _                   -> fb_rhs) <$> mb_fb_row
      (alt_builders, group_uses) <- mapAndUnzipM (build_outer (isJust mb_fb_row)) grouped
      let inner_uses    = sum group_uses
          need_outer_fb = isJust mb_fb_row && (not (all (`elem` map fst grouped) domain_cons) || inner_uses == 0)
          total_uses    = inner_uses + (if need_outer_fb then 1 else 0)
          plug_alts leaf = [b leaf | b <- alt_builders] ++
            [ L fb_loc (unLoc (mkSimpleMatch CaseAlt (noLocA [case unLoc fb_pat of { VarPat{} -> nlWildPatName; _ -> fb_pat }]) leaf))
            | need_outer_fb, Just (fb_loc, fb_pat, _) <- [mb_fb_row] ]
      pure (plug_alts, (, total_uses) <$> mb_fb_expr)
  | otherwise = pure Nothing
  where
    resolve_con domain_ty domain_cons (mloc, pat, rhs)
      | ConPat { pat_con = cname, pat_args = PrefixCon _ subpats } <- unLoc (stripParPat (retainedConPat pat))
      = do { cl <- liftMaybeT (tcLookupConLike (unLoc cname))
           ; case cl of
               RealDataCon dc
                 | dc `elem` domain_cons, is_indexed || isJust (nativeIndexDataConTyArgs domain_ty dc)
                 , subpats `lengthIs` dataConSourceArity dc, all isValidDepSubPat subpats
                 -> pure (dc, (mloc, cname, subpats, rhs))
               _ -> mzero }
      | otherwise = mzero

    mk_untyped_join local_fb plug = do
      join_nm <- newSysName (mkVarOcc "dep_sub_join")
      let binds = HsValBinds noAnn (XValBindsLR (HsVBG [(NonRecursive, [genSimpleFunBind join_nm [nlWildPatName] local_fb])] []))
      pure (wrapGenSpan (genHsLet (wrapGenSpan binds) (plug (wrapGenSpan (genHsApps join_nm [genLHsVar (dataConName unitDataCon)])))))

    build_outer has_fb (dc, (mloc, cname, first_subs, first_rhs) :| rest_rows)
      | null rest_rows && all (isRetainedVarPat . stripParPat) first_subs
      = pure (const (L mloc (unLoc (mkSimpleMatch CaseAlt (noLocA [L mloc (ConPat noExtField cname (PrefixCon noExtField first_subs))]) first_rhs))), 0)
      | dataConSourceArity dc == 0 = mzero
      | otherwise = do
          let raw_rows = (first_subs, first_rhs) : [(subs, r) | (_, _, subs, r) <- rest_rows]
          sub_specs <- liftMaybeT $ zipWithM chooseSubColName [1 .. dataConSourceArity dc] (transpose (map fst raw_rows))
          (plug_b, uses_b) <- compileDepClauseCols noAnn mk_untyped_join sub_specs [(r, False) | r <- raw_rows] has_fb
          let con_pat = L mloc (ConPat noExtField cname (PrefixCon noExtField (map (nlVarPat . fst) sub_specs)))
          pure (L mloc . unLoc . mkSimpleMatch CaseAlt (noLocA [con_pat]) . plug_b, uses_b)

-- | Desugar nested constructor patterns in @foreach@ function equations or
-- lambdas into outer argument variables over a Maranget decision tree with
-- join-point fallback sharing.
-- See Note [Dependent clause-tree compilation with join points].
desugarDepClauseTree
  :: HsMatchContextRn -> VisArity -> Type
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (Maybe ([LPat GhcRn], MatchGroup GhcRn (LHsExpr GhcRn)))
desugarDepClauseTree mctxt arity ty group@(MG { mg_alts = L loc rows })
  | arity >= 1
  , Just parsed0 <- traverse (parseDepClauseRow arity) rows
  = runMaybeT $ do
      let parsed        = [(ps, rhs) | (_, ps, rhs) <- parsed0]
          col_pats_by_c = transpose (map (map stripParPat . fst) parsed)
          col_has_con   = map (any (not . isRetainedVarPat)) col_pats_by_c
      arg_specs <- liftMaybeT $ zipWith3M choose_col_name [1 .. arity] col_pats_by_c (col_ty_infos arity ty)
      let (peeled_args, rhs_ty) = peel_dep_sig (zip arg_specs col_has_con) ty
          root_ids = [aid | (True, True, aid) <- peeled_args, anyFreeVarsOfType (== aid) rhs_ty]
          dep_vars = [(is_dep, aid) | not (null root_ids), (is_dep, _, aid) <- peeled_args
                                    , aid `elem` root_ids || anyFreeVarsOfType (`elem` root_ids) (varType aid)]
          mk_join local_fb plug = do
            (jb, jc) <- mkDepJoinBinds dep_vars [] rhs_ty local_fb
            pure (wrapGenSpan (genHsLet (wrapGenSpan jb) (plug jc)))
      (plug_body, _) <- compileDepClauseCols (snd (mg_ext group)) mk_join arg_specs
                          [(r, has_hole (snd r)) | r <- parsed] False
      let arg_pats = map (nlVarPat . fst) arg_specs
          outer    = mkSimpleMatch mctxt (L loc arg_pats) (plug_body (panic "desugarDepClauseTree: no outer fb"))
      pure (arg_pats, group { mg_alts = L loc [outer] })
  | otherwise = pure Nothing
  where
    has_hole :: Data a => a -> Bool
    has_hole thing
      | Just (HsHole {}) <- (cast thing :: Maybe (HsExpr GhcRn)) = True
      | Just _ <- (cast thing :: Maybe Name)    = False
      | Just _ <- (cast thing :: Maybe NameSet) = False
      | Just _ <- (cast thing :: Maybe Type)    = False
      | otherwise = or (gmapQ has_hole thing)

    col_ty_infos 0 _ = []
    col_ty_infos n sigma = case coreFullView sigma of
      DepFunTy DepFunVisible b body -> (Just (getOccName b), True, Just (varType b)) : col_ty_infos (n - 1) body
      ForAllTy _ (Bndr tv Required) body -> (Just (getOccName tv), False, Nothing) : col_ty_infos (n - 1) body
      ForAllTy _ _ body -> col_ty_infos n body
      FunTy af _ _ arg_ty body
        | isVisibleFunArg af -> (Nothing, False, Just arg_ty) : col_ty_infos (n - 1) body
        | otherwise          -> col_ty_infos n body
      _ -> replicate n (Nothing, False, Nothing)

    choose_col_name idx col_pats (mb_dep_occ, is_dep_bndr, mb_col_ty)
      | all isRetainedVarPat col_pats
      , (first_loc, first_nm) : rest_vars <- var_locs
      , let first_occ = nameOccName first_nm
      , all ((== first_occ) . nameOccName . snd) rest_vars
      = (\u -> (mkInternalName u first_occ (locA first_loc), False)) <$> newUnique
      | otherwise
      = (\u -> (mkInternalName u fallback_occ generatedSrcSpan, can_case_alias)) <$> newUnique
      where
        var_locs = [(nloc, nm) | L _ (VarPat _ (L nloc nm)) <- col_pats]
        fallback_occ = maybe (fromMaybe (mkVarOcc ("arg_" ++ show idx)) mb_dep_occ) (nameOccName . snd) (listToMaybe var_locs)
        can_case_alias = not is_dep_bndr && maybe False (not . isSigmaTy) mb_col_ty

    peel_dep_sig [] sigma = ([], sigma)
    peel_dep_sig (((col_nm, _), has_con) : rest) sigma = case coreFullView sigma of
      DepFunTy DepFunVisible b _ ->
        let aid = mkLocalId col_nm (idMult b) (varType b)
            (more, res) = peel_dep_sig rest (applyTypeToArgs sigma [Var aid])
        in ((True, has_con, aid) : more, res)
      FunTy af _ mult arg_ty body | isVisibleFunArg af ->
        let aid = mkLocalId col_nm mult arg_ty
            (more, res) = peel_dep_sig rest body
        in ((False, has_con, aid) : more, res)
      _ -> ([], sigma)

-- @tcGRHSsPat@ typechecks @[GRHSs]@ that occur in a @PatMonoBind@.
tcGRHSsPat :: Mult -> GRHSs GhcRn (LHsExpr GhcRn) -> ExpRhoType
           -> TcM (GRHSs GhcTc (LHsExpr GhcTc))
-- Used for pattern bindings
tcGRHSsPat mult grhss res_ty
  = tcScalingUsage mult $ do
      grhss' <- prepareUnguardedWhereGRHSs res_ty grhss
      tcGRHSs PatBindRhs tcBody grhss' res_ty

{- *********************************************************************
*                                                                      *
                tcMatch
*                                                                      *
********************************************************************* -}

-- | Type checker for a body of a match alternative
type TcMatchAltChecker body   -- c.f. TcStmtChecker, also in this module
  =  LocatedA (body GhcRn)
  -> ExpRhoType
  -> TcM (LocatedA (body GhcTc))

type AnnoBody body
  = ( Outputable (body GhcRn)
    , Anno (Match GhcRn (LocatedA (body GhcRn))) ~ SrcSpanAnnA
    , Anno (Match GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
    , Anno [LocatedA (Match GhcRn (LocatedA (body GhcRn)))] ~ SrcSpanAnnA
    , Anno [LocatedA (Match GhcTc (LocatedA (body GhcTc)))] ~ SrcSpanAnnA
    , Anno (GRHS GhcRn (LocatedA (body GhcRn))) ~ EpAnnCO
    , Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ EpAnnCO
    , Anno (StmtLR GhcRn GhcRn (LocatedA (body GhcRn))) ~ SrcSpanAnnA
    , Anno (StmtLR GhcTc GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
    )

-- | Type-check a MatchGroup.
tcMatches :: (AnnoBody body, Outputable (body GhcTc))
          => HsMatchContextRn
          -> TcMatchAltChecker body
          -> [ExpPatType]             -- ^ Expected pattern types.
          -> ExpRhoType               -- ^ Expected result-type of the Match.
          -> MatchGroup GhcRn (LocatedA (body GhcRn))
          -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))

tcMatches = tcMatchesFrom id

-- | Reuse expression-let checking for eligible unguarded @where@ bindings.
-- See Note [Unguarded where aliases].
tcExprMatchesWithWhereFrom
  :: (Match GhcRn (LHsExpr GhcRn) -> Match GhcRn (LHsExpr GhcRn))
  -> HsMatchContextRn -> [ExpPatType] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcExprMatchesWithWhereFrom = tcExprMatchesWithWhereFromWith tcBody

-- | Like 'tcExprMatchesWithWhereFrom', with a custom RHS checker.
tcExprMatchesWithWhereFromWith
  :: TcMatchAltChecker HsExpr
  -> (Match GhcRn (LHsExpr GhcRn) -> Match GhcRn (LHsExpr GhcRn))
  -> HsMatchContextRn -> [ExpPatType] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcExprMatchesWithWhereFromWith tc_body source ctxt pat_tys exp_ty group = do
  prepared <- prepareUnguardedWhereMatches exp_ty group
  tcMatchesFromWithIndex Nothing (const (map (source . unLoc) (unLoc (mg_alts group))))
    ctxt tc_body pat_tys exp_ty prepared

-- | Prepare all eligible unguarded equations in a 'MatchGroup'.
prepareUnguardedWhereMatches
  :: ExpRhoType -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcRn (LHsExpr GhcRn))
prepareUnguardedWhereMatches exp_ty group@MG { mg_alts = L loc matches } = do
  prepared <- mapM (prepareUnguardedWhere exp_ty) matches
  return (group { mg_alts = L loc prepared })

-- | Move one eligible where group into its expression RHS without changing patterns.
-- See Note [Unguarded where aliases].
prepareUnguardedWhere :: ExpRhoType -> LMatch GhcRn (LHsExpr GhcRn)
                     -> TcM (LMatch GhcRn (LHsExpr GhcRn))
prepareUnguardedWhere exp_ty (L match_loc match) = do
  grhss' <- prepareUnguardedWhereGRHSs exp_ty (m_grhss match)
  return (L match_loc (match { m_grhss = grhss' }))

-- | Move an eligible unguarded @where@ clause into a @let@ wrapping the single RHS.
prepareUnguardedWhereGRHSs :: ExpRhoType -> GRHSs GhcRn (LHsExpr GhcRn)
                          -> TcM (GRHSs GhcRn (LHsExpr GhcRn))
prepareUnguardedWhereGRHSs (Check expected) original@GRHSs
  { grhssGRHSs = L rhs_loc (GRHS ext [] rhs) :| []
  , grhssLocalBinds = binds@(L binds_loc
      (HsValBinds _ (XValBindsLR (HsVBG groups sigs)))) }
  | hasNativeDependentExpr (Type expected)
    || (isTauTy expected && not (anyFreeVarsOfType (\v -> isTyVar v && isMetaTyVar v) expected))
  , not (null groups), all nonrecursive_function groups, all isTypeLSig sigs
  = do { enabled <- xoptM LangExt.DependentTypes
       ; strict <- xoptM LangExt.Strict
       ; if enabled && not strict
         then let body = wrapGenSpan' (combineSrcSpans (getLocA rhs) (locA binds_loc))
                          (HsLet noExtField binds rhs)
                  local = L binds_loc (EmptyLocalBinds noExtField)
              in return (original
                   { grhssGRHSs = L rhs_loc (GRHS ext [] body) :| []
                   , grhssLocalBinds = local })
         else return original }
  where
    nonrecursive_function (NonRecursive, [L _ FunBind{}]) = True
    nonrecursive_function _ = False
prepareUnguardedWhereGRHSs _ grhss = return grhss

{- Note [Unguarded where aliases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One unguarded equation can put its where declarations in a let around its
RHS. The patterns and the local declarations keep their original identities.
Check the let in the pattern scope. Mark only the new let as generated
and use the original equation for errors.
This reuses checked expression aliases without checking any declaration twice.
Keep inferred or mutable result types on the ordinary path. Keep Strict and
extension-off checking unchanged.

Do not move guarded or multiple equations. Their local declarations scope over
guards, and guard failure can continue with another equation. A let in each
RHS would lose that scope and sharing. A multi-way if would change fallthrough.
The generic tcGRHSs path also checks commands and remains unchanged.
-}

-- | Check prepared clauses and report errors against their source equations.
tcMatchesFrom :: (AnnoBody body, Outputable (body GhcTc))
              => (Match GhcRn (LocatedA (body GhcRn)) -> Match GhcRn (LocatedA (body GhcRn)))
              -> HsMatchContextRn -> TcMatchAltChecker body -> [ExpPatType] -> ExpRhoType
              -> MatchGroup GhcRn (LocatedA (body GhcRn))
              -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))
tcMatchesFrom source = tcMatchesFromWithIndex Nothing (map (source . unLoc))

tcMatchesFromWithIndex :: (AnnoBody body, Outputable (body GhcTc))
                       => Maybe (Id, TcCoercion -> LocatedA (body GhcTc) -> LocatedA (body GhcTc))
                       -> ([LMatch GhcRn (LocatedA (body GhcRn))] -> [Match GhcRn (LocatedA (body GhcRn))])
                       -> HsMatchContextRn -> TcMatchAltChecker body -> [ExpPatType] -> ExpRhoType
                       -> MatchGroup GhcRn (LocatedA (body GhcRn))
                       -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))
tcMatchesFromWithIndex index source ctxt tc_body pat_tys exp_ty (MG { mg_alts = L l matches
                                                     , mg_ext = (origin, _) })
  | null matches  -- Deal with case e of {}
    -- Since there are no branches, no one else will fill in exp_ty
    -- when in inference mode, so we must do it ourselves,
    -- here, using expTypeToType
  = do { tcEmitBindingUsage bottomUE
         -- See Note [Pattern types for EmptyCase]
       ; let vis_pat_tys = filter isVisibleExpPatType pat_tys
       ; pat_ty <- case vis_pat_tys of
           [ExpFunPatTy t]      -> scaledExpTypeToType t
           [ExpForAllPatTy ma tvb] -> failWithTc $ TcRnEmptyCase ctxt (EmptyCaseForall (ma,tvb))
           []                   -> panic "tcMatches: no arguments in EmptyCase"
           _t1:(_t2:_ts)        -> panic "tcMatches: multiple arguments in EmptyCase"
       ; rhs_ty <- expTypeToType exp_ty
       ; return (MG { mg_alts = L l []
                    , mg_ext = MatchGroupTc [pat_ty] rhs_ty origin
                    }) }

  | otherwise
  = do { let exp_ty' = adjustExpTypeForCaseBranches exp_ty matches
             tc_match (original, match) = tcCollectingUsage $
                              tcMatchWithIndex index tc_body pat_tys exp_ty' original match
       ; (usages, matches') <- mapAndUnzipM tc_match (zipEqual (source matches) matches)
       ; tcEmitBindingUsage $ supUEs usages
       ; pat_tys  <- mapM readScaledExpType (filter_out_forall_pat_tys pat_tys)
       ; rhs_ty   <- readExpType exp_ty
       ; traceTc "tcMatches" (ppr matches' $$ ppr pat_tys $$ ppr rhs_ty)
       ; return (MG { mg_alts   = L l matches'
                    , mg_ext    = MatchGroupTc pat_tys rhs_ty origin
                    }) }
  where
    -- We filter out foralls because we have no use for them in HsToCore.
    filter_out_forall_pat_tys :: [ExpPatType] -> [Scaled ExpSigmaTypeFRR]
    filter_out_forall_pat_tys = mapMaybe match_fun_pat_ty
      where
        match_fun_pat_ty (ExpFunPatTy t)  = Just t
        match_fun_pat_ty ExpForAllPatTy{} = Nothing

{- Note [Pattern types for EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In tcMatches, we might encounter an empty list of matches if the user wrote
`case x of {}` or `\case {}`.

* First of all, both `case x of {}` and `\case {}` match on exactly one visible
  argument, which follows from

    checkArgCounts :: MatchGroup GhcRn ... -> TcM VisArity
    checkArgCounts (MG { mg_alts = L _ [] })
      = return 1
    ...

  So we expect vis_pat_tys to be a singleton list [pat_ty] and panic otherwise.

  Multi-case `\cases {}` can't violate this assumption in `tcMatches` because it
  must have been rejected earlier in `rnMatchGroup`.

  Other MatchGroup contexts (function equations `f x = ...`, lambdas `\a b -> ...`,
  etc) are not considered here because there is no syntax to construct them with
  an empty list of alternatives.

* With lambda-case, we run the risk of trying to match on a type argument:

    f :: forall (xs :: Type) -> ()
    f = \case {}

  This is not valid and it used to trigger a panic in pmcMatches (#25004).
  We reject it by inspecting the expected pattern type:

    ; pat_ty <- case vis_pat_tys of
        [ExpFunPatTy t]      -> ...    -- value argument, ok
        [ExpForAllPatTy tvb] -> ...    -- type argument, error!

  Test case: typecheck/should_fail/T25004
-}

-------------
-- | Check a rewritten equation with its original source in the error context.
tcMatchWithEnv :: (AnnoBody body)
               => (forall r. TcM r -> TcM r)
               -> TcMatchAltChecker body
               -> [ExpPatType]
               -> ExpRhoType
               -> Match GhcRn (LocatedA (body GhcRn))
               -> LMatch GhcRn (LocatedA (body GhcRn))
               -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))
tcMatchWithEnv wrap_grhss = tcMatchWithIndexAndEnv wrap_grhss Nothing

tcMatchWithIndex :: AnnoBody body
                 => Maybe (Id, TcCoercion -> LocatedA (body GhcTc) -> LocatedA (body GhcTc))
                 -> TcMatchAltChecker body -> [ExpPatType] -> ExpRhoType
                 -> Match GhcRn (LocatedA (body GhcRn))
                 -> LMatch GhcRn (LocatedA (body GhcRn))
                 -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))
tcMatchWithIndex = tcMatchWithIndexAndEnv id

tcMatchWithIndexAndEnv :: AnnoBody body
                       => (forall r. TcM r -> TcM r)
                       -> Maybe (Id, TcCoercion -> LocatedA (body GhcTc) -> LocatedA (body GhcTc))
                       -> TcMatchAltChecker body -> [ExpPatType] -> ExpRhoType
                       -> Match GhcRn (LocatedA (body GhcRn))
                       -> LMatch GhcRn (LocatedA (body GhcRn))
                       -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))
tcMatchWithIndexAndEnv wrap_grhss index tc_body pat_tys rhs_ty source_match match
  = do { (L loc r) <- wrapLocMA (tc_match pat_tys rhs_ty) match
       ; return (L loc r) }
  where
    tc_match pat_tys rhs_ty
             (Match { m_ctxt = ctxt, m_pats = L l pats, m_grhss = grhss })
      = add_match_ctxt $
        do { (pats', grhss') <- case index of
               Nothing -> tcMatchPats ctxt pats pat_tys (wrap_grhss (tcGRHSs ctxt tc_body grhss rhs_ty))
               Just (scrutinee, wrap_rhs) ->
                 tcMatchPatsWithIndex scrutinee ctxt pats pat_tys (wrap_grhss . check_grhss scrutinee wrap_rhs)

             -- NB: pats' are just the /value/ patterns
             -- See Note [tcMatchPats] in GHC.Tc.Gen.Pat

           ; return (Match { m_ext   = noExtField
                           , m_ctxt  = ctxt
                           , m_pats  = L l pats'
                           , m_grhss = grhss' }) }
      where
        -- For (\x -> e), tcExpr has already said "In the expression \x->e"
        --     so we don't want to add "In the lambda abstraction \x->e"
        -- But for \cases with many alternatives, it is helpful to say
        --     which particular alternative we are looking at
        check_grhss scrutinee wrap_rhs (Just proof)
          | Check ty <- rhs_ty
          = do { co <- tcRetainedIndexCoercion (TermVarTy scrutinee) (mkCoVarCo proof) ty
               ; let check_rhs body rhs = wrap_rhs co <$> tc_body body rhs
               ; tcGRHSs ctxt check_rhs grhss (mkCheckExpType (pSnd (coercionKind co))) }
        check_grhss _ _ _ = tcGRHSs ctxt tc_body grhss rhs_ty

        add_match_ctxt thing_inside = case ctxt of
            LamAlt LamSingle -> thing_inside
            StmtCtxt (HsDoStmt{}) -> thing_inside -- this is an expanded do stmt
            RecUpd -> thing_inside -- record update is Expanded out so ignore it
            _          -> addErrCtxt (MatchInCtxt source_match) thing_inside

-------------
tcGRHSs :: AnnoBody body
        => HsMatchContextRn
        -> TcMatchAltChecker body
        -> GRHSs GhcRn (LocatedA (body GhcRn))
        -> ExpRhoType
        -> TcM (GRHSs GhcTc (LocatedA (body GhcTc)))
-- Notice that we pass in the full res_ty, so that we get
-- good inference from simple things like
--      f = \(x::forall a.a->a) -> <stuff>
-- We used to force it to be a monotype when there was more than one guard
-- but we don't need to do that any more
tcGRHSs ctxt tc_body (GRHSs _ grhss (L l binds)) res_ty
  = do  { (binds', grhss') <- tcLocalBinds binds $ do
                                       tcGRHSNE ctxt tc_body grhss res_ty
        ; return (GRHSs emptyComments grhss' (L l binds')) }

tcGRHSNE :: forall body. AnnoBody body
           => HsMatchContextRn -> TcMatchAltChecker body
           -> NonEmpty (LGRHS GhcRn (LocatedA (body GhcRn))) -> ExpRhoType
           -> TcM (NonEmpty (LGRHS GhcTc (LocatedA (body GhcTc))))
tcGRHSNE ctxt tc_body grhss res_ty
   = do { (usages, grhss') <- unzip <$> traverse (wrapLocSndMA tc_alt) grhss
        ; tcEmitBindingUsage $ supUEs usages
        ; return grhss' }
   where
     stmt_ctxt = PatGuard ctxt

     tc_alt :: GRHS GhcRn (LocatedA (body GhcRn))
            -> TcM (UsageEnv, GRHS GhcTc (LocatedA (body GhcTc)))
     tc_alt (GRHS _ guards rhs)
       = tcCollectingUsage $
         do  { (guards', rhs')
                   <- tcStmtsAndThen stmt_ctxt tcGuardStmt guards res_ty $
                      tc_body rhs
             ; return (GRHS noAnn guards' rhs') }

{-
************************************************************************
*                                                                      *
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
*                                                                      *
************************************************************************
-}

tcDoStmts :: HsDoFlavour
          -> LocatedA [LStmt GhcRn (LHsExpr GhcRn)]
          -> ExpRhoType
          -> TcM (HsExpr GhcTc)          -- Returns a HsDo
tcDoStmts ListComp (L l stmts) res_ty
  = do  { res_ty <- expTypeToType res_ty
        ; (co, elt_ty) <- matchExpectedListTy res_ty
        ; let list_ty = mkListTy elt_ty
        ; stmts' <- tcStmts (HsDoStmt ListComp) (tcLcStmt listTyCon) stmts
                            (mkCheckExpType elt_ty)
        ; return $ mkHsWrapCo co (HsDo list_ty ListComp (L l stmts')) }

tcDoStmts MonadComp (L l stmts) res_ty
  = do  { stmts' <- tcStmts (HsDoStmt MonadComp) tcMcStmt stmts res_ty
        ; res_ty <- readExpType res_ty
        ; return (HsDo res_ty MonadComp (L l stmts')) }


tcDoStmts doExpr@(DoExpr _) (L l stmts) res_ty
  = do { stmts' <- tcStmts (HsDoStmt doExpr) tcDoStmt stmts res_ty
       ; res_ty <- readExpType res_ty
       ; return (HsDo res_ty doExpr (L l stmts')) }

-- NB: ghcistmts should fail, MDoExpr is handled by expansions
tcDoStmts ctxt _ _ = pprPanic "tcDoStmts" (pprHsDoFlavour ctxt)

tcBody :: LHsExpr GhcRn -> ExpRhoType -> TcM (LHsExpr GhcTc)
tcBody body res_ty
  = do  { traceTc "tcBody" (ppr res_ty)
        ; tcPolyLExpr body res_ty
        }

{-
************************************************************************
*                                                                      *
\subsection{tcStmts}
*                                                                      *
************************************************************************
-}

type TcExprStmtChecker = TcStmtChecker HsExpr ExpRhoType
type TcCmdStmtChecker  = TcStmtChecker HsCmd  TcRhoType

type TcStmtChecker body rho_type
  =  forall thing. HsStmtContextRn
                -> Stmt GhcRn (LocatedA (body GhcRn))
                -> rho_type                 -- Result type for comprehension
                -> (rho_type -> TcM thing)  -- Checker for what follows the stmt
                -> TcM (Stmt GhcTc (LocatedA (body GhcTc)), thing)

tcStmts :: (AnnoBody body) => HsStmtContextRn
        -> TcStmtChecker body rho_type   -- NB: higher-rank type
        -> [LStmt GhcRn (LocatedA (body GhcRn))]
        -> rho_type
        -> TcM [LStmt GhcTc (LocatedA (body GhcTc))]
tcStmts ctxt stmt_chk stmts res_ty
  = do { (stmts', _) <- tcStmtsAndThen ctxt stmt_chk stmts res_ty $
                        const (return ())
       ; return stmts' }

tcStmtsAndThen :: (AnnoBody body) => HsStmtContextRn
               -> TcStmtChecker body rho_type    -- NB: higher-rank type
               -> [LStmt GhcRn (LocatedA (body GhcRn))]
               -> rho_type
               -> (rho_type -> TcM thing)
               -> TcM ([LStmt GhcTc (LocatedA (body GhcTc))], thing)

-- Note the higher-rank type.  stmt_chk is applied at different
-- types in the equations for tcStmts

tcStmtsAndThen _ _ [] res_ty thing_inside
  = do  { thing <- thing_inside res_ty
        ; return ([], thing) }

-- LetStmts are handled uniformly, regardless of context
tcStmtsAndThen ctxt stmt_chk (L loc (LetStmt x (L l binds)) : stmts)
                                                             res_ty thing_inside
  = do  { (binds', (stmts',thing)) <- tcLocalBinds binds $
              tcStmtsAndThen ctxt stmt_chk stmts res_ty thing_inside
        ; return (L loc (LetStmt x (L l binds')) : stmts', thing) }

-- Don't set the error context for an ApplicativeStmt.  It ought to be
-- possible to do this with a popErrCtxt in the tcStmt case for
-- ApplicativeStmt, but it did something strange and broke a test (ado002).
tcStmtsAndThen ctxt stmt_chk (L loc stmt : stmts) res_ty thing_inside
  | XStmtLR ApplicativeStmt{} <- stmt
  = do  { (stmt', (stmts', thing)) <-
             stmt_chk ctxt stmt res_ty $ \ res_ty' ->
               tcStmtsAndThen ctxt stmt_chk stmts res_ty'  $
                 thing_inside
        ; return (L loc stmt' : stmts', thing) }

  -- For the vanilla case, handle the location-setting part
  | otherwise
  = do  { (stmt', (stmts', thing)) <-
                setSrcSpanA loc                             $
                addErrCtxt (StmtErrCtxt ctxt (L loc stmt))  $
                stmt_chk ctxt stmt res_ty                   $ \ res_ty' ->
                popErrCtxt                                  $
                tcStmtsAndThen ctxt stmt_chk stmts res_ty'  $
                thing_inside
        ; return (L loc stmt' : stmts', thing) }

---------------------------------------------------
--              Pattern guards
---------------------------------------------------

tcGuardStmt :: TcExprStmtChecker
tcGuardStmt _ (BodyStmt _ guard _ _) res_ty thing_inside
  = do  { guard' <- tcScalingUsage ManyTy $ tcCheckMonoExpr guard boolTy
          -- Scale the guard to Many (see #19120 and #19193)
        ; thing  <- thing_inside res_ty
        ; return (BodyStmt boolTy guard' noSyntaxExpr noSyntaxExpr, thing) }

tcGuardStmt ctxt (BindStmt _ pat rhs) res_ty thing_inside
  = do  { -- The Many on the next line and the unrestricted on the line after
          -- are linked. These must be the same multiplicity. Consider
          --   x <- rhs -> u
          --
          -- The multiplicity of x in u must be the same as the multiplicity at
          -- which the rhs has been consumed. When solving #18738, we want these
          -- two multiplicity to still be the same.
          (rhs', rhs_ty) <- tcScalingUsage ManyTy $
                            tcInferRhoFRRNC FRRBindStmtGuard rhs
                                   -- Stmt has a context already
        ; (pat', thing)  <- tcCheckPat_O (StmtCtxt ctxt) (lexprCtOrigin rhs)
                                         pat (unrestricted rhs_ty) $
                            thing_inside res_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

tcGuardStmt _ stmt _ _
  = pprPanic "tcGuardStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           List comprehensions
--               (no rebindable syntax)
---------------------------------------------------

-- Dealt with separately, rather than by tcMcStmt, because
--   a) We have special desugaring rules for list comprehensions,
--      which avoid creating intermediate lists.  They in turn
--      assume that the bind/return operations are the regular
--      polymorphic ones, and in particular don't have any
--      coercion matching stuff in them.  It's hard to avoid the
--      potential for non-trivial coercions in tcMcStmt

{-
Note [List comprehension isn't linear]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The usefulness of list comprehension in conjunction with linear types is dubious.
After all, statements are made to be run many times, for instance in

[u | y <- [0,1], stmts]

both u and stmts are going to be run several times.

In principle, though, there are some positions in a monad comprehension
expressions which could be considered linear. We could try and make it so that
these positions are considered linear by the typechecker, but in practice the
desugarer doesn't take enough care to ensure that these are indeed desugared to
linear sites. We tried in the past, and it turned out that we'd miss a
desugaring corner case (#25772).

Until there's a demand for this very specific improvement, let's instead be
conservative, and consider list comprehension to be completely non-linear.

Here are the case where list comprehension could be linear, together with the
known challenges.

(LCL1) [ y | () <- xs, y <- [0,1]] could be linear in `xs`.
But, the way the desugaring works, we get something like

case xs of
  () : xs ' -> letrec next_stmt = … xs' …

In the current typing rules for letrec in Core, next_stmt is necessarily of
multiplicity Many and so is every free variable, including xs'. Which, in turns,
requires xs to be of multiplicity Many.

Rodrigo Mesquita worked out, in his master thesis, how to make letrecs having
non-Many multiplicities. But it's a fair bit of work to implement.

Since nobody actually cares about [ y | () <- xs, y <- [0,1]] being linear, then
we just conservatively make it unrestricted instead.

If we're to change that, we have to be careful that [ y | _ <- xs, y <- [0,1]]
isn't linear in `xs` since the elements of `xs` are ignored. So we'd still have
to call `tcScalingUsage` on `xs` in `tcLcStmt`, we'd just have to create a fresh
multiplicity variable. We'd also use the same multiplicity variable in the call
to `tcCheckPat` instead of `unrestricted`.

(LCL2) [ y | b, y <- [0,1]] could be linear in `b`. This actually works fine
with -O0 (as far as anybody knows), but -O and higher desugar list comprehension
using the `build` combinator (this was the cause of #25772). But `build` isn't
defined to be linear. The consequences of making `build` linear are
unknown. It's not worth trying until a real need arises.

(LCL3) [ x | ] could be linear in x. But it's unclear why anybody would want to
write this instead of [ x ]. Besides, this syntax is currently rejected by the
parser. The `build` obstacle of (LCL2) applies here too.
-}

tcLcStmt :: TyCon       -- The list type constructor ([])
         -> TcExprStmtChecker

tcLcStmt _ _ (LastStmt x body noret _) elt_ty thing_inside
  = do { -- see (LCL3) in Note [List comprehension isn't linear]
         body' <- tcScalingUsage ManyTy $ tcMonoLExprNC body elt_ty
       ; thing <- thing_inside (panic "tcLcStmt: thing_inside")
       ; return (LastStmt x body' noret noSyntaxExpr, thing) }

-- A generator, pat <- rhs
tcLcStmt m_tc ctxt (BindStmt _ pat rhs) elt_ty thing_inside
 = do   { pat_ty <- newFlexiTyVarTy liftedTypeKind
          -- see (LCL1) in Note [List comprehension isn't linear]
        ; rhs'   <- tcScalingUsage ManyTy $ tcCheckMonoExpr rhs (mkTyConApp m_tc [pat_ty])
        ; (pat', thing)  <- tcCheckPat (StmtCtxt ctxt) pat (unrestricted pat_ty) $
                            tcScalingUsage ManyTy $
                            thing_inside elt_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

-- A boolean guard
tcLcStmt _ _ (BodyStmt _ rhs _ _) elt_ty thing_inside
  = do  { -- Regarding the `tcScalingUsage ManyTy` on the `rhs`,
          -- see (LCL2) in Note [List comprehension isn't linear]
          rhs'  <- tcScalingUsage ManyTy $ tcCheckMonoExpr rhs boolTy
        ; thing <- tcScalingUsage ManyTy $ thing_inside elt_ty
        ; return (BodyStmt boolTy rhs' noSyntaxExpr noSyntaxExpr, thing) }

-- ParStmt: See notes with tcMcStmt and Note [Scoping in parallel list comprehensions]
tcLcStmt m_tc ctxt (ParStmt _ bndr_stmts_s _ _) elt_ty thing_inside
  = tcScalingUsage ManyTy $ -- parallel list comprehension never desugars to something linear.
    do  { env <- getLocalRdrEnv
        ; (pairs', thing) <- loop env [] bndr_stmts_s
        ; return (ParStmt unitTy pairs' noExpr noSyntaxExpr, thing) }
  where
    loop
     :: LocalRdrEnv -> [Name] -> NonEmpty (ParStmtBlock GhcRn GhcRn)
     -> TcM (NonEmpty (ParStmtBlock GhcTc GhcTc), _)
    -- Invariant: on entry to `loop`, the LocalRdrEnv is set to
    --            origEnv, the LocalRdrEnv for the entire comprehension
    loop origEnv priorBinds (ParStmtBlock x stmts names _ :| pairs)
      = do { (stmts', (ids, pairs', thing))
                <- tcStmtsAndThen ctxt (tcLcStmt m_tc) stmts elt_ty $ \ _elt_ty' ->
                   do { ids <- tcLookupLocalIds names
                      ; (pairs', thing) <- setLocalRdrEnv origEnv $
                            loop1 origEnv (names ++ priorBinds) pairs
                      ; return (ids, pairs', thing) }
           ; return ( ParStmtBlock x stmts' ids noSyntaxExpr :| pairs', thing ) }

    loop1
     :: LocalRdrEnv -> [Name] -> [ParStmtBlock GhcRn GhcRn]
     -> TcM ([ParStmtBlock GhcTc GhcTc], _)
    -- matching in the branches
    loop1 _ binds [] = [ ([], a) | a <- bindLocalNames binds (thing_inside elt_ty) ]
    loop1 env binds (x:xs) = [ (toList ys, a) | (ys, a) <- loop env binds (x:|xs) ]

tcLcStmt m_tc ctxt (TransStmt { trS_form = form, trS_stmts = stmts
                              , trS_bndrs =  bindersMap
                              , trS_by = by, trS_using = using }) elt_ty thing_inside
  = tcScalingUsage ManyTy $ -- Transform statements are too complex: just make everything multiplicity Many
    do { let (bndr_names, n_bndr_names) = unzip bindersMap
             unused_ty = pprPanic "tcLcStmt: inner ty" (ppr bindersMap)
             -- The inner 'stmts' lack a LastStmt, so the element type
             --  passed in to tcStmtsAndThen is never looked at
       ; (stmts', (bndr_ids, by'))
            <- tcStmtsAndThen (TransStmtCtxt ctxt) (tcLcStmt m_tc) stmts unused_ty $ \_ -> do
               { by' <- traverse tcInferRho by
               ; bndr_ids <- tcLookupLocalIds bndr_names
               ; return (bndr_ids, by') }

       ; let m_app ty = mkTyConApp m_tc [ty]

       --------------- Typecheck the 'using' function -------------
       -- using :: ((a,b,c)->t) -> m (a,b,c) -> m (a,b,c)       (ThenForm)
       --       :: ((a,b,c)->t) -> m (a,b,c) -> m (m (a,b,c)))  (GroupForm)

         -- n_app :: Type -> Type   -- Wraps a 'ty' into '[ty]' for GroupForm
       ; let n_app = case form of
                       ThenForm -> (\ty -> ty)
                       _        -> m_app

             by_arrow :: Type -> Type     -- Wraps 'ty' to '(a->t) -> ty' if the By is present
             by_arrow = case by' of
                          Nothing       -> \ty -> ty
                          Just (_,e_ty) -> \ty -> (alphaTy `mkVisFunTyManyU` e_ty) `mkVisFunTyManyU` ty

             tup_ty        = mkBigCoreVarTupTy bndr_ids
             poly_arg_ty   = m_app alphaTy
             poly_res_ty   = m_app (n_app alphaTy)
             using_poly_ty = mkInfForAllTy unmatchableDataConTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkVisFunTyManyU` poly_res_ty

       ; using' <- tcCheckPolyExpr using using_poly_ty
       ; let final_using = fmap (mkHsWrap (WpTyApp tup_ty)) using'

             -- 'stmts' returns a result of type (m1_ty tuple_ty),
             -- typically something like [(Int,Bool,Int)]
             -- We don't know what tuple_ty is yet, so we use a variable
       ; let mk_n_bndr :: Name -> TcId -> TcId
             mk_n_bndr n_bndr_name bndr_id = mkLocalId n_bndr_name ManyTy (n_app (idType bndr_id))

             -- Ensure that every old binder of type `b` is linked up with its
             -- new binder which should have type `n b`
             -- See Note [TransStmt binder map] in GHC.Hs.Expr
             n_bndr_ids  = zipWith mk_n_bndr n_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` n_bndr_ids

       -- Type check the thing in the environment with
       -- these new binders and return the result
       ; thing <- tcExtendIdEnv n_bndr_ids (thing_inside elt_ty)

       ; return (TransStmt { trS_stmts = stmts', trS_bndrs = bindersMap'
                           , trS_by = fmap fst by', trS_using = final_using
                           , trS_ret = noSyntaxExpr
                           , trS_bind = noSyntaxExpr
                           , trS_fmap = noExpr
                           , trS_ext = unitTy
                           , trS_form = form }, thing) }

tcLcStmt _ _ stmt _ _
  = pprPanic "tcLcStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           Monad comprehensions
--        (supports rebindable syntax)
---------------------------------------------------

tcMcStmt :: TcExprStmtChecker

tcMcStmt _ (LastStmt x body noret return_op) res_ty thing_inside
  = do  { (body', return_op')
            <- tcSyntaxOp MCompOrigin return_op [SynRho] res_ty $
               \ [a_ty] [mult]->
               tcScalingUsage mult $ tcCheckMonoExprNC body a_ty
        ; thing      <- thing_inside (panic "tcMcStmt: thing_inside")
        ; return (LastStmt x body' noret return_op', thing) }

-- Generators for monad comprehensions ( pat <- rhs )
--
--   [ body | q <- gen ]  ->  gen :: m a
--                            q   ::   a
--

tcMcStmt ctxt (BindStmt xbsrn pat rhs) res_ty thing_inside
           -- (>>=) :: rhs_ty -> (pat_ty -> new_res_ty) -> res_ty
  = do  { ((rhs_ty, rhs', pat_mult, pat', thing, new_res_ty), bind_op')
            <- tcSyntaxOp MCompOrigin (xbsrn_bindOp xbsrn)
                          [SynRho, SynFun SynAny SynRho] res_ty $
               \ [rhs_ty, pat_ty, new_res_ty] [rhs_mult, fun_mult, pat_mult] ->
               do { rhs' <- tcScalingUsage rhs_mult $ tcCheckMonoExprNC rhs rhs_ty
                  ; (pat', thing) <- tcScalingUsage fun_mult $ tcCheckPat (StmtCtxt ctxt) pat (Scaled pat_mult pat_ty) $
                                     thing_inside (mkCheckExpType new_res_ty)
                  ; return (rhs_ty, rhs', pat_mult, pat', thing, new_res_ty) }

        ; hasFixedRuntimeRep_syntactic (FRRBindStmt MonadComprehension) rhs_ty

        -- If (but only if) the pattern can fail, typecheck the 'fail' operator
        ; fail_op' <- fmap join . forM (xbsrn_failOp xbsrn) $ \fail ->
            tcMonadFailOp (MCompPatOrigin pat) pat' fail new_res_ty

        ; let xbstc = XBindStmtTc
                { xbstc_bindOp = bind_op'
                , xbstc_boundResultType = new_res_ty
                , xbstc_boundResultMult = pat_mult
                , xbstc_failOp = fail_op'
                }
        ; return (BindStmt xbstc pat' rhs', thing) }

-- Boolean expressions.
--
--   [ body | stmts, expr ]  ->  expr :: m Bool
--
tcMcStmt _ (BodyStmt _ rhs then_op guard_op) res_ty thing_inside
  = do  { -- Deal with rebindable syntax:
          --    guard_op :: test_ty -> rhs_ty
          --    then_op  :: rhs_ty -> new_res_ty -> res_ty
          -- Where test_ty is, for example, Bool
        ; ((thing, rhs', rhs_ty, new_res_ty, test_ty, guard_op'), then_op')
            <- tcSyntaxOp MCompOrigin then_op [SynRho, SynRho] res_ty $
               \ [rhs_ty, new_res_ty] [rhs_mult, fun_mult] ->
               do { ((rhs', test_ty), guard_op')
                      <- tcScalingUsage rhs_mult $
                         tcSyntaxOp MCompOrigin guard_op [SynAny]
                                    (mkCheckExpType rhs_ty) $
                         \ [test_ty] [test_mult] -> do
                           rhs' <- tcScalingUsage test_mult $ tcCheckMonoExpr rhs test_ty
                           return $ (rhs', test_ty)
                  ; thing <- tcScalingUsage fun_mult $ thing_inside (mkCheckExpType new_res_ty)
                  ; return (thing, rhs', rhs_ty, new_res_ty, test_ty, guard_op') }

        ; hasFixedRuntimeRep_syntactic FRRBodyStmtGuard test_ty
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt MonadComprehension 1) rhs_ty
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt MonadComprehension 2) new_res_ty

        ; return (BodyStmt rhs_ty rhs' then_op' guard_op', thing) }

-- Grouping statements
--
--   [ body | stmts, then group by e using f ]
--     ->  e :: t
--         f :: forall a. (a -> t) -> m a -> m (m a)
--   [ body | stmts, then group using f ]
--     ->  f :: forall a. m a -> m (m a)

-- We type [ body | (stmts, group by e using f), ... ]
--     f <optional by> [ (a,b,c) | stmts ] >>= \(a,b,c) -> ...body....
--
-- We type the functions as follows:
--     f <optional by> :: m1 (a,b,c) -> m2 (a,b,c)              (ThenForm)
--                     :: m1 (a,b,c) -> m2 (n (a,b,c))          (GroupForm)
--     (>>=) :: m2 (a,b,c)     -> ((a,b,c)   -> res) -> res     (ThenForm)
--           :: m2 (n (a,b,c)) -> (n (a,b,c) -> res) -> res     (GroupForm)
--
tcMcStmt ctxt (TransStmt { trS_stmts = stmts, trS_bndrs = bindersMap
                         , trS_by = by, trS_using = using, trS_form = form
                         , trS_ret = return_op, trS_bind = bind_op
                         , trS_fmap = fmap_op }) res_ty thing_inside
  = do { m1_ty   <- newFlexiTyVarTy typeToTypeKind
       ; m2_ty   <- newFlexiTyVarTy typeToTypeKind
       ; tup_ty  <- newFlexiTyVarTy liftedTypeKind
       ; by_e_ty <- newFlexiTyVarTy liftedTypeKind  -- The type of the 'by' expression (if any)

         -- n_app :: Type -> Type   -- Wraps a 'ty' into '(n ty)' for GroupForm
       ; n_app <- case form of
                    ThenForm -> return (\ty -> ty)
                    _        -> do { n_ty <- newFlexiTyVarTy typeToTypeKind
                                   ; return (n_ty `mkAppTy`) }
       ; let by_arrow :: Type -> Type
             -- (by_arrow res) produces ((alpha->e_ty) -> res)     ('by' present)
             --                          or res                    ('by' absent)
             by_arrow = case by of
                          Nothing -> \res -> res
                          Just {} -> \res -> (alphaTy `mkVisFunTyManyU` by_e_ty) `mkVisFunTyManyU` res

             poly_arg_ty  = m1_ty `mkAppTy` alphaTy
             using_arg_ty = m1_ty `mkAppTy` tup_ty
             poly_res_ty  = m2_ty `mkAppTy` n_app alphaTy
             using_res_ty = m2_ty `mkAppTy` n_app tup_ty
             using_poly_ty = mkInfForAllTy unmatchableDataConTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkVisFunTyManyU` poly_res_ty

             -- 'stmts' returns a result of type (m1_ty tuple_ty),
             -- typically something like [(Int,Bool,Int)]
             -- We don't know what tuple_ty is yet, so we use a variable
       ; let (bndr_names, n_bndr_names) = unzip bindersMap
       ; (stmts', (bndr_ids, by', return_op')) <-
            tcStmtsAndThen (TransStmtCtxt ctxt) tcMcStmt stmts
                           (mkCheckExpType using_arg_ty) $ \res_ty' -> do
                { by' <- case by of
                           Nothing -> return Nothing
                           Just e  -> do { e' <- tcCheckMonoExpr e by_e_ty
                                         ; return (Just e') }

                -- Find the Ids (and hence types) of all old binders
                ; bndr_ids <- tcLookupLocalIds bndr_names

                -- 'return' is only used for the binders, so we know its type.
                --   return :: (a,b,c,..) -> m (a,b,c,..)
                ; (_, return_op') <- tcSyntaxOp MCompOrigin return_op
                                       [synKnownType (mkBigCoreVarTupTy bndr_ids)]
                                       res_ty' $ \ _ _ -> return ()

                ; return (bndr_ids, by', return_op') }

       --------------- Typecheck the 'bind' function -------------
       -- (>>=) :: m2 (n (a,b,c)) -> ( n (a,b,c) -> new_res_ty ) -> res_ty
       ; new_res_ty <- newFlexiTyVarTy liftedTypeKind
       ; (_, bind_op')  <- tcSyntaxOp MCompOrigin bind_op
                             [ synKnownType using_res_ty
                             , synKnownType (n_app tup_ty `mkVisFunTyManyU` new_res_ty) ]
                             res_ty $ \ _ _ -> return ()

       --------------- Typecheck the 'fmap' function -------------
       ; fmap_op' <- case form of
                       ThenForm -> return noExpr
                       _ -> fmap unLoc . tcCheckPolyExpr (noLocA fmap_op) $
                            mkInfForAllTy unmatchableDataConTy alphaTyVar $
                            mkInfForAllTy unmatchableDataConTy betaTyVar  $
                            (alphaTy `mkVisFunTyManyU` betaTy)
                            `mkVisFunTyManyU` (n_app alphaTy)
                            `mkVisFunTyManyU` (n_app betaTy)

       --------------- Typecheck the 'using' function -------------
       -- using :: ((a,b,c)->t) -> m1 (a,b,c) -> m2 (n (a,b,c))

       ; using' <- tcCheckPolyExpr using using_poly_ty
       ; let final_using = fmap (mkHsWrap (WpTyApp tup_ty)) using'

       --------------- Building the bindersMap ----------------
       ; let mk_n_bndr :: Name -> TcId -> TcId
             mk_n_bndr n_bndr_name bndr_id = mkLocalId n_bndr_name ManyTy (n_app (idType bndr_id))

             -- Ensure that every old binder of type `b` is linked up with its
             -- new binder which should have type `n b`
             -- See Note [TransStmt binder map] in GHC.Hs.Expr
             n_bndr_ids = zipWithEqual mk_n_bndr n_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` n_bndr_ids

       -- Type check the thing in the environment with
       -- these new binders and return the result
       ; thing <- tcExtendIdEnv n_bndr_ids $
                  thing_inside (mkCheckExpType new_res_ty)

       ; return (TransStmt { trS_stmts = stmts', trS_bndrs = bindersMap'
                           , trS_by = by', trS_using = final_using
                           , trS_ret = return_op', trS_bind = bind_op'
                           , trS_ext = n_app tup_ty
                           , trS_fmap = fmap_op', trS_form = form }, thing) }

-- A parallel set of comprehensions
--      [ (g x, h x) | ... ; let g v = ...
--                   | ... ; let h v = ... ]
--
-- It's possible that g,h are overloaded, so we need to feed the LIE from the
-- (g x, h x) up through both lots of bindings (so we get the bindLocalMethods).
-- Similarly if we had an existential pattern match:
--
--      data T = forall a. Show a => C a
--
--      [ (show x, show y) | ... ; C x <- ...
--                         | ... ; C y <- ... ]
--
-- Then we need the LIE from (show x, show y) to be simplified against
-- the bindings for x and y.
--
-- It's difficult to do this in parallel, so we rely on the renamer to
-- ensure that g,h and x,y don't duplicate, and simply grow the environment.
-- So the binders of the first parallel group will be in scope in the second
-- group.  But that's fine; there's no shadowing to worry about.
--
-- Note: The `mzip` function will get typechecked via:
--
--   ParStmt [st1::t1, st2::t2, st3::t3]
--
--   mzip :: m st1
--        -> (m st2 -> m st3 -> m (st2, st3))   -- recursive call
--        -> m (st1, (st2, st3))
--
tcMcStmt ctxt (ParStmt _ bndr_stmts_s mzip_op bind_op) res_ty thing_inside
  = do { m_ty   <- newFlexiTyVarTy typeToTypeKind

       ; let mzip_ty  = mkInfForAllTys [(unmatchableDataConTy,tv) | tv <- [alphaTyVar, betaTyVar]] $
                        (m_ty `mkAppTy` alphaTy)
                        `mkVisFunTyManyU`
                        (m_ty `mkAppTy` betaTy)
                        `mkVisFunTyManyU`
                        (m_ty `mkAppTy` mkBoxedTupleTy [alphaTy, betaTy])
       ; mzip_op' <- unLoc `fmap` tcCheckPolyExpr (noLocA mzip_op) mzip_ty

        -- type dummies since we don't know all binder types yet.
       ; tup_tys_and_bndr_stmts_s <- traverse (\ bndr_stmts@(ParStmtBlock _ _ names _) ->
           [ (tup_tys, bndr_stmts)
           | tup_tys <- mkBigCoreTupTy <$> traverse (newOpenFlexiFRRTyVarTy . FRRBinder) names ]) bndr_stmts_s

       -- Typecheck bind:
       ; let tuple_ty = mk_tuple_ty (NE.map fst tup_tys_and_bndr_stmts_s)

       ; (((blocks', thing), inner_res_ty), bind_op')
           <- tcSyntaxOp MCompOrigin bind_op
                         [ synKnownType (m_ty `mkAppTy` tuple_ty)
                         , SynFun (synKnownType tuple_ty) SynRho ] res_ty $
              \ [inner_res_ty] _ ->
              do { stuff <- loop m_ty (mkCheckExpType inner_res_ty) tup_tys_and_bndr_stmts_s
                 ; return (stuff, inner_res_ty) }

       ; return (ParStmt inner_res_ty blocks' mzip_op' bind_op', thing) }

  where
    mk_tuple_ty tys = foldr1 (\tn tm -> mkBoxedTupleTy [tn, tm]) tys

    loop
     :: Type -> ExpRhoType -> NonEmpty (Type, ParStmtBlock GhcRn GhcRn)
     -> TcM (NonEmpty (ParStmtBlock GhcTc GhcTc), _)
    loop m_ty inner_res_ty ((tup_ty_in, ParStmtBlock x stmts names return_op) :| xs)
      = do { let m_tup_ty = m_ty `mkAppTy` tup_ty_in
           ; (stmts', (ids, return_op', pairs', thing))
                <- tcStmtsAndThen ctxt tcMcStmt stmts (mkCheckExpType m_tup_ty) $
                   \m_tup_ty' ->
                   do { ids <- tcLookupLocalIds names
                      ; let tup_ty = mkBigCoreVarTupTy ids
                      ; (_, return_op') <-
                          tcSyntaxOp MCompOrigin return_op
                                     [synKnownType tup_ty] m_tup_ty' $
                                     \ _ _ -> return ()
                      ; (pairs', thing) <- loop1 m_ty inner_res_ty xs
                      ; return (ids, return_op', pairs', thing) }
           ; return (ParStmtBlock x stmts' ids return_op' :| pairs', thing) }

    loop1
     :: Type -> ExpRhoType -> [(Type, ParStmtBlock GhcRn GhcRn)]
     -> TcM ([ParStmtBlock GhcTc GhcTc], _)
    -- matching in the branches
    loop1 _ r [] = [ ([], a) | a <- thing_inside r ]
    loop1 m r (x:xs) = [ (toList ys, a) | (ys, a) <- loop m r (x:|xs) ]

tcMcStmt _ stmt _ _
  = pprPanic "tcMcStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           Do-notation
--        (supports rebindable syntax)
---------------------------------------------------

tcDoStmt :: TcExprStmtChecker

tcDoStmt _ (LastStmt x body noret _) res_ty thing_inside
  = do { body' <- tcMonoLExprNC body res_ty
       ; thing <- thing_inside (panic "tcDoStmt: thing_inside")
       ; return (LastStmt x body' noret noSyntaxExpr, thing) }
tcDoStmt ctxt (BindStmt xbsrn pat rhs) res_ty thing_inside
  = do  {       -- Deal with rebindable syntax:
                --       (>>=) :: rhs_ty ->_rhs_mult (pat_ty ->_pat_mult new_res_ty) ->_fun_mult res_ty
                -- This level of generality is needed for using do-notation
                -- in full generality; see #1537

          ((rhs_ty, rhs', pat_mult, pat', new_res_ty, thing), bind_op')
            <- tcSyntaxOp DoStmtOrigin (xbsrn_bindOp xbsrn) [SynRho, SynFun SynAny SynRho] res_ty $
                \ [rhs_ty, pat_ty, new_res_ty] [rhs_mult,fun_mult,pat_mult] ->
                do { rhs' <-tcScalingUsage rhs_mult $ tcCheckMonoExprNC rhs rhs_ty
                   ; (pat', thing) <- tcScalingUsage fun_mult $ tcCheckPat (StmtCtxt ctxt) pat (Scaled pat_mult pat_ty) $
                                      thing_inside (mkCheckExpType new_res_ty)
                   ; return (rhs_ty, rhs', pat_mult, pat', new_res_ty, thing) }

        ; hasFixedRuntimeRep_syntactic (FRRBindStmt DoNotation) rhs_ty

        -- If (but only if) the pattern can fail, typecheck the 'fail' operator
        ; fail_op' <- fmap join . forM (xbsrn_failOp xbsrn) $ \fail ->
            tcMonadFailOp (DoPatOrigin pat) pat' fail new_res_ty
        ; let xbstc = XBindStmtTc
                { xbstc_bindOp = bind_op'
                , xbstc_boundResultType = new_res_ty
                , xbstc_boundResultMult = pat_mult
                , xbstc_failOp = fail_op'
                }
        ; return (BindStmt xbstc pat' rhs', thing) }

tcDoStmt _ (BodyStmt _ rhs then_op _) res_ty thing_inside
  = do  {       -- Deal with rebindable syntax;
                --   (>>) :: rhs_ty -> new_res_ty -> res_ty
        ; ((rhs', rhs_ty, new_res_ty, thing), then_op')
            <- tcSyntaxOp DoStmtOrigin then_op [SynRho, SynRho] res_ty $
               \ [rhs_ty, new_res_ty] [rhs_mult,fun_mult] ->
               do { rhs' <- tcScalingUsage rhs_mult $ tcCheckMonoExprNC rhs rhs_ty
                  ; thing <- tcScalingUsage fun_mult $ thing_inside (mkCheckExpType new_res_ty)
                  ; return (rhs', rhs_ty, new_res_ty, thing) }
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt DoNotation 1) rhs_ty
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt DoNotation 2) new_res_ty
        ; return (BodyStmt rhs_ty rhs' then_op' noSyntaxExpr, thing) }
tcDoStmt ctxt (RecStmt { recS_stmts = L l stmts, recS_later_ids = later_names
                       , recS_rec_ids = rec_names, recS_ret_fn = ret_op
                       , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op })
         res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- mapM (newOpenFlexiFRRTyVarTy . FRRBinder) tup_names
        ; let tup_ids = zipWith (\n t -> mkLocalId n ManyTy t) tup_names tup_elt_tys
                -- Many because it's a recursive definition
              tup_ty  = mkBigCoreTupTy tup_elt_tys

        ; tcExtendIdEnv tup_ids $ do
        { ((stmts', (ret_op', tup_rets)), stmts_ty)
                <- runInferRho $ \ exp_ty ->
                   tcStmtsAndThen ctxt tcDoStmt stmts exp_ty $ \ inner_res_ty ->
                   do { tup_rets <- zipWithM tcCheckId tup_names
                                      (map mkCheckExpType tup_elt_tys)
                             -- Unify the types of the "final" Ids (which may
                             -- be polymorphic) with those of "knot-tied" Ids
                      ; (_, ret_op')
                          <- tcSyntaxOp DoStmtOrigin ret_op [synKnownType tup_ty]
                                        inner_res_ty $ \_ _ -> return ()
                      ; return (ret_op', tup_rets) }

        ; ((_, mfix_op'), mfix_res_ty)
            <- runInferRho $ \ exp_ty ->
               tcSyntaxOp DoStmtOrigin mfix_op
                          [synKnownType (mkVisFunTyManyU tup_ty stmts_ty)] exp_ty $
               \ _ _ -> return ()

        ; ((thing, new_res_ty), bind_op')
            <- tcSyntaxOp DoStmtOrigin bind_op
                          [ synKnownType mfix_res_ty
                          , SynFun (synKnownType tup_ty) SynRho ]
                          res_ty $
               \ [new_res_ty] _ ->
               do { thing <- thing_inside (mkCheckExpType new_res_ty)
                  ; return (thing, new_res_ty) }

        ; let rec_ids = takeList rec_names tup_ids
        ; later_ids <- tcLookupLocalIds later_names
        ; traceTc "tcdo" $ vcat [ppr rec_ids <+> ppr (map idType rec_ids),
                                 ppr later_ids <+> ppr (map idType later_ids)]
        ; return (RecStmt { recS_stmts = L l stmts', recS_later_ids = later_ids
                          , recS_rec_ids = rec_ids, recS_ret_fn = ret_op'
                          , recS_mfix_fn = mfix_op', recS_bind_fn = bind_op'
                          , recS_ext = RecStmtTc
                            { recS_bind_ty = new_res_ty
                            , recS_later_rets = []
                            , recS_rec_rets = tup_rets
                            , recS_ret_ty = stmts_ty} }, thing)
        }}

tcDoStmt ctxt (XStmtLR (ApplicativeStmt _ pairs mb_join)) res_ty thing_inside
  = do  { let tc_app_stmts ty = tcApplicativeStmts ctxt pairs ty $
                                thing_inside . mkCheckExpType
        ; ((pairs', body_ty, thing), mb_join') <- case mb_join of
            Nothing -> (, Nothing) <$> tc_app_stmts res_ty
            Just join_op ->
              second Just <$>
              (tcSyntaxOp DoStmtOrigin join_op [SynRho] res_ty $
               \ [rhs_ty] [rhs_mult] -> tcScalingUsage rhs_mult $ tc_app_stmts (mkCheckExpType rhs_ty))

        ; return (XStmtLR $ ApplicativeStmt body_ty pairs' mb_join', thing) }

tcDoStmt _ stmt _ _
  = pprPanic "tcDoStmt: unexpected Stmt" (ppr stmt)



---------------------------------------------------
-- MonadFail Proposal warnings
---------------------------------------------------

-- The idea behind issuing MonadFail warnings is that we add them whenever a
-- failable pattern is encountered. However, instead of throwing a type error
-- when the constraint cannot be satisfied, we only issue a warning in
-- "GHC.Tc.Errors".

tcMonadFailOp :: CtOrigin
              -> LPat GhcTc
              -> SyntaxExpr GhcRn    -- The fail op
              -> TcType              -- Type of the whole do-expression
              -> TcRn (FailOperator GhcTc)  -- Typechecked fail op
-- Get a 'fail' operator expression, to use if the pattern match fails.
-- This won't be used in cases where we've already determined the pattern
-- match can't fail (so the fail op is Nothing), however, it seems that the
-- isIrrefutableHsPat test is still required here for some reason I haven't
-- yet determined.
tcMonadFailOp orig pat fail_op res_ty = do
    is_strict <- xoptM LangExt.Strict
    comps <- getCompleteMatchesTcM
    if isIrrefutableHsPat is_strict (irrefutableConLikeTc comps) pat
      then return Nothing
      else Just . snd <$> (tcSyntaxOp orig fail_op [synKnownType stringTy]
                            (mkCheckExpType res_ty) $ \_ _ -> return ())

{-
Note [Treat rebindable syntax first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking
        do { bar; ... } :: IO ()
we want to typecheck 'bar' in the knowledge that it should be an IO thing,
pushing info from the context into the RHS.  To do this, we check the
rebindable syntax first, and push that information into (tcLExprNC rhs).
Otherwise the error shows up when checking the rebindable syntax, and
the expected/inferred stuff is back to front (see #3613).

Note [typechecking ApplicativeStmt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
join ((\pat1 ... patn -> body) <$> e1 <*> ... <*> en)

fresh type variables:
   pat_ty_1..pat_ty_n
   exp_ty_1..exp_ty_n
   t_1..t_(n-1)

body  :: body_ty
(\pat1 ... patn -> body) :: pat_ty_1 -> ... -> pat_ty_n -> body_ty
pat_i :: pat_ty_i
e_i   :: exp_ty_i
<$>   :: (pat_ty_1 -> ... -> pat_ty_n -> body_ty) -> exp_ty_1 -> t_1
<*>_i :: t_(i-1) -> exp_ty_i -> t_i
join :: tn -> res_ty

Note [Scoping in parallel list comprehensions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a parallel list comprehension like [ ebody | a <- blah1; e1 | b <- blah2; e2 ]
we want to ensure that in the lexical environment, tcl_rdr :: LocalRdrEnv, we have
  * 'a' in scope in e1, but not 'b'
  * 'b' in scope in e2, but not 'a'
  * Both in scope in ebody
We don't want too /many/ variables in the LocalRdrEnv, else we make stupid
suggestions for an out-of-scope variable (#22940).

To achieve this we:
  * At the start of each branch, reset the LocalRdrEnv to the outer scope.
  * Before typechecking ebody, add to LocalRdrEnv all the variables bound in
    all branches. This step is done with bindLocalNames.
-}

tcApplicativeStmts
  :: HsStmtContextRn
  -> [(SyntaxExpr GhcRn, ApplicativeArg GhcRn)]
  -> ExpRhoType                         -- rhs_ty
  -> (TcRhoType -> TcM t)               -- thing_inside
  -> TcM ([(SyntaxExpr GhcTc, ApplicativeArg GhcTc)], Type, t)

tcApplicativeStmts ctxt pairs rhs_ty thing_inside
 = do { body_ty <- newFlexiTyVarTy liftedTypeKind
      ; let arity = length pairs
      ; ts <- replicateM (arity-1) $ newInferExpType IIF_DeepRho
      ; exp_tys <- replicateM arity $ newFlexiTyVarTy liftedTypeKind
      ; pat_tys <- replicateM arity $ newFlexiTyVarTy liftedTypeKind
      ; let fun_ty = mkVisFunTysManyU pat_tys body_ty

       -- NB. do the <$>,<*> operators first, we don't want type errors here
       --     i.e. goOps before goArgs
       -- See Note [Treat rebindable syntax first]
      ; let (ops, args) = unzip pairs
      ; ops' <- goOps fun_ty (zip3 ops (ts ++ [rhs_ty]) exp_tys)

      -- Typecheck each ApplicativeArg separately
      -- See Note [ApplicativeDo and constraints]
      ; args' <- mapM (goArg body_ty) (zip3 args pat_tys exp_tys)

      -- Bring into scope all the things bound by the args,
      -- and typecheck the thing_inside
      -- See Note [ApplicativeDo and constraints]
      ; res <- tcExtendIdEnv (concatMap get_arg_bndrs args') $
               thing_inside body_ty

      ; return (zip ops' args', body_ty, res) }
  where
    goOps _ [] = return []
    goOps t_left ((op,t_i,exp_ty) : ops)
      = do { (_, op')
               <- tcSyntaxOp DoStmtOrigin op
                             [synKnownType t_left, synKnownType exp_ty] t_i $
                   \ _ _ -> return ()
           ; t_i <- readExpType t_i
           ; ops' <- goOps t_i ops
           ; return (op' : ops') }

    goArg :: Type -> (ApplicativeArg GhcRn, Type, Type)
          -> TcM (ApplicativeArg GhcTc)

    goArg body_ty (ApplicativeArgOne
                    { xarg_app_arg_one = fail_op
                    , app_arg_pattern = pat
                    , arg_expr = rhs
                    , ..
                    }, pat_ty, exp_ty)
      = setSrcSpan (combineSrcSpans (getLocA pat) (getLocA rhs)) $
        addErrCtxt (StmtErrCtxt ctxt (L (getLoc rhs) $ mkRnBindStmt pat rhs))   $
        do { rhs'      <- tcCheckMonoExprNC rhs exp_ty
           ; (pat', _) <- tcCheckPat (StmtCtxt ctxt) pat (unrestricted pat_ty) $
                          return ()
           ; fail_op' <- fmap join . forM fail_op $ \fail ->
               tcMonadFailOp (DoPatOrigin pat) pat' fail body_ty

           ; return (ApplicativeArgOne
                      { xarg_app_arg_one = fail_op'
                      , app_arg_pattern = pat'
                      , arg_expr        = rhs'
                      , .. }
                    ) }

    goArg _body_ty (ApplicativeArgMany x stmts ret pat ctxt, pat_ty, exp_ty)
      = do { (stmts', (ret',pat')) <-
                tcStmtsAndThen (HsDoStmt ctxt) tcDoStmt stmts (mkCheckExpType exp_ty) $
                \res_ty  -> do
                  { ret'      <- tcExpr ret res_ty
                  ; (pat', _) <- tcCheckPat (StmtCtxt (HsDoStmt ctxt)) pat (unrestricted pat_ty) $
                                 return ()
                  ; return (ret', pat')
                  }
           ; return (ApplicativeArgMany x stmts' ret' pat' ctxt) }

    get_arg_bndrs :: ApplicativeArg GhcTc -> [Id]
    get_arg_bndrs (ApplicativeArgOne { app_arg_pattern = pat }) = collectPatBinders CollNoDictBinders pat
    get_arg_bndrs (ApplicativeArgMany { bv_pattern =  pat })    = collectPatBinders CollNoDictBinders pat

{- Note [ApplicativeDo and constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An applicative-do is supposed to take place in parallel, so
constraints bound in one arm can't possibly be available in another
(#13242).  Our current rule is this (more details and discussion
on the ticket). Consider

   ...stmts...
   ApplicativeStmts [arg1, arg2, ... argN]
   ...more stmts...

where argi :: ApplicativeArg. Each 'argi' itself contains one or more Stmts.
Now, we say that:

* Constraints required by the argi can be solved from
  constraint bound by ...stmts...

* Constraints and existentials bound by the argi are not available
  to solve constraints required either by argj (where i /= j),
  or by ...more stmts....

* Within the stmts of each 'argi' individually, however, constraints bound
  by earlier stmts can be used to solve later ones.

To achieve this, we just typecheck each 'argi' separately, bring all
the variables they bind into scope, and typecheck the thing_inside.

************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************
-}

-- | @checkArgCounts@ takes a @[RenamedMatch]@ and decides whether the same
-- number of /required/ (aka visible) args are used in each equation.
-- Returns the arity, the number of required args
-- E.g.  f @a True  y = ...
--       f    False z = ...
--       The MatchGroup for `f` has arity 2, not 3
checkArgCounts :: AnnoBody body
               => MatchGroup GhcRn (LocatedA (body GhcRn))
               -> TcM VisArity
checkArgCounts (MG { mg_alts = L _ [] })
    = return 1 -- See Note [Empty MatchGroups] in GHC.Rename.Bind
               --   case e of {} or \case {}
               -- Both have arity 1

checkArgCounts (MG { mg_alts = L _ (match1:matches) })
    | null matches  -- There was only one match; nothing to check
    = return n_args1

    -- Two or more matches: check that they agree on arity
    | Just bad_matches <- mb_bad_matches
    = failWithTc $ TcRnMatchesHaveDiffNumArgs (m_ctxt (unLoc match1))
                 $ MatchArgMatches match1 bad_matches
    | otherwise
    = return n_args1
  where
    n_args1 = reqd_args_in_match match1
    mb_bad_matches = NE.nonEmpty [m | m <- matches, reqd_args_in_match m /= n_args1]

    reqd_args_in_match :: LocatedA (Match GhcRn body1) -> VisArity
    -- Counts the number of /required/ (aka visible) args in the match
    reqd_args_in_match (L _ (Match { m_pats = L _ pats })) = count isVisArgLPat pats

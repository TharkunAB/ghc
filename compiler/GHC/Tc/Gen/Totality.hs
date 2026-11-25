-- | Conservative advisory totality report for top-level bindings.
-- See Note [Advisory structural totality report].
module GHC.Tc.Gen.Totality
  ( TotalityStatus (..)
  , reportModuleTotality
  ) where

import GHC.Prelude

import GHC.Core.ConLike (ConLike (..))
import GHC.Core.DataCon (DataCon, dataConOrigArgTys, dataConTyCon)
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.Predicate (isPredTy)
import GHC.Core.TyCo.Rep (FunTyFlag (..), Type (..))
import GHC.Core.TyCon
  ( TyCon
  , isAbstractTyCon
  , isAlgTyCon
  , isClassTyCon
  , isFamilyTyCon
  , isNewTyCon
  , tyConDataCons
  )
import GHC.Core.Type (coreFullView, isUnliftedType)
import GHC.Data.Bag (bagToList)
import GHC.Data.Graph.Directed
  ( Node (..)
  , SCC (..)
  , stronglyConnCompFromEdgedVerticesUniq
  )
import GHC.Driver.DynFlags
  ( DynFlags
  , GeneralFlag (Opt_Hpc)
  , WarningFlag (..)
  , gopt_unset
  , initPromotionTickContext
  , wopt_set
  )
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags, hsc_unit_env)
import GHC.Hs
import GHC.HsToCore.Binds (dsTopLHsBinds)
import GHC.HsToCore.Errors.Types (DsMessage (..))
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types (TcGblEnv (..))
import GHC.Types.Basic (RecFlag (..))
import GHC.Types.Error
  ( MessageClass (MCInfo)
  , MsgEnvelope (..)
  , getErrorMessages
  , getWarningMessages
  , mkLocMessage
  )
import GHC.Types.Id (Id, idName, idType, isClassOpId_maybe, isDataConId_maybe)
import GHC.Types.Name
  ( Name
  , getOccName
  , isDerivedOccName
  , nameModule_maybe
  , nameSrcSpan
  , stableNameCmp
  )
import GHC.Types.Name.Env (NameEnv, extendNameEnv, extendNameEnvList, lookupNameEnv, mkNameEnv)
import GHC.Types.Name.Ppr (mkNamePprCtx)
import GHC.Types.Name.Set (NameSet, elemNameSet, mkNameSet)
import GHC.Types.SrcLoc
  ( GenLocated (..)
  , SrcSpan (..)
  , isGeneratedSrcSpan
  , unLoc
  )
import GHC.Types.Var (varType)
import GHC.Types.Var.Set
  ( VarSet
  , elemVarSet
  , emptyVarSet
  , extendVarSet
  , unionVarSet
  , unionVarSets
  )
import GHC.Unit.Module (Module)
import GHC.Utils.Logger (logFlags, putLogMsg)
import GHC.Utils.Outputable
  ( Depth (AllTheWay)
  , Outputable (..)
  , colon
  , parens
  , text
  , withUserStyle
  , (<+>)
  , (<>)
  )

import Control.Monad (foldM, forM_)
import Data.List (nub, sortBy)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)

{- Note [Advisory structural totality report]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC Proposal #378 (Section 4.10) keeps Haskell partial by default. Type
conversion, proof erasure, and module interfaces do not assume termination.
At the same time, authors of proofs and indexed programs want local feedback
on whether a function is total on finite, total inputs. See the local R05
assurance task in docs/dependent-haskell/ROADMAP.md.

When -freport-totality is enabled, reportModuleTotality inspects the zonked
top-level value bindings of the module and emits one informational MCInfo
line per user binding. The report is strictly advisory:
  1. It never fails compilation, even under -Werror.
  2. It does not change Core conversion, simplifier rules, or .hi files.
  3. It distinguishes three statuses:
       * checked-structural: covered, unguarded equations over closed,
         strictly positive algebraic data types; first-order argument and
         result types; no external or higher-order calls; and either no
         recursion or direct self-recursion on a strict constructor subterm
         in one fixed parameter position.
       * trusted-import: reserved for external functions with an explicit
         verified contract. Ordinary Haskell imports carry no totality
         contract, so calls to external functions stay unknown.
       * unknown (reason): mutual recursion, uncovered or guarded equations,
         higher-order calls or function-typed parameters, unlifted or
         non-positive domains, calls to external imports, calls to local
         bindings that are not checked-structural, or non-structural
         recursive arguments.

Why recompute the call graph?
Type inference breaks binding cycles at explicit type signatures (see
Note [Polymorphic recursion] in GHC.Hs.Binds). A signed function in the
zonked AST therefore sits in its own singleton AbsBinds even when it calls
or is called by another top-level function. Furthermore, an unsigned
recursive binding refers to abe_mono internally and abe_poly externally.
Function reportModuleTotality maps every abe_mono and abe_poly Name to one
canonical top-level Name and recomputes strongly connected components across
all user top-level bindings before classification.

Why run pattern-match coverage privately?
In default compilations without -Wincomplete-patterns, the desugarer skips
exhaustiveness warnings in GHC.HsToCore.Pmc.reportWarnings. Conversely,
enabling -Wincomplete-patterns globally would fail under -Werror. Function
hasUncoveredPatterns runs GHC's pattern-match checker on each candidate
binding in a private DsM state with incomplete-pattern flags enabled and
inspects the resulting DsMessages in memory without emitting warnings.
-}

-- | Advisory totality classification for one top-level binding.
-- See Note [Advisory structural totality report].
data TotalityStatus
  = TotalityCheckedStructural
  | TotalityTrustedImport
  | TotalityUnknown !String
  deriving (Eq, Show)

instance Outputable TotalityStatus where
  ppr TotalityCheckedStructural = text "checked-structural"
  ppr TotalityTrustedImport     = text "trusted-import"
  ppr (TotalityUnknown reason)  = text "unknown" <+> parens (text reason)

-- | Extracted metadata for one user-written top-level binding.
data TopBinding = TopBinding
  { tbPolyId     :: !Id
  , tbName       :: !Name
  , tbSpan       :: !SrcSpan
  , tbOuterBind  :: !(LHsBind GhcTc)
  , tbInnerBind  :: !(HsBind GhcTc)
  }

-- | Per-parameter structural relationship for local variables in scope.
data ParamEnv = ParamEnv
  { peEqual    :: ![VarSet]
  , peSubterms :: ![VarSet]
  }

-- | A recorded direct self-call with its runtime arguments and active subterms.
data SelfCall = SelfCall
  { scArgs     :: ![LHsExpr GhcTc]
  , scSubterms :: ![VarSet]
  }

-- | Analyze zonked top-level bindings and log advisory totality statuses.
-- See Note [Advisory structural totality report].
reportModuleTotality :: HscEnv -> TcGblEnv -> IO ()
reportModuleTotality hsc_env tcg_env = do
  let dflags       = hsc_dflags hsc_env
      logger       = hsc_logger hsc_env
      this_mod     = tcg_semantic_mod tcg_env
      ptc          = initPromotionTickContext dflags
      name_ppr_ctx = mkNamePprCtx ptc (hsc_unit_env hsc_env) (tcg_rdr_env tcg_env)
      (raw_tops, alias_pairs) = collectTopBindings this_mod (tcg_binds tcg_env)
      tops         = sortBy compareTopBinding raw_tops
      top_set      = mkNameSet (map tbName tops)
      alias_env    = mkNameEnv alias_pairs
      resolve v    = resolveTopCallee this_mod top_set alias_env v
      nodes        = [ DigraphNode tb (tbName tb) (bindingCallees resolve (tbInnerBind tb))
                     | tb <- tops
                     ]
      sccs         = stronglyConnCompFromEdgedVerticesUniq nodes
      pmc_hsc_env  = hscUpdateFlags enablePmcFlags hsc_env

  (status_env, _) <- foldM (classifyScc pmc_hsc_env tcg_env this_mod resolve)
                           (mkNameEnv [], mkNameEnv [])
                           sccs

  forM_ tops $ \tb -> do
    let status = case lookupNameEnv status_env (tbName tb) of
                   Just st -> st
                   Nothing -> TotalityUnknown "unanalyzed binding"
        msg    = text "totality:" <+> ppr (getOccName (tbName tb)) <> colon <+> ppr status
        doc    = withUserStyle name_ppr_ctx AllTheWay (mkLocMessage MCInfo (tbSpan tb) msg)
    putLogMsg logger (logFlags logger) MCInfo (tbSpan tb) doc

-- | Enable pattern-match exhaustiveness checks in a local 'DynFlags' copy.
enablePmcFlags :: DynFlags -> DynFlags
enablePmcFlags dflags =
  wopt_set
    (wopt_set
      (wopt_set (gopt_unset dflags Opt_Hpc) Opt_WarnIncompletePatterns)
      Opt_WarnIncompleteUniPatterns)
    Opt_WarnIncompleteRecordSelectors

-- | Order top-level bindings deterministically by source span and name.
compareTopBinding :: TopBinding -> TopBinding -> Ordering
compareTopBinding a b =
  case compareSpan (tbSpan a) (tbSpan b) of
    EQ  -> stableNameCmp (tbName a) (tbName b)
    ord -> ord
  where
    compareSpan (RealSrcSpan r1 _) (RealSrcSpan r2 _) = compare r1 r2
    compareSpan (RealSrcSpan _ _)  _                  = LT
    compareSpan _                  (RealSrcSpan _ _)  = GT
    compareSpan _                  _                  = EQ

-- | Extract user-written top-level bindings and map mono/poly names to canonical names.
collectTopBindings :: Module -> LHsBinds GhcTc -> ([TopBinding], [(Name, Name)])
collectTopBindings this_mod binds = foldr step ([], []) binds
  where
    isUserName nm =
      nameModule_maybe nm == Just this_mod
        && not (isDerivedOccName (getOccName nm))
        && not (isGeneratedSrcSpan (nameSrcSpan nm))

    step lbind@(L loc bind) (acc_tops, acc_aliases) =
      case bind of
        XHsBindsLR abs_b@(AbsBinds { abs_exports = exports, abs_binds = inner_binds }) ->
          let new_aliases =
                [ (nm, idName poly)
                | ABE { abe_poly = poly, abe_mono = mono } <- exports
                , isUserName (idName poly)
                , nm <- [idName poly, idName mono]
                ]
              mono_to_abe = mkNameEnv
                [ (idName mono, abe)
                | abe@(ABE { abe_poly = poly, abe_mono = mono }) <- exports
                , isUserName (idName poly)
                ]
              new_tops = concatMap (extractInner lbind loc abs_b mono_to_abe) inner_binds
          in (new_tops ++ acc_tops, new_aliases ++ acc_aliases)
        FunBind { fun_id = L id_loc poly }
          | isUserName (idName poly)
          , not (isGeneratedSrcSpan (locA loc)) ->
              let span_use = if isGeneratedSrcSpan (locA loc) then locA id_loc else locA loc
                  tb = TopBinding
                    { tbPolyId    = poly
                    , tbName      = idName poly
                    , tbSpan      = span_use
                    , tbOuterBind = lbind
                    , tbInnerBind = bind
                    }
              in (tb : acc_tops, (idName poly, idName poly) : acc_aliases)
        PatBind { pat_lhs = lhs }
          | not (isGeneratedSrcSpan (locA loc)) ->
              let bndrs = filter (isUserName . idName)
                            (collectPatBinders CollNoDictBinders lhs)
                  tbs =
                    [ TopBinding
                        { tbPolyId    = bndr
                        , tbName      = idName bndr
                        , tbSpan      = locA loc
                        , tbOuterBind = lbind
                        , tbInnerBind = bind
                        }
                    | bndr <- bndrs
                    ]
                  aliases = [(idName bndr, idName bndr) | bndr <- bndrs]
              in (tbs ++ acc_tops, aliases ++ acc_aliases)
        _ -> (acc_tops, acc_aliases)

    extractInner outer_lbind outer_loc abs_b mono_to_abe (L inner_loc inner) =
      case inner of
        FunBind { fun_id = L _ mono }
          | Just abe@(ABE { abe_poly = poly }) <- lookupNameEnv mono_to_abe (idName mono)
          , not (isGeneratedSrcSpan (locA inner_loc)) ->
              let single_outer = L outer_loc (XHsBindsLR (abs_b { abs_exports = [abe] }))
              in [ TopBinding
                     { tbPolyId    = poly
                     , tbName      = idName poly
                     , tbSpan      = locA inner_loc
                     , tbOuterBind = single_outer
                     , tbInnerBind = inner
                     }
                 ]
        PatBind { pat_lhs = lhs }
          | not (isGeneratedSrcSpan (locA inner_loc)) ->
              let bndrs = collectPatBinders CollNoDictBinders lhs
              in [ TopBinding
                     { tbPolyId    = poly
                     , tbName      = idName poly
                     , tbSpan      = locA inner_loc
                     , tbOuterBind = outer_lbind
                     , tbInnerBind = inner
                     }
                 | bndr <- bndrs
                 , Just (ABE { abe_poly = poly }) <- [lookupNameEnv mono_to_abe (idName bndr)]
                 ]
        _ -> []

-- | Resolve an identifier occurrence to a canonical top-level binding in this module.
resolveTopCallee :: Module -> NameSet -> NameEnv Name -> Id -> Maybe Name
resolveTopCallee this_mod top_set alias_env v
  | Just target <- lookupNameEnv alias_env nm = Just target
  | nameModule_maybe nm == Just this_mod
  , nm `elemNameSet` top_set                  = Just nm
  | otherwise                                 = Nothing
  where
    nm = idName v

-- | Collect all canonical top-level callees referenced inside a binding body.
bindingCallees :: (Id -> Maybe Name) -> HsBind GhcTc -> [Name]
bindingCallees resolve bind = nub (goBind bind)
  where
    goBind :: HsBind GhcTc -> [Name]
    goBind (FunBind { fun_matches = mg }) = goMatchGroup mg
    goBind (PatBind { pat_lhs = p, pat_rhs = grhss }) = goLPat p ++ goGRHSs grhss
    goBind (VarBind { var_rhs = rhs }) = goLExpr rhs
    goBind (XHsBindsLR (AbsBinds { abs_binds = bs })) = concatMap (goBind . unLoc) bs
    goBind (PatSynBind {}) = []

    goMatchGroup :: MatchGroup GhcTc (LHsExpr GhcTc) -> [Name]
    goMatchGroup (MG { mg_alts = L _ alts }) = concatMap (goMatch . unLoc) alts

    goMatch :: Match GhcTc (LHsExpr GhcTc) -> [Name]
    goMatch (Match { m_pats = L _ pats, m_grhss = grhss }) =
      concatMap goLPat pats ++ goGRHSs grhss

    goGRHSs :: GRHSs GhcTc (LHsExpr GhcTc) -> [Name]
    goGRHSs (GRHSs { grhssGRHSs = grhss, grhssLocalBinds = L _ lbinds }) =
      concatMap (goGRHS . unLoc) (NE.toList grhss) ++ goLocalBinds lbinds

    goGRHS :: GRHS GhcTc (LHsExpr GhcTc) -> [Name]
    goGRHS (GRHS _ guards body) = concatMap (goStmt . unLoc) guards ++ goLExpr body

    goLocalBinds :: HsLocalBinds GhcTc -> [Name]
    goLocalBinds (HsValBinds _ (ValBinds _ vbs)) =
      concatMap (goBind . unLoc) (val_binds vbs)
    goLocalBinds (HsValBinds _ (XValBindsLR (HsVBG grps _))) =
      concatMap (\(_, bs) -> concatMap (goBind . unLoc) bs) grps
    goLocalBinds (HsIPBinds _ (IPBinds _ ip_binds)) =
      concatMap (\(L _ (IPBind _ _ e)) -> goLExpr e) ip_binds
    goLocalBinds (EmptyLocalBinds _) = []

    goStmt :: ExprStmt GhcTc -> [Name]
    goStmt (LastStmt _ body _ _) = goLExpr body
    goStmt (BindStmt _ pat rhs)  = goLPat pat ++ goLExpr rhs
    goStmt (BodyStmt _ body _ _) = goLExpr body
    goStmt (LetStmt _ (L _ lbs)) = goLocalBinds lbs
    goStmt (ParStmt _ blocks _ _) =
      concatMap (\(ParStmtBlock _ stmts _ _) -> concatMap (goStmt . unLoc) stmts) blocks
    goStmt (TransStmt { trS_stmts = stmts, trS_using = u, trS_by = mb_by }) =
      concatMap (goStmt . unLoc) stmts ++ goLExpr u ++ maybe [] goLExpr mb_by
    goStmt (RecStmt { recS_stmts = L _ stmts }) = concatMap (goStmt . unLoc) stmts
    goStmt (XStmtLR (ApplicativeStmt _ args _)) =
      concatMap (\(_, arg) -> goAppArg arg) args

    goAppArg :: ApplicativeArg GhcTc -> [Name]
    goAppArg (ApplicativeArgOne _ pat expr _) = goLPat pat ++ goLExpr expr
    goAppArg (ApplicativeArgMany _ stmts ret pat _) =
      concatMap (goStmt . unLoc) stmts ++ goExpr ret ++ goLPat pat

    goLPat :: LPat GhcTc -> [Name]
    goLPat (L _ pat) = goPat pat

    goPat :: Pat GhcTc -> [Name]
    goPat (ParPat _ p)        = goLPat p
    goPat (LazyPat _ p)       = goLPat p
    goPat (BangPat _ p)       = goLPat p
    goPat (AsPat _ _ p)       = goLPat p
    goPat (SigPat _ p _)      = goLPat p
    goPat (ModifiedPat _ _ p) = goLPat p
    goPat (ListPat _ ps)      = concatMap goLPat ps
    goPat (TuplePat _ ps _)   = concatMap goLPat ps
    goPat (SumPat _ p _ _)    = goLPat p
    goPat (OrPat _ ps)        = concatMap goLPat (NE.toList ps)
    goPat (ViewPat _ e p)     = goLExpr e ++ goLPat p
    goPat (ConPat { pat_args = details }) =
      concatMap goLPat (conPatArgs details)
    goPat (XPat (CoPat _ inner _))          = goPat inner
    goPat (XPat (ExpansionPat _ expanded))  = goPat expanded
    goPat (XPat (NativeExistsPat _ _ _ _ inner)) = goPat inner
    goPat _ = []

    goLExpr :: LHsExpr GhcTc -> [Name]
    goLExpr (L _ e) = goExpr e

    goExpr :: HsExpr GhcTc -> [Name]
    goExpr (HsVar _ (L _ v))
      | Just target <- resolve v = [target]
      | otherwise                = []
    goExpr (HsLam _ _ mg)            = goMatchGroup mg
    goExpr (HsApp _ f a)             = goLExpr f ++ goLExpr a
    goExpr (HsAppType _ f _)         = goLExpr f
    goExpr (HsAppRetained _ f a)     = goLExpr f ++ goLExpr a
    goExpr (NegApp _ e _)            = goLExpr e
    goExpr (HsPar _ e)               = goLExpr e
    goExpr (ExplicitTuple _ args _)  = concatMap goTupArg args
    goExpr (ExplicitSum _ _ _ e)     = goLExpr e
    goExpr (HsCase _ s mg)           = goLExpr s ++ goMatchGroup mg
    goExpr (HsIf _ c t f)            = goLExpr c ++ goLExpr t ++ goLExpr f
    goExpr (HsMultiIf _ grhss)       = concatMap (goGRHS . unLoc) (NE.toList grhss)
    goExpr (HsLet _ (L _ lbs) body)  = goLocalBinds lbs ++ goLExpr body
    goExpr (HsDo _ _ (L _ stmts))    = concatMap (goStmt . unLoc) stmts
    goExpr (ExplicitList _ es)       = concatMap goLExpr es
    goExpr (RecordCon { rcon_flds = HsRecFields { rec_flds = flds } }) =
      concatMap (\(L _ fb) -> goLExpr (hfbRHS fb)) flds
    goExpr (ExprWithTySig _ e _)     = goLExpr e
    goExpr (ArithSeq _ _ info)       = goArithSeq info
    goExpr (HsStatic _ e)            = goLExpr e
    goExpr (HsPragE _ _ e)           = goLExpr e
    goExpr (HsProc _ p _)            = goLPat p
    goExpr (XExpr xexpr)             = goXXExpr xexpr
    goExpr _                         = []

    goTupArg :: HsTupArg GhcTc -> [Name]
    goTupArg (Present _ e) = goLExpr e
    goTupArg (Missing _)   = []

    goArithSeq :: ArithSeqInfo GhcTc -> [Name]
    goArithSeq (From e1)           = goLExpr e1
    goArithSeq (FromThen e1 e2)    = goLExpr e1 ++ goLExpr e2
    goArithSeq (FromTo e1 e2)      = goLExpr e1 ++ goLExpr e2
    goArithSeq (FromThenTo a b c)  = goLExpr a ++ goLExpr b ++ goLExpr c

    goXXExpr :: XXExprGhcTc -> [Name]
    goXXExpr (WrapExpr _ e)                     = goExpr e
    goXXExpr (ExpandedThingTc (HSE _ e))        = goLExpr e
    goXXExpr (NativeExistsPack _ _ e)           = goLExpr e
    goXXExpr (NativeExistsUnpack _ _ _ p body)  = goLExpr p ++ goLExpr body
    goXXExpr (NativeCaseAlias s _ _ _ body)     = goLExpr s ++ goLExpr body
    goXXExpr (NativeLetAlias r _ _ _ _ body)    = goLExpr r ++ goLExpr body
    goXXExpr (HsTick _ e)                       = goLExpr e
    goXXExpr (HsBinTick _ _ e)                  = goLExpr e
    goXXExpr (HsRecSelTc (FieldOcc _ (L _ sel)))
      | Just target <- resolve sel              = [target]
      | otherwise                               = []
    goXXExpr (ConLikeTc _)                      = []

-- | Extract sub-patterns from constructor pattern details.
conPatArgs :: HsConPatDetails GhcTc -> [LPat GhcTc]
conPatArgs (PrefixCon _ ps) = ps
conPatArgs (InfixCon _ p1 p2) = [p1, p2]
conPatArgs (RecCon _ (HsRecFields { rec_flds = flds })) = map (hfbRHS . unLoc) flds

-- | Classify one strongly connected component in bottom-up topological order.
classifyScc
  :: HscEnv
  -> TcGblEnv
  -> Module
  -> (Id -> Maybe Name)
  -> (NameEnv TotalityStatus, NameEnv Int)
  -> SCC TopBinding
  -> IO (NameEnv TotalityStatus, NameEnv Int)
classifyScc pmc_hsc_env tcg_env this_mod resolve (status_env, arity_env) scc =
  case scc of
    CyclicSCC tbs@(_ : _ : _) ->
      let updates = [(tbName tb, TotalityUnknown "mutual recursion") | tb <- tbs]
      in pure (extendNameEnvList status_env updates, arity_env)
    CyclicSCC [tb] ->
      classifySingle pmc_hsc_env tcg_env this_mod resolve (status_env, arity_env) True tb
    CyclicSCC [] ->
      pure (status_env, arity_env)
    AcyclicSCC tb ->
      classifySingle pmc_hsc_env tcg_env this_mod resolve (status_env, arity_env) False tb

-- | Classify a single non-mutually-recursive top-level binding.
classifySingle
  :: HscEnv
  -> TcGblEnv
  -> Module
  -> (Id -> Maybe Name)
  -> (NameEnv TotalityStatus, NameEnv Int)
  -> Bool
  -> TopBinding
  -> IO (NameEnv TotalityStatus, NameEnv Int)
classifySingle pmc_hsc_env tcg_env this_mod resolve (status_env, arity_env) is_cyclic tb = do
  uncovered <- hasUncoveredPatterns pmc_hsc_env tcg_env (tbOuterBind tb)
  let (status, mb_arity) =
        evaluateBinding this_mod resolve status_env arity_env uncovered is_cyclic tb
      status_env' = extendNameEnv status_env (tbName tb) status
      arity_env'  = case mb_arity of
                      Just ar -> extendNameEnv arity_env (tbName tb) ar
                      Nothing -> arity_env
  pure (status_env', arity_env')

-- | Run GHC's pattern-match checker privately on one binding to detect missing cases.
hasUncoveredPatterns :: HscEnv -> TcGblEnv -> LHsBind GhcTc -> IO Bool
hasUncoveredPatterns pmc_hsc_env tcg_env lbind = do
  (msgs, mb_res) <- initDs pmc_hsc_env tcg_env (dsTopLHsBinds [lbind])
  let warns = bagToList (getWarningMessages msgs)
      errs  = bagToList (getErrorMessages msgs)
  pure (isNothing mb_res || not (null errs) || any isIncompleteDiag warns)
  where
    isNothing Nothing  = True
    isNothing (Just _) = False

    isIncompleteDiag :: MsgEnvelope DsMessage -> Bool
    isIncompleteDiag env = case errMsgDiagnostic env of
      DsNonExhaustivePatterns {}   -> True
      DsIncompleteRecordSelector {} -> True
      DsMaxPmCheckModelsReached {} -> True
      _                            -> False

-- | Pure totality check for a single top-level binding once PMC results are known.
evaluateBinding
  :: Module
  -> (Id -> Maybe Name)
  -> NameEnv TotalityStatus
  -> NameEnv Int
  -> Bool
  -> Bool
  -> TopBinding
  -> (TotalityStatus, Maybe Int)
evaluateBinding this_mod resolve status_env arity_env pmc_uncovered is_cyclic tb =
  case tbInnerBind tb of
    FunBind { fun_matches = MG { mg_alts = L _ matches } } ->
      case checkFunMatches matches of
        Left reason -> (TotalityUnknown reason, Nothing)
        Right (param_arity, match_envs)
          | pmc_uncovered ->
              (TotalityUnknown "partial or guarded equations", Nothing)
          | otherwise ->
              case traverse (checkMatchBody this_mod resolve status_env arity_env (tbName tb))
                            match_envs of
                Left reason -> (TotalityUnknown reason, Nothing)
                Right call_lists ->
                  case checkFirstOrderSignature param_arity (idType (tbPolyId tb)) of
                    Left reason -> (TotalityUnknown reason, Nothing)
                    Right () ->
                      let self_calls = concat call_lists
                      in if not is_cyclic && null self_calls
                           then (TotalityCheckedStructural, Just param_arity)
                           else if hasDecreasingPosition param_arity self_calls
                             then (TotalityCheckedStructural, Just param_arity)
                             else (TotalityUnknown "non-structural recursive call", Nothing)
    PatBind { pat_lhs = lhs, pat_rhs = grhss }
      | pmc_uncovered ->
          (TotalityUnknown "partial or guarded equations", Nothing)
      | otherwise ->
          case checkPattern lhs of
            Left reason -> (TotalityUnknown reason, Nothing)
            Right _ ->
              case checkGRHSsBody this_mod resolve status_env arity_env (tbName tb)
                                  (ParamEnv [] []) grhss of
                Left reason -> (TotalityUnknown reason, Nothing)
                Right self_calls ->
                  case checkFirstOrderSignature 0 (idType (tbPolyId tb)) of
                    Left reason -> (TotalityUnknown reason, Nothing)
                    Right ()
                      | not is_cyclic && null self_calls ->
                          (TotalityCheckedStructural, Just 0)
                      | otherwise ->
                          (TotalityUnknown "non-structural recursive call", Nothing)
    _ -> (TotalityUnknown "unsupported binding form", Nothing)

-- | Validate equation patterns and guards and build initial subterm environments.
checkFunMatches
  :: [LMatch GhcTc (LHsExpr GhcTc)]
  -> Either String (Int, [(ParamEnv, GRHSs GhcTc (LHsExpr GhcTc))])
checkFunMatches [] = Left "partial or guarded equations"
checkFunMatches matches = do
  rows <- traverse checkOne matches
  let arities = map fst rows
  case arities of
    ar : rest | all (== ar) rest -> Right (ar, map snd rows)
    _                            -> Left "partial or guarded equations"
  where
    checkOne (L _ (Match { m_pats = L _ raw_pats, m_grhss = grhss })) = do
      let val_pats = filter (not . isPredVarPat) raw_pats
      pat_infos <- traverse checkPattern val_pats
      let env = ParamEnv
            { peEqual    = map fst pat_infos
            , peSubterms = map snd pat_infos
            }
      pure (length val_pats, (env, grhss))

-- | Detect inserted dictionary or equality evidence patterns in retained equations.
isPredVarPat :: LPat GhcTc -> Bool
isPredVarPat (L _ pat0) = go pat0
  where
    go (VarPat _ (L _ v))            = isPredTy (varType v)
    go (ParPat _ (L _ p))            = go p
    go (LazyPat _ (L _ p))           = go p
    go (BangPat _ (L _ p))           = go p
    go (XPat (CoPat _ inner _))      = go inner
    go (XPat (ExpansionPat _ inner)) = go inner
    go _                             = False

-- | Validate a pattern and return @(equalVars, strictSubtermVars)@.
checkPattern :: LPat GhcTc -> Either String (VarSet, VarSet)
checkPattern (L _ pat0) = go False pat0
  where
    go :: Bool -> Pat GhcTc -> Either String (VarSet, VarSet)
    go under_con pat = case pat of
      WildPat _ ->
        Right (emptyVarSet, emptyVarSet)
      VarPat _ (L _ v)
        | isPredTy (varType v) ->
            Right (emptyVarSet, emptyVarSet)
        | under_con ->
            Right (emptyVarSet, extendVarSet emptyVarSet v)
        | otherwise ->
            Right (extendVarSet emptyVarSet v, emptyVarSet)
      ParPat _ (L _ p) ->
        go under_con p
      BangPat _ (L _ p) ->
        go under_con p
      SigPat _ (L _ p) _ ->
        go under_con p
      ModifiedPat _ _ (L _ p) ->
        go under_con p
      LazyPat _ (L _ p)
        | isIrrefutableVarPat p -> go under_con p
        | otherwise             -> Left "partial or guarded equations"
      AsPat _ (L _ v) (L _ p) -> do
        (eqs, subs) <- go under_con p
        if under_con
          then Right (eqs, extendVarSet subs v)
          else Right (extendVarSet eqs v, subs)
      TuplePat _ ps Boxed -> do
        parts <- traverse (\(L _ p) -> go under_con p) ps
        Right (unionVarSets (map fst parts), unionVarSets (map snd parts))
      TuplePat _ _ Unboxed ->
        Left "unlifted or non-positive domain"
      ListPat _ ps -> do
        let step_con = under_con || not (null ps)
        parts <- traverse (\(L _ p) -> go step_con p) ps
        Right (unionVarSets (map fst parts), unionVarSets (map snd parts))
      ConPat { pat_con = L _ (RealDataCon dc), pat_args = details } -> do
        let tc = dataConTyCon dc
        if not (isClosedPositiveAlgTyCon tc)
          then Left "unlifted or non-positive domain"
          else do
            parts <- traverse (\(L _ p) -> go True p) (conPatArgs details)
            Right (emptyVarSet, unionVarSets (map snd parts))
      ConPat { pat_con = L _ (PatSynCon _) } ->
        Left "partial or guarded equations"
      XPat (CoPat _ inner _) ->
        go under_con inner
      XPat (ExpansionPat _ inner) ->
        go under_con inner
      XPat (NativeExistsPat _ _ payload _ inner) -> do
        (eqs, subs) <- go under_con inner
        if under_con
          then Right (eqs, extendVarSet subs payload)
          else Right (extendVarSet eqs payload, subs)
      InvisPat {} ->
        Right (emptyVarSet, emptyVarSet)
      EmbTyPat {} ->
        Right (emptyVarSet, emptyVarSet)
      _ ->
        Left "partial or guarded equations"

    isIrrefutableVarPat (VarPat {})             = True
    isIrrefutableVarPat (WildPat {})            = True
    isIrrefutableVarPat (ParPat _ (L _ inner))  = isIrrefutableVarPat inner
    isIrrefutableVarPat _                       = False

-- | Check a single equation's right-hand side and local bindings.
checkMatchBody
  :: Module
  -> (Id -> Maybe Name)
  -> NameEnv TotalityStatus
  -> NameEnv Int
  -> Name
  -> (ParamEnv, GRHSs GhcTc (LHsExpr GhcTc))
  -> Either String [SelfCall]
checkMatchBody this_mod resolve status_env arity_env self_name (param_env, grhss) =
  checkGRHSsBody this_mod resolve status_env arity_env self_name param_env grhss

-- | Check unguarded 'GRHSs' and any local @where@ bindings.
checkGRHSsBody
  :: Module
  -> (Id -> Maybe Name)
  -> NameEnv TotalityStatus
  -> NameEnv Int
  -> Name
  -> ParamEnv
  -> GRHSs GhcTc (LHsExpr GhcTc)
  -> Either String [SelfCall]
checkGRHSsBody this_mod resolve status_env arity_env self_name param_env
               (GRHSs { grhssGRHSs = grhss, grhssLocalBinds = L _ lbinds }) = do
  local_calls <- checkLocalBinds this_mod resolve status_env arity_env self_name param_env lbinds
  case NE.toList grhss of
    [L _ (GRHS _ [] body)] -> do
      rhs_calls <- checkExpr this_mod resolve status_env arity_env self_name param_env body
      Right (local_calls ++ rhs_calls)
    _ -> Left "partial or guarded equations"

-- | Conservatively check non-recursive local @let@ and @where@ bindings.
checkLocalBinds
  :: Module
  -> (Id -> Maybe Name)
  -> NameEnv TotalityStatus
  -> NameEnv Int
  -> Name
  -> ParamEnv
  -> HsLocalBinds GhcTc
  -> Either String [SelfCall]
checkLocalBinds _ _ _ _ _ _ (EmptyLocalBinds _) = Right []
checkLocalBinds _ _ _ _ _ _ (HsIPBinds {})      = Left "higher-order call"
checkLocalBinds _ _ _ _ _ _ (HsValBinds _ (ValBinds {})) =
  Left "unsupported local binding"
checkLocalBinds this_mod resolve status_env arity_env self_name param_env
                (HsValBinds _ (XValBindsLR (HsVBG grps _))) =
  fmap concat (traverse checkGroup grps)
  where
    checkGroup (Recursive, _) = Left "non-structural recursive call"
    checkGroup (NonRecursive, bs) = fmap concat (traverse (checkOneLocal . unLoc) bs)

    checkOneLocal (XHsBindsLR (AbsBinds { abs_binds = inner_bs })) =
      fmap concat (traverse (checkOneLocal . unLoc) inner_bs)
    checkOneLocal (FunBind { fun_id = L _ fid, fun_matches = MG { mg_alts = L _ [L _ m] } })
      | Match { m_pats = L _ pats, m_grhss = grhss } <- m
      , null (filter (not . isPredVarPat) pats)
      , Right () <- checkFirstOrderSignature 0 (idType fid) =
          checkGRHSsBody this_mod resolve status_env arity_env self_name param_env grhss
    checkOneLocal (PatBind { pat_lhs = lhs, pat_rhs = grhss }) = do
      _ <- checkPattern lhs
      checkGRHSsBody this_mod resolve status_env arity_env self_name param_env grhss
    checkOneLocal (VarBind { var_rhs = rhs }) =
      checkExpr this_mod resolve status_env arity_env self_name param_env rhs
    checkOneLocal _ =
      Left "higher-order call"

-- | Check an expression and collect all direct self-calls.
checkExpr
  :: Module
  -> (Id -> Maybe Name)
  -> NameEnv TotalityStatus
  -> NameEnv Int
  -> Name
  -> ParamEnv
  -> LHsExpr GhcTc
  -> Either String [SelfCall]
checkExpr this_mod resolve status_env arity_env self_name param_env lexpr =
  let (head_expr, val_args) = peelAppSpine lexpr
  in case head_expr of
       XExpr (ConLikeTc (RealDataCon dc)) ->
         checkConCall dc val_args
       XExpr (ConLikeTc (PatSynCon _)) ->
         Left "partial or guarded equations"
       HsVar _ (L _ v)
         | Just dc <- isDataConId_maybe v ->
             checkConCall dc val_args
         | Just target <- resolve v ->
             checkLocalTopCall target val_args
         | isJust (isClassOpId_maybe v) ->
             if nameModule_maybe (idName v) == Just this_mod
               then Left "higher-order call"
               else Left "imports external function"
         | isJust (nameModule_maybe (idName v)) ->
             Left "imports external function"
         | not (null val_args) ->
             Left "higher-order call"
         | otherwise ->
             Right []
       XExpr (HsRecSelTc (FieldOcc _ (L _ sel)))
         | Just target <- resolve sel ->
             checkLocalTopCall target val_args
         | isJust (nameModule_maybe (idName sel))
         , nameModule_maybe (idName sel) /= Just this_mod ->
             Left "imports external function"
         | length val_args <= 1 ->
             checkArgs val_args
         | otherwise ->
             Left "higher-order call"
       _ | not (null val_args) ->
             Left "higher-order call"
         | otherwise ->
             checkNonApp head_expr
  where
    checkArgs :: [LHsExpr GhcTc] -> Either String [SelfCall]
    checkArgs args =
      fmap concat (traverse (checkExpr this_mod resolve status_env arity_env self_name param_env) args)

    checkConCall :: DataCon -> [LHsExpr GhcTc] -> Either String [SelfCall]
    checkConCall dc args
      | not (isClosedPositiveAlgTyCon (dataConTyCon dc)) =
          Left "unlifted or non-positive domain"
      | otherwise =
          checkArgs args

    checkLocalTopCall :: Name -> [LHsExpr GhcTc] -> Either String [SelfCall]
    checkLocalTopCall target args
      | target == self_name = do
          nested <- checkArgs args
          let here = SelfCall { scArgs = args, scSubterms = peSubterms param_env }
          Right (here : nested)
      | otherwise =
          case lookupNameEnv status_env target of
            Just TotalityCheckedStructural ->
              case lookupNameEnv arity_env target of
                Just expected_ar | length args == expected_ar -> checkArgs args
                _ -> Left "higher-order call"
            Just TotalityTrustedImport ->
              checkArgs args
            _ ->
              Left "calls non-total local function"

    checkNonApp :: HsExpr GhcTc -> Either String [SelfCall]
    checkNonApp expr = case expr of
      ExplicitTuple _ tup_args Boxed ->
        case traverse tupArgExpr tup_args of
          Just es -> checkArgs es
          Nothing -> Left "higher-order type"
      ExplicitTuple _ _ Unboxed ->
        Left "unlifted or non-positive domain"
      ExplicitList _ es ->
        checkArgs es
      RecordCon { rcon_con = L _ (RealDataCon dc), rcon_flds = HsRecFields { rec_flds = flds } } ->
        checkConCall dc (map (hfbRHS . unLoc) flds)
      RecordCon { rcon_con = L _ (PatSynCon _) } ->
        Left "partial or guarded equations"
      HsIf _ c t f ->
        checkArgs [c, t, f]
      HsCase _ scrut (MG { mg_alts = L _ alts }) -> do
        scrut_calls <- checkArgs [scrut]
        let mb_scrut_var = unwrapExprVar scrut
        alt_calls <- traverse (checkCaseAlt mb_scrut_var . unLoc) alts
        Right (scrut_calls ++ concat alt_calls)
      HsLet _ (L _ lbs) body -> do
        lb_calls <- checkLocalBinds this_mod resolve status_env arity_env self_name param_env lbs
        body_calls <- checkArgs [body]
        Right (lb_calls ++ body_calls)
      HsLam {} ->
        Left "higher-order type"
      HsMultiIf {} ->
        Left "partial or guarded equations"
      XExpr (NativeExistsPack _ _ payload) ->
        checkArgs [payload]
      XExpr (NativeExistsUnpack _ _ _ pkg body) ->
        checkArgs [pkg, body]
      XExpr (NativeCaseAlias scrut alias_id _ _ body) -> do
        scrut_calls <- checkArgs [scrut]
        let param_env' = extendAliasEnv (unwrapExprVar scrut) alias_id param_env
        body_calls <- checkExpr this_mod resolve status_env arity_env self_name param_env' body
        Right (scrut_calls ++ body_calls)
      XExpr (NativeLetAlias rhs _ alias_id _ _ body) -> do
        rhs_calls <- checkArgs [rhs]
        let param_env' = extendAliasEnv (unwrapExprVar rhs) alias_id param_env
        body_calls <- checkExpr this_mod resolve status_env arity_env self_name param_env' body
        Right (rhs_calls ++ body_calls)
      HsLit {} ->
        Right []
      _ ->
        Left "imports external function"

    tupArgExpr :: HsTupArg GhcTc -> Maybe (LHsExpr GhcTc)
    tupArgExpr (Present _ e) = Just e
    tupArgExpr (Missing _)   = Nothing

    checkCaseAlt :: Maybe Id -> Match GhcTc (LHsExpr GhcTc) -> Either String [SelfCall]
    checkCaseAlt mb_scrut_var (Match { m_pats = L _ raw_pats, m_grhss = grhss }) =
      case filter (not . isPredVarPat) raw_pats of
        [pat] -> do
          (eqs, subs) <- checkPattern pat
          let param_env' = refineCaseEnv mb_scrut_var eqs subs param_env
          checkGRHSsBody this_mod resolve status_env arity_env self_name param_env' grhss
        _ -> Left "partial or guarded equations"

-- | Propagate parameter equality and strict subterm sets across a @case@ alternative.
refineCaseEnv :: Maybe Id -> VarSet -> VarSet -> ParamEnv -> ParamEnv
refineCaseEnv Nothing _ _ env = env
refineCaseEnv (Just scrut_var) pat_eqs pat_subs (ParamEnv eq_list sub_list) =
  let step (eqs, subs)
        | scrut_var `elemVarSet` subs =
            (eqs, subs `unionVarSet` pat_eqs `unionVarSet` pat_subs)
        | scrut_var `elemVarSet` eqs =
            (eqs `unionVarSet` pat_eqs, subs `unionVarSet` pat_subs)
        | otherwise =
            (eqs, subs)
      (eq_list', sub_list') = unzip (map step (zip eq_list sub_list))
  in ParamEnv eq_list' sub_list'

-- | Propagate parameter equality and subterm sets across a local variable alias.
extendAliasEnv :: Maybe Id -> Id -> ParamEnv -> ParamEnv
extendAliasEnv Nothing _ env = env
extendAliasEnv (Just src) dst (ParamEnv eq_list sub_list) =
  let step (eqs, subs) =
        ( if src `elemVarSet` eqs then extendVarSet eqs dst else eqs
        , if src `elemVarSet` subs then extendVarSet subs dst else subs
        )
      (eq_list', sub_list') = unzip (map step (zip eq_list sub_list))
  in ParamEnv eq_list' sub_list'

-- | Peel wrappers and collect runtime value arguments in left-to-right call order.
peelAppSpine :: LHsExpr GhcTc -> (HsExpr GhcTc, [LHsExpr GhcTc])
peelAppSpine lexpr = go (unLoc lexpr) []
  where
    go :: HsExpr GhcTc -> [LHsExpr GhcTc] -> (HsExpr GhcTc, [LHsExpr GhcTc])
    go (HsPar _ (L _ e)) args                      = go e args
    go (HsPragE _ _ (L _ e)) args                  = go e args
    go (ExprWithTySig _ (L _ e) _) args            = go e args
    go (HsAppType _ (L _ f) _) args                = go f args
    go (HsApp _ (L _ f) arg) args                  = go f (arg : args)
    go (HsAppRetained _ (L _ f) arg) args          = go f (arg : args)
    go (XExpr (WrapExpr _ e)) args                 = go e args
    go (XExpr (ExpandedThingTc (HSE _ (L _ e)))) a = go e a
    go (XExpr (HsTick _ (L _ e))) args             = go e args
    go (XExpr (HsBinTick _ _ (L _ e))) args        = go e args
    go expr args                                   = (expr, args)

-- | Extract a bare variable from an expression, looking through erased wrappers.
unwrapExprVar :: LHsExpr GhcTc -> Maybe Id
unwrapExprVar lexpr =
  case peelAppSpine lexpr of
    (HsVar _ (L _ v), []) | isNothingDataCon v -> Just v
    _                                          -> Nothing
  where
    isNothingDataCon v = case isDataConId_maybe v of
      Nothing -> True
      Just _  -> False

-- | Check whether one fixed parameter position strictly decreases at every self-call.
hasDecreasingPosition :: Int -> [SelfCall] -> Bool
hasDecreasingPosition param_arity self_calls
  | param_arity <= 0 || null self_calls = False
  | any (\sc -> length (scArgs sc) /= param_arity) self_calls = False
  | otherwise = any decreasesAt [0 .. param_arity - 1]
  where
    decreasesAt :: Int -> Bool
    decreasesAt pos = all (decreasesCall pos) self_calls

    decreasesCall :: Int -> SelfCall -> Bool
    decreasesCall pos (SelfCall { scArgs = args, scSubterms = subs_by_pos })
      | arg_expr : _ <- drop pos args
      , subs : _     <- drop pos subs_by_pos
      , Just v       <- unwrapExprVar arg_expr
      = v `elemVarSet` subs
      | otherwise = False

-- | Check that @param_arity@ value parameters and the remaining result are first-order and lifted.
checkFirstOrderSignature :: Int -> Type -> Either String ()
checkFirstOrderSignature arity0 ty0 = peelParams arity0 (coreFullView ty0)
  where
    peelParams :: Int -> Type -> Either String ()
    peelParams remaining ty = case coreFullView ty of
      ForAllTy _ _ body ->
        peelParams remaining body
      FunTy FTF_C_T _ _ _ body ->
        peelParams remaining body
      DepDictTy _ body ->
        peelParams remaining body
      FunTy FTF_T_T _ _ arg_ty body
        | remaining > 0 -> do
            checkValueDomain arg_ty
            peelParams (remaining - 1) body
        | otherwise ->
            Left "higher-order type"
      DepFunTy _ binder body
        | remaining > 0 -> do
            checkValueDomain (varType binder)
            peelParams (remaining - 1) body
        | otherwise ->
            Left "higher-order type"
      FunTy _ _ _ arg_ty body
        | remaining > 0 -> do
            checkValueDomain arg_ty
            peelParams (remaining - 1) body
        | otherwise ->
            Left "unlifted or non-positive domain"
      other
        | remaining == 0 -> checkValueDomain other
        | otherwise      -> Left "higher-order type"

    checkValueDomain :: Type -> Either String ()
    checkValueDomain ty
      | isUnliftedType ty    = Left "unlifted or non-positive domain"
      | containsFunctionTy ty = Left "higher-order type"
      | otherwise            = Right ()

-- | Detect value function arrows inside a parameter or result type.
containsFunctionTy :: Type -> Bool
containsFunctionTy ty0 = case coreFullView ty0 of
  FunTy FTF_T_T _ _ _ _   -> True
  FunTy FTF_C_T _ _ _ res -> containsFunctionTy res
  FunTy _ _ _ a b         -> containsFunctionTy a || containsFunctionTy b
  DepFunTy {}             -> True
  ForAllTy _ _ body       -> containsFunctionTy body
  ExistsTy _ _ body       -> containsFunctionTy body
  DepDictTy _ body        -> containsFunctionTy body
  TyConApp _ args         -> any containsFunctionTy args
  AppTy f a               -> containsFunctionTy f || containsFunctionTy a
  CastTy t _              -> containsFunctionTy t
  _                       -> False

-- | Check that a 'TyCon' is a closed algebraic data type with strictly positive fields.
isClosedPositiveAlgTyCon :: TyCon -> Bool
isClosedPositiveAlgTyCon tc =
  isAlgTyCon tc
    && not (isNewTyCon tc)
    && not (isClassTyCon tc)
    && not (isFamilyTyCon tc)
    && not (isAbstractTyCon tc)
    && all strictlyPositiveCon (tyConDataCons tc)
  where
    strictlyPositiveCon :: DataCon -> Bool
    strictlyPositiveCon dc =
      all (strictlyPositiveIn tc . scaledThing) (dataConOrigArgTys dc)

-- | Check that 'tc' never occurs to the left of an arrow or under a type variable.
strictlyPositiveIn :: TyCon -> Type -> Bool
strictlyPositiveIn target ty0 = case coreFullView ty0 of
  FunTy FTF_T_T _ _ a b -> not (mentionsTyCon target a) && strictlyPositiveIn target b
  FunTy _ _ _ a b       -> not (mentionsTyCon target a) && strictlyPositiveIn target b
  DepFunTy _ bndr b     -> not (mentionsTyCon target (varType bndr)) && strictlyPositiveIn target b
  ForAllTy _ _ b        -> strictlyPositiveIn target b
  ExistsTy _ _ b        -> strictlyPositiveIn target b
  DepDictTy _ b         -> strictlyPositiveIn target b
  TyConApp _ args       -> all (strictlyPositiveIn target) args
  AppTy f a             -> not (mentionsTyCon target f) && not (mentionsTyCon target a)
  CastTy t _            -> strictlyPositiveIn target t
  _                     -> True

-- | Test whether 'target' appears anywhere in a type.
mentionsTyCon :: TyCon -> Type -> Bool
mentionsTyCon target ty0 = case coreFullView ty0 of
  TyConApp tc args  -> tc == target || any (mentionsTyCon target) args
  FunTy _ _ _ a b   -> mentionsTyCon target a || mentionsTyCon target b
  DepFunTy _ bndr b -> mentionsTyCon target (varType bndr) || mentionsTyCon target b
  ForAllTy _ _ b    -> mentionsTyCon target b
  ExistsTy _ _ b    -> mentionsTyCon target b
  DepDictTy _ b     -> mentionsTyCon target b
  AppTy f a         -> mentionsTyCon target f || mentionsTyCon target a
  CastTy t _        -> mentionsTyCon target t
  _                 -> False

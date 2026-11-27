{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecursiveDo        #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typechecking user-specified @MonoTypes@
module GHC.Tc.Gen.HsType (
        -- Type signatures
        kcClassSigType, tcClassSigType,
        tcHsSigType, tcHsSigWcType,
        tcHsPartialSigType, PendingSigCalls(..),
        tcStandaloneKindSig, tcStandaloneKindSigWithDomain,
        funsSigCtxt, addSigCtxt, pprSigCtxt,

        tcHsClsInstType,
        tcDefaultDeclClass, tcHsDeriv, tcDerivStrategy,
        tcHsTypeApp, tcHsTypeAppWithSource, tcCheckEmbeddedType, tcCheckEmbeddedTypeAtKind,
        buildPendingLargeElimHeader, buildPendingLargeElimHeaderWithArity,
        UserTypeCtxt(..),
        bindImplicitTKBndrs_Tv, bindImplicitTKBndrs_Skol,
            bindImplicitTKBndrs_Q_Tv, bindImplicitTKBndrs_Q_Skol,
        bindExplicitTKBndrs_Tv, bindExplicitTKBndrs_Skol,
            bindExplicitTKBndrs_Q_Tv, bindExplicitTKBndrs_Q_Skol,

        bindOuterFamEqnTKBndrs_Q_Tv, bindOuterFamEqnTKBndrs,
        primFamilyPatKinds,
        tcOuterTKBndrs, scopedSortOuter, outerTyVars, outerTyVarBndrs,
        tcConDeclTelescope, tcCheckRetainedConType,
        bindOuterSigTKBndrs_Tv,
        tcExplicitTKBndrs, tcRuntimeInvisForAllTelescopes,
        tcCheckRuntimeForAllMatchability,
        bindNamedWildCardBinders,

        -- Type checking type and class decls, and instances thereof
        bindTyClTyVars, bindTyClTyVarsAndZonk,
        tcFamTyPats, tcInferredMatchabilityFamPats, matchabilityArrow, tcCheckMatchabilityArrow,
        inferredMatchabilityIndex, familyKindUsesMatchability, tcCheckMatchabilitySynRhs,
        synonymCallbackIndex, tcCheckSynonymMetadata, tcCheckSynonymRhs,
        maybeEtaExpandAlgTyCon, tcbVisibilities,
        etaExpandAlgTyCon,

          -- tyvars
        zonkAndScopedSort,

        -- Kind-checking types
        -- No kind generalisation, no checkValidType
        InitialKindStrategy(..),
        SAKS_or_CUSK(..),
        ContextKind(..),
        kcDeclHeader, kcInferSumPayloadHeader, checkForDuplicateScopedTyVars,
        tcHsLiftedType,   tcHsOpenType,
        tcHsLiftedTypeNC, tcHsOpenTypeNC,
        tcInferLHsType, tcInferLHsTypeKind, tcInferLHsTypeUnsaturated,
        tcCheckLHsTypeInContext, tcCheckFamilyRhs, tcCheckInferredFamilyRhs, tcCheckDatatypeField, tcCheckLHsType,
        tcHsContext, tcLHsPredType,

        kindGeneralizeAll,

        -- Sort-checking kinds
        tcLHsKindSig, tcLHsFamilyResultKindSig, checkDataKindSig, DataSort(..),
        checkClassKindSig,

        -- Modifiers
        tcModifiersAndWarn, tcMult, tcConArrowMult, tcModifiersMult,

        -- Pattern type signatures
        tcHsPatSigType, tcHsTyPat, tcRuleBndrSig,
        HoleMode(..),

        -- Utils
        tyLitFromOverloadedLit,
        tcTypeRepTarget_maybe,
        tcRetainedAmbientIndices,
        isLocalTySynLSig, tcWithLocalTySyns,
        drainForgetMTyCons,
        withRollbackSynthTypeState,
        runPendingSynthChecks,
        pendingSynthSigmaPayloads,
        lookupPendingSynthTyCon,
        tcRegisterSynthesizedSourceFun,
        tcCommitSynthesizedSourceFunBind,
        drainSynthesizedSourceFuns,
        recordLocalSynthFunId,
        withLocalSynthFunIds,
        setLocalSimpleFunBinds,
        lookupLocalSimpleFunBind,
        tcSynthesizeMillerMotive,

   ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)

import GHC.Prelude hiding ( head, init, last, tail )

import GHC.Hs
import GHC.Rename.HsType (isInferredForEachBndr, splitForEachBndrModifiers)
import GHC.Rename.Utils
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Origin
import GHC.Tc.Types.LclEnv
import GHC.Tc.Types.ErrCtxt
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Validity
import GHC.Core.OpenWhnf (EvalFailure(Exhausted))
import GHC.Tc.Gen.OpenTypeEval (OpenTypeFailure(CannotReduce, InvalidInput, UnavailableDefinition),
                               DefinitionFailure(MissingTemplate), instantiateOpenDictMethod, instantiateOpenMethod,
                               openTypeFunctionSignature, observeOpenType,
                               openTypeFailureReason, readOpenMethodWorker)
import GHC.Tc.Gen.SourceTable (recordSourceUses, recordUnfoldedSourceUses, sourceTableFamilyUse, sourceTableTypeUses,
                              sourceCaseKindUse, sourceTableOccurrence, sourceTableReferenceUse, indexedProjectionSource, isSourceConstructorCase, monomorphicEnumCaseArity, sourceCaseVisibleArity, sourceTableOwner, sourcePairBuilderField,
                              isFinalCoreIdentity, isFinalCoreReverseWrapper, isFinalStructuralSource,
                              isLargeElimResultKind, toKindMatchableArrows,
                              lookupObservedPairField, lookupPairObserver, lookupPairRecipe, canObservePairSource,
                              lookupSourceUnaryMethod, lookupSourceBinaryMethod, sourceMapSelector, lookupSourceContainerMethod,
                              lookupSourceGroundDFun, lookupSourceCheckedMapMethod,
                              recordSourceMethodUse, recordSourceProofAliasUse,
                              takeSourceProofRulesFor, withSourceProofSignature)
import {-# SOURCE #-} GHC.Tc.Gen.ClosedEval (tcPrepareClosedApplication, tcPrepareClosedFunctionValue, tcPrepareClosedResult, tcPrepareClosedObservation, tcCheckClosedCallContext, tcPreflightClosedCall, tcDecodeClosedCallOperand, tcHasClosedCallExecutable)
import GHC.Tc.Utils.Unify
import GHC.Tc.Instance.Family (tcGetFamInstEnvs)
import GHC.Iface.Env (lookupOrig, newGlobalBinder)
import GHC.Iface.Load (loadInterfaceForName)
import GHC.IfaceToCore
import GHC.Tc.Solver
import GHC.Tc.Zonk.Type
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate ( tcInstInvisibleTyBinders, tcInstInvisibleTyBindersN,
                                  tcInstInvisibleTyBinder, tcSkolemiseInvisibleBndrs,
                                  tcInstTypeBndrs, tcInstSkolTyVarBndrsX,
                                  tcRuntimeForAllBndrs )
import GHC.Tc.Zonk.TcType

import GHC.Core.Reduction (Reduction(..), reductionCoercion, reductionReducedType)
import GHC.Core.Coercion (coercionKind, emptyLiftingContext,
                          extendLiftingContextAndInScope, hasCoercionHole, liftCoSubst,
                          mkGReflRightMCo, mkNomReflCo, mkTransCo, mkTyConAppCo,
                          tyCoVarsOfCo)
import GHC.Core.Coercion.Axiom (Branched, ClosedTyFam(CTF, CTF_Abstract, CTF_Scoped),
                                CoAxBranch, CoAxiom(..), coAxBranchRHS, coAxiomBranches,
                                fromBranches)
import GHC.Core.TyCon
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Core.FamInstEnv (familyNameInstances, famInstTyCon, lookupFamInstEnv, fim_instance,
                            mkBranchedCoAxiom, mkCoAxBranch, reduceTyFamApp_maybe)
import GHC.Tc.TyCl.Build (newImplicitBinder)
import qualified GHC.Core as Core
import GHC.Core.Dependent ( hasNativeDependentExpr )
import GHC.Core.Predicate ( getClassPredTys_maybe, isCTupleClass )
import GHC.Core.Type
import GHC.Core.TyCo.FVs (injectiveVarsOfType)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Unify ( BindFlag(..), alwaysBindTv, tcMatchTy, tcMatchTyKis,
                        tcMatchTyX_BM, tcUnifyTy, tcUnifyTys,
                        typeListsAreApart, typesAreApart )

import GHC.Builtin( allNameStrings )
import GHC.Builtin.Modules (gHC_INTERNAL_TYPENATS)
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs (typeRepIdOcc)
import GHC.Builtin.Uniques (mkAlphaTyVarUnique)
import GHC.Builtin.WiredIn.Types
import GHC.Builtin.WiredIn.Prim

import GHC.Types.Literal (litValue, mkLitIntWrap, mkLitWordWrap)
import GHC.Types.Id ( idName, isClassOpId_maybe, isRecordSelector, mkLocalId, mkSysLocalM, setInlinePragma, sourceTableId_maybe, sourceTableKeepsExecutable )
import GHC.Types.Id.Info ( IdDetails(SourceTableId) )
import GHC.Types.InlinePragma ( InlineSpec(NoInline), neverInlinePragma, setInlinePragmaSpec )
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Name.Set (NameSet, mkNameSet, unionNameSet, emptyNameSet, isEmptyNameSet, elemNameSet, extendNameSet)
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Data.Pair (pFst, pSnd)
import GHC.Types.Error (mkPlainError)
import GHC.Types.TypeEnv (TypeEnv, lookupTypeEnv)
import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM (listToUFM_Directly)
import GHC.Types.Unique.Supply

import GHC.Unit.Module (Module, isInteractiveModule, mkModule, mkModuleName, moduleName, moduleNameString, moduleUnit, stringToUnit, unitString)
import GHC.Unit.Module.ModIface (mi_hash_fn)

import GHC.Utils.Encoding (zEncodeString)
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Driver.DynFlags
import GHC.Driver.Env (hsc_plugins, hsc_units, lookupType)
import GHC.Driver.Plugins (mapPlugins, tcPlugin)
import GHC.Driver.Ppr (showPpr, showSDocForUser)
import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString
import GHC.Data.List.Infinite ( Infinite (..) )
import qualified GHC.Data.List.Infinite as Inf
import GHC.Data.List.SetOps
import GHC.Data.Maybe as Maybe
import GHC.Data.Bag( unitBag, mapBagM_ )

import Data.Function ( on )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Dynamic (fromDynamic, toDyn)
import Data.List (elemIndex, intercalate, isPrefixOf, mapAccumL, nub, sort)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep)
import Control.Monad
import Data.Data (Data, cast, gmapQ)
import Data.Tuple (swap)
import Text.Read (readMaybe)

{-
        ----------------------------
                General notes
        ----------------------------

Unlike with expressions, type-checking types both does some checking and
desugars at the same time. This is necessary because we often want to perform
equality checks on the types right away, and it would be incredibly painful
to do this on un-desugared types. Luckily, desugared types are close enough
to HsTypes to make the error messages sane.

During type-checking, we perform as little validity checking as possible.
Generally, after type-checking, you will want to do validity checking, say
with GHC.Tc.Validity.checkValidType.

Validity checking
~~~~~~~~~~~~~~~~~
Some of the validity check could in principle be done by the kind checker,
but not all:

- During desugaring, we normalise by expanding type synonyms.  Only
  after this step can we check things like type-synonym saturation
  e.g.  type T k = k Int
        type S a = a
  Then (T S) is ok, because T is saturated; (T S) expands to (S Int);
  and then S is saturated.  This is a GHC extension.

- Similarly, also a GHC extension, we look through synonyms before complaining
  about the form of a class or instance declaration

- Ambiguity checks involve functional dependencies

Also, in a mutually recursive group of types, we can't look at the TyCon until we've
finished building the loop.  So to keep things simple, we postpone most validity
checking until step (3).

%************************************************************************
%*                                                                      *
              Check types AND do validity checking
*                                                                      *
************************************************************************

Note [Keeping implicitly quantified variables in order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the user implicitly quantifies over variables (say, in a type
signature), we need to come up with some ordering on these variables.
This is done by bumping the TcLevel, bringing the tyvars into scope,
and then type-checking the thing_inside. The constraints are all
wrapped in an implication, which is then solved. Finally, we can
zonk all the binders and then order them with scopedSort.

It's critical to solve before zonking and ordering in order to uncover
any unifications. You might worry that this eager solving could cause
trouble elsewhere. I don't think it will. Because it will solve only
in an increased TcLevel, it can't unify anything that was mentioned
elsewhere. Additionally, we require that the order of implicitly
quantified variables is manifest by the scope of these variables, so
we're not going to learn more information later that will help order
these variables.

Note [Recipe for checking a signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kind-checking a user-written signature requires several steps:

 0. Bump the TcLevel
 1.   Bind any lexically-scoped type variables.
 2.   Generate constraints.
 3. Solve constraints.
 4. Sort any implicitly-bound variables into dependency order
 5. Promote tyvars and/or kind-generalize.
 6. Zonk.
 7. Check validity.

Very similar steps also apply when kind-checking a type or class
declaration.

The general pattern looks something like this.  (But NB every
specific instance varies in one way or another!)

    do { (tclvl, wanted, (spec_tkvs, ty))
              <- pushLevelAndSolveEqualitiesX "tc_top_lhs_type" $
                 bindImplicitTKBndrs_Skol sig_vars              $
                 <kind-check the type>

       ; spec_tkvs <- zonkAndScopedSort spec_tkvs

       ; reportUnsolvedEqualities skol_info spec_tkvs tclvl wanted

       ; let ty1 = mkSpecForAllTys spec_tkvs ty
       ; kvs <- kindGeneralizeAll ty1

       ; final_ty <- zonkTcTypeToType (mkInfForAllTys kvs ty1)

       ; checkValidType final_ty

This pattern is repeated many times in GHC.Tc.Gen.HsType,
GHC.Tc.Gen.Sig, and GHC.Tc.TyCl, with variations.  In more detail:

* pushLevelAndSolveEqualitiesX (Step 0, step 3) bumps the TcLevel,
  calls the thing inside to generate constraints, solves those
  constraints as much as possible, returning the residual unsolved
  constraints in 'wanted'.

* bindImplicitTKBndrs_Skol (Step 1) binds the user-specified type
  variables E.g.  when kind-checking f :: forall a. F a -> a we must
  bring 'a' into scope before kind-checking (F a -> a)

* zonkAndScopedSort (Step 4) puts those user-specified variables in
  the dependency order.  (For "implicit" variables the order is no
  user-specified.  E.g.  forall (a::k1) (b::k2). blah k1 and k2 are
  implicitly brought into scope.

* reportUnsolvedEqualities (Step 3 continued) reports any unsolved
  equalities, carefully wrapping them in an implication that binds the
  skolems.  We can't do that in pushLevelAndSolveEqualitiesX because
  that function doesn't have access to the skolems.

* kindGeneralize (Step 5). See Note [Kind generalisation]

* The final zonkTcTypeToType must happen after promoting/generalizing,
  because promoting and generalizing fill in metavariables.


Doing Step 3 (constraint solving) eagerly (rather than building an
implication constraint and solving later) is necessary for several
reasons:

* Exactly as for Solver.simplifyInfer: when generalising, we solve all
  the constraints we can so that we don't have to quantify over them
  or, since we don't quantify over constraints in kinds, float them
  and inhibit generalisation.

* Most signatures also bring implicitly quantified variables into
  scope, and solving is necessary to get these in the right order
  (Step 4) see Note [Keeping implicitly quantified variables in
  order]).

Note [Kind generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 5 of Note [Recipe for checking a signature], namely
kind-generalisation, is done by
    kindGeneraliseAll
    kindGeneraliseSome
    kindGeneraliseNone

Here, we have to deal with the fact that metatyvars generated in the
type will have a bumped TcLevel, because explicit foralls raise the
TcLevel. To avoid these variables from ever being visible in the
surrounding context, we must obey the following dictum:

  Every metavariable in a type must either be
    (A) generalized, or
    (B) promoted, or        See Note [Promotion in signatures]
    (C) a cause to error    See Note [Naughty quantification candidates]
                            in GHC.Tc.Utils.TcMType

There are three steps (look at kindGeneraliseSome):

1. candidateQTyVarsOfType finds the free variables of the type or kind,
   to generalise

2. filterConstrainedCandidates filters out candidates that appear
   in the unsolved 'wanteds', and promotes the ones that get filtered out
   thereby.

3. quantifyTyVars quantifies the remaining type variables

The kindGeneralize functions do not require pre-zonking; they zonk as they
go.

kindGeneraliseAll specialises for the case where step (2) is vacuous.
kindGeneraliseNone specialises for the case where we do no quantification,
but we must still promote.

If you are actually doing kind-generalization, you need to bump the
level before generating constraints, as we will only generalize
variables with a TcLevel higher than the ambient one.
Hence the "pushLevel" in pushLevelAndSolveEqualities.

Note [Promotion in signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If an unsolved metavariable in a signature is not generalized
(because we're not generalizing the construct -- e.g., pattern
sig -- or because the metavars are constrained -- see kindGeneralizeSome)
we need to promote to maintain (WantedInv) of Note [TcLevel invariants]
in GHC.Tc.Utils.TcType. Note that promotion is identical in effect to generalizing
and the reinstantiating with a fresh metavariable at the current level.
So in some sense, we generalize *all* variables, but then re-instantiate
some of them.

Here is an example of why we must promote:
  foo (x :: forall a. a -> Proxy b) = ...

In the pattern signature, `b` is unbound, and will thus be brought into
scope. We do not know its kind: it will be assigned kappa[2]. Note that
kappa is at TcLevel 2, because it is invented under a forall. (A priori,
the kind kappa might depend on `a`, so kappa rightly has a higher TcLevel
than the surrounding context.) This kappa cannot be solved for while checking
the pattern signature (which is not kind-generalized). When we are checking
the *body* of foo, though, we need to unify the type of x with the argument
type of bar. At this point, the ambient TcLevel is 1, and spotting a
metavariable with level 2 would violate the (WantedInv) invariant of
Note [TcLevel invariants]. So, instead of kind-generalizing,
we promote the metavariable to level 1. This is all done in kindGeneralizeNone.

-}

funsSigCtxt :: [LocatedN Name] -> UserTypeCtxt
-- Returns FunSigCtxt, with no redundant-context-reporting,
-- form a list of located names
funsSigCtxt (L _ name1 : _) = FunSigCtxt name1 NoRRC
funsSigCtxt []              = panic "funSigCtxt"

addSigCtxt :: UserTypeCtxt -> UserSigType -> TcM a -> TcM a
addSigCtxt ctxt hs_ty thing_inside
  = setSrcSpan l $
    addErrCtxt (UserSigCtxt ctxt hs_ty) $
    thing_inside
  where
    l = case hs_ty of
      UserLHsSigType ty -> getLocA ty
      UserLHsType ty    -> getLocA ty

pprSigCtxt :: Outputable hs_ty => UserTypeCtxt -> LocatedA hs_ty -> SDoc
-- (pprSigCtxt ctxt <extra> <type>)
-- prints    In the type signature for 'f':
--              f :: <type>
-- The <extra> is either empty or "the ambiguity check for"
pprSigCtxt ctxt hs_ty
  | Just n <- isSigMaybe ctxt
  = hang (text "In the type signature:")
       2 (pprPrefixOcc n <+> dcolon <+> ppr hs_ty)

  | otherwise
  = hang (text "In" <+> pprUserTypeCtxt ctxt <> colon)
       2 (ppr hs_ty)

tcHsSigWcType :: Bool -> UserTypeCtxt -> LHsSigWcType GhcRn -> TcM Type
-- This one is used when we have a LHsSigWcType, but in
-- a place where wildcards aren't allowed. The renamer has
-- already checked this, so we can simply ignore it.
-- See Note [Retained Pi type formation] in GHC.Rename.HsType.
tcHsSigWcType allow_function ctxt sig_ty =
  tc_hs_sig_type allow_function ctxt (dropWildCards sig_ty)

kcClassSigType :: [LocatedN Name] -> LHsSigType GhcRn -> TcM ()
-- This is a special form of tcClassSigType that is used during the
-- kind-checking phase to infer the kind of class variables. Cf. tc_lhs_sig_type.
-- Importantly, this does *not* kind-generalize. Consider
--   class SC f where
--     meth :: forall a (x :: f a). Proxy x -> ()
-- When instantiating Proxy with kappa, we must unify kappa := f a. But we're
-- still working out the kind of f, and thus f a will have a coercion in it.
-- Coercions may block unification (Note [Equalities with heterogeneous kinds] in
-- GHC.Tc.Solver.Equality, wrinkle (EIK2)) and so we fail to unify. If we try to
-- kind-generalize, we'll end up promoting kappa to the top level (because
-- kind-generalization is normally done right before adding a binding to the context),
-- and then we can't set kappa := f a, because a is local.
kcClassSigType names
    sig_ty@(L _ (HsSig { sig_bndrs = hs_outer_bndrs, sig_body = hs_ty }))
  = addSigCtxt (funsSigCtxt names) (UserLHsSigType sig_ty) $
    do { _ <- bindOuterSigTKBndrs_Tv hs_outer_bndrs    $
              tcCheckLHsType hs_ty liftedTypeKind
       ; return () }

tcClassSigType :: [LocatedN Name] -> LHsSigType GhcRn -> TcM Type
-- Does not do validity checking
tcClassSigType names sig_ty
  = addSigCtxt sig_ctxt (UserLHsSigType sig_ty) $
    do { skol_info <- mkSkolemInfo skol_info_anon
       ; (implic, ty) <- tc_lhs_sig_type Nothing skol_info sig_ty (TheKind liftedTypeKind)
       ; emitImplication implic
       ; return ty }
       -- Do not zonk-to-Type, nor perform a validity check
       -- We are in a knot with the class and associated types
       -- Zonking and validity checking is done by tcClassDecl
       --
       -- No need to fail here if the type has an error:
       --   If we're in the kind-checking phase, the solveEqualities
       --     in kcTyClGroup catches the error
       --   If we're in the type-checking phase, the solveEqualities
       --     in tcClassDecl1 gets it
       -- Failing fast here degrades the error message in, e.g., tcfail135:
       --   class Foo f where
       --     baa :: f a -> f
       -- If we fail fast, we're told that f has kind `k1` when we wanted `*`.
       -- It should be that f has kind `k2 -> *`, but we never get a chance
       -- to run the solver where the kind of f is touchable. This is
       -- painfully delicate.
  where
    sig_ctxt = funsSigCtxt names
    skol_info_anon = SigTypeSkol sig_ctxt

tcHsSigType :: UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
-- Does validity checking
-- See Note [Recipe for checking a signature]
tcHsSigType = tc_hs_sig_type False

-- | Check a signature with explicit permission for retained function domains.
tc_hs_sig_type :: Bool -> UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
tc_hs_sig_type allow_function ctxt sig_ty
  = addSigCtxt ctxt (UserLHsSigType sig_ty) $
    do { traceTc "tcHsSigType {" (ppr sig_ty)
       ; skol_info <- mkSkolemInfo skol_info
          -- Generalise here: see Note [Kind generalisation]
       ; let retained_sig = case ctxt of
               FunSigCtxt{} -> Just allow_function
               _ -> Nothing
       ; (implic, ty) <- tc_lhs_sig_type retained_sig skol_info sig_ty (expectedKindInCtxt ctxt)

       -- Float out constraints, failing fast if not possible
       -- See Note [Failure in local type signatures] in GHC.Tc.Solver
       ; traceTc "tcHsSigType 2" (ppr implic)
       ; simplifyAndEmitFlatConstraints (mkImplicWC (unitBag implic))

       ; ty <- liftZonkM $ zonkTcType ty
       ; checkValidType ctxt ty
       ; traceTc "end tcHsSigType }" (ppr ty)
       ; return ty }
  where
    skol_info = SigTypeSkol ctxt

tc_lhs_sig_type :: Maybe Bool -> SkolemInfo -> LHsSigType GhcRn
               -> ContextKind -> TcM (Implication, TcType)
-- Nothing selects the ordinary type checker. Just selects the retained
-- signature walker and supplies its function-domain permission.
-- Kind-checks/desugars an 'LHsSigType',
--   solve equalities,
--   and then kind-generalizes.
-- This will never emit constraints, as it uses solveEqualities internally.
-- No validity checking or zonking
-- Returns also an implication for the unsolved constraints
tc_lhs_sig_type retained_sig skol_info full_hs_ty@(L loc (HsSig { sig_bndrs = hs_outer_bndrs
                                                   , sig_body = hs_ty })) ctxt_kind
  = setSrcSpanA loc $
    do { (tc_lvl, wanted, (exp_kind, outer_ma, (outer_bndrs, ty)))
              <- pushLevelAndSolveKindArrowsX "tc_lhs_sig_type" $
                 -- See Note [Failure in local type signatures]
                 do { exp_kind <- newExpectedKind ctxt_kind
                          -- See Note [Escaping kind in type signatures]
                    ; outer_ma <- tcForAllMatchability (hsOuterMatchability hs_outer_bndrs)
                    ; let binder_mode = smVanilla
                            { sm_clone = False
                            , sm_tvtv = SMDSkolemTv skol_info
                            , sm_sig_kind_arrows = True }
                    ; stuff <- tcOuterTKBndrsX binder_mode skol_info hs_outer_bndrs $
                               (case retained_sig of
                                  Just allow_function -> tcCheckRetainedSigType allow_function skol_info
                                  Nothing | SigTypeSkol ExprSigCtxt{} <- getSkolemInfo skol_info
                                          -> tcCheckExprSigType
                                  Nothing -> tcCheckLHsType) hs_ty exp_kind
                    ; return (exp_kind, outer_ma, stuff) }

       -- Default any unconstrained variables free in the kind
       -- See Note [Escaping kind in type signatures]
       ; exp_kind_dvs <- candidateQTyVarsOfType exp_kind
       ; doNotQuantifyTyVars exp_kind_dvs (err_ctx exp_kind)

       ; traceTc "tc_lhs_sig_type" (ppr hs_outer_bndrs $$ ppr outer_bndrs)
       ; outer_bndrs <- scopedSortOuter outer_bndrs

       ; let outer_tv_bndrs = map (fromMaybe unmatchableDataConTy outer_ma,) (outerTyVarBndrs outer_bndrs)
             ty1 = mkInvisForAllTys outer_tv_bndrs ty
       ; kvs <- kindGeneralizeSome skol_info wanted ty1

       -- Build an implication for any as-yet-unsolved kind equalities
       -- See Note [Skolem escape in type signatures]
       ; implic <- buildTvImplication (getSkolemInfo skol_info) kvs tc_lvl wanted

       ; return (implic, mkInfForAllTys (map (unmatchableDataConTy,) kvs) ty1) }
  where
    err_ctx exp_kind tidy_env
      = do { (tidy_env2, exp_kind) <- zonkTidyTcType tidy_env exp_kind
           ; return (tidy_env2, UninfTyCtx_Sig exp_kind full_hs_ty) }

{- Note [Skipping no-op within-TC zonks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'zonkTcType' maps over the entire type and rebuilds every non-nullary
application in 'ZonkM', even when the type contains no mutable 'MetaTv's,
no coercions or coercion holes, and no 'MonoTcTyCon's. During dependent
typechecking, ambient runtime index types and closed kinds are inspected
repeatedly. 'canZonkChangeType' performs an allocation-free pure scan and
skips 'liftZonkM' when zonking is an exact identity.
-}

-- | Test whether 'zonkTcType' can change any node or metadata of a 'TcType'.
-- See Note [Skipping no-op within-TC zonks].
canZonkChangeType :: TcType -> Bool
canZonkChangeType = go
  where
    go (TyVarTy tv)           = (isTcTyVar tv && isMetaTyVar tv) || go (tyVarKind tv)
    go (TermVarTy v)          = go_id v
    go (TermAppTy f a)        = go_id f || go a
    go (AppTy f a)            = go f || go a
    go (TyConApp tc args)     = isMonoTcTyCon tc || any go args
    go (ForAllTy ma (Bndr b _) r) = go ma || go (varType b) || go r
    go (FunTy _ ma mult a r)  = go ma || go mult || go a || go r
    go (LitTy _)              = False
    go (CastTy _ _)           = True
    go (CoercionTy _)         = True
    go (DepFunTy _ b r)       = go_id b || go r
    go (ExistsTy _ b r)       = go (tyVarKind b) || go r
    go (DepDictTy b r)        = go_id b || go r
    go (ClassAppTy s a d x k) = go_id s || go a || go d || go x || go k

    go_id v = go (varType v) || go (idMult v)

-- | Zonk a 'TcType' only when it contains mutable metavariables, coercions,
-- or a 'MonoTcTyCon'. See Note [Skipping no-op within-TC zonks].
zonkTcTypeIfNeeded :: TcType -> TcM TcType
zonkTcTypeIfNeeded ty
  | canZonkChangeType ty = liftZonkM (zonkTcType ty)
  | otherwise            = pure ty

-- | Zonk an 'Id''s type and multiplicity only when 'canZonkChangeType' holds.
-- See Note [Skipping no-op within-TC zonks].
zonkIdIfNeeded :: TcId -> TcM TcId
zonkIdIfNeeded v
  | canZonkChangeType (varType v) || canZonkChangeType (idMult v) = liftZonkM (zonkId v)
  | otherwise = pure v

-- | Test whether a generic AST field is an ExactPrint or core leaf that
-- cannot contain renamed surface types or identifiers.
isAstLeafAnnotation :: Data a => a -> Bool
isAstLeafAnnotation thing
  | Just (_ :: SrcSpanAnnA) <- cast thing = True
  | Just (_ :: SrcSpanAnnN) <- cast thing = True
  | Just (_ :: SrcSpan)     <- cast thing = True
  | Just (_ :: Name)        <- cast thing = True
  | Just (_ :: Type)        <- cast thing = True
  | otherwise                             = False

-- | Resolve free runtime Names to their existing lexical index Ids.
-- See Note [Local runtime indices in signatures] and
-- Note [Runtime indices in visible type arguments] in GHC.Rename.HsType.
tcRetainedAmbientIndices :: RetainedQuantificationError -> LHsType GhcRn -> TcM (NameEnv Id)
tcRetainedAmbientIndices invalid_reason hs_ty
  = do { experimental <- xoptM LangExt.DependentTypes
       ; if experimental
         then case dedup_ambient emptyNameSet (ambient_names hs_ty) of
                []         -> return emptyNameEnv
                candidates -> mkNameEnv . catMaybes <$> mapM check_index candidates
         else return emptyNameEnv }
  where
    dedup_ambient _ [] = []
    dedup_ambient seen (item@(_, name) : rest)
      | name `elemNameSet` seen = dedup_ambient seen rest
      | otherwise               = item : dedup_ambient (extendNameSet seen name) rest

    check_index (span, name) = setSrcSpan span $
      do { thing <- tcLookupLcl_maybe name
         ; case thing of
             Just ATyVar{} -> return Nothing
             Just (ATcId { tct_id = index }) ->
               do { index <- zonkIdIfNeeded index
                  ; mb_rep <- tcTypeRepTarget_maybe (varType index)
                  ; if isJust mb_rep
                    then return Nothing
                    else do
                      { unless (not (isExternalName name)
                                && isManyTy (idMult index)
                                && isNativeOpaqueValueId index) invalid_index
                      ; level <- getThLevel
                      ; case level of
                          Comp -> return ()
                          _ -> failWithTc $ TcRnRetainedQuantificationError RetainedStaging
                      ; return (Just (name, index)) } }
             _ | isTyVarName name -> return Nothing
               | otherwise -> do
                   env <- getGblEnv
                   if isInteractiveStmtTerm env name
                     then return Nothing
                     else invalid_index }
    invalid_index :: TcM a
    invalid_index = failWithTc $ TcRnRetainedQuantificationError invalid_reason

    -- Keep only free runtime occurrences. Telescope binders have their own Ids.
    -- Imported source calls are handled by tc_app_ty.
    ambient_names :: Data a => a -> [(SrcSpan, Name)]
    ambient_names thing
      | isAstLeafAnnotation thing = []
      | Just (L loc (HsTyVar _ _ (L _ (WithUserRdr _ name))))
          <- (cast thing :: Maybe (LHsType GhcRn))
      , isVarName name || isTyVarName name
      , not (isExternalName name) = [(locA loc, name)]
      | Just (L _ (HsForEachTy _ bndrs _)) <- (cast thing :: Maybe (LHsType GhcRn))
      = drop_bound (mkNameSet (mapMaybe hsLTyVarName bndrs)) thing
      | Just (L _ (XHsType (HsTypeExistsRn _ bndr _))) <- (cast thing :: Maybe (LHsType GhcRn))
      = drop_bound (mkNameSet (maybeToList (hsLTyVarName bndr))) thing
      | Just (L _ (XHsType (HsTypeLambdaQuoteRn bndrs _))) <- (cast thing :: Maybe (LHsType GhcRn))
      = drop_bound (mkNameSet (mapMaybe hsLTyVarName bndrs)) thing
      | otherwise = concat (gmapQ ambient_names thing)

    drop_bound bound node
      | isEmptyNameSet bound = concat (gmapQ ambient_names node)
      | otherwise = filter (\(_, name) -> not (name `elemNameSet` bound))
                           (concat (gmapQ ambient_names node))

-- | Check an expression annotation with the runtime indices already in scope.
-- See Note [Runtime indices in expression signatures].
tcCheckExprSigType :: LHsType GhcRn -> TcKind -> TcM TcType
tcCheckExprSigType hs_ty kind = do
  indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex hs_ty
  tc_check_lhs_type (typeLevelMode { mode_indices = indices }) hs_ty kind

-- | Test whether an 'LSig' encodes a local @type@ synonym from @-XExtendedLet@.
isLocalTySynLSig :: LSig GhcRn -> Bool
isLocalTySynLSig (L _ (TypeSig _ [] [L _ nm] _)) = isTcClsNameSpace (nameNameSpace nm)
isLocalTySynLSig _ = False

-- | Kind-check topologically sorted local @type@ synonyms from a @let@ or @where@
-- block and scope them in 'TcLclEnv' over the enclosed bindings and body.
-- See Note [Local type synonyms in let and where] in GHC.Rename.Bind.
tcWithLocalTySyns :: [LSig GhcRn] -> TcM a -> TcM a
tcWithLocalTySyns [] thing_inside = thing_inside
tcWithLocalTySyns (L loc (TypeSig _ [] [L _ syn_name]
                    (HsWC { hswc_body = L _ (HsSig { sig_bndrs = HsOuterExplicit { hso_bndrs = tvbs }
                                                   , sig_body = hs_ty }) })) : rest)
                  thing_inside
  = do { entry <- setSrcSpanA loc $ check_one syn_name tvbs hs_ty
       ; updLclCtxt (tcExtendLocalTypeEnv [(syn_name, entry)]) $
           tcWithLocalTySyns rest thing_inside }
  where
    check_one name [] rhs_hs =
      do { indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex rhs_hs
         ; let mode = typeLevelMode { mode_indices = indices }
         ; (rhs_ty, rhs_kind) <- solveEqualities "tcWithLocalTySyns(0)" $
                                 tc_infer_lhs_type mode rhs_hs
         ; rhs_ty'   <- liftZonkM (zonkTcType rhs_ty)
         ; rhs_kind' <- liftZonkM (zonkTcType rhs_kind)
         ; pure (ATcTypeLet name rhs_ty' rhs_kind') }
    check_one name bndrs rhs_hs =
      do { indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex rhs_hs
         ; skol_info <- mkSkolemInfo (TyConSkol TypeSynonymFlavour name)
         ; let mode = typeLevelMode { mode_indices = indices }
               invis_flags = [ case tvb_flag b of
                                 InferredSpec  -> True
                                 SpecifiedSpec -> False
                             | L _ b <- bndrs ]
         ; (tc_bndrs0, (rhs_ty0, rhs_kind0)) <-
             solveEqualities "tcWithLocalTySyns(n)" $
             tcExplicitTKBndrs skol_info bndrs $
               tc_infer_lhs_type mode rhs_hs
         ; (tc_bndrs, rhs_ty, rhs_kind) <- liftZonkM $ do
             rhs_ty'   <- zonkTcType rhs_ty0
             rhs_kind' <- zonkTcType rhs_kind0
             let zonk_bndr (Bndr tv vis) = do
                   k' <- zonkTcType (tyVarKind tv)
                   pure (Bndr (setTyVarKind tv k') vis)
             bndrs' <- mapM zonk_bndr tc_bndrs0
             pure (bndrs', rhs_ty', rhs_kind')
         ; let mk_tc_bndrs [] [] = ([], tyCoVarsOfType rhs_kind)
               mk_tc_bndrs (Bndr tv _ : bs) (is_invis : fs) =
                 let (rest_tcbs, fvs) = mk_tc_bndrs bs fs
                     tcb | is_invis           = Bndr tv (NamedTCB Inferred)
                         | tv `elemVarSet` fvs = Bndr tv (NamedTCB Required)
                         | otherwise          = Bndr tv AnonTCB
                     fvs' = (fvs `delVarSet` tv) `unionVarSet` tyCoVarsOfType (tyVarKind tv)
                 in (tcb : rest_tcbs, fvs')
               mk_tc_bndrs (Bndr tv _ : bs) [] =
                 let (rest_tcbs, fvs) = mk_tc_bndrs bs []
                     tcb | tv `elemVarSet` fvs = Bndr tv (NamedTCB Required)
                         | otherwise          = Bndr tv AnonTCB
                     fvs' = (fvs `delVarSet` tv) `unionVarSet` tyCoVarsOfType (tyVarKind tv)
                 in (tcb : rest_tcbs, fvs')
               mk_tc_bndrs [] (_ : _) = ([], tyCoVarsOfType rhs_kind)
               -- Any extra implicit kind binders added by tcExplicitTKBndrs precede bndrs.
               extra_invis = replicate (length tc_bndrs - length invis_flags) True
               (tycon_bndrs, _) = mk_tc_bndrs tc_bndrs (extra_invis ++ invis_flags)
               full_kind = mkTyConKind tycon_bndrs rhs_kind
               roles     = map (const Nominal) tycon_bndrs
               syn_tc    = buildSynTyCon name full_kind tycon_bndrs rhs_kind roles rhs_ty
         ; pure (AGlobal (ATyCon syn_tc)) }
tcWithLocalTySyns (_ : rest) thing_inside = tcWithLocalTySyns rest thing_inside

-- | Check a `type (...)` or unadorned T2T expression RHS for a Type-valued source equation.
tcCheckEmbeddedType :: LHsWcType GhcRn -> TcM Type
tcCheckEmbeddedType wc = tcCheckEmbeddedTypeAtKind wc liftedTypeKind

-- | Check an embedded type expression at an expected large-elimination result kind
-- (such as `Type` or `Type -> Type`).
tcCheckEmbeddedTypeAtKind :: LHsWcType GhcRn -> Kind -> TcM Type
tcCheckEmbeddedTypeAtKind (HsWC { hswc_ext = sig_wcs, hswc_body = hs_ty0 }) exp_res_kind
  = do { unless (null sig_wcs && hsTypeHasNoAnonWildcards hs_ty0) $
           failWithTc (TcRnRetainedQuantificationError
             (RetainedTypeOperandContext (Just exp_res_kind)))
       ; mb_enclosing_fun <- enclosing_fun_name
       ; hs_ty <- promote_embedded_source_heads mb_enclosing_fun hs_ty0
       ; carry_binds <- local_type_carry_binders hs_ty
       ; tcExtendNameTyVarEnv [(nm, tv) | (nm, tv) <- carry_binds] $ do
           { indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex hs_ty
           ; pending <- pending_source_families mb_enclosing_fun hs_ty
           ; ty <- tcExtendRecEnv pending $
                   solveEqualities "tcCheckEmbeddedType" $
                   tc_check_lhs_type (typeLevelMode { mode_indices = indices })
                                     hs_ty exp_res_kind
           ; zonked <- liftZonkM (zonkTcType ty)
           ; pure (mkInfForAllTys [(unmatchableDataConTy, tv) | (_, tv) <- carry_binds] zonked) } }
  where
    enclosing_fun_name :: TcM (Maybe Name)
    enclosing_fun_name = do
      lcl <- getLclEnv
      let ctxts = getLclEnvErrCtxt lcl
          from_ctxt =
            [ nm
            | c <- ctxts
            , Just nm <- [case c of
                UserSigCtxt (FunSigCtxt nm _) _        -> Just nm
                MatchCtxt (FunRhs { mc_fun = L _ nm }) -> Just nm
                _                                      -> Nothing] ]
          from_not_top =
            [ idName b
            | TcIdBndr b NotTopLevel <- reverse (getLclEnvBinderStack lcl) ]
          from_top =
            [ bndr_nm
            | bndr <- getLclEnvBinderStack lcl
            , Just bndr_nm <- [case bndr of
                TcIdBndr b TopLevel            -> Just (idName b)
                TcIdBndr_ExpType nm _ TopLevel -> Just nm
                _                              -> Nothing] ]
      pure (listToMaybe (from_ctxt ++ from_not_top ++ from_top))

    count_arrows :: Type -> Int
    count_arrows t =
      let (_, tau) = splitForAllForAllTyBinders t
          go ty | Just (FTF_T_T, _, _, _, rest) <- splitFunTy_maybe ty = 1 + go rest
                | otherwise = 0
      in go tau

    arity_for :: Maybe Name -> Name -> Type -> Maybe Int
    arity_for (Just enc_nm) fn fun_ty
      | fn == enc_nm || nameOccName fn == nameOccName enc_nm
      , let k = count_arrows fun_ty - count_arrows exp_res_kind
      , k > 0 = Just k
    arity_for _ _ _ = Nothing

    promote_embedded_source_heads :: Maybe Name -> LHsType GhcRn -> TcM (LHsType GhcRn)
    promote_embedded_source_heads mb_enc root = do
      gbl <- getGblEnv
      lcl <- getLclEnv
      let cur_mod   = tcg_semantic_mod gbl
          lcl_bndrs = nonDetNameEnvElts (tcl_env (tcl_lcl_ctxt lcl))
          local_type_fun nm = listToMaybe
            [ fun_id
            | ATcId { tct_id = fun_id } <- lcl_bndrs
            , idName fun_id == nm
            , isExternalName nm ]
          go_ty :: LHsType GhcRn -> TcM (LHsType GhcRn)
          go_ty (L l t) = L l <$> case t of
            HsParTy ann inner -> HsParTy ann <$> go_ty inner
            HsAppTy x f a -> HsAppTy x <$> go_ty f <*> go_ty a
            HsAppKindTy x f a -> HsAppKindTy x <$> go_ty f <*> go_ty a
            HsFunTy x m a r -> HsFunTy x m <$> go_ty a <*> go_ty r
            HsOpTy x a op b -> HsOpTy x <$> go_ty a <*> go_ty op <*> go_ty b
            HsListTy ann e -> HsListTy ann <$> go_ty e
            HsTupleTy ann s ts -> HsTupleTy ann s <$> traverse go_ty ts
            HsKindSig ann e k -> HsKindSig ann <$> go_ty e <*> go_ty k
            HsForAllTy x tele body -> HsForAllTy x tele <$> go_ty body
            HsQualTy x (L lc (HsContext an cs)) body ->
              HsQualTy x <$> (L lc . HsContext an <$> traverse go_ty cs) <*> go_ty body
            XHsType (HsTypeLambdaQuoteRn binders body) ->
              XHsType . HsTypeLambdaQuoteRn binders <$> go_ty body
            XHsType (HsTypeLambdaCheckRn binders body helper) ->
              XHsType <$> (HsTypeLambdaCheckRn binders <$> go_ty body <*> go_ty helper)
            HsTyVar ann NotPromoted (L vloc (WithUserRdr rdr nm))
              | isVarName nm, isExternalName nm -> do
                  mb_tbl_nm <- resolve_source_head mb_enc cur_mod gbl local_type_fun nm
                  pure $ HsTyVar ann NotPromoted (L vloc (WithUserRdr rdr (fromMaybe nm mb_tbl_nm)))
            other -> pure other
      go_ty root

    resolve_source_head mb_enc cur_mod gbl local_type_fun nm = do
      mb_id <- case local_type_fun nm of
        Just fid -> pure (Just fid)
        Nothing  -> tcLookupIdMaybe nm
      case mb_id >>= sourceTableId_maybe of
        Just tbl -> pure (Just (tyConName tbl))
        Nothing
          | nameModule_maybe nm == Just cur_mod -> do
              fam_nm <- lookupOrig cur_mod (mkSourceTableOcc (nameOccName nm))
              if isJust (lookupTypeEnv (tcg_type_env gbl) fam_nm)
                then pure (Just fam_nm)
                else case mb_id of
                  Just fid -> do
                    fun_ty <- liftZonkM (zonkTcType (varType fid))
                    mb_hdr <- buildPendingLargeElimHeaderWithArity (arity_for mb_enc nm fun_ty) fam_nm fun_ty
                    pure (fam_nm <$ mb_hdr)
                  Nothing -> pure Nothing
          | otherwise -> pure Nothing

    local_type_carry_binders :: LHsType GhcRn -> TcM [(Name, TyVar)]
    local_type_carry_binders root = do
      let free_local = uniq_names (local_var_names root)
      fmap catMaybes $ forM free_local $ \nm -> do
        mb_thing <- tcLookupLcl_maybe nm
        case mb_thing of
          Just (ATcId { tct_id = v }) | not (isExternalName nm) -> do
            v' <- zonkIdIfNeeded v
            vt <- liftZonkM (zonkTcType (varType v'))
            if isManyTy (idMult v') && is_carry_type_kind vt
              then do
                let carry_occ = mkTyVarOcc ("$emb_carry:" ++ occNameString (nameOccName (idName v')))
                tv_nm <- newSysName carry_occ
                pure (Just (nm, mkTyVar tv_nm vt))
              else pure Nothing
          _ -> pure Nothing

    is_carry_type_kind :: Type -> Bool
    is_carry_type_kind t
      | isLargeElimResultKind t = True
      | Just (FTF_T_T, ma, mult, dom, res) <- splitFunTy_maybe t
      = ma `eqType` unmatchableDataConTy && isManyTy mult &&
        tcIsLiftedTypeKind (typeKind dom) && is_carry_type_kind res
      | otherwise = False

    local_var_names :: Data a => a -> [Name]
    local_var_names thing
      | isAstLeafAnnotation thing = []
      | Just (L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))))
          <- (cast thing :: Maybe (LHsType GhcRn))
      , not (isExternalName name) = [name]
      | otherwise = concat (gmapQ local_var_names thing)

    pending_source_families :: Maybe Name -> LHsType GhcRn -> TcM [(Name, TyThing)]
    pending_source_families mb_enc root = do
      gbl <- getGblEnv
      lcl <- getLclEnv
      let lcl_bndrs = nonDetNameEnvElts (tcl_env (tcl_lcl_ctxt lcl))
          find_local_fun fam_name = do
            term_occ <- sourceTableTermOcc (nameOccName fam_name)
            listToMaybe
              [ fun_id
              | ATcId { tct_id = fun_id } <- lcl_bndrs
              , let fn = idName fun_id
              , isExternalName fn
              , nameModule_maybe fn == nameModule_maybe fam_name
              , nameOccName fn == term_occ ]
          make_pending fam_name
            | isJust (lookupTypeEnv (tcg_type_env gbl) fam_name) = pure Nothing
            | Just fun_id <- find_local_fun fam_name = do
                fun_ty <- liftZonkM (zonkTcType (varType fun_id))
                let mb_arity = arity_for mb_enc (idName fun_id) fun_ty
                mb_tc <- fmap fst <$> buildPendingLargeElimHeaderWithArity mb_arity fam_name fun_ty
                pure ((\tc -> (fam_name, ATyCon tc)) <$> mb_tc)
            | otherwise = pure Nothing
      catMaybes <$> mapM make_pending (uniq_names (source_family_names root))

    uniq_names :: [Name] -> [Name]
    uniq_names = go emptyNameSet
      where
        go _ [] = []
        go seen (n : ns)
          | n `elemNameSet` seen = go seen ns
          | otherwise = n : go (extendNameSet seen n) ns

    source_family_names :: Data a => a -> [Name]
    source_family_names thing
      | isAstLeafAnnotation thing = []
      | Just (L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))))
          <- (cast thing :: Maybe (LHsType GhcRn))
      , isSourceFamilyName name = [name]
      | otherwise = concat (gmapQ source_family_names thing)

-- | Build an abstract placeholder family and TcTyCon from a Type-valued signature.
-- See Note [Staging mutually recursive large eliminations] in GHC.Tc.Module.
buildPendingLargeElimHeader :: Name -> Type -> TcM (Maybe (TyCon, TcTyCon))
buildPendingLargeElimHeader = buildPendingLargeElimHeaderWithArity Nothing

-- | Like 'buildPendingLargeElimHeader', optionally fixing the syntactic clause arity
-- so higher-kinded results (`List Eff -> Type -> Type`, arity 1) and Type-valued
-- accumulators (`List Eff -> Type -> Type`, arity 2) build distinct family headers.
buildPendingLargeElimHeaderWithArity :: Maybe Int -> Name -> Type -> TcM (Maybe (TyCon, TcTyCon))
buildPendingLargeElimHeaderWithArity mb_arity fam_name fun_ty = runMaybeT $ do
  let (tele, tau) = splitForAllForAllTyBinders fun_ty
  tvs <- MaybeT $ pure $ traverse (\(ma, Bndr tv vis) -> do
           guard (ma `eqType` unmatchableDataConTy
                  && isSpecifiedForAllTyFlag vis
                  && isTyVar tv
                  && tcIsLiftedTypeKind (typeKind (tyVarKind tv)))
           pure tv) tele
  let peel_n 0 t
        | isLargeElimResultKind t = pure ([], toKindMatchableArrows t)
        | otherwise = Nothing
      peel_n n t
        | n > 0
        , Just (FTF_T_T, ma, mult, arg, rest) <- splitFunTy_maybe t
        , ma `eqType` unmatchableDataConTy && isManyTy mult
        = do { (args, res) <- peel_n (n - 1) rest; pure (arg : args, res) }
        | otherwise = Nothing
      collect_arrows t
        | Just (FTF_T_T, ma, mult, arg, rest) <- splitFunTy_maybe t
        , ma `eqType` unmatchableDataConTy && isManyTy mult
        = do { (args, res) <- collect_arrows rest; pure (arg : args, res) }
        | tcIsLiftedTypeKind t = pure ([], t)
        | otherwise = Nothing
  (domains, res_kind) <- MaybeT $ pure $ case mb_arity of
    Just k  -> peel_n k tau
    Nothing -> collect_arrows tau
  guard (not (null domains))
  arg_tvs <- lift $ freshAnonArgTyVars domains
  abstract_tc <- lift $ mkSynthClosedFamily fam_name tvs arg_tvs res_kind SynthAbstractFam
  let tc_fam = mkTcTyCon fam_name (tyConKind abstract_tc) (tyConBinders abstract_tc) 0 res_kind
                 noTcTyConScopedTyVars False ClosedTypeFamilyFlavour
  pure (abstract_tc, tc_fam)

{- Note [Runtime indices in expression signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Inside a function with a retained argument n, a proof can have the
expression signature (plusZero n :: plus n 'Zero :~: n). Use the same
lexical Id for n in the proof and the annotation. The signature checker
validates the Id in the current context, as for a visible type argument.
Use the ordinary type checker with those indices in scope. The annotation
does not introduce a retained binder or evaluate its operand.
-}

-- | Check retained telescopes in function positions of a complete signature.
-- See Note [Retained Pi type formation] in GHC.Rename.HsType.
tcCheckRetainedSigType :: Bool -> SkolemInfo -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckRetainedSigType allow_function skol_info hs_ty kind
  = do { indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex hs_ty
       ; let proof_owner = case getSkolemInfo skol_info of
               SigTypeSkol (FunSigCtxt name _) | isExternalName name -> Just name
               _ -> Nothing
       ; tcCheckRetainedType allow_function (Just skol_info)
           (typeLevelMode { mode_indices = indices, mode_source_proof_owner = proof_owner }) hs_ty kind }

-- | Check a retained type without changing the caller's index environment.
-- See Note [Retained Pi type formation] in GHC.Rename.HsType.
tcCheckRetainedType :: Bool -> Maybe SkolemInfo -> TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckRetainedType allow_function witness_skol = go
  where
    go :: TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
    go mode (L _ (HsParTy _ body)) kind = go mode body kind
    go mode (L _ (HsSpliceTy (HsUntypedSpliceTop finalizers body) _)) kind =
      do { addModFinalizersWithLclEnv finalizers
         ; go mode body kind }
    go mode (L loc (HsForAllTy { hst_tele = tele@HsForAllInvis{}, hst_body = body })) kind
      | not (isEmptyNameEnv (mode_indices mode)) || hsHasForEachTy (unLoc body)
      = setSrcSpanA loc $
        tcExtendIdEnv (nonDetNameEnvElts (mode_indices mode)) $
        do { (binders, body_ty) <- tcTKTelescope mode tele (go (mode { mode_under_forall = True }) body kind)
           ; return (mkForAllTys binders body_ty) }
    go mode (L loc ty@(HsQualTy _ ctxt body)) kind
      | hsHasForEachTy (unLoc body)
      = setSrcSpanA loc $
        do { theta <- tc_hs_context mode ctxt
           ; dictionaries <- if allow_function
                              then newEvVars theta
                              else pure []
           ; let body_mode = (sourceMethodContext mode theta)
                             { mode_dictionaries = dictionaries ++ mode_dictionaries mode }
           ; body_ty <- go body_mode body liftedTypeKind
           ; let sigma
                   | null dictionaries = tcMkPhiTy theta body_ty
                   | otherwise = foldr bind_dictionary body_ty dictionaries
           ; checkExpKind mode ty sigma liftedTypeKind (Check kind) }
    go mode (L loc (HsFunTy _ mult arg res)) kind
      = setSrcSpanA loc $
        tc_fun_type_x (go mode) (pure unmatchableDataConTy) mode mult arg res (Check kind)
    go mode (L loc ty@(HsForAllTy { hst_tele = tele@HsForAllVis{}
                                 , hst_body = body })) kind
      | allow_function
      , not (isEmptyNameEnv (mode_indices mode)) || hsHasForEachTy (unLoc body)
      = setSrcSpanA loc $
        tcExtendIdEnv (nonDetNameEnvElts (mode_indices mode)) $
        tc_vis_forall_type mode ty tele (go (mode { mode_under_forall = True }) body) (Check kind)
    go mode (L loc ty@(HsForEachTy ann bndrs body)) kind
      = setSrcSpanA loc $
        do { level <- getThLevel
           ; case level of
               Comp -> return ()
               _ -> failWithTc $ TcRnRetainedQuantificationError RetainedStaging
           ; when (null bndrs) $
               failWithTc $ TcRnRetainedQuantificationError RetainedBinderCount
           ; body_ty <- check_bndrs mode bndrs
           ; checkExpKind mode ty body_ty liftedTypeKind (Check kind) }
      where
        check_bndrs :: TcTyMode -> [LHsTyVarBndr () GhcRn] -> TcM TcType
        check_bndrs mode [] = go mode body liftedTypeKind
        check_bndrs mode (L lb bndr : rest)
          = setSrcSpanA lb $
            let inferred = not visible && isInferredForEachBndr bndr
                mark_id  = if inferred then markInferredRetainedBinder else id
                (bndr_mods, raw_kind) = splitForEachBndrModifiers bndr
                tc_bndr_mult = case bndr_mods of
                  [] -> pure ManyTy
                  _  -> do { mb_mult <- tc_modifiers_mult mode bndr_mods
                           ; maybe (pure ManyTy) (liftZonkM . zonkTcType) mb_mult }
                check_not_fun domain_ty =
                  when (not allow_function && isFunTy domain_ty) $
                    failWithTc $ TcRnRetainedQuantificationError $
                      if mode_complete_type_arg mode == Just False
                      then RetainedPartialTypeArgument
                      else RetainedFunctionSignature
                mk_type_witness bndr_mult skol chosen_name body_ty =
                  do { tv_name <- newSysName (mkTyVarOccFS (occNameFS (nameOccName chosen_name)))
                     ; tv <- newSkolemTyVar skol tv_name liftedTypeKind
                     ; selector <- tcLookupKnownOccId typeRepIdOcc
                     ; let (_, rep_ty) = tcSplitPhiTy $
                             piResultTys (varType selector) [liftedTypeKind, mkTyVarTy tv]
                     ; body_ty' <- body_ty tv
                     ; let fun_ty | visible = tcMkVisFunTy unmatchableDataConTy bndr_mult rep_ty body_ty'
                                  | otherwise = mkDepFunTyWithVis DepFunInvisible
                                                  (mark_id (mkLocalId chosen_name bndr_mult rep_ty)) body_ty'
                     ; return (mkInfForAllTy unmatchableDataConTy tv fun_ty) }
            in case (tvb_var bndr, raw_kind) of
              (HsBndrVar _ (L _ name), HsBndrKind _ domain) ->
                do { bndr_mult <- tc_bndr_mult
                   ; domain_ty <- tcRetainedBinderDomain valid_domain mode domain
                   ; case witness_skol of
                       Just skol | isLiftedTypeKind domain_ty ->
                         -- See Note [Retained Pi type formation] in GHC.Rename.HsType.
                         mk_type_witness bndr_mult skol name $ \tv ->
                           tcExtendNameTyVarEnv [(name, tv)] (check_bndrs mode rest)
                       _ ->
                         do { check_not_fun domain_ty
                            ; let index = mark_id (mkLocalId name bndr_mult domain_ty)
                                  mode' = mode { mode_indices = extendNameEnv (mode_indices mode) name index }
                            ; body_ty <- tcExtendTermScopeBinders [index] (check_bndrs mode' rest)
                            ; return (mkDepFunTyWithVis (hsForEachVisibility ann) index body_ty) } }
              (HsBndrWildCard _, HsBndrKind _ domain)
                | visible ->
                    do { bndr_mult <- tc_bndr_mult
                       ; domain_ty <- tc_check_lhs_type mode domain liftedTypeKind >>= zonkTcTypeIfNeeded
                       ; case witness_skol of
                           Just skol | isLiftedTypeKind domain_ty ->
                             do { chosen <- newSysName (mkVarOcc "_")
                                ; mk_type_witness bndr_mult skol chosen $ \_ -> check_bndrs mode rest }
                           _ ->
                             do { body_ty <- check_bndrs mode rest
                                ; return (tcMkVisFunTy unmatchableDataConTy bndr_mult domain_ty body_ty) } }
                | otherwise ->
                    do { bndr_mult <- tc_bndr_mult
                       ; domain_ty <- tcRetainedBinderDomain valid_domain mode domain
                       ; chosen <- newSysName (mkVarOcc "_")
                       ; case witness_skol of
                           Just skol | isLiftedTypeKind domain_ty ->
                             mk_type_witness bndr_mult skol chosen $ \_ -> check_bndrs mode rest
                           _ ->
                             do { check_not_fun domain_ty
                                ; let index = mark_id (mkLocalId chosen bndr_mult domain_ty)
                                ; body_ty <- tcExtendTermScopeBinders [index] (check_bndrs mode rest)
                                ; return (mkDepFunTyWithVis DepFunInvisible index body_ty) } }
              (HsBndrVar _ (L _ name), HsBndrNoKind _) ->
                do { bndr_mult <- tc_bndr_mult
                   ; domain_meta <- newFlexiTyVarTy liftedTypeKind
                   ; let temp_index = mark_id (mkLocalId name bndr_mult domain_meta)
                         mode' = mode { mode_indices = extendNameEnv (mode_indices mode) name temp_index }
                   ; raw_body_ty <- tcExtendTermScopeBinders [temp_index] (check_bndrs mode' rest)
                   ; domain_ty0 <- liftZonkM (zonkTcType domain_meta)
                   ; domain_ty1 <- default_unannotated_domain domain_ty0
                   ; domain_ty <- if valid_domain domain_ty1
                                  then pure domain_ty1
                                  else do { ty <- inferRetainedCallbackKinds
                                                    (L lb (HsTyVar noAnn NotPromoted (noLocA (noUserRdr name)))) domain_ty1
                                          ; unless (valid_domain ty) $
                                              failWithTc $ TcRnRetainedQuantificationError
                                                (RetainedUnsupportedDomain ty)
                                          ; pure ty }
                   ; body_ty0 <- liftZonkM (zonkTcType raw_body_ty)
                   ; bndr_mult' <- liftZonkM (zonkTcType bndr_mult)
                   ; case witness_skol of
                       Just skol | isLiftedTypeKind domain_ty ->
                         mk_type_witness bndr_mult' skol name $ \tv ->
                           pure (replaceTermVarInType temp_index (mkTyVarTy tv) body_ty0)
                       _ ->
                         do { check_not_fun domain_ty
                            ; let index = mark_id (mkLocalId name bndr_mult' domain_ty)
                                  body_ty = replaceTermVarInType temp_index (TermVarTy index) body_ty0
                            ; return (mkDepFunTyWithVis (hsForEachVisibility ann) index body_ty) } }
              (HsBndrWildCard _, HsBndrNoKind _) ->
                do { bndr_mult <- tc_bndr_mult
                   ; case witness_skol of
                       Just skol ->
                         do { chosen <- newSysName (mkVarOcc "_")
                            ; mk_type_witness bndr_mult skol chosen $ \_ -> check_bndrs mode rest }
                       Nothing ->
                         failWithTc $ TcRnRetainedQuantificationError (RetainedUnsupportedDomain liftedTypeKind) }

        default_unannotated_domain :: TcType -> TcM TcType
        default_unannotated_domain domain_ty0
          | Just tv <- getTyVar_maybe domain_ty0
          , isTcTyVar tv && isMetaTyVar tv
          = do { liftZonkM (writeMetaTyVar tv liftedTypeKind); pure liftedTypeKind }
          | Just (tc, [arg]) <- tcSplitTyConApp_maybe domain_ty0
          , tc == tYPETyCon
          , Just r_tv <- getTyVar_maybe arg
          , isTcTyVar r_tv && isMetaTyVar r_tv
          = do { liftZonkM (writeMetaTyVar r_tv liftedRepTy); pure liftedTypeKind }
          | otherwise = pure domain_ty0

        visible = hsForEachVisibility ann == DepFunVisible
        valid_domain ty = isNativeRetainedBinderType ty
                       || (isJust witness_skol && isLiftedTypeKind ty)
    go mode body kind = tc_check_lhs_type mode body kind

    bind_dictionary dict body
      | isNativeClassDictionaryId dict, dict `elemVarSet` tyCoVarsOfType body
      = DepDictTy dict body
      | otherwise = tcMkPhiTy [varType dict] body

{- Note [Local runtime indices in signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A complete local value signature can mention an already-bound runtime index
(GHC Proposal #378, Eisenberg, Kiss, Pickering, Gundry, and McBride, 2020),
for example @xs :: Vec Int n@ under a @do@-bound @n@. The binding renamer adds
a signature-local type lookup alias for the term 'Name'. Find its 'ATcId' in
the local type environment, zonk its type and multiplicity without changing its
'Unique', and require a local unrestricted 'Id' of a supported closed index
domain (or an ordinary function type at a call head; see Note [Source opaque
call indices]).

Seed 'mode_indices' only with runtime 'Name's referenced by this signature,
excluding the signature's own @foreach@ binders. A @let@ alias supplies no
definitional equality between distinct runtime indices.
-}

-- | Check the field suffix of a retained GADT constructor signature.
-- The Boolean selects final domain validation. Kind inference checks only
-- domain kinds because the datatype constructor knots are not yet available.
-- Return explicit metadata too: mkDepFunTy can erase a vacuous binder.
tcCheckRetainedConType :: Bool
                      -> Bool -- ^ Permit retained fields to determine the result indices
                      -> (HsModifiedFunArr GhcRn -> TcM Mult)
                      -> LHsType GhcRn -> TcM ([Maybe Id], TcType)
tcCheckRetainedConType validate_domain allow_result tc_field_mult
  = go allow_result typeLevelMode
  where
    valid_field_domain ty = not validate_domain || Core.isNativeRetainedGadtFieldType ty
    go allow mode (L _ (HsParTy _ body)) = go allow mode body
    go allow mode (L loc (HsForEachTy ann bndrs body))
      = setSrcSpanA loc $
        do { when (null bndrs) $
               failWithTc $ TcRnRetainedQuantificationError RetainedBinderCount
           ; let vis = hsForEachVisibility ann
           ; bind_indices allow vis mode bndrs body }
    go allow mode (L loc (HsFunTy _ mult arg res))
      = setSrcSpanA loc $
        do { arg_ty <- tc_check_lhs_type mode arg liftedTypeKind
           ; mult_ty <- tc_field_mult mult
           ; let independent = not (anyFreeVarsOfType isId mult_ty)
           ; (binders, body_ty) <- go (allow && independent) mode res
           ; return (Nothing : binders, tcMkVisFunTy unmatchableDataConTy mult_ty arg_ty body_ty) }
    go _ _ (L loc HsForAllTy{}) = unsupported loc
    go _ _ (L loc HsQualTy{})   = unsupported loc
    go allow mode body@(L loc _)
      = setSrcSpanA loc $
        do { res_ty <- tc_check_lhs_type mode body liftedTypeKind
           ; when (anyFreeVarsOfType isId res_ty
                   && (validate_domain || not (hasCoercionHole res_ty))) $
               do { checked <- if validate_domain
                               then liftZonkM $ zonkTcType res_ty
                               else pure res_ty
                  ; unless (allow && retained_result mode checked) $
                      failWithTc $ TcRnRetainedQuantificationError (RetainedConstructorResult checked) }
           ; return ([], res_ty) }

    retained_result mode ty
      | Just (_, args) <- tcSplitTyConApp_maybe ty
      , Just fields <- traverse result_fields (filter (isNothing . getTyVar_maybe) args)
      , let binders = nonDetNameEnvElts (mode_indices mode)
            field_set = mkVarSet (concat fields)
      = sizeVarSet field_set == length binders
        && field_set `subVarSet` mkVarSet binders
      | otherwise = False

    result_fields ty
      | Just fields <- retainedResultFields_maybe ty
      , not validate_domain ||
          (all (valid_result_field_domain . varType) fields && validRetainedResultKind ty)
      = Just fields
      | otherwise = Nothing
      where
        valid_result_field_domain fty
          = isNativeClosedIndexType fty || Core.isNativeIndexedGadtFieldDomain fty

    bind_indices allow _ mode [] body = go allow mode body
    bind_indices allow vis mode (L loc bndr : rest) body
      = setSrcSpanA loc $
        let mark_bndr
              | isInvisibleDepFunFlag vis
              = (if isInferredForEachBndr bndr then markInferredRetainedBinder else id)
                . markInvisibleRetainedBinder
              | otherwise = id
        in case bndr of
          HsTvb _ () (HsBndrVar _ (L _ name)) (HsBndrKind _ domain) ->
            do { domain_ty <- tcRetainedBinderDomain valid_field_domain mode domain
               ; let index = mark_bndr (mkLocalId name ManyTy domain_ty)
                     mode' = mode { mode_indices = extendNameEnv (mode_indices mode) name index }
                     vis'  | isInvisibleDepFunFlag vis, isInferredForEachBndr bndr = DepFunInferred
                           | otherwise = vis
               ; (binders, body_ty) <- tcExtendTermScopeBinders [index] $
                                         bind_indices allow vis mode' rest body
               ; return (Just index : binders, mkDepFunTyWithVis vis' index body_ty) }
          HsTvb _ () (HsBndrWildCard _) (HsBndrKind _ domain)
            | isInvisibleDepFunFlag vis ->
              do { domain_ty <- tcRetainedBinderDomain valid_field_domain mode domain
                 ; sys_name <- newSysName (mkVarOcc "_")
                 ; let index = mark_bndr (mkLocalId sys_name ManyTy domain_ty)
                       vis'  | isInferredForEachBndr bndr = DepFunInferred
                             | otherwise                 = DepFunInvisible
                 ; (binders, body_ty) <- tcExtendTermScopeBinders [index] $
                                           bind_indices allow vis mode rest body
                 ; return (Just index : binders, mkDepFunTyWithVis vis' index body_ty) }
            | otherwise ->
              do { domain_ty <- tc_check_lhs_type mode domain liftedTypeKind
                 ; (binders, body_ty) <- bind_indices allow vis mode rest body
                 ; return (Nothing : binders, tcMkVisFunTy unmatchableDataConTy ManyTy domain_ty body_ty) }
          HsTvb _ () (HsBndrVar _ (L _ name)) (HsBndrNoKind _) ->
            do { domain_meta <- newFlexiTyVarTy liftedTypeKind
               ; let temp_index = mark_bndr (mkLocalId name ManyTy domain_meta)
                     mode' = mode { mode_indices = extendNameEnv (mode_indices mode) name temp_index }
               ; (binders, raw_body_ty) <- tcExtendTermScopeBinders [temp_index] $
                                             bind_indices allow vis mode' rest body
               ; domain_ty <- liftZonkM (zonkTcType domain_meta)
               ; unless (valid_field_domain domain_ty) $
                   failWithTc $ TcRnRetainedQuantificationError (RetainedUnsupportedDomain domain_ty)
               ; body_ty0 <- liftZonkM (zonkTcType raw_body_ty)
               ; let index = mark_bndr (mkLocalId name ManyTy domain_ty)
                     body_ty = replaceTermVarInType temp_index (TermVarTy index) body_ty0
               ; return (Just index : binders, mkDepFunTyWithVis vis index body_ty) }
          _ -> failWithTc $ TcRnRetainedQuantificationError RetainedBinderCount

    unsupported loc = setSrcSpanA loc $
      failWithTc $ TcRnRetainedQuantificationError RetainedPackageDeclaration

-- | Check the domain of a retained binder before bringing it into scope.
-- Constructor kind inference skips the domain predicate until the datatype
-- constructor knots are available.
tcRetainedBinderDomain :: (Type -> Bool) -> TcTyMode -> LHsType GhcRn -> TcM TcType
tcRetainedBinderDomain valid_domain mode domain
  = do { domain_ty <- tc_check_lhs_type mode domain liftedTypeKind
       ; domain_ty <- liftZonkM $ zonkTcType domain_ty
       ; if valid_domain domain_ty then return domain_ty else
         do { ty <- inferRetainedCallbackKinds domain domain_ty
            ; unless (valid_domain ty) $
                failWithTc $ TcRnRetainedQuantificationError (RetainedUnsupportedDomain ty)
            ; return ty } }

-- | Infer the lifted endpoint kinds required by one retained arrow.
-- See Note [Inferring the endpoint kinds of a retained callback].
inferRetainedCallbackKinds :: LHsType GhcRn -> TcType -> TcM TcType
inferRetainedCallbackKinds source ty
  | FunTy { ft_arg = arg, ft_res = res } <- coreFullView ty
  , let is_flex ep =
          let ki = typeKind ep
          in isMetaTyVarTy ki || maybe False isMetaTyVarTy (kindRep_maybe ki)
        flexible = filter is_flex [arg, res]
  , not (null flexible)
  = do { forM_ flexible $ \endpoint ->
           discardResult $ unifyKind (Just (HsTypeRnThing (unLoc source)))
                                     (typeKind endpoint) liftedTypeKind
       ; liftZonkM $ zonkTcType ty }
  | otherwise = return ty

{- Note [Inferring the endpoint kinds of a retained callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In forall a b. foreach (f :: a -> b) -> ..., the ordinary arrow checker
infers a :: TYPE r1 and b :: TYPE r2. Signature generalisation has not
yet defaulted the representation variables. To check f x in a later
type, the source checker needs the callback's argument and result kinds
now. Unify each flexible representation with the required lifted kind
and check the complete domain. An explicit representation variable is
rigid and keeps its declared kind.

For f :: a -> b -> c, checking the binder fixes the kind of a. The
result b -> c is already lifted. Check the kinds of b and c when a
second argument is supplied in a type. A partial application f x
can return an ordinary function with a representation-polymorphic domain.
-}

{- Note [Source retained signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained @foreach@ quantifier (GHC Proposal #378) binds a runtime 'Id' with
a 'VarName' and installs a type-variable lookup alias to that 'Name'. The
signature checker validates each domain before extending 'mode_indices' for
subsequent binders and the body; every runtime binder has multiplicity 'Many'
(see Weirich, Voizard, Amorim, and Eisenberg, ICFP 2017, for the underlying
System DC kernel).

In a complete value signature, @foreach (a :: Type)@ elaborates to an erased
@forall {a :: Type}@ paired with an explicit or 'Typeable'-supplied @TypeRep a@
witness, preserving phase separation between erased types and runtime evidence
(GHC Proposal #378; see Note [Instantiating a retained Type witness] in
"GHC.Tc.Utils.Instantiate").

Complete named signatures also permit function-valued retained binders
@foreach (f :: A -> B) -> ...@ and nested retained telescopes inside 'HsQualTy',
'HsFunTy', and 'HsForAllVis' arrows. In 'tcHsTypeApp', 'mode_complete_type_arg'
records whether the explicit type argument is wildcard-free so complete visible
type arguments can form retained function types (GHC Proposal #281, Eisenberg,
2020; GHC Proposal #378).
-}

{- Note [Escaping kind in type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider kind-checking the signature for `foo` (#19495, #24686):
  type family T (r :: RuntimeRep) :: TYPE r

  foo :: forall (r :: RuntimeRep). T r
  foo = ...

We kind-check the type with expected kind `TYPE delta` (see newExpectedKind),
where `delta :: RuntimeRep` is as-yet unknown. (We can't use `TYPE LiftedRep`
because we allow signatures like `foo :: Int#`.)

Suppose we are at level L currently.  We do this
  * pushLevelAndSolveEqualitiesX: moves to level L+1
  * newExpectedKind: allocates delta{L+1}. Note carefully that
    this call is /outside/ the tcOuterTKBndrs call.
  * tcOuterTKBndrs: pushes the level again to L+2, binds skolem r{L+2}
  * kind-check the body (T r) :: TYPE delta{L+1}

Then
* We can't unify delta{L+1} with r{L+2}.
  And rightly so: skolem would escape.

* If delta{L+1} is unified with some-kind{L}, that is fine.
  This can happen
      \(x::a) -> let y :: a; y = x in ...(x :: Int#)...
  Here (x :: a :: TYPE gamma) is in the environment when we check
  the signature y::a.  We unify delta := gamma, and all is well.

* If delta{L+1} is unconstrained, we /must not/ quantify over it!
  E.g. Consider f :: Any   where Any :: forall k. k
  We kind-check this with expected kind TYPE kappa. We get
      Any @(TYPE kappa) :: TYPE kappa
  We don't want to generalise to     forall k. Any @k
  because that is ill-kinded: the kind of the body of the forall,
  (Any @k :: k) mentions the bound variable k.

  Instead we want to default it to LiftedRep.

  An alternative would be to promote it, similar to the monomorphism
  restriction, but the MR is pretty confusing.  Defaulting seems better

How does that defaulting happen?  Well, since we /currently/ default
RuntimeRep variables during generalisation, it'll happen in
kindGeneralize. But in principle we might allow generalisation over
RuntimeRep variables in future.  Moreover, what if we had
   kappa{L+1} := F alpha{L+1}
where F :: Type -> RuntimeRep.   Now it's alpha that is free in the kind
and it /won't/ be defaulted.

So we use doNotQuantifyTyVars to either default the free vars of
exp_kind (if possible), or error (if not).

Note [Skolem escape in type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcHsSigType is tricky.  Consider (T11142)
  foo :: forall b. (forall k (a :: k). SameKind a b) -> ()
This is ill-kinded because of a nested skolem-escape.

That will show up as an un-solvable constraint in the implication
returned by buildTvImplication in tc_lhs_sig_type.  See Note [Skolem
escape prevention] in GHC.Tc.Utils.TcType for why it is unsolvable
(the unification variable for b's kind is untouchable).

Then, in GHC.Tc.Solver.simplifyAndEmitFlatConstraints (called from tcHsSigType)
we'll try to float out the constraint, be unable to do so, and fail.
See GHC.Tc.Solver Note [Failure in local type signatures] for more
detail on this.

The separation between tcHsSigType and tc_lhs_sig_type is because
tcClassSigType wants to use the latter, but *not* fail fast, because
there are skolems from the class decl which are in scope; but it's fine
not to because tcClassDecl1 has a solveEqualities wrapped around all
the tcClassSigType calls.

That's why tcHsSigType does simplifyAndEmitFlatConstraints (which
fails fast) but tcClassSigType just does emitImplication (which does
not).  Ugh.

c.f. see also Note [Skolem escape and forall-types]. The difference
is that we don't need to simplify at a forall type, only at the
top level of a signature.
-}

-- Does validity checking and zonking.
tcStandaloneKindSig :: TyClDecl GhcRn
                    -> LStandaloneKindSig GhcRn -> TcM (Name, Kind)
tcStandaloneKindSig decl ksig = do
  (name, pure_kind, _) <- tcStandaloneKindSigWithDomain decl ksig
  return (name, pure_kind)

-- | Check a standalone kind signature and extract an optional canonical
-- constrained kind for top-level open or closed type families under
-- @DependentTypes@.
tcStandaloneKindSigWithDomain :: TyClDecl GhcRn
                              -> LStandaloneKindSig GhcRn
                              -> TcM (Name, Kind, Maybe Kind)
tcStandaloneKindSigWithDomain decl (L _ (StandaloneKindSig _ (L _ name) ksig))
  = addSigCtxt ctxt (UserLHsSigType ksig) $
    do { raw_kind <- tc_top_lhs_type_x (Just decl) KindLevel ctxt ksig
       ; edt <- xoptM LangExt.DependentTypes
       ; case if edt && is_top_type_family decl
                then splitStandaloneFamilyConstrainedKind decl raw_kind
                else Nothing of
           Just (pure_kind, theta, ckind)
             | not (any isEqPred theta) -> do
                 checkValidType ctxt pure_kind
                 checkValidTheta SigmaCtxt theta
                 return (name, pure_kind, Just ckind)
           _ -> do
             checkValidType ctxt raw_kind
             return (name, raw_kind, Nothing) }
  where
   ctxt = StandaloneKindSigCtxt name
   is_top_type_family (FamDecl _ FamilyDecl{fdInfo = OpenTypeFamily{}})   = True
   is_top_type_family (FamDecl _ FamilyDecl{fdInfo = ClosedTypeFamily{}}) = True
   is_top_type_family _ = False

-- | Split top-level class premises out of a type family's standalone kind
-- signature, rewriting term-style @Collects c => c -> ...@ arguments into
-- visible @forall c ->@ binders and building a canonical constrained kind.
splitStandaloneFamilyConstrainedKind
  :: TyClDecl GhcRn -> Kind -> Maybe (Kind, ThetaType, Kind)
splitStandaloneFamilyConstrainedKind decl raw_kind
  | null theta = Nothing
  | otherwise  = Just (pure_kind, theta, canonical_kind)
  where
    HsQTvs { hsq_explicit = hs_bndrs } = tyClDeclTyVars decl
    vis_hs_bndrs = [b | L _ b@(HsTvb _ HsBndrRequired{} _ _) <- hs_bndrs]

    split_pis ty
      | Just (ma, bndr, rest) <- splitPiTy_maybe ty
      = let (pis, res) = split_pis rest in ((ma, bndr) : pis, res)
      | otherwise = ([], ty)

    (all_pis, res_kind) = split_pis raw_kind
    is_constraint_pi (_, Anon _ af) = isInvisibleFunArg af
    is_constraint_pi _              = False

    theta   = [pred_ty | (_, Anon (Scaled _ pred_ty) af) <- all_pis, isInvisibleFunArg af]
    val_pis = filter (not . is_constraint_pi) all_pis
    theta_fvs = tyCoVarsOfTypes theta

    invis_candidates =
      [ tv
      | (_, Named (Bndr tv vis)) <- val_pis
      , isInvisibleForAllTyFlag vis
      , tv `elemVarSet` theta_fvs ]

    vis_pis = [b | (_, b) <- val_pis, not (isInvisiblePiTyBinder b)]

    promoted_tvs = mkVarSet
      [ tv
      | tv <- invis_candidates
      , [vis_idx] <- [anon_positions tv]
      , not (tv `elemVarSet` other_fvs tv vis_idx)
      , not (has_explicit_hs_kind vis_idx) ]

    anon_positions tv =
      [ i
      | (i, Anon (Scaled _ arg_ty) af) <- zip [0 :: Int ..] vis_pis
      , isVisibleFunArg af
      , Just v <- [getTyVar_maybe arg_ty]
      , v == tv ]

    other_fvs tv vis_idx =
      let vis_other_fvs = unionVarSets
            [ pi_fvs b
            | (i, b) <- zip [0 :: Int ..] vis_pis
            , i /= vis_idx ]
          invis_other_fvs = unionVarSets
            [ tyCoVarsOfType (tyVarKind v)
            | (_, Named (Bndr v vis)) <- val_pis
            , isInvisibleForAllTyFlag vis
            , v /= tv ]
      in vis_other_fvs `unionVarSet` invis_other_fvs `unionVarSet` tyCoVarsOfType res_kind

    pi_fvs (Named (Bndr v _))       = tyCoVarsOfType (tyVarKind v)
    pi_fvs (Anon (Scaled m ty) _)   = tyCoVarsOfType m `unionVarSet` tyCoVarsOfType ty

    has_explicit_hs_kind vis_idx =
      case drop vis_idx vis_hs_bndrs of
        HsTvb _ _ _ HsBndrKind{} : _ -> True
        _ -> False

    val_pis1 = mapMaybe rewrite_pi val_pis
    rewrite_pi (_, Named (Bndr tv vis))
      | isInvisibleForAllTyFlag vis, tv `elemVarSet` promoted_tvs
      = Nothing
    rewrite_pi (ma, Anon (Scaled _ arg_ty) af)
      | isVisibleFunArg af
      , Just tv <- getTyVar_maybe arg_ty
      , tv `elemVarSet` promoted_tvs
      = Just (ma, Named (Bndr tv Required))
    rewrite_pi pair = Just pair

    explicit_invis_hs =
      any (\(L _ (HsTvb _ flag _ _)) -> case flag of
               HsBndrRequired{}  -> False
               HsBndrInvisible{} -> True) hs_bndrs

    (kept_pis, hidden_tvs) = partition_pis val_pis1
    partition_pis [] = ([], [])
    partition_pis ((_, Named (Bndr tv vis)) : rest)
      | isInvisibleForAllTyFlag vis
      , not explicit_invis_hs
      , let (rest_kept, rest_hidden) = partition_pis rest
            used_later = tyCoVarsOfType res_kind `unionVarSet`
                         unionVarSets (map (pi_fvs . snd) rest_kept)
      , not (tv `elemVarSet` used_later)
      = (rest_kept, tv : rest_hidden)
    partition_pis (p : rest) =
      let (rest_kept, rest_hidden) = partition_pis rest
      in (p : rest_kept, rest_hidden)

    mk_pi (ma, Named b) rest = ForAllTy ma b rest
    mk_pi (ma, Anon (Scaled m arg_ty) af) rest = FunTy af ma m arg_ty rest

    pure_kind = foldr mk_pi res_kind kept_pis

    inner_constrained =
      mkInfForAllTys [(unmatchableDataConTy, h) | h <- hidden_tvs]
                     (tcMkDFunPhiTy theta res_kind)

    mk_canonical (_, (ma, Named b)) rest = ForAllTy ma b rest
    mk_canonical (i, (ma, Anon (Scaled _ arg_k) _)) rest =
      let anon_tv = mkTyVar (mkSysTvName (mkAlphaTyVarUnique (i + 1)) (fsLit "_anon")) arg_k
      in ForAllTy ma (Bndr anon_tv Required) rest

    canonical_kind = foldr mk_canonical inner_constrained (zip [0 :: Int ..] kept_pis)

tcTopLHsType :: UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
tcTopLHsType ctxt lsig_ty
  = checkNoErrs $  -- Fail eagerly to avoid follow-on errors.  We are at
                   -- top level so these constraints will never be solved later.
    tc_top_lhs_type TypeLevel ctxt lsig_ty

tc_top_lhs_type :: TypeOrKind -> UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
-- tc_top_lhs_type is used for kind-checking top-level LHsSigTypes where
--   we want to fully solve /all/ equalities, and report errors
-- Does zonking, but not validity checking because it's used
--   for things (like deriving and instances) that aren't
--   ordinary types
-- Used for both types and kinds
tc_top_lhs_type = tc_top_lhs_type_x Nothing

-- | Check a top-level signature with optional declaration-prefix constraints.
-- See Note [Checked declaration-kind matchability].
tc_top_lhs_type_x :: Maybe (TyClDecl GhcRn)
                  -> TypeOrKind -> UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
tc_top_lhs_type_x decl tyki ctxt (L loc sig_ty@(HsSig { sig_bndrs = hs_outer_bndrs
                                               , sig_body = body }))
  = setSrcSpanA loc $
    do { traceTc "tc_top_lhs_type {" (ppr sig_ty)
       ; skol_info <- mkSkolemInfo skol_info_anon
       ; (tclvl, wanted, (outer_bndrs, ma_bndrs, ty))
              <- pushLevelAndSolveEqualitiesX "tc_top_lhs_type"    $
                 do { kind <- newExpectedKind (expectedKindInCtxt ctxt)
                    ; outer_ma <- tcForAllMatchability (hsOuterMatchability hs_outer_bndrs)
                    ; (outer_bndrs, ty) <- tcOuterTKBndrs skol_info hs_outer_bndrs $
                        tc_check_lhs_type mode body kind
                    ; ma_bndrs <- forAllMatchabilities mode outer_ma (outerTyVarBndrs outer_bndrs)
                    ; return (outer_bndrs, ma_bndrs, ty) }

       ; outer_bndrs <- scopedSortOuter outer_bndrs
       ; let ma_env = mkVarEnv [(binderVar bndr, ma) | (ma,bndr) <- ma_bndrs]
             outer_tv_bndrs = [(lookupVarEnv_NF ma_env (binderVar bndr), bndr)
                              | bndr <- outerTyVarBndrs outer_bndrs]
             ty1 = mkInvisForAllTys outer_tv_bndrs ty

       ; wanted <- case decl of
           Nothing -> return wanted
           Just decl ->
             do { (_, wanted', ()) <- pushLevelAndSolveEqualitiesX
                                       "checkDeclKindMatchability" $
                    do { emitConstraints wanted
                       ; checkDeclKindMatchability decl ty1 }
                ; return wanted' }
       ; when (isJust decl) $ defaultDeclKindMatchabilities ty1
       ; kvs <- kindGeneralizeAll skol_info ty1  -- "All" because it's a top-level type
       ; reportUnsolvedEqualities skol_info kvs tclvl wanted

       ; let inferred_bndrs = map (mkForAllTyBinder Inferred) kvs
       ; provisional <- liftZonkM $ zonkTcType $
           mkForAllTys (map (defaultForAllMatchability tyki,) inferred_bndrs) ty1
       ; let prefix_len = maybe 0 (\decl -> length (declKindPrefix decl provisional)) decl
             inferred_mas = [ case decl of
                                Just decl | i < prefix_len -> tyConFlavourMatchability (tyClDeclFlavour decl)
                                _ -> defaultForAllMatchability tyki
                            | i <- [0 .. length kvs - 1] ]
       ; final_ty <- initZonkEnv DefaultFlexi
                   $ zonkTcTypeToTypeX (mkInfForAllTys (zip inferred_mas kvs) ty1)
       ; traceTc "tc_top_lhs_type }" (vcat [ppr sig_ty, ppr final_ty])
       ; return final_ty }
  where
    skol_info_anon = SigTypeSkol ctxt
    mode = (mkMode tyki) { mode_declaration = isJust decl }

{- Note [Checked declaration-kind matchability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Per GHC Proposal #242 (Kiss et al., ICFP 2019) and GHC Proposal #425, the
consumed Pi prefix of a declaration is Matchable ('M') for data, data families,
and classes, and Unmatchable ('U') for type families and type synonyms, while
its residual result kind is independent.

During standalone kind signature checking, omitted matchability annotations get
fresh local metavariables. After solving formation equalities (exposing any
binders beneath 'CastTy'), 'checkDeclKindMatchability' constrains only the Pi
prefix consumed by header matching and datatype eta-expansion;
'defaultDeclKindMatchabilities' then defaults remaining local metavariables in
the residual kind to 'M' before kind generalization.
-}

-- | Check all Pi nodes consumed by the declaration header.
checkDeclKindMatchability :: TyClDecl GhcRn -> TcKind -> TcM ()
checkDeclKindMatchability decl kind
  = do { kind <- liftZonkM $ zonkTcType kind
       ; forM_ (zip [0..] (declKindPrefix decl kind)) $ \ (i, (ma, _)) ->
           checkDeclArrowMatchability kind i ma expected_ma }
  where
    expected_ma = tyConFlavourMatchability (tyClDeclFlavour decl)

-- | Preserve the source synonym in an error for a consumed declaration binder.
checkDeclArrowMatchability :: Kind -> Int -> Matchability -> Matchability -> TcM ()
checkDeclArrowMatchability kind index actual expected
  | actual `tcEqType` unmatchableDataConTy
  , expected `tcEqType` matchableDataConTy
  , Just (name, bndr) <- synonymAt index kind = do
      mod <- tcg_semantic_mod <$> getGblEnv
      let forall_vis = case bndr of
            Named (Bndr _ vis) -> Just vis
            Anon{}             -> Nothing
      addErrCtxt (DeclKindSynonymCtxt name forall_vis (nameIsLocalOrFrom mod name)) check
  | otherwise = check
  where
    check = discardResult $ unifyType Nothing actual expected

    -- An alias can merely forward a U arrow supplied by its caller.
    -- Only report metadata supplied by the unsubstituted synonym RHS.
    synonymAt i ty
      | Just (tc, _) <- splitTyConAppNoView_maybe ty
      , let name = tyConName tc
      , not (isWiredInName name)
      , Just rhs <- synTyConRhs_maybe tc
      , Just (ma, bndr) <- binderAt i rhs
      , ma `tcEqType` unmatchableDataConTy = Just (name, bndr)
      | i > 0, Just (_, _, rest) <- splitPiTy_maybe ty =
          synonymAt (i - 1) rest
      | otherwise = Nothing

    binderAt i ty = do
      (ma, bndr, rest) <- splitPiTy_maybe ty
      if i == 0 then Just (ma, bndr) else binderAt (i - 1) rest

-- | Select exactly the prefix consumed by header matching and data eta expansion.
-- See Note [Checked declaration-kind matchability].
declKindPrefix :: TyClDecl GhcRn -> TcKind -> [(Matchability, PiTyBinder)]
declKindPrefix decl = go hs_bndrs
  where
    flav = tyClDeclFlavour decl
    HsQTvs { hsq_explicit = hs_bndrs } = tyClDeclTyVars decl

    go [] kind
      | needsEtaExpansion flav = eta_prefix kind
      | otherwise = []
    go hs_bndrs@(hs_bndr : rest) kind
      | Just (ma, bndr, body) <- splitPiTy_maybe kind
      = case bndr of
          Anon _ af | isInvisibleFunArg af -> go hs_bndrs body
          _ ->
            let vis = case bndr of
                        Named (Bndr _ vis) -> NamedTCB vis
                        Anon{} -> AnonTCB
            in if zippableTyConBinder vis (hsTyVarBndrFlag (unLoc hs_bndr))
               then (ma,bndr) : go rest body
               else if skippableTyConBinder vis
                    then (ma,bndr) : go hs_bndrs body
                    else [] -- The header matcher reports incompatible visibility.
      | otherwise = [] -- The header matcher reports missing binders.

    eta_prefix kind
      | Just (ma, bndr, body) <- splitPiTy_maybe kind
      = case bndr of
          Anon _ af | isInvisibleFunArg af -> eta_prefix body
          _ -> (ma,bndr) : eta_prefix body
      | otherwise = []

-- | Default unresolved local arrow metadata before kind generalization.
defaultDeclKindMatchabilities :: TcKind -> TcM ()
defaultDeclKindMatchabilities kind
  = do { candidates <- candidateQTyVarsOfKind kind
       ; forM_ (dVarSetElems (dv_kvs candidates)) $ \ tv ->
           when (isMetaTyVar tv && not (isTyVarTyVar tv)
                 && tcEqType (tyVarKind tv) matchabilityTy) $
             liftZonkM $ writeMetaTyVar tv matchableDataConTy }

-- | Typecheck the class in a default declaration, checking that:
--
--  - it is indeed a class (not e.g. a type family),
--  - that the class expects some invisible arguments followed
--    by a single visible argument.
tcDefaultDeclClass :: LIdP GhcRn -> TcM (Maybe Class)
tcDefaultDeclClass l_nm
  = setSrcSpan (locA l_nm) $
  do { let nm = unLoc l_nm
     ; thing <- tcLookupGlobal nm
     ; case thing of
        ATyCon tc
          | Just cls <- tyConClass_maybe tc
          -> if is_unary (tyConBinders tc)
             then return $ Just cls
             else
               do { addErrTc $ TcRnNonUnaryTypeclassConstraint DefaultDeclCtxt (NameThing nm)
                  ; return Nothing }

        _ -> do { addErrTc $ TcRnIllegalDefaultClass nm
                ; return Nothing }
     }
  where
    is_unary :: [TyConBinder] -> Bool
    is_unary = ( `lengthIs` 1 ) . dropWhile isInvisibleTyConBinder

-----------------
tcHsDeriv :: LHsSigType GhcRn -> TcM (Maybe (Class, [TyCoVar], [Type], Kind))
-- ^ Like tcHsSigType, but for the @...deriving( C ty1 ty2 )@ clause
--
-- Returns a class constraint with the last argument missing, and the
-- expected kind of the remaining argument.
--
-- E.g.:
--
--  @class C (a::*) (b::k->k)@
--  @data T a b = ... deriving( C Int )@
--
-- This function returns @(C, [k], [k, Int], k->k)@.
--
-- Return values are fully zonked
tcHsDeriv hs_ty
  = do { ty <- tcTopLHsType DerivClauseCtxt hs_ty

       ; let (ma_tvs, pred) = splitForAllTyCoVars ty
             tvs = map snd ma_tvs
             (_, kind_args, _) = splitFunTys (typeKind pred)
      -- Checking that `pred` a is type class application

       ; case splitTyConApp_maybe pred of
            Just (tc, tc_args) ->
              case tyConClass_maybe tc of
                Just cls ->
                  case kind_args of
                    [Scaled _ last_kind] ->
                      return $ Just $
                        (cls, tvs, tc_args, last_kind)
                    _ ->
                      do { addErrTc $ TcRnNonUnaryTypeclassConstraint DerivClauseCtxt (TypeThing pred)
                         ; return Nothing
                         }
                Nothing ->
                  do { addErrTc $ TcRnIllegalInstance
                                $ IllegalClassInstance (TypeThing ty)
                                $ IllegalInstanceHead
                                $ InstHeadNonClassHead
                                $ InstNonClassTyCon
                                    (noUserRdr $ tyConName tc)
                                    (fmap tyConName $ tyConFlavour tc)
                     ; return Nothing }
            Nothing ->
              do { addErrTc $ TcRnIllegalDerivingItem hs_ty; return Nothing }
       }

-- | Typecheck a deriving strategy. For most deriving strategies, this is a
-- no-op, but for the @via@ strategy, this requires typechecking the @via@ type.
tcDerivStrategy :: Maybe (LDerivStrategy GhcRn)
                   -- ^ The deriving strategy
                -> TcM (Maybe (LDerivStrategy GhcTc), [TcTyVar])
                   -- ^ The typechecked deriving strategy and the tyvars that it binds
                   -- (if using 'ViaStrategy').
tcDerivStrategy mb_lds
  = case mb_lds of
      Nothing -> boring_case Nothing
      Just (L loc ds) ->
        setSrcSpanA loc $ do
          (ds', tvs) <- tc_deriv_strategy ds
          pure (Just (L loc ds'), tvs)
  where
    tc_deriv_strategy :: DerivStrategy GhcRn
                      -> TcM (DerivStrategy GhcTc, [TyVar])
    tc_deriv_strategy (StockStrategy    _) = boring_case (StockStrategy    noExtField)
    tc_deriv_strategy (AnyclassStrategy _) = boring_case (AnyclassStrategy noExtField)
    tc_deriv_strategy (NewtypeStrategy  _) = boring_case (NewtypeStrategy  noExtField)
    tc_deriv_strategy (ViaStrategy hs_sig)
      = do { ty <- tcTopLHsType DerivClauseCtxt hs_sig
             -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
             --           in GHC.Tc.Utils.TcType
           ; rec { (via_tvs, via_pred) <- tcSkolemiseInvisibleBndrs (DerivSkol via_pred) ty }
           ; pure (ViaStrategy via_pred, via_tvs) }

    boring_case :: ds -> TcM (ds, [a])
    boring_case ds = pure (ds, [])

tcHsClsInstType :: UserTypeCtxt    -- InstDeclCtxt or SpecInstCtxt
                -> LHsSigType GhcRn
                -> TcM Type
-- Like tcHsSigType, but for a class instance declaration
tcHsClsInstType user_ctxt hs_inst_ty
  = setSrcSpan (getLocA hs_inst_ty) $
    do { inst_ty <- tcTopLHsType user_ctxt hs_inst_ty
       -- Check before instance consumers discard the forall annotations.
       -- See Note [Runtime forall matchability] in GHC.Tc.Utils.Instantiate.
       ; skol_info <- mkSkolemInfo (SigTypeSkol user_ctxt)
       ; let (bndrs, body) = splitForAllForAllTyBinders inst_ty
             subst0 = mkEmptySubst (mkInScopeSet (tyCoVarsOfType inst_ty))
       ; (subst, skol_bndrs) <- tcInstSkolTyVarBndrsX skol_info subst0 bndrs
       ; (_, prefix_implics) <- tcRuntimeForAllBndrs skol_info [] [] skol_bndrs
                                  (substTy subst body)
       ; mapBagM_ checkImplicationInvariants prefix_implics
       ; emitImplications prefix_implics
       ; checkValidInstance user_ctxt hs_inst_ty inst_ty }

----------------------------------------------
-- | Type-check a visible type application
tcHsTypeApp :: LHsWcType GhcRn -> Kind -> TcM Type
tcHsTypeApp = tcHsTypeAppWithSource Nothing

-- | Type-check the lowered argument. Use the written syntax in its context.
-- See Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
-- See Note [Renaming type lambdas and type cases] in GHC.Rename.HsType.
tcHsTypeAppWithSource :: Maybe HsAppTypeSource -> LHsWcType GhcRn -> Kind -> TcM Type
tcHsTypeAppWithSource source wc_ty kind
  | HsWC { hswc_ext = sig_wcs, hswc_body = hs_ty } <- wc_ty
  = do { mode <- mkHoleMode TypeLevel HM_VTA
                 -- HM_VTA: See Note [Wildcards in visible type application]
       ; indices <- tcRetainedAmbientIndices RetainedTypeArgumentIndex hs_ty
       ; let complete = null sig_wcs && hsTypeHasNoAnonWildcards hs_ty
             mode' = mode { mode_indices = indices
                          , mode_complete_type_arg = Just complete }
       ; ty <- addTypeCtxtWithSource source hs_ty $
               solveEqualities "tcHsTypeApp" $
               -- We are looking at a user-written type, very like a
               -- signature so we want to solve its equalities right now
               bindNamedWildCardBinders sig_wcs $ \ _ ->
               tc_check_lhs_type mode' hs_ty kind

       -- We do not kind-generalize type applications: we just
       -- instantiate with exactly what the user says.
       -- See Note [No generalization in type application]
       -- We still must call kindGeneralizeNone, though, according
       -- to Note [Recipe for checking a signature]
       ; kindGeneralizeNone ty
       ; ty <- liftZonkM $ zonkTcType ty
       ; checkValidType TypeAppCtxt ty
       ; return ty }

{- Note [Wildcards in visible type application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A HsWildCardBndrs's hswc_ext now only includes /named/ wildcards, so
any unnamed wildcards stay unchanged in hswc_body.  When called in
tcHsTypeApp, tcCheckLHsTypeInContext will call emitAnonTypeHole
on these anonymous wildcards. However, this would trigger
error/warning when an anonymous wildcard is passed in as a visible type
argument, which we do not want because users should be able to write
@_ to skip a instantiating a type variable variable without fuss. The
solution is to switch the PartialTypeSignatures flags here to let the
typechecker know that it's checking a '@_' and do not emit hole
constraints on it.  See related Note [Wildcards in visible kind
application] and Note [The wildcard story for types] in GHC.Hs.Type

Ugh!

Note [No generalization in type application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not kind-generalize type applications. Imagine

  id @(Proxy Nothing)

If we kind-generalized, we would get

  id @(forall {k}. Proxy @(Maybe k) (Nothing @k))

which is very sneakily impredicative instantiation.

There is also the possibility of mentioning a wildcard
(`id @(Proxy _)`), which definitely should not be kind-generalized.

-}

tcFamTyPats :: TyCon
            -> HsFamEqnPats GhcRn                -- Patterns
            -> TcM (TcType, TcKind)          -- (lhs_type, lhs_kind)
-- Check the LHS of a type/data family instance
-- e.g.   type instance F ty1 .. tyn = ...
-- Used for both type and data families
tcFamTyPats = tcFamTyPatsX []

-- | Give an inferred metadata binder a LHS argument before checking the RHS.
tcInferredMatchabilityFamPats :: TyCon -> HsFamEqnPats GhcRn
                               -> TcM (TcType, TcKind, [TcType])
tcInferredMatchabilityFamPats fam_tc hs_pats = do
  let indices = takeWhile inferredMatchabilityIndex (tyConBinders fam_tc)
  vars <- replicateM (length indices) (newFlexiTyVar matchabilityTy)
  owner <- tcl_kind_arrow_defaults <$> getLclEnv
  forM_ owner $ \(_, ref) ->
    updTcRef ref ([v | (b, v) <- zip indices vars, familyKindUsesMatchability fam_tc b] ++)
  let mas = mkTyVarTys vars
  (lhs, result_kind) <- tcFamTyPatsX mas fam_tc hs_pats
  pure (lhs, result_kind, mas)

tcFamTyPatsX :: [TcType] -> TyCon -> HsFamEqnPats GhcRn -> TcM (TcType, TcKind)
tcFamTyPatsX prefix fam_tc hs_pats
  = do { traceTc "tcFamTyPats {" $
         vcat [ ppr fam_tc, text "arity:" <+> ppr fam_arity ]

       ; raw_fields <- primFamilyPatEnabled fam_tc
       ; mode <- mkHoleMode TypeLevel (HM_FamPat raw_fields)
                 -- HM_FamPat: See Note [Wildcards in family instances] in
                 -- GHC.Rename.Module
       ; let fun_ty = mkTyConApp fam_tc prefix
       ; (fam_app, res_kind) <- tcInferTyApps mode lhs_fun fun_ty hs_pats

       -- Hack alert: see Note [tcFamTyPats: zonking the result kind]
       ; res_kind <- liftZonkM $ zonkTcType res_kind

       ; traceTc "End tcFamTyPats }" $
         vcat [ ppr fam_tc, text "res_kind:" <+> ppr res_kind ]

       ; return (fam_app, res_kind) }
  where
    fam_name  = tyConName fam_tc
    fam_arity = tyConArity fam_tc
    lhs_fun   = noLocA (HsTyVar noAnn NotPromoted (noLocA $ noUserRdr fam_name))

{- Note [tcFamTyPats: zonking the result kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#19250)
    F :: forall k. k -> k
    type instance F (x :: Constraint) = ()

The tricky point is this:
  is that () an empty type tuple (() :: Type), or
  an empty constraint tuple (() :: Constraint)?
We work this out in a hacky way, by looking at the expected kind:
see Note [Inferring tuple kinds].

In this case, we kind-check the RHS using the kind gotten from the LHS:
see the call to tcCheckLHsTypeInContext in tcTyFamInstEqnGuts in GHC.Tc.Tycl.

But we want the kind from the LHS to be /zonked/, so that when
kind-checking the RHS (tcCheckLHsTypeInContext) we can "see" what we learned
from kind-checking the LHS (tcFamTyPats).  In our example above, the
type of the LHS is just `kappa` (by instantiating the forall k), but
then we learn (from x::Constraint) that kappa ~ Constraint.  We want
that info when kind-checking the RHS.

Easy solution: just zonk that return kind.  Of course this won't help
if there is lots of type-family reduction to do, but it works fine in
common cases.
-}


{-
************************************************************************
*                                                                      *
            The main kind checker: no validity checks here
*                                                                      *
************************************************************************
-}

---------------------------
tcHsOpenType, tcHsLiftedType,
  tcHsOpenTypeNC, tcHsLiftedTypeNC :: LHsType GhcRn -> TcM TcType
-- Used for type signatures
-- Do not do validity checking
tcHsOpenType   hs_ty = addTypeCtxt hs_ty $ tcHsOpenTypeNC hs_ty
tcHsLiftedType hs_ty = addTypeCtxt hs_ty $ tcHsLiftedTypeNC hs_ty

tcHsOpenTypeNC   hs_ty = do { ek <- newOpenTypeKind; tcCheckLHsType hs_ty ek }
tcHsLiftedTypeNC hs_ty = tcCheckLHsType hs_ty liftedTypeKind

-- Like tcCheckLHsType, but takes an expected kind
tcCheckLHsTypeInContext :: LHsType GhcRn -> ContextKind -> TcM TcType
tcCheckLHsTypeInContext hs_ty exp_kind
  = addTypeCtxt hs_ty $
    do { ek <- newExpectedKind exp_kind
       ; tcCheckLHsType hs_ty ek }

-- | Infer a callback arrow for a parameter of an unsigned datatype.
-- See Note [Inferred datatype callback arrows].
tcCheckDatatypeField :: NameSet -> LHsType GhcRn -> ContextKind -> TcM TcType
tcCheckDatatypeField parameters field exp_kind = addTypeCtxt field $
  do { ek <- newExpectedKind exp_kind
     ; tc_check_lhs_type
         (typeLevelMode {mode_callback_arrow = InferDatatypeArrows parameters}) field ek }

{- Note [Inferred datatype callback arrows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an unsigned Haskell 98 declaration such as data Box f = Box (f Bool),
the kind inference pass gives the application of a datatype parameter
a fresh arrow modifier. The kind of Box can quantify that modifier.
A locally bound existential has a different Name and keeps the usual
checking rules.

When applying Box, defer an inferred modifier if a supplied argument
checks against an arrow kind whose modifier is that variable. Its kind
can then constrain the same variable. Generalise it while inferring a
datatype or a closed family. In the final constructor check, use the
modifier in the already inferred kind, including for nested recursive
fields. If the context supplies no constraint, keep the previous default.
No new inference follows from the result of applying the callback.
-}

-- | Check an equation of an unsigned closed family. See Note
-- [Inferred closed family callback arrows].
tcCheckInferredFamilyRhs :: LHsType GhcRn -> TcKind -> TcM TcType
tcCheckInferredFamilyRhs rhs kind = do
  enabled <- unsaturatedTypeFamiliesEnabled
  poly_kinds <- xoptM LangExt.PolyKinds
  let arrows
        | enabled && poly_kinds = InferUnsignedFamilyArrows
        | otherwise = DefaultUnsignedFamilyArrows
  addTypeCtxt (familyRhsForContext rhs) $
    tc_check_lhs_type (typeLevelMode {mode_callback_arrow = arrows}) rhs kind

-- | Check a family equation without generalising a new arrow modifier.
-- See Note [Callback kinds in checked family equations].
tcCheckFamilyRhs :: LHsType GhcRn -> TcKind -> TcM TcType
tcCheckFamilyRhs rhs kind = do
  enabled <- unsaturatedTypeFamiliesEnabled
  if enabled
     then addTypeCtxt (familyRhsForContext rhs) $
          tc_check_lhs_type (typeLevelMode {mode_callback_arrow = CheckFamilyRhsArrows}) rhs kind
     else tcCheckLHsTypeInContext rhs (TheKind kind)

-- | Display the source branch inside its generated alias bindings.
familyRhsForContext :: LHsType GhcRn -> LHsType GhcRn
familyRhsForContext (L _ (XHsType (HsTypeLetRn Nothing _ rhs body)))
  | Just (L _ (WithUserRdr _ name)) <- hsTyGetAppHead_maybe rhs
  , "$case-let-rhs:" `isPrefixOf` occNameString (nameOccName name)
  = familyRhsForContext body
familyRhsForContext rhs = rhs

{- Note [Inferred closed family callback arrows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an unsigned closed family, an application such as f x can determine
the kind of f. With DependentTypes and PolyKinds, use a fresh
matchability for the missing arrow. Generalise it with the other kind
variables. Use fresh matchabilities also when forwarding f to an inferred
family. Defaulting the receiver's index would specialise the new family.

In the final equation check, a recursive reference is a TcTyCon. It and
other inferred family RHSs must still use fresh matchabilities. Otherwise
a forwarding or recursive equation can specialise its axiom to U even
when the family kind is polymorphic. An explicit signature or a LHS
pattern keeps its separate checking mode. When generalisation is disabled
an unsigned family retains the default M kind for its callback.
-}

{- Note [Callback kinds in checked family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a declared callback kind Type %(Flip m) -> Type. Checking
the equation's LHS emits a wanted for the kind of the callback.
If Flip m has to reduce, the callback may still have an unknown kind
when the RHS applies it. Use an arrow with an owned matchability
metavariable. The solver can read Flip m from the LHS wanted before
defaulting the arrow to M. See Note [Owning omitted arrows in
kind-valued arguments] in GHC.Tc.Solver. This mode does not generalise
the arrow or infer a higher-rank kind.
-}

tcInferLHsType :: LHsType GhcRn -> TcM TcType
tcInferLHsType hs_ty
  = do { (ty,_kind) <- tcInferLHsTypeKind hs_ty
       ; return ty }

tcInferLHsTypeKind :: LHsType GhcRn -> TcM (TcType, TcKind)
-- Called from outside: set the context
-- Eagerly instantiate any trailing invisible binders
tcInferLHsTypeKind lhs_ty@(L loc hs_ty)
  = addTypeCtxt lhs_ty $
    setSrcSpanA loc    $  -- Cover the tcInstInvisibleTyBinders
    do { (res_ty, res_kind) <- tc_infer_hs_type typeLevelMode hs_ty
       ; tcInstInvisibleTyBinders (Just TypeLevel) res_ty res_kind }
  -- See Note [Do not always instantiate eagerly in types]

-- Used to check the argument of GHCi :kind
-- Allow and report wildcards, e.g. :kind T _
-- Do not saturate family applications: see Note [Dealing with :kind]
-- Does not instantiate eagerly; See Note [Do not always instantiate eagerly in types]
tcInferLHsTypeUnsaturated :: LHsType GhcRn -> TcM (TcType, TcKind)
tcInferLHsTypeUnsaturated hs_ty
  = addTypeCtxt hs_ty $
    do { mode <- mkHoleMode TypeLevel HM_Sig  -- Allow and report holes
       ; case splitHsAppTys_maybe (unLoc hs_ty) of
           Just (hs_fun_ty, hs_args)
              | L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ nm))) <- hs_fun_ty
              , isVarName nm || isFieldOcc (nameOccName nm)
              -> tc_infer_lhs_type
                   (mode { mode_complete_type_arg = Just (hsTypeHasNoAnonWildcards hs_ty) })
                   hs_ty
              | otherwise
              -> do { (fun_ty, _ki) <- tcInferTyAppHead mode hs_fun_ty hs_args
                    ; tcInferTyApps_nosat TypeLevel Nothing mode hs_fun_ty fun_ty hs_args }
                      -- Notice the 'nosat'; do not instantiate trailing
                      -- invisible arguments of a type family.
                      -- See Note [Dealing with :kind]
           Nothing -> tc_infer_lhs_type mode hs_ty }

{- Note [Dealing with :kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GHCi command
  ghci> type family F :: Either j k
  ghci> :kind F
  F :: forall {j,k}. Either j k

We will only get the 'forall' if we /refrain/ from saturating those
invisible binders. But generally we /do/ saturate those invisible
binders (see tcInferTyApps), and we want to do so for nested application
even in GHCi.  Consider for example (#16287)
  ghci> type family F :: k
  ghci> data T :: (forall k. k) -> Type
  ghci> :kind T F
We want to reject this. It's just at the very top level that we want
to switch off saturation.

So tcInferLHsTypeUnsaturated does a little special case for top level
applications.  Actually the common case is a bare variable, as above.

Note [Do not always instantiate eagerly in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Terms are eagerly instantiated. This means that if you say

  x = id

then `id` gets instantiated to have type alpha -> alpha. The variable
alpha is then unconstrained and regeneralized. So we may well end up with
  x = /\a. id @a
But we cannot do this in types, as we have no type-level lambda.

So, we must be careful only to instantiate at the last possible moment, when
we're sure we're never going to want the lost polymorphism again. This is done
in calls to `tcInstInvisibleTyBinders`; a particular case in point is in
`checkExpectedKind`.

For example, suppose we have:
    Actual:  ∀ k2 k. k -> k2   -> k
  Expected:  ∀    k. k -> Type -> k
We must very delicately instantiate just k2 to kappa, and then unify
  (∀ k. k -> Type -> k) ~ (∀ k. k -> kappa -> k)

Otherwise, we are careful /not/ to instantiate.  For example:
  * at a variable  in `tcTyVar`
  * in `tcInferLHsTypeUnsaturated`, which is used by :kind in GHCi.

************************************************************************
*                                                                      *
      Type-checking modes
*                                                                      *
************************************************************************

The kind-checker is parameterised by a TcTyMode, which contains some
information about where we're checking a type.

The renamer issues errors about what it can. All errors issued here must
concern things that the renamer can't handle.

-}

-- | Check the multiplicity and the matchability of a constructor field arrow.
tcConArrowMult :: HsModifiedFunArr GhcRn -> TcM (Maybe Mult)
tcConArrowMult ann = do
  (mult, ma) <- tc_arrow_modifiers typeLevelMode ann
  forM_ ma $ \actual -> void (unifyType Nothing actual matchableDataConTy)
  pure mult

-- | Check multiplicity annotations in a field declaration.
tcMult :: HsModifiedFunArr GhcRn -> TcM (Maybe Mult)
tcMult = tc_mult typeLevelMode

tcModifiersAndWarn :: [LHsModifier GhcRn] -> TcM [TcType]
tcModifiersAndWarn mods =
  tc_modifiers typeLevelMode mods Nothing (const $ Left DontSuggestLinear)

tcModifiersMult :: [LHsModifier GhcRn] -> TcM (Maybe Mult)
tcModifiersMult = tc_modifiers_mult typeLevelMode

-- | Info about the context in which we're checking a type. Currently,
-- differentiates only between types and kinds, but this will likely
-- grow, at least to include the distinction between patterns and
-- not-patterns.
--
-- To find out where the mode is used, search for 'mode_tyki'
--
-- This data type is purely local, not exported from this module
data TcTyMode
  = TcTyMode { mode_tyki :: TypeOrKind
             , mode_declaration :: Bool
             , mode_callback_arrow :: CallbackArrowMode
             , mode_source_givens :: [PredType]
             , mode_holes :: HoleInfo
             , mode_indices :: NameEnv Id
             , mode_dictionaries :: [EvVar]
             , mode_pending_calls :: Maybe (NameEnv TcTyVar, PendingSigCalls, TcRef [TcPendingTypeCall])
               -- See Note [Pending calls in partial signatures] in GHC.Tc.Solver.
             , mode_synonym_metadata :: Maybe (Name -> [Bool] -> TcM [Type], Maybe (TypeOrKind -> TcM Matchability))
             , mode_source_proof_owner :: Maybe Name
             , mode_complete_type_arg :: Maybe Bool
             , mode_under_forall :: !Bool
             , mode_lambda_root :: !(Maybe (Name, Name))
             , mode_sigma_root :: !(Maybe (Name, Name)) }
               -- Nothing outside a visible type argument. Just records
               -- completeness of the whole argument, not the current child.
               -- See Note [Retained Pi type formation] in GHC.Rename.HsType.

data CallbackArrowMode
  = OrdinaryArrowChecking
  | DefaultUnsignedFamilyArrows
  | CheckFamilyRhsArrows
  | InferUnsignedFamilyArrows
  | InferDatatypeArrows NameSet
  | FixedTypeArrowMatchability TcType

-- See Note [Levels for wildcards]
-- Nothing <=> no wildcards expected
type HoleInfo = Maybe (TcLevel, HoleMode)

-- HoleMode says how to treat the occurrences
-- of anonymous wildcards; see tcAnonWildCardOcc
data HoleMode = HM_Sig      -- Partial type signatures: f :: _ -> Int
              | HM_FamPat !Bool -- Family instances, with raw Int# fields in opted-in closed equations
              | HM_VTA      -- Visible type and kind application:
                            --   f @(Maybe _)
                            --   Maybe @(_ -> _)
              | HM_TyAppPat -- Visible type applications in patterns:
                            --   foo (Con @_ @t x) = ...
                            --   case x of Con @_ @t v -> ...

mkMode :: TypeOrKind -> TcTyMode
mkMode tyki = TcTyMode { mode_tyki = tyki, mode_holes = Nothing
                      , mode_declaration = False
                      , mode_callback_arrow = OrdinaryArrowChecking
                      , mode_source_givens = []
                      , mode_indices = emptyNameEnv
                      , mode_dictionaries = []
                      , mode_pending_calls = Nothing
                      , mode_synonym_metadata = Nothing
                      , mode_source_proof_owner = Nothing
                      , mode_complete_type_arg = Nothing, mode_under_forall = False, mode_lambda_root = Nothing, mode_sigma_root = Nothing }

typeLevelMode, kindLevelMode :: TcTyMode
-- These modes expect no wildcards (holes) in the type
kindLevelMode = mkMode KindLevel
typeLevelMode = mkMode TypeLevel

mkHoleMode :: TypeOrKind -> HoleMode -> TcM TcTyMode
mkHoleMode tyki hm
  = do { lvl <- getTcLevel
       ; return (TcTyMode { mode_tyki  = tyki
                          , mode_declaration = False
                         , mode_callback_arrow = OrdinaryArrowChecking
                          , mode_source_givens = []
                          , mode_holes = Just (lvl,hm)
                          , mode_indices = emptyNameEnv
                          , mode_dictionaries = []
                          , mode_pending_calls = Nothing
                          , mode_synonym_metadata = Nothing
                      , mode_source_proof_owner = Nothing
                          , mode_complete_type_arg = Nothing, mode_under_forall = False, mode_lambda_root = Nothing, mode_sigma_root = Nothing }) }

instance Outputable HoleMode where
  ppr HM_Sig      = text "HM_Sig"
  ppr HM_FamPat{} = text "HM_FamPat"
  ppr HM_VTA      = text "HM_VTA"
  ppr HM_TyAppPat = text "HM_TyAppPat"

instance Outputable TcTyMode where
  ppr (TcTyMode { mode_tyki = tyki, mode_holes = hm, mode_indices = indices
               , mode_complete_type_arg = complete, mode_declaration = declaration })
    = text "TcTyMode" <+> braces (sep [ ppr tyki <> comma
                                      , ppr hm, ppr (nonDetNameEnvElts indices)
                                      , ppr complete, ppr declaration ])

-- | Defer implicit metadata only while a declaration kind is checked.
matchabilityInstantiationMode :: TcTyMode -> Maybe TypeOrKind
matchabilityInstantiationMode mode
  | mode_declaration mode = Nothing
  | otherwise = Just (mode_tyki mode)

-- | Associate metadata with each semantic binder.
-- See Note [Source forall matchability] in GHC.Hs.Type.
forAllMatchabilities :: TcTyMode -> Maybe Matchability -> [bndr] -> TcM [(Matchability, bndr)]
forAllMatchabilities mode explicit_ma = mapM $ \ bndr ->
  do { ma <- case explicit_ma of
               Just ma -> return ma
               Nothing | mode_declaration mode -> newFlexiTyVarTy matchabilityTy
                       | otherwise -> return (defaultForAllMatchability (mode_tyki mode))
     ; return (ma,bndr) }

-- | Check a source annotation before its whole binder group enters scope.
tcForAllMatchability :: Maybe (LHsType GhcRn) -> TcM (Maybe Matchability)
tcForAllMatchability = traverse (\ma -> tc_check_lhs_type kindLevelMode ma matchabilityTy)

-- | Require runtime declaration quantifiers to have unmatchable metadata.
-- Use ordinary equality so type synonyms and reducing families are accepted.
tcCheckRuntimeForAllMatchability :: Maybe (LHsType GhcRn) -> TcM ()
tcCheckRuntimeForAllMatchability Nothing = return ()
tcCheckRuntimeForAllMatchability (Just hs_ma) = setSrcSpanA (getLoc hs_ma) $
  do { ma <- tc_check_lhs_type kindLevelMode hs_ma matchabilityTy
     ; _ <- unifyType Nothing ma unmatchableDataConTy
     ; return () }

{-
Note [Bidirectional type checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In types, as in terms, we use bidirectional type infefence.  The main workhorse
function looks like this:

    type ExpKind = ExpType
    data ExpType = Check TcSigmaKind | Infer ...(hole TcRhoType)...

    tcHsType :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType

* When the `ExpKind` argument is (Check ki), we /check/ that the type has
  kind `ki`
* When the `ExpKind` argument is (Infer hole), we /infer/ the kind of the
  type, and fill the hole with that kind
-}

------------------------------------------
-- | Check and desugar a type, returning the core type and its
-- possibly-polymorphic kind. Much like 'tcInferRho' at the expression
-- level.
tc_infer_lhs_type :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
tc_infer_lhs_type mode (L span ty)
  = setSrcSpanA span $
    tc_infer_hs_type mode ty

---------------------------
-- | Infer the kind of a type and desugar. This is the "up" type-checker,
-- as described in Note [Bidirectional type checking]
tc_infer_hs_type :: TcTyMode -> HsType GhcRn -> TcM (TcType, TcKind)
tc_infer_hs_type mode rn_ty
  = runInferKind $ \exp_kind ->
    tcHsType mode rn_ty exp_kind

{-
Note [Typechecking HsCoreTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsCoreTy is an escape hatch that allows embedding Core Types in HsTypes.
As such, there's not much to be done in order to typecheck an HsCoreTy,
since it's already been typechecked to some extent. There is one thing that
we must do, however: we must substitute the type variables from the tcl_env.
To see why, consider GeneralizedNewtypeDeriving, which is one of the main
clients of HsCoreTy (example adapted from #14579):

  newtype T a = MkT a deriving newtype Eq

This will produce an InstInfo GhcPs that looks roughly like this:

  instance forall a_1. Eq a_1 => Eq (T a_1) where
    (==) = coerce @(  a_1 ->   a_1 -> Bool) -- The type within @(...) is an HsCoreTy
                  @(T a_1 -> T a_1 -> Bool) -- So is this
                  (==)

This is then fed into the renamer. Since all of the type variables in this
InstInfo use Exact RdrNames, the resulting InstInfo GhcRn looks basically
identical. Things get more interesting when the InstInfo is fed into the
typechecker, however. GHC will first generate fresh skolems to instantiate
the instance-bound type variables with. In the example above, we might generate
the skolem a_2 and use that to instantiate a_1, which extends the local type
environment (tcl_env) with [a_1 :-> a_2]. This gives us:

  instance forall a_2. Eq a_2 => Eq (T a_2) where ...

To ensure that the body of this instance is well scoped, every occurrence of
the `a` type variable should refer to a_2, the new skolem. However, the
HsCoreTys mention a_1, not a_2. Luckily, the tcl_env provides exactly the
substitution we need ([a_1 :-> a_2]) to fix up the scoping. We apply this
substitution to each HsCoreTy and all is well:

  instance forall a_2. Eq a_2 => Eq (T a_2) where
    (==) = coerce @(  a_2 ->   a_2 -> Bool)
                  @(T a_2 -> T a_2 -> Bool)
                  (==)
-}

------------------------------------------
tcCheckLHsType :: LHsType GhcRn -> TcKind -> TcM TcType
tcCheckLHsType hs_ty exp_kind
  = tc_check_lhs_type typeLevelMode hs_ty exp_kind

tc_check_lhs_type :: TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
tc_check_lhs_type mode (L span ty) exp_kind
  = setSrcSpanA span $
    tc_check_hs_type mode ty exp_kind

tc_check_hs_type :: TcTyMode -> HsType GhcRn -> TcKind -> TcM TcType
-- See Note [Bidirectional type checking]
tc_check_hs_type mode ty ek = tcHsType mode ty (Check ek)

tcLHsType :: TcTyMode -> LHsType GhcRn -> ExpKind -> TcM TcType
tcLHsType mode (L span ty) exp_kind
  = setSrcSpanA span $
    tcHsType mode ty exp_kind

-- | Instantiate the already checked captures of a lifted let RHS.
-- See Note [Checked RHSs for cases in type lets] in GHC.Tc.TyCl.
tcInferTypeLetRhs :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
tcInferTypeLetRhs mode source
  | (L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))), args) <- splitHsAppTys (unLoc source)
  , Just captures <- traverse capture args = do
      env <- getLclTypeEnv
      case lookupNameEnv env name of
        Just (ATcCaseLet params rhs kind Nothing)
          | equalLength params captures -> do
              actuals <- check [] [] params captures
              -- Solved metas can refer to this helper's lexical captures.
              -- Expose those references before substituting the actuals.
              (rhs', kind') <- liftZonkM $ (,) <$> zonkTcType rhs <*> zonkTcType kind
              instantiate params actuals rhs' kind'
        Just (ATcCaseLet params rhs kind (Just tc)) -> do
          (applied, _) <- tcInferTyApps mode source (mkTyConTy tc) args
          case splitTyConAppNoView_maybe applied of
            Just (tc', actuals) | tc' == tc, equalLength params actuals ->
              instantiate params actuals rhs kind
            _ -> panic "tcInferTypeLetRhs: checked helper arity"
        _ -> ordinary
  | otherwise = ordinary
  where
    ordinary = tc_infer_lhs_type mode source
    instantiate params actuals rhs kind = do
      -- A capture can occur as the head of an application in rhs. Its
      -- checked kind must expose the arrows before substitution.
      -- See Note [The Purely Kinded Type Invariant (PKTI)].
      args <- liftZonkM (zonkTcTypes actuals)
      pure (substTyWith params args rhs, substTyWith params args kind)
    capture (HsValArg _ arg@(L _ HsTyVar{})) = Just arg
    capture _ = Nothing

    check _ _ [] [] = pure []
    check vs args (v:rest) (arg:others) = do
      actual <- tc_check_lhs_type mode arg (substTyWith vs args (varType v))
      (actual :) <$> check (v:vs) (actual:args) rest others
    check _ _ _ _ = panic "tcInferTypeLetRhs: capture arity"

tcHsType :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
-- The main workhorse for type kind checking
-- See Note [Bidirectional type checking]

tcHsType mode (HsParTy _ ty)   exp_kind = tcLHsType mode ty exp_kind
tcHsType mode (HsDocTy _ ty _) exp_kind = tcLHsType mode ty exp_kind

-- HsSpliced is an annotation produced by 'GHC.Rename.Splice.rnSpliceType'.
-- Here we get rid of it and add the finalizers to the global environment
-- while capturing the local environment.
--
-- See Note [Delaying modFinalizers in untyped splices].
tcHsType mode (HsSpliceTy (HsUntypedSpliceTop mod_finalizers ty) _)
           exp_kind
  = do addModFinalizersWithLclEnv mod_finalizers
       tcLHsType mode ty exp_kind

tcHsType _ (HsSpliceTy (HsUntypedSpliceNested n) s) _ = pprPanic "tcHsType: invalid nested splice" (pprUntypedSplice True (Just n) s)

---------- Functions and applications
tcHsType mode (HsFunTy _ mult ty1 ty2) exp_kind
  = tc_fun_type mode mult ty1 ty2 exp_kind

tcHsType mode (HsOpTy _ ty1 tyop ty2) exp_kind
  | L _ (HsTyVar _ _ op) <- tyop
  , unLocWithUserRdr op `hasKey` unrestrictedFunTyConKey
  = tc_fun_type mode (HsModifiedFunArr noExtField [] $ HsStandardArr noExtField) ty1 ty2 exp_kind

--------- Foralls
tcHsType mode ty@HsForEachTy{} exp_kind
  | TypeLevel <- mode_tyki mode
  = do { experimental <- xoptM LangExt.DependentTypes
       ; unless experimental $ failWithTc TcRnRetainedQuantificationNotImplemented
       ; ty' <- tcCheckRetainedType (mode_complete_type_arg mode == Just True) Nothing
                                    mode (noLocA ty) liftedTypeKind
       ; checkExpKind mode ty ty' liftedTypeKind exp_kind }
  | otherwise
  = failWithTc TcRnRetainedQuantificationNotImplemented

tcHsType mode HsForAllTy{} _
  | not (isEmptyNameEnv (mode_indices mode))
  = failWithTc $ TcRnRetainedQuantificationError RetainedPolymorphicSignature

tcHsType mode t@(HsForAllTy { hst_tele = tele, hst_body = ty }) exp_kind
  | HsForAllInvis{} <- tele
  = do { (tv_bndrs, ty') <- tcTKTelescope mode tele (tcLHsType (mode { mode_under_forall = True }) ty exp_kind)
       ; return (mkForAllTys tv_bndrs ty') }
                 -- For an invisible forall, we allow the body to have
                 -- an arbitrary kind (hence exp_kind above).
                 -- See Note [Body kind of a HsForAllTy]

  | HsForAllVis{} <- tele
  = tc_vis_forall_type mode t tele (tc_check_lhs_type (mode { mode_under_forall = True }) ty) exp_kind

tcHsType mode t@(HsQualTy { hst_ctxt = ctxt, hst_body = rn_ty }) exp_kind
  | null (hsc_ctxt $ unLoc ctxt)
  = tcLHsType mode rn_ty exp_kind
    -- See Note [Body kind of a HsQualTy], point (BK1)
  | Check kind <- exp_kind     -- Checking mode
  , isConstraintLikeKind kind  -- CONSTRAINT rep
  = do { ctxt' <- tc_hs_context mode ctxt
         -- See Note [Body kind of a HsQualTy], point (BK2)
       ; ty'   <- tc_check_lhs_type (sourceMethodContext mode ctxt') rn_ty constraintKind
       ; let res_ty = tcMkDFunPhiTy ctxt' ty'
       ; checkExpKind mode t res_ty constraintKind exp_kind }

  | otherwise
  = do { ctxt' <- tc_hs_context mode ctxt

      ; ek <- newOpenTypeKind  -- The body kind (result of the function) can
                                -- be TYPE r, for any r, hence newOpenTypeKind
      ; ty' <- tc_check_lhs_type (sourceMethodContext mode ctxt') rn_ty ek
      ; let res_ty = tcMkPhiTy ctxt' ty'
      ; checkExpKind mode t res_ty liftedTypeKind exp_kind }

--------- Lists, arrays, and tuples
tcHsType mode rn_ty@(HsListTy _ elt_ty) exp_kind
  = do { tau_ty <- tc_check_lhs_type mode elt_ty liftedTypeKind
       ; checkWiredInTyCon listTyCon
       ; checkExpKind mode rn_ty (mkListTy tau_ty) liftedTypeKind exp_kind }

tcHsType mode rn_ty@(HsTupleTy _ tup_sort tys) exp_kind
  = do k <- expTypeToType exp_kind
       tc_hs_tuple_ty rn_ty mode tup_sort tys k

tcHsType mode rn_ty@(HsSumTy _ hs_tys) exp_kind
  = do { let arity = length hs_tys
       ; arg_kinds <- mapM (\_ -> newOpenTypeKind) hs_tys
       ; tau_tys   <- zipWithM (tc_check_lhs_type mode) hs_tys arg_kinds
       ; let arg_reps = map kindRep arg_kinds
             arg_tys  = arg_reps ++ tau_tys
             sum_ty   = mkTyConApp (sumTyCon arity) arg_tys
             sum_kind = unboxedSumKind arg_reps
       ; checkExpKind mode rn_ty sum_ty sum_kind exp_kind
       }

--------- Promoted lists and tuples
tcHsType mode rn_ty@(HsExplicitListTy _ _ tys) exp_kind
  -- See Note [Kind-checking explicit lists]

  | null tys
  = do let ty = mkTyConTy promotedNilDataCon
       let kind = mkSpecForAllTys [(matchableDataConTy, alphaTyVar)] $ mkListTy alphaTy
       checkExpKind mode rn_ty ty kind exp_kind

  | otherwise
  = do { tks <- mapM (tc_infer_lhs_type mode) tys
       ; (taus', kind) <- unifyKinds mode tys tks
       ; let ty = (foldr (mk_cons kind) (mk_nil kind) taus')
       ; checkExpKind mode rn_ty ty (mkListTy kind) exp_kind }
  where
    mk_cons k a b = mkTyConApp (promoteDataCon consDataCon) [k, a, b]
    mk_nil  k     = mkTyConApp (promoteDataCon nilDataCon) [k]

tcHsType mode rn_ty@(HsExplicitTupleTy _ _ tys) exp_kind
  -- using newMetaKindVar means that we force instantiations of any polykinded
  -- types. At first, I just used tc_infer_lhs_type, but that led to #11255.
  = do { ks   <- replicateM arity newMetaKindVar
       ; taus <- zipWithM (tc_check_lhs_type mode) tys ks
       ; let kind_con   = tupleTyCon           Boxed arity
             ty_con     = promotedTupleDataCon Boxed arity
             tup_k      = mkTyConApp kind_con ks
       ; checkTupSize arity
       ; checkExpKind mode rn_ty (mkTyConApp ty_con (ks ++ taus)) tup_k exp_kind }
  where
    arity = length tys

--------- Constraint types
tcHsType mode rn_ty@(HsIParamTy _ (L _ n) ty) exp_kind
  = do { massert (isTypeLevel (mode_tyki mode))
       ; ty' <- tc_check_lhs_type mode ty liftedTypeKind
       ; let n' = mkStrLitTy $ hsIPNameFS n
       ; ipClass <- tcLookupKnownKeyClass ipClassKey
       ; checkExpKind mode rn_ty (mkClassPred ipClass [n',ty'])
                           constraintKind exp_kind }

tcHsType mode rn_ty@(HsStarTy _) exp_kind
  -- Desugaring 'HsStarTy' to 'Data.Kind.Type' here means that we don't
  -- have to handle it in 'coreView'
  = checkExpKind mode rn_ty liftedTypeKind liftedTypeKind exp_kind

--------- Literals
tcHsType mode rn_ty@(HsTyLit _ lit) exp_kind
  = tc_hs_lit_ty mode rn_ty lit exp_kind

--------- Wildcards

tcHsType mode ty@(HsWildCardTy _) ek
  | Just (_, HM_VTA) <- mode_holes mode
  , not (isEmptyNameEnv (mode_indices mode))
  = failWithTc $ TcRnRetainedQuantificationError RetainedPartialTypeArgument
  | otherwise
  = do k <- expTypeToType ek
       tcAnonWildCardOcc NoExtraConstraint mode ty k

--------- Type applications
tcHsType mode rn_ty@(HsTyVar{})     exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsAppTy{})     exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsAppKindTy{}) exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsOpTy{})      exp_kind = tc_app_ty mode rn_ty exp_kind

tcHsType mode rn_ty@(HsKindSig _ ty sig) exp_kind
  = do { let mode' = mode { mode_tyki = KindLevel
                            , mode_pending_calls = Nothing
                            , mode_complete_type_arg = Nothing }
       ; checked_kind <- tc_lhs_kind_sig mode' KindSigCtxt sig
                 -- We must typecheck the kind signature, and solve all
                 -- its equalities etc; from this point on we may do
                 -- things like instantiate its foralls, so it needs
                 -- to be fully determined (#14904)
       ; sig' <- infer_known_arrow checked_kind
       ; traceTc "tcHsType:sig" (ppr ty $$ ppr sig')
       ; ty' <- tcAddKindSigPlaceholders sig $
                tc_check_lhs_type mode ty sig'
       ; checkExpKind mode rn_ty ty' sig' exp_kind }
  where
    -- Keep the separate kind-shape check required by #14904. A declared arrow is
    -- enough evidence to select U here, without delaying the M default for an
    -- annotation whose subject has an undetermined kind.
    infer_known_arrow checked_kind
      | TypeLevel <- mode_tyki mode
      , not (mode_declaration mode)
      , isEmptyNameEnv (mode_indices mode)
      , matchabilityArrow sig
      , L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))) <- ignoreParens ty
      , FunTy flag _ mult domain result <- checked_kind
      = do { thing <- tcLookup name
           ; known <- traverse (liftZonkM . zonkTcType) (declared_kind thing)
           ; return $ case known >>= tcSplitFunTy_maybe of
               Just (ma, _, _) | ma `tcEqType` unmatchableDataConTy ->
                 FunTy flag ma mult domain result
               _ -> checked_kind }
      | otherwise = pure checked_kind

    declared_kind (ATyVar _ tv) = Just (tyVarKind tv)
    declared_kind (AGlobal (ATyCon tc))
      | tyConArity tc == 0 = Just (tyConKind tc)
    declared_kind _ = Nothing

-- The case alias needs the checked pattern from its enclosing family equation.
-- See Note [Type case aliases] in GHC.Tc.TyCl.
tcHsType mode src@(XHsType (HsTypeExistsRn flag binder body)) exp_kind
  = do { skol <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn [unLoc binder]))
       ; (binders, body') <- tcExplicitTKBndrs skol [binder] $
           tcLHsType mode body (Check liftedTypeKind)
       ; case binders of
           [Bndr tv _] -> do
             setSrcSpanA (getLoc binder) $ do
               kind <- liftZonkM $ zonkTcType (varType tv)
               unless (typeKind kind `tcEqType` liftedTypeKind) $
                 failWithTc (TcRnExistentialBinderKind kind)
             checkExpKind mode src (ExistsTy flag tv body') liftedTypeKind exp_kind
           _ -> panic "tcHsType: existential binder count" }

tcHsType mode (XHsType (HsGadtFieldRn field)) exp_kind =
  tcLHsType mode (cdf_type field) exp_kind
tcHsType _ (XHsType HsTypeCaseAliasRn{}) _ = failWithTc TcRnTypeCaseContext
tcHsType mode src@(XHsType (HsTypeCaseRn scrut rows)) exp_kind =
  tcHsTypeCase mode src scrut rows exp_kind
tcHsType mode src@(XHsType (HsTypeLambdaQuoteRn binders body)) exp_kind =
  tcHsTypeLambda Nothing mode src binders body exp_kind
tcHsType mode src@(XHsType (HsTypeLambdaCheckRn binders body helper)) exp_kind =
  case splitHsAppTys (unLoc helper) of
    (L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ syn_nm))), hs_cap_args)
      | Just cap_nms <- traverse extract_cap_nm hs_cap_args ->
          tcHsTypeLambda (Just (syn_nm, cap_nms)) mode src binders body exp_kind
    _ -> failWithTc TcRnTypeLambdaContext
  where
    extract_cap_nm (HsValArg _ (L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ nm))))) = Just nm
    extract_cap_nm _ = Nothing

tcHsType mode src@(XHsType (HsTypeLetProductCheckRn (L loc name) arity rhs)) exp_kind = do
  parent <- setSrcSpanA loc $ do
    thing <- tcLookup name
    con <- case thing of
      AGlobal (AConLike (RealDataCon con)) -> pure con
      APromotionErr err -> promotionErr name err
      _ -> wrongThingErr WrongThingDataCon thing name
    let parent = dataConTyCon con
    unless (isVanillaAlgTyCon parent
            && not (isNewTyCon parent || isTypeDataTyCon parent)
            && isVanillaDataCon con
            && null (dataConRepArgBinders con)
            && tyConSingleDataCon_maybe parent == Just con) $
      failWithTc (TcRnTypeLetInvalidConstructor con)
    let expected = dataConSourceArity con
    checkTc (arity == expected)
      (TcRnArityMismatch (AConLike (RealDataCon con)) expected arity)
    pure parent
  (_, params) <- newMetaTyVars (tyConTyVars parent)
  let product_kind = mkTyConApp parent (mkTyVarTys params)
  rhs' <- tcLHsType mode rhs (Check product_kind)
  checkExpKind mode src rhs' product_kind exp_kind

-- See Note [Typechecking type lets].
tcHsType mode (XHsType (HsTypeLetRn _ (L _ binder) rhs body)) exp_kind = do
  (_, in_kc) <- classifySynthSite mode
  let check_case_metas = not in_kc && letUsedInCaseRn binder body
      solve_rhs = if check_case_metas then solveEqualities "tcHsTypeLet" else id
  before_metas <- if check_case_metas then ambientMetaTyVars exp_kind else pure emptyVarSet
  (checked_rhs, checked_kind) <- solve_rhs $ do
    (rhs_ty, rhs_kind) <- tcInferTypeLetRhs mode rhs
    if isTypeLetPairOcc (nameOccName binder)
      then do
        first_kind <- newMetaKindVar
        second_kind <- newMetaKindVar
        let pair_kind = mkTyConApp (tupleTyCon Boxed 2) [first_kind, second_kind]
        rhs' <- setSrcSpanA (getLoc rhs) $
          checkExpKind mode (unLoc rhs) rhs_ty rhs_kind (Check pair_kind)
        pure (rhs', pair_kind)
      else pure (rhs_ty, rhs_kind)
  rhs_metas <- if check_case_metas
    then do
      tys_z <- liftZonkM (zonkTcTypes [checked_rhs, checked_kind])
      pure [ tv | tv <- tyCoVarsOfTypesList tys_z
                , isMetaTyVar tv
                , not (tyVarKind tv `tcEqType` matchabilityTy)
                , not (tv `elemVarSet` before_metas) ]
    else pure []
  res <- updLclCtxt (tcExtendLocalTypeEnv [(binder, ATcTypeLet binder checked_rhs checked_kind)]) $
           tcLHsType mode body exp_kind
  when (not (null rhs_metas)) $ do
    after_env <- ambientMetaTyVars exp_kind
    zonked <- liftZonkM (zonkTcTypes (mkTyVarTys rhs_metas))
    when (anyVarSet (\tv -> isMetaTyVar tv && not (tv `elemVarSet` after_env)) (tyCoVarsOfTypes zonked)) $
      setSrcSpan (getLocA rhs) $ failWithTc $ mkTcRnUnknownMessage $ mkPlainError [] $
        text "A type let used in a case body cannot generalize the kind of its right-hand side."
  pure res

{- Note [Typechecking type lets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kind check the RHS once per pass in the enclosing scope. Check it even
when the body does not use the name. Every use shares the checked Type and its kind
metavariables. The binder can shadow an outer type name. The renamer
gives each binder its own Name and keeps the new name out of its RHS.
Read the Type at each occurrence. The let itself never enters Core.

Check the kind of a pair pattern's scrutinee before binding its fields.
Use the source RHS and span in the kind error. The RHS inference above
still instantiates the checked captures of any lifted type case.
-}

-- See Note [Typechecking HsCoreTys]
tcHsType mode rn_ty@(XHsType (HsCoreTyRn ty)) exp_kind
  = do env <- getLclEnv
       -- Raw uniques since we go from NameEnv to TvSubstEnv.
       let subst_prs :: [(Unique, TcType)]
           subst_prs = [ (getUnique nm, mkTyVarTy tv)
                       | ATyVar nm tv <- nonDetNameEnvElts (getLclEnvTypeEnv env) ]
           in_scope = mkInScopeSet $
                      tyCoVarsOfTypes (ty : map snd subst_prs)
           subst = mkTvSubst in_scope (listToUFM_Directly subst_prs)
           ty'   = substTy subst ty
       checkExpKind mode rn_ty ty' (typeKind ty') exp_kind

tc_hs_lit_ty :: TcTyMode
             -> HsType GhcRn
             -> HsLit GhcRn
             -> ExpKind
             -> TcM TcType
tc_hs_lit_ty mode rn_ty (HsNatural _ lit) exp_kind
  = do let n = il_value lit
       -- Ensure that a type-level integer is nonnegative (#8306, #8412, #26861)
       when (n < 0) $
         addErr $ TcRnNegativeNumTypeLiteral lit
       checkWiredInTyCon naturalTyCon
       let raw = mkNumLitTy n
       converted <- if n < 0 then pure Nothing else tcConvertedNumeral mode exp_kind raw
       let ty = fromMaybe raw converted
       checkExpKind mode rn_ty ty (typeKind ty) exp_kind
tc_hs_lit_ty mode rn_ty (HsString _ s) exp_kind
  = do checkWiredInTyCon typeSymbolKindCon
       checkExpKind mode rn_ty (mkStrLitTy $ mkFastStringShortText s) typeSymbolKind exp_kind
tc_hs_lit_ty mode rn_ty (HsChar _ c) exp_kind
  = do checkWiredInTyCon charTyCon
       checkExpKind mode rn_ty (mkCharLitTy c) charTy exp_kind
tc_hs_lit_ty mode rn_ty lit@(HsCharPrim _ c) exp_kind
  = do { native <- xoptM LangExt.DependentTypes
       ; if native
           then do { checkWiredInTyCon charTyCon
                   ; checkExpKind mode rn_ty (mkCharPrimLitTy c) charPrimTy exp_kind }
           else failWithTc (TcRnUnpromotableLit lit) }
tc_hs_lit_ty mode rn_ty lit exp_kind
  | Just (mk_prim_lit, kind) <- prim_lit lit
  = do { native <- xoptM LangExt.DependentTypes
       ; if native
           then do { platform <- targetPlatform <$> getDynFlags
                   ; checkExpKind mode rn_ty (LitTy (mk_prim_lit platform)) kind exp_kind }
           else failWithTc (TcRnUnpromotableLit lit) }
  | otherwise = failWithTc (TcRnUnpromotableLit lit)
  where
    prim_lit (HsIntPrim _ n) =
      Just (\platform -> IntPrimTyLit (litValue (mkLitIntWrap platform n)), intPrimTy)
    prim_lit (HsWordPrim _ n) =
      Just (\platform -> WordPrimTyLit (litValue (mkLitWordWrap platform n)), wordPrimTy)
    prim_lit _ = Nothing

-- | Elaborate a literal at a known result kind. See Note [Registered type numerals].
tcConvertedNumeral :: TcTyMode -> ExpKind -> Type -> TcM (Maybe Type)
tcConvertedNumeral mode (Check expected) raw
  | Just (_, HM_FamPat{}) <- mode_holes mode = pure Nothing
  | otherwise = do
      enabled <- xoptM LangExt.DependentTypes
      if not enabled then pure Nothing else do
        target <- liftZonkM (zonkTcType expected)
        case splitTyConApp_maybe target of
          Just (tc, args)
            | tc /= naturalTyCon, isAlgTyCon tc, isLiftedAlgTyCon tc
            , length args == tyConArity tc, noFreeVarsOfType target ->
                registered target
          _ -> pure Nothing
  where
    registered target = do
      name <- lookupOrig gHC_INTERNAL_TYPENATS (mkTcOcc "FromNatural")
      envs <- tcGetFamInstEnvs
      env <- getGblEnv
      let visible inst = let m = nameModule (getName inst)
                         in m == tcg_semantic_mod env || m `elem` imp_finsts (tcg_imports env)
      case filter visible (familyNameInstances envs name) of
        inst : _ | let fam = famInstTyCon inst
                       args = [target, raw]
                 , isTypeFamilyTyCon fam, tyConArity fam == 2
                 , any (visible . fim_instance) (lookupFamInstEnv envs fam args)
                 -> do marker <- lookupOrig gHC_INTERNAL_TYPENATS (mkTcOcc "NaturalLiteral")
                       tc <- tcLookupTyCon marker
                       pure (Just (mkTyConApp tc args))
        _ -> pure Nothing
tcConvertedNumeral _ (Infer _) _ = pure Nothing

{- Note [Registered type numerals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Core literal 3 always has kind Natural. Suppose T is a user-defined
Peano type, and a module in the import closure provides this equation:

  type instance FromNatural T n = ToPeano n

In a dependent module, when a type signature expects T, elaborate 3 to
FromNatural T (3 :: Natural). The instance and the ordinary family
reducer determine its value. This follows the explicit conversion in
Agda's FROMNAT and the promoted FromInteger in singletons. It does not
require a change to the Core literal or to runtime number conversion.

Keep the converted type inside the ordinary synonym NaturalLiteral T 3
until reduction needs its expansion. GHC.Iface.Type can then display
the source numeral with its kind in an expected type. An explicitly
written FromNatural application can be stuck even without a registered
instance. Do not print that application as a source numeral.

Use the usual Natural literal during inference and when checking family
patterns. Family applications cannot occur in a family pattern. A missing
registration leaves the ordinary kind mismatch at the source numeral.
Do not evaluate the instance RHS when deciding whether the syntax applies.

Find the family by its defining Name in the instance environment. This
also works when boot libraries contain no FromNatural declaration. Limit
the witness to instances in scope here. Loading an unrelated interface
can put more instances in the external package state. See Note [The
type family instance consistency story] in GHC.Tc.Instance.Family.
-}

tc_hs_tuple_ty :: HsType GhcRn
               -> TcTyMode
               -> HsTupleSort
               -> [LHsType GhcRn]
               -> TcKind
               -> TcM TcType
-- See Note [Distinguishing tuple kinds] in GHC.Hs.Type
-- See Note [Inferring tuple kinds]
tc_hs_tuple_ty rn_ty mode HsBoxedOrConstraintTuple hs_tys exp_kind
     -- (NB: not zonking before looking at exp_k, to avoid left-right bias)
  | Just tup_sort <- tupKindSort_maybe exp_kind
  = traceTc "tcHsType tuple" (ppr hs_tys) >>
    tc_tuple rn_ty mode tup_sort hs_tys exp_kind
  | otherwise
  = do { traceTc "tcHsType tuple 2" (ppr hs_tys)
       ; (tys, kinds) <- mapAndUnzipM (tc_infer_lhs_type mode) hs_tys
       ; kinds <- liftZonkM $ mapM zonkTcType kinds
           -- Infer each arg type separately, because errors can be
           -- confusing if we give them a shared kind.  Eg #7410
           -- (Either Int, Int), we do not want to get an error saying
           -- "the second argument of a tuple should have kind *->*"

       ; let (arg_kind, tup_sort)
               = case [ (k,s) | k <- kinds
                              , Just s <- [tupKindSort_maybe k] ] of
                    ((k,s) : _) -> (k,s)
                    [] -> (liftedTypeKind, BoxedTuple)
         -- In the [] case, it's not clear what the kind is, so guess *

       ; tys' <- sequence [ setSrcSpanA loc $
                            checkExpectedKind mode hs_ty ty kind arg_kind
                          | ((L loc hs_ty),ty,kind) <- zip3 hs_tys tys kinds ]

       ; finish_tuple mode rn_ty tup_sort tys' (map (const arg_kind) tys') exp_kind }
tc_hs_tuple_ty rn_ty mode HsUnboxedTuple tys exp_kind
  | isTypeLevel (mode_tyki mode)
  = do { edt <- xoptM LangExt.DependentTypes
       ; ubx <- xoptM LangExt.UnboxedTuples
       ; if edt && ubx
           then tc_promoted_or_unboxed_tuple rn_ty mode tys exp_kind
           else tc_tuple rn_ty mode UnboxedTuple tys exp_kind }
tc_hs_tuple_ty rn_ty mode HsUnboxedTuple tys exp_kind =
    tc_tuple rn_ty mode UnboxedTuple tys exp_kind

-- | Elaborate @(# t1, ..., tn #)@ as a promoted unboxed tuple value when the
-- expected kind is @(# k1, ..., kn #)@ or when all inferred elements are
-- promoted values of lifted kinds. Fall back to the unboxed tuple type form
-- otherwise. See Note [Unboxed Char# and unboxed tuple promotion].
tc_promoted_or_unboxed_tuple
  :: HsType GhcRn -> TcTyMode -> [LHsType GhcRn] -> TcKind -> TcM TcType
tc_promoted_or_unboxed_tuple rn_ty mode tys exp_kind = do
  exp_kind' <- liftZonkM (zonkTcType exp_kind)
  case tcSplitTyConApp_maybe exp_kind' of
    Just (tc, kind_args)
      | isUnboxedTupleTyCon tc
      , let tup_arity = tyConArity tc `div` 2
      , arity == tup_arity
      -> do { checkTupSize arity
            ; let (rep_tys, elem_kinds) = splitAt tup_arity kind_args
                  reps = replicate arity liftedRepTy
            ; zipWithM_ (\hs_ty rep_ty ->
                discardResult $
                  unifyKind (Just (HsTypeRnThing (unLoc hs_ty))) rep_ty liftedRepTy)
                tys rep_tys
            ; tau_tys <- zipWithM (tc_check_lhs_type mode) tys elem_kinds
            ; let prom_tc  = promotedTupleDataCon Unboxed arity
                  prom_ty  = mkTyConApp prom_tc (reps ++ elem_kinds ++ tau_tys)
                  act_kind = mkTyConApp tc (reps ++ elem_kinds)
            ; checkExpectedKind mode rn_ty prom_ty act_kind exp_kind }
    _ | Just tv <- getTyVar_maybe exp_kind'
      , isMetaTyVar tv
      , arity > 0
      -> do { (tau_tys, tau_kinds) <- mapAndUnzipM (tc_infer_lhs_type mode) tys
            ; tau_kinds' <- liftZonkM (mapM zonkTcType tau_kinds)
            ; if all is_promoted_lifted_elem_kind tau_kinds'
                then do { checkTupSize arity
                        ; let reps     = replicate arity liftedRepTy
                              prom_tc  = promotedTupleDataCon Unboxed arity
                              tup_tc   = tupleTyCon Unboxed arity
                              prom_ty  = mkTyConApp prom_tc (reps ++ tau_kinds' ++ tau_tys)
                              act_kind = mkTyConApp tup_tc (reps ++ tau_kinds')
                        ; checkExpectedKind mode rn_ty prom_ty act_kind exp_kind }
                else finish_tuple mode rn_ty UnboxedTuple tau_tys tau_kinds' exp_kind }
    _ -> tc_tuple rn_ty mode UnboxedTuple tys exp_kind
  where
    arity = length tys
    is_promoted_lifted_elem_kind k =
      isNothing (kindRep_maybe k) && tcIsLiftedTypeKind (typeKind k)

{- Note [Unboxed Char# and unboxed tuple promotion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under DependentTypes (and DependentTypes), GHC extends DataKinds
promotion to two unboxed value forms:

1. Primitive character literals and C# pattern binders:
   * A type-level primitive character literal such as 'a'# elaborates to
     mkCharPrimLitTy 'a' (TyConApp charPrimLitTyCon [LitTy (CharTyLit 'a')])
     of kind Char#. Because charPrimLitTyCon is a wired-in generative nominal
     PrimTyCon, 'C# 'a'# :: Char unifies and proves apartness in closed type
     families without adding a new IfaceTyLit tag.
   * Closed type family patterns may match 'C# c, 'C# _, and 'C# (c :: Char#)
     using the scope-restricted primitive pattern-binder judgment in
     Note [Primitive family pattern binders] in GHC.Core.Type.

2. Promoted unboxed tuples at lifted element kinds (when UnboxedTuples is
   enabled):
   * At KindLevel, (# a, b #) defaults element kinds to Type (liftedRepTy) and
     tc_fun_type_x permits lifted unboxed tuple kinds (# a, b #) on either side
     of a kind arrow (for example, type FstU :: (# a, b #) -> a and
     type SwapU :: (# a, b #) -> (# b, a #)).
   * At TypeLevel, (# x, y #) (punned) and '(# x, y #) (explicitly ticked)
     elaborate to promotedTupleDataCon Unboxed arity with RuntimeRep arguments
     fixed to LiftedRep, yielding kind (# a, b #). When the expected kind is an
     unconstrained metavariable, (# t1, t2 #) promotes only when every element
     has a value kind k :: Type (keeping (# Int, Bool #) as an unboxed tuple
     type).
-}

{-
Note [Kind-checking explicit lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type, suppose we have an application (F [t1,t2]),
where [t1,t2] is an explicit list, and
   F :: [ki] -> blah

Then we want to return the type
   F ((:) @ki t2 ((:) @ki t2 ([] @ki)))
where the argument list is instantiated to F's argument kind `ki`.

But what about (G []), where
   G :: (forall k. [k]) -> blah

Here we want to return (G []), with no instantiation at all.  But since we have
no lambda in types, we must be careful not to instantiate that `[]`, because we
can't re-generalise it.  Hence, when kind-checking an explicit list, we need a
special case for `[]`.

Note [Variable Specificity and Forall Visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A HsForAllTy contains an HsForAllTelescope to denote the visibility of the forall
binder. Furthermore, each invisible type variable binder also has a
Specificity. Together, these determine the variable binders (ForAllTyFlag) for each
variable in the generated ForAllTy type.

This table summarises this relation:
----------------------------------------------------------------------------
| User-written type         HsForAllTelescope   Specificity        ForAllTyFlag
|---------------------------------------------------------------------------
| f :: forall a. type       HsForAllInvis       SpecifiedSpec      Specified
| f :: forall {a}. type     HsForAllInvis       InferredSpec       Inferred
| f :: forall a -> type     HsForAllVis         SpecifiedSpec      Required
| f :: forall {a} -> type   HsForAllVis         InferredSpec       /
|   This last form is nonsensical and is thus rejected.
----------------------------------------------------------------------------

For more information regarding the interpretation of the resulting ForAllTyFlag, see
Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility] in "GHC.Core.TyCo.Rep".
-}

------------------------------------------

tc_mult :: TcTyMode -> HsModifiedFunArr GhcRn -> TcM (Maybe Mult)
tc_mult mode (HsModifiedFunArr _ mods arr) = do
  mMult <- tc_modifiers_mult mode mods
  case (arr, mMult) of
    (HsStandardArr _, _) -> pure mMult
    (HsLinearArr _, Nothing) -> pure $ Just oneDataConTy
    (HsLinearArr _, Just _) -> failWithTc TcRnTooManyMultiplicities

-- | The two modifier kinds that can annotate a function arrow.
data ArrowModifier = ArrowMultiplicity | ArrowMatchability

-- | Check the arrow annotations. See Note [Arrow matchability modifiers].
tc_arrow_modifiers :: TcTyMode -> HsModifiedFunArr GhcRn
                   -> TcM (Maybe Mult, Maybe Matchability)
tc_arrow_modifiers _ (HsModifiedFunArr _ [] arr) =
  case arr of
    HsStandardArr _ -> pure (Nothing, Nothing)
    HsLinearArr _   -> pure (Just oneDataConTy, Nothing)
tc_arrow_modifiers mode ann@(HsModifiedFunArr _ mods arr) = do
  modifiers <- xoptM LangExt.Modifiers
  if not modifiers then (, Nothing) <$> tc_mult mode ann else do
    linear_types <- xoptM LangExt.LinearTypes
    let classify k
          | tcEqType k matchabilityTy = Right ArrowMatchability
          | isMultiplicityTy k && linear_types = Right ArrowMultiplicity
          | isMultiplicityTy k = Left SuggestLinear
          | otherwise = Left DontSuggestLinear
    (mult, ma) <- foldM (addModifier classify) (Nothing, Nothing) mods
    case (arr, mult) of
      (HsLinearArr _, Nothing) -> pure (Just oneDataConTy, ma)
      (HsLinearArr _, Just _) -> failWithTc TcRnTooManyMultiplicities
      (HsStandardArr _, _) -> pure (mult, ma)
  where
    addModifier classify (mult, ma) modifier = setSrcSpanA (getLoc modifier) $ do
      found <- tc_modifier_with mode modifier Nothing classify
      case found of
        Nothing -> pure (mult, ma)
        Just (ArrowMultiplicity, m)
          | isNothing mult -> pure (Just m, ma)
          | otherwise -> failWithTc TcRnTooManyMultiplicities
        Just (ArrowMatchability, m)
          | isNothing ma -> pure (mult, Just m)
          | otherwise -> failWithTc TcRnTooManyMatchabilities

{- Note [Arrow matchability modifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With Modifiers, an arrow can have one annotation of kind Matchability.
With LinearTypes, it can also have one of kind Multiplicity. Infer each
modifier's kind once so the order of these annotations is immaterial.
An explicit matchability takes precedence over the context's default.
For example, Map can specify an unmatchable callback as (a %U -> b).
A constructor field arrow must still be matchable.

With NoModifiers, keep checking the multiplicity against the expected
kind. Older linear signatures can use %p without a separate kind for p.
-}

-- | Extract the optional Multiplicity modifier from a list. Nothing if none of
-- them are Multiplicities, error if more than one is. Non-Multiplicity
-- modifiers generate warnings but are otherwise ignored.
--
-- See Note [Typechecking Multiplicity modifiers].
tc_modifiers_mult :: TcTyMode -> [LHsModifier GhcRn] -> TcM (Maybe Mult)
tc_modifiers_mult mode mods = do
  modifiers <- xoptM LangExt.Modifiers
  linearTypes <- xoptM LangExt.LinearTypes
  case (linearTypes, modifiers) of
    (False, _) ->
      go_infer $ \k -> if isMultiplicityTy k
        then Left SuggestLinear
        else Left DontSuggestLinear
    (True, False) -> go_check
    (True, True) ->
      go_infer $ \k -> if isMultiplicityTy k
        then Right ()
        else Left DontSuggestLinear
  where
    go_infer is_mult = do
      mults <- tc_modifiers mode mods (Just multiplicityTyConName) is_mult
      case mults of
        [] -> pure Nothing
        [m] -> pure $ Just m
        _ -> failWithTc TcRnTooManyMultiplicities

    go_check = case mods of
      [] -> pure Nothing
      [L _ (HsModifier _ m)] -> Just <$> tc_check_lhs_type mode m multiplicityTy
      _ -> failWithTc TcRnTooManyMultiplicities

-- | Typecheck a single modifier.
--
-- If the modifier's kind is unknown, emit an error, optionally suggesting the
-- name of a kind to give as a type signature.
--
-- If it has a known kind, it's passed to the @check_expected_kind@ argument to
-- determine if the modifier is recognised in this context. @'Right' '()'@
-- indicates that it's recognised, and we return 'Just' the modifier's type.
-- @'Left' 'DontSuggestLinear'@ indicates that it's not recognised, and we
-- return 'Nothing'. @'Left' 'SuggestLinear' indicates that it's not recognised,
-- but the user may want to enable @-XLinearTypes@.
--
-- For example, when typechecking function arrows, this argument is
--
-- > if isMultiplicityTy k then Right () else Left DontSuggestLinear
--
-- That means that if we see
--
-- * @Int %m -> Int@, we emit an error (unless @m@'s kind is known from context)
-- * @Int %(m :: Multiplicity) -> Int@, we return @'Just' m@
-- * @Int %True -> Int@, we return 'Nothing' and emit a warning.
tc_modifier :: TcTyMode
            -> LHsModifier GhcRn
            -> Maybe Name -- ^ If modifier has unknown kind, suggest this one.
            -> (TcKind -> Either SuggestLinear ())
               -- ^ Given the Kind of a modifier, is the modifier expected?
            -> TcM (Maybe TcType)
tc_modifier mode modifier mSuggestKind check_expected_kind =
  fmap (fmap snd) $ tc_modifier_with mode modifier mSuggestKind check_expected_kind

tc_modifier_with :: TcTyMode -> LHsModifier GhcRn -> Maybe Name
                 -> (TcKind -> Either SuggestLinear a) -> TcM (Maybe (a, TcType))
tc_modifier_with mode (L _ mod@(HsModifier modPrintsAs ty)) mSuggestKind check_expected_kind = do
  (inf_ty, inf_kind) <- tc_infer_lhs_type mode ty
  -- bug: zonking here means that (1) is rejected, but (2) is accepted.
  --     (1): Int %(m :: Multiplicity) -> Int %m -> Int
  --     (2): Int %m -> Int %(m :: Multiplicity) -> Int
  inf_kind' <- liftZonkM $ zonkTcType inf_kind
  case inf_kind' of
    -- This checks for a modifier of unknown kind. It doesn't detect poly-kinded
    -- modifiers. e.g. %Just and %Nothing don't fail here, and possibly they
    -- should.
    TyVarTy _ -> failWithTc $ TcRnUnknownModifierKind mod mSuggestKind
    _ -> case check_expected_kind inf_kind' of
      Right tag -> return $ Just (tag, inf_ty)
      Left suggestLinear -> do
        linearTypes <- xoptM LangExt.LinearTypes
        warn_unrecognised <- woptM Opt_WarnUnrecognisedModifiers
        let suggestLinear' = case modPrintsAs of
              ModifierPrintsAs1 | not linearTypes -> SuggestLinear
              _ -> suggestLinear
        diagnosticTc warn_unrecognised $ TcRnUnrecognisedModifier mod suggestLinear'
        pure Nothing

tc_modifiers :: TcTyMode
             -> [LHsModifier GhcRn]
             -> Maybe Name
             -> (TcKind -> Either SuggestLinear ())
             -> TcM [TcType]
tc_modifiers mode mods mSuggestName is_expected_kind =
  catMaybes <$> mapM (\m -> tc_modifier mode m mSuggestName is_expected_kind) mods

{-
Note [Typechecking Multiplicity modifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When looking for Multiplicity modifiers, we need to consider various
combinations of extensions.

- -XLinearTypes -XModifiers is the basic case. We infer the kind of each
   modifier, modifier to see whether it's a Multiplicity. If there's more than
   one Multiplicity modifier, we throw an error.

     %One %True           => Just One; warning (unrecognised modifier True)
     %True                => Nothing; warning (unrecognised modifier True)
     %One %One            => error (too many multiplicities)
     %(m :: Multiplicity) => Just m
     %m                   => error (modifier m has unknown kind)

- With -XLinearTypes -XNoModifiers, we permit at most one modfier. We /check/
  that its kind is Multiplicity, rather than inferring, which has less need for
  type annotations. This is for backwards compatibility with the days when
  LinearTypes existed but Modifiers didn't.

     %One %True           => error (too many multiplicities)
     %True                => error (True is not of kind Multiplicity)
     %(m :: Multiplicity) => Just m
     %m                   => Just m

- With -XNoLinearTypes, we never return a Multiplicity, and we warn about all
  modifiers. We don't need to check for -XModifiers, because with
  -XNoLinearTypes -XNoModifiers, there won't be any modifiers to typecheck in
  the first place.

     %One %True => Nothing; warning (unrecognised modifier One, suggest enabling
                   -XLinearTypes); warning (unrecognised modifier True)
     %m         => error (modifier m has unknown kind)
-}

------------------------------------------
tc_fun_type :: TcTyMode -> HsModifiedFunArr GhcRn -> LHsType GhcRn -> LHsType GhcRn -> ExpKind
            -> TcM TcType
tc_fun_type mode = tc_fun_type_x (tc_check_lhs_type mode) default_ma mode
  where
    default_ma = case mode_callback_arrow mode of
      FixedTypeArrowMatchability ma -> pure ma
      _                             -> pure unmatchableDataConTy

-- | Recognize the existing kind scheme used by the inferred arrow index.
inferredMatchabilityIndex :: TyConBinder -> Bool
inferredMatchabilityIndex b =
  isInferredTyConBinder b && tcEqType (tyVarKind (binderVar b)) matchabilityTy

-- | A receiver can learn its index from a kind in its checked telescope.
familyKindUsesMatchability :: TyCon -> TyConBinder -> Bool
familyKindUsesMatchability tc b =
  any (elemVarSet (binderVar b) . tyCoVarsOfType)
      (tyConResKind tc : map (tyVarKind . binderVar) (tyConBinders tc))

-- | Does the index annotate a curried arrow in a synonym parameter's kind?
synonymCallbackIndex :: TyVar -> Kind -> Bool
synonymCallbackIndex tv (FunTy { ft_af = af, ft_ma = arrow, ft_res = result })
  | isVisibleFunArg af =
      case arrow of
        TyVarTy index | index == tv -> True
        _ -> synonymCallbackIndex tv result
synonymCallbackIndex _ _ = False

-- | Check a synonym RHS with controlled instantiation of metadata.
-- The flag for each index records whether it occurs on a callback arrow.
tcCheckSynonymMetadata :: Maybe Name -> (Name -> [Bool] -> TcM [Type]) -> Maybe (TypeOrKind -> TcM Matchability)
                       -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckSynonymMetadata owner get_indices callback_arrow rhs kind =
  addTypeCtxt rhs $
    tc_check_lhs_type
      (typeLevelMode { mode_synonym_metadata = Just (get_indices, callback_arrow)
                     , mode_source_proof_owner = owner }) rhs kind

-- | Check an ordinary synonym RHS and retain references to checked aliases.
tcCheckSynonymRhs :: Name -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckSynonymRhs owner rhs kind =
  addTypeCtxt rhs $
    tc_check_lhs_type (typeLevelMode { mode_source_proof_owner = Just owner }) rhs kind

hasInferredMatchability :: TyCon -> Bool
hasInferredMatchability tc
  | b : _ <- tyConBinders tc, inferredMatchabilityIndex b =
      case tyConFlavour tc of
        ClosedTypeFamilyFlavour -> True
        OpenFamilyFlavour IAmType _ -> True
        TypeSynonymFlavour -> True
        _ -> False
  | otherwise = False

-- | Check an omitted arrow or pass the index directly through a type alias.
tcCheckMatchabilitySynRhs :: TcType -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckMatchabilitySynRhs ma hs_ty kind
  | matchabilityArrow hs_ty = tcCheckMatchabilityArrow ma hs_ty kind
  | otherwise =
      tcCheckIndexedMatchability
        (typeLevelMode { mode_callback_arrow = FixedTypeArrowMatchability ma })
        (pure ma) hs_ty kind

-- | Supply metadata to a direct application of a declared indexed type.
tcCheckIndexedMatchability :: TcTyMode -> TcM TcType -> LHsType GhcRn
                            -> TcKind -> TcM TcType
tcCheckIndexedMatchability mode index hs_ty@(L loc _) kind
  | (hs_fun@(L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name)))), hs_args)
      <- splitHsAppTys (unLoc (ignoreParens hs_ty))
  , isTyConName name =
      setSrcSpanA loc $ do
        (fun_ty, _) <- tcInferTyAppHead mode hs_fun hs_args
        case fun_ty of
          TyConApp tc [] | hasInferredMatchability tc -> do
            ma <- index
            (ty, inferred) <- tcInferTyApps mode hs_fun (mkTyConApp tc [ma]) hs_args
            checkExpKind mode (unLoc hs_ty) ty inferred (Check kind)
          _ -> ordinary
  | otherwise = ordinary
  where
    ordinary = tc_check_lhs_type mode hs_ty kind

-- | Match the source arrow whose metadata can come from a declaration.
matchabilityArrow :: LHsType GhcRn -> Bool
matchabilityArrow hs_ty
  | L _ (HsFunTy _ (HsModifiedFunArr _ [] HsStandardArr{}) _ _) <- ignoreParens hs_ty
  = True
  | otherwise = False

-- | Check one omitted arrow using its declaration's metadata index.
tcCheckMatchabilityArrow :: TcType -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckMatchabilityArrow ma hs_ty kind
  | L l (HsFunTy _ arr a b) <- ignoreParens hs_ty
  , matchabilityArrow hs_ty
  = setSrcSpanA l $
      tc_fun_type_x (tcCheckMatchabilitySynRhs ma) (pure ma) typeLevelMode arr a b (Check kind)
  | otherwise = tcCheckLHsType hs_ty kind

{- Note [Checking local lambda captures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A local type lambda is lowered to an application of a private synonym
and closed family. If an enclosing variable has no written kind, the
helper can infer a more general capture telescope than the original
body permits. A family equation can restrict that telescope only once
the family is applied. Checking `Proxy (\x -> ...)` must also check
the body even though it does not apply the family.

Keep the original body with its lexical Names until we check the local
signature. Use the helper's argument kinds for fresh rigid parameters
in a nested implication. Check the original body with the ordinary
signature skolems still in scope. The helper still supplies the
resulting type and its existing equality rules.

Restrict the recheck to ordinary types. A type case can require
branch-local equalities. Splices, source terms, and generated source
families can run compile-time code. The renamer refuses these bodies
when formation would require this extra check.
-}

-- | Check a required forall with the supplied checker for its body.
-- See Note [Body kind of a HsForAllTy] and Note [Retained Pi type formation] in GHC.Rename.HsType.
tc_vis_forall_type :: TcTyMode -> HsType GhcRn -> HsForAllTelescope GhcRn
                   -> (TcKind -> TcM TcType) -> ExpKind -> TcM TcType
tc_vis_forall_type mode ty tele check_body exp_kind
  = do { ek <- newOpenTypeKind
       ; (tv_bndrs, body) <- tcTKTelescope mode tele (check_body ek)
       -- Keep the caller's mode and let the enclosing signature generalise kinds.
       ; checkExpKind mode ty (mkForAllTys tv_bndrs body) ek exp_kind }

-- | Check an ordinary arrow with the supplied checker for its child types.
-- See Note [Source retained signatures].
tc_fun_type_x :: (LHsType GhcRn -> TcKind -> TcM TcType)
              -> TcM Type -> TcTyMode -> HsModifiedFunArr GhcRn -> LHsType GhcRn -> LHsType GhcRn
              -> ExpKind -> TcM TcType
tc_fun_type_x check type_level_ma mode mult ty1 ty2 exp_kind = case mode_tyki mode of
  TypeLevel ->
    do { traceTc "tc_fun_type" (ppr ty1 $$ ppr ty2)
       ; arg_k <- newOpenTypeKind
       ; res_k <- newOpenTypeKind
       ; ty1'  <- check ty1 arg_k
       ; ty2'  <- check ty2 res_k
       ; (mult', explicit_ma) <- tc_arrow_modifiers mode mult
       ; ma <- maybe type_level_ma pure explicit_ma
       ; checkExpKind mode (HsFunTy noExtField mult ty1 ty2)
                      (tcMkVisFunTy ma (fromMaybe manyDataConTy mult') ty1' ty2')
                      liftedTypeKind exp_kind }
  KindLevel ->  -- no representation polymorphism in kinds. yet.
    do { edt <- xoptM LangExt.DependentTypes
       ; ubx <- xoptM LangExt.UnboxedTuples
       ; (ty1', ty2') <-
           if edt && ubx
             then do { arg_k <- newOpenTypeKind
                     ; res_k <- newOpenTypeKind
                     ; t1 <- check ty1 arg_k
                     ; t2 <- check ty2 res_k
                     ; check_kind_fun_endpoint ty1 t1 arg_k
                     ; check_kind_fun_endpoint ty2 t2 res_k
                     ; pure (t1, t2) }
             else (,) <$> check ty1 liftedTypeKind
                      <*> check ty2 liftedTypeKind
       ; (mult', explicit_ma) <- tc_arrow_modifiers mode mult
       ; ma <- case explicit_ma of
                 Just ma -> pure ma
                 Nothing | mode_declaration mode -> newFlexiTyVarTy matchabilityTy
                         | otherwise -> pure matchableDataConTy
       ; checkExpKind mode (HsFunTy noExtField mult ty1 ty2)
                      (tcMkVisFunTy ma (fromMaybe manyDataConTy mult') ty1' ty2')
                      liftedTypeKind exp_kind }
  where
    -- Admit (# a, b #) with lifted element kinds as a kind-arrow endpoint.
    -- See Note [Unboxed Char# and unboxed tuple promotion].
    check_kind_fun_endpoint hs_end end_ty end_kind = do
      end_ty' <- liftZonkM (zonkTcType end_ty)
      unless (is_lifted_ubx_tuple end_ty') $
        discardResult $
          unifyKind (Just (HsTypeRnThing (unLoc hs_end))) end_kind liftedTypeKind

    is_lifted_ubx_tuple ty
      | Just (tc, args) <- tcSplitTyConApp_maybe ty
      , isUnboxedTupleTyCon tc
      , let reps = take (tyConArity tc `div` 2) args
      = all (`tcEqType` liftedRepTy) reps
      | otherwise = False

{- Note [Skolem escape and forall-types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Checking telescopes].

Consider
  f :: forall a. (forall kb (b :: kb). Proxy '[a, b]) -> ()

The Proxy '[a,b] forces a and b to have the same kind.  But a's
kind must be bound outside the 'forall a', and hence escapes.
We discover this by building an implication constraint for
each forall.  So the inner implication constraint will look like
    forall kb (b::kb).  kb ~ ka
where ka is a's kind.  We can't unify these two, /even/ if ka is
unification variable, because it would be untouchable inside
this inner implication.

That's what the pushLevelAndCaptureConstraints, plus subsequent
buildTvImplication/emitImplication is all about, when kind-checking
HsForAllTy.

Note that

* We don't need to /simplify/ the constraints here
  because we aren't generalising. We just capture them.

* We can't use emitResidualTvConstraint, because that has a fast-path
  for empty constraints.  We can't take that fast path here, because
  we must do the bad-telescope check even if there are no inner wanted
  constraints. See Note [Checking telescopes] in
  GHC.Tc.Types.Constraint.  Lacking this check led to #16247.
-}

{- *********************************************************************
*                                                                      *
                Tuples
*                                                                      *
********************************************************************* -}

---------------------------
tupKindSort_maybe :: TcKind -> Maybe TupleSort
tupKindSort_maybe k
  | Just (k', _) <- splitCastTy_maybe k = tupKindSort_maybe k'
  | Just k'      <- coreView k          = tupKindSort_maybe k'
  | isConstraintKind k                  = Just ConstraintTuple
  | tcIsLiftedTypeKind k                = Just BoxedTuple
  | otherwise                           = Nothing

tc_tuple :: HsType GhcRn -> TcTyMode -> TupleSort -> [LHsType GhcRn] -> TcKind -> TcM TcType
tc_tuple rn_ty mode tup_sort tys exp_kind
  = do { edt <- xoptM LangExt.DependentTypes
       ; arg_kinds <- case tup_sort of
           BoxedTuple      -> return (replicate arity liftedTypeKind)
           UnboxedTuple
             | KindLevel <- mode_tyki mode, edt
             -> return (replicate arity liftedTypeKind)
             | otherwise
             -> replicateM arity newOpenTypeKind
           ConstraintTuple -> return (replicate arity constraintKind)
       ; tau_tys <- zipWithM (tc_check_lhs_type mode) tys arg_kinds
       ; finish_tuple mode rn_ty tup_sort tau_tys arg_kinds exp_kind }
  where
    arity   = length tys

finish_tuple :: TcTyMode
             -> HsType GhcRn
             -> TupleSort
             -> [TcType]    -- ^ argument types
             -> [TcKind]    -- ^ of these kinds
             -> TcKind      -- ^ expected kind of the whole tuple
             -> TcM TcType
finish_tuple mode rn_ty tup_sort tau_tys tau_kinds exp_kind = do
  traceTc "finish_tuple" (ppr tup_sort $$ ppr tau_kinds $$ ppr exp_kind)
  case tup_sort of
    ConstraintTuple -> do
      let tycon = cTupleTyCon arity
      checkCTupSize arity
      check_expected_kind (mkTyConApp tycon tau_tys) constraintKind
    BoxedTuple -> do
      let tycon = tupleTyCon Boxed arity
      checkTupSize arity
      checkWiredInTyCon tycon
      check_expected_kind (mkTyConApp tycon tau_tys) liftedTypeKind
    UnboxedTuple -> do
      let tycon    = tupleTyCon Unboxed arity
          tau_reps = map kindRep tau_kinds
          -- See also Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
          arg_tys  = tau_reps ++ tau_tys
          res_kind = unboxedTupleKind tau_reps
      checkTupSize arity
      check_expected_kind (mkTyConApp tycon arg_tys) res_kind
  where
    arity = length tau_tys
    check_expected_kind ty act_kind =
      checkExpectedKind mode rn_ty ty act_kind exp_kind

{- *********************************************************************
*                                                                      *
                Type applications
*                                                                      *
********************************************************************* -}

splitHsAppTys_maybe :: HsType GhcRn -> Maybe (LHsType GhcRn, [LHsTypeArg GhcRn])
splitHsAppTys_maybe hs_ty
  | is_app hs_ty = Just (splitHsAppTys hs_ty)
  | otherwise    = Nothing
  where
    is_app :: HsType GhcRn -> Bool
    is_app (HsAppKindTy {})        = True
    is_app (HsAppTy {})            = True
    is_app (HsOpTy _ _ tyop _)
      | L _ (HsTyVar _ _ op) <- tyop
      , unLocWithUserRdr op `hasKey` unrestrictedFunTyConKey
      -- I'm not sure why this funTyConKey test is necessary
      -- Can it even happen?  Perhaps for   t1 `(->)` t2
      -- but then maybe it's ok to treat that like a normal
      -- application rather than using the special rule for HsFunTy
      = False
    is_app (HsOpTy {})             = True
    is_app (HsTyVar {})            = True
    is_app (HsParTy _ (L _ ty))    = is_app ty
    is_app _                       = False

splitHsAppTys :: HsType GhcRn -> (LHsType GhcRn, [LHsTypeArg GhcRn])

splitHsAppTys hs_ty = go (noLocA hs_ty) []
  where
    go :: LHsType GhcRn
       -> [HsArg GhcRn (LHsType GhcRn) (LHsKind GhcRn)]
       -> (LHsType GhcRn,
           [HsArg GhcRn (LHsType GhcRn) (LHsKind GhcRn)]) -- AZ temp
    go (L _  (HsAppTy _ f a))      as = go f (HsValArg noExtField a : as)
    go (L _  (HsAppKindTy _ ty k)) as = go ty (HsTypeArg noExtField k : as)
    go (L sp (HsParTy _ f))        as = go f (HsArgPar (locA sp) : as)
    go (L _  (HsOpTy _ l tyop r))  as =
      (tyop, HsValArg noExtField l : HsValArg noExtField r : as)
    go f as = (f, as)

---------------------------
tcInferTyAppHead :: TcTyMode -> LHsType GhcRn -> [LHsTypeArg GhcRn] -> TcM (TcType, TcKind)
-- Version of tc_infer_lhs_type specialised for the head of an
-- application. In particular, for a HsTyVar (which includes type
-- constructors, it does not zoom off into tcInferTyApps and family
-- saturation
tcInferTyAppHead mode (L _ (HsTyVar _ _ (L _ (WithUserRdr _ tv)))) args
  -- A saturated case records its rule use with the instantiated kinds.
  -- See Note [Source family evidence uses] in GHC.Tc.Gen.SourceTable.
  | isSourceFamilyName tv
  = do table <- tcLookupTyCon tv
       case sourceCaseVisibleArity table of
         Just arity | lengthIs [() | HsValArg{} <- args] arity
           -> return (mkTyConTy table, tyConKind table)
         _ -> do
           -- Keep legacy eager checking for fully applied source tables.
           let source_arity = length (dropWhile isInvisibleTyConBinder (tyConBinders table))
           when (length [() | HsValArg{} <- args] >= source_arity) $
             sourceTableReferenceUse table
           tcTyVar mode tv
  | otherwise = tcTyVar mode tv
tcInferTyAppHead mode ty _
  = tc_infer_lhs_type mode ty

tc_app_ty :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
tc_app_ty = tcTermAppInType

-- | Unified 4-stage term-in-type application router:
--   1. Scoped class method 'ClassAppTy' and local runtime index heads ('TermAppTy'),
--   2. Certified 'SourceTable' families ('TyConApp'), promoted class methods, and
--      partial-signature wildcard deferrals,
--   3. Pure symbolic 'OpenTypeEval' with single 'ClosedEval' bytecode fallback,
--   4. Uninterpreted closed family 'U:f' when a 'TcPlugin' is active.
-- See Note [Captured term names in ordinary types] and
-- Note [Uninterpreted term applications for TcPlugins].
tcTermAppInType :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
tcTermAppInType mode rn_ty exp_kind
  -- Post-renaming lowered dependent sum: GHC.Sigma.Sigma domain (\b -> body)
  | TypeLevel <- mode_tyki mode
  , (L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ s_nm))),
     [HsValArg _ dom_hs, HsValArg _ (L lam_loc (XHsType (HsTypeLambdaQuoteRn [L _ (HsTvb _ () (HsBndrVar _ bndr) (HsBndrNoKind _))] body_hs)))])
      <- splitHsAppTys rn_ty
  , isSigmaTyConName s_nm
  = tcSomeTypeInHsType mode rn_ty s_nm dom_hs (locA lam_loc) bndr body_hs exp_kind
  -- Stage 1a: Scoped dictionary method call on a retained index.
  | not (null (mode_dictionaries mode))
  , (L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr _ method))), [HsValArg _ arg]) <- splitHsAppTys rn_ty
  , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ operand))) <- ignoreParens arg
  , Just index <- lookupNameEnv (mode_indices mode) operand
  , isVarName method, isExternalName method
  = do mb_sel <- tcLookupIdMaybe method
       case mb_sel of
         Just sel | isJust (isClassOpId_maybe sel) -> setSrcSpanA loc $ do
           let inst = varType index
           case Core.nativeClassSelectorTys sel inst of
             Just (predicate, domain, result)
               | domain `tcEqType` inst
               , Just dict <- case Core.projectUniqueClassDict predicate (mode_dictionaries mode) of
                   Core.ScopedDirectDict d       -> Just d
                   Core.ScopedSuperclassDict d _ -> Just d
                   _                             -> Nothing
               , isNativeClassDictionaryId dict
               -> checkExpKind mode rn_ty
                    (ClassAppTy sel inst (TermVarTy dict) (TermVarTy index) result) result exp_kind
             _ -> failWithTc (TcRnRetainedQuantificationError RetainedCallIndex)
         _ -> stage1_runtime_or_external

  | otherwise = stage1_runtime_or_external
  where
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty
    value_args = hsValArgsOnly hs_args
    val_args_only = [arg | HsValArg _ arg <- hs_args]
    ordinary_call = isNothing (mode_holes mode) || isJust (mode_complete_type_arg mode)
    ready_partial_call
      | Just (_, RetrySigCalls, _) <- mode_pending_calls mode = True
      | otherwise = False
    unsupported = failWithTc . TcRnNoSourceTable

    -- Stage 1b: Local runtime function index head (TermVarTy / TermAppTy).
    stage1_runtime_or_external
      | L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))) <- hs_fun_ty
      , Just head_id <- lookupNameEnv (mode_indices mode) name
      , isNativeIndexFunctionId head_id || isNativeDepFunctionId head_id
      = tc_app_ty_runtime_head mode rn_ty exp_kind head_id
      | L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr rdr name))) <- hs_fun_ty
      = do env <- getGblEnv
           let inter_term = isInteractiveStmtTerm env name
           if (isVarName name || isFieldOcc (nameOccName name)) && (isExternalName name || inter_term)
             then do
               enabled <- xoptM LangExt.DependentTypes
               let field_ready = not (isFieldOcc (nameOccName name))
                              || inter_term
                              || not (nameIsLocalOrFrom (tcg_semantic_mod env) name)
                              || isJust (lookupTypeEnv (tcg_type_env env) name)
                              || name `elemNameEnv` tcg_local_open_defs env
               if enabled && field_ready
                 then setSrcSpanA loc (dispatch_term_head rdr name)
                 else tc_app_ty_generic mode rn_ty exp_kind
             else if isSourceFamilyName name
               then dispatch_source_family loc rdr name
               else tc_app_ty_generic mode rn_ty exp_kind
      | otherwise = tc_app_ty_generic mode rn_ty exp_kind

    -- Stage 2-4 on a resolved T:f family head.
    dispatch_source_family loc rdr name
      | Just args <- value_args
      , not (null args)
      , isTypeLevel (mode_tyki mode)
      , ordinary_call || (lengthIs args 1 && ready_partial_call)
      = do env <- getGblEnv
           if nameIsLocalOrFrom (tcg_semantic_mod env) name
              && isNothing (lookupTypeEnv (tcg_type_env env) name)
             then do
               owner <- closedFamilyOwner name
               if owner `elemNameEnv` tcg_local_open_defs env
                 then setSrcSpanA loc (dispatch_term_head rdr owner)
                 else do
                   fallback <- localFallbackFamilyOwner name
                   case fallback of
                     Just fun -> setSrcSpanA loc $ do
                       staging <- closedOperandStaging args
                       if isJust staging
                         then run_closed_tree (RunClosedFunction fun) args
                         else stage4_uninterp fun (unsupported (getName fun))
                     Nothing -> setSrcSpanA loc (unsupported owner)
             else do
               fallback <- localFallbackFamilyOwner name
               case fallback of
                 Just fun -> setSrcSpanA loc $ do
                   staging <- closedOperandStaging args
                   if isJust staging
                     then run_closed_tree (RunClosedFunction fun) args
                     else stage4_uninterp fun (unsupported (getName fun))
                 Nothing
                   | ordinary_call || (lengthIs args 1 && isQual rdr && ready_partial_call) -> do
                       staging <- closedOperandStaging args
                       executable <- tcClosedCaseExecutableOwner False rdr name args staging
                       case executable of
                         Just owner -> setSrcSpanA loc (run_closed_tree (RunClosedFunction owner) args)
                         Nothing
                           | ordinary_call, isNothing staging, any (isJust . bareClosedCallback) args -> do
                               table <- tcLookupTyCon name
                               checkOpenCallbackInterfaces table args
                               mapM_ checkNominalCallback args
                               tc_app_ty_generic mode rn_ty exp_kind
                           | ordinary_call, any nominalCallback args -> do
                               mapM_ checkNominalCallback args
                               tc_app_ty_generic mode rn_ty exp_kind
                           | ordinary_call, staging == Just True -> do
                               table <- tcLookupTyCon name
                               if supportsClosedFamilyTree table args
                                 then setSrcSpanA loc (run_closed_tree (CheckClosedCase table) args)
                                 else tc_app_ty_generic mode rn_ty exp_kind
                           | otherwise -> tc_app_ty_generic mode rn_ty exp_kind
                   | otherwise -> tc_app_ty_generic mode rn_ty exp_kind
      | otherwise = tc_app_ty_generic mode rn_ty exp_kind

    -- Stage 2-4 on a resolved term Name (both same-module and imported).
    dispatch_term_head rdr name
      | not (isTypeLevel (mode_tyki mode)) = unsupported name
      | otherwise = do
          env <- getGblEnv
          let inter_term = isInteractiveStmtTerm env name
              is_local = not inter_term && nameIsLocalOrFrom (tcg_semantic_mod env) name
          mb_fun <- resolve_term_head env is_local name
          case (mb_fun, value_args) of
            (Nothing, _) -> unsupported name
            (Just fun, Nothing)
              | ordinary_call, (is_local || not (null hs_args)) ->
                  stage4_uninterp fun (unsupported name)
              | otherwise -> unsupported name
            (Just fun, Just args)
              -- Stage 2: Certified SourceTable, opaque callback, or pending wildcard deferral.
              | Just table <- partial_source_callback fun args
              -> checked_callback table
              | is_local, ordinary_call
              , Just table <- local_certified_table env fun args
              -> use_source_table table
              | not is_local, ordinary_call, null args, isQual rdr
              , isNativeNamedOpaqueFunctionId fun, isNothing (sourceTableId_maybe fun)
              -> checkExpKind mode rn_ty (TermVarTy fun) (varType fun) exp_kind
              | not is_local, ordinary_call
              , Just table <- imported_certified_table fun args
              -> do prefer_uninterp <- hasImportedUninterpretedFamily fun
                    let use_tbl = do when (lengthIs args 1) (void (tcOpenTypeContext mode name))
                                     use_source_table table
                    if prefer_uninterp
                      then stage4_uninterp fun use_tbl
                      else use_tbl
              | [arg] <- args, is_local || isQual rdr
              , Just defer <- defer_pending_call name fun arg
              -> defer
              -- Stage 3: Pure OpenTypeEval with ClosedEval bytecode fallback.
              | ordinary_call
              , not is_local || isJust (lookupNameEnv (tcg_local_open_defs env) name) || isJust (isClassOpId_maybe fun)
              , Just sig@(_, domains, _, _) <- openTypeFunctionSignature is_local fun
              , equalLength domains args
              , let is_open_arg = if is_local then is_local_open_operand else isOpenTypeOperand mode
              , is_local || any is_open_arg args
              , all (\a -> is_open_arg a || isClosedConTypeOperand a) args
              -> do outcome <- tcTryOpenTypeCall mode name fun sig args
                    case outcome of
                      Right (result, result_kind) -> checkExpKind mode rn_ty result result_kind exp_kind
                      Left err@(CannotReduce Exhausted) -> reportOpenTypeFailure name err
                      Left (UnavailableDefinition _ MissingTemplate)
                        | not is_local -> stage3_closed_or_uninterp is_local rdr name fun args
                      Left err -> stage4_uninterp fun (reportOpenTypeFailure name err)
              | ordinary_call
              , not is_local || isJust (lookupNameEnv (tcg_local_open_defs env) name) || isJust (isClassOpId_maybe fun)
              , Just telescope@(_, inputs, _) <- indexedOpenTypeFunction fun
              , equalLength inputs args
              , all (\a -> known_constructor a || isOpenTypeOperand mode a) args
              , any known_constructor args, any open_indexed_operand args
              -> do observed <- tcIndexedOpenTypeCall mode fun telescope args
                    case observed of
                      Just (result, result_kind) -> checkExpKind mode rn_ty result result_kind exp_kind
                      Nothing
                        | not is_local -> stage3_closed_or_uninterp is_local rdr name fun args
                        | otherwise    -> stage4_uninterp fun (unsupported name)
              | not (null args)
              , ordinary_call || ((is_local || isQual rdr) && ready_partial_call && lengthIs args 1)
              , not is_local || isJust (lookupNameEnv (tcg_local_open_defs env) name) || isJust (isClassOpId_maybe fun)
              -> stage3_closed_or_uninterp is_local rdr name fun args
              -- Stage 4: Uninterpreted closed family U:f when TcPlugins are active.
              | ordinary_call -> stage4_uninterp fun (unsupported name)
              | otherwise -> unsupported name

    resolve_term_head env True name =
      case lookupTypeEnv (tcg_type_env env) name of
        Just (AnId fun) -> pure (Just fun)
        _ -> case lookupNameEnv (tcg_local_open_defs env) name of
          Just (fun, _) -> pure (Just fun)
          Nothing       -> tcLookupIdMaybe name
    resolve_term_head env False name
      | ordinary_call || isJust (mode_pending_calls mode) =
          case lookupTypeEnv (tcg_type_env env) name of
            Just (AnId fun) -> pure (Just fun)
            _ -> do
              thing <- tcLookupGlobal name
              pure $ case thing of { AnId fun -> Just fun; _ -> Nothing }
      | otherwise = pure Nothing

    local_certified_table env fun args
      | isRecordSelector fun
      , Just table <- sourceTableId_maybe fun
      , lengthIs args 1 || equalLength args (tyConVisibleTyVars table)
      = Just table
      | Just (local_fun, _) <- lookupNameEnv (tcg_local_open_defs env) (idName fun)
      , Just table <- sourceTableOccurrence env local_fun >>= sourceTableId_maybe
      , lengthIs args 1 || equalLength args (tyConVisibleTyVars table)
      = Just table
      | otherwise = Nothing

    imported_certified_table fun [left, right]
      | Just (table, nullary : recursive) <- indexedProjectionSource fun
      , (L _ (HsTyVar _ _ (L _ (WithUserRdr _ operand))), left_args) <-
          splitHsAppTys (unLoc (indexed_operand left))
      , (null left_args && (indexed_variable operand || operand == getName nullary)) ||
        ((any ((== operand) . getName) recursive || operand == getName fun) &&
         any open_indexed_operand [left, right])
      = Just table
    imported_certified_table fun [arg]
      | Just table <- sourceTableId_maybe fun
      , (isFinalCoreIdentity fun && retained_identity_operand arg) ||
        (isFinalCoreReverseWrapper fun && open_indexed_operand arg && not (isOpenTypeOperand mode arg))
      = Just table
    imported_certified_table _ _ = Nothing

    stage3_closed_or_uninterp is_local _rdr name fun args = do
      selected <- if ordinary_call then checkedClassMethod fun args else pure Nothing
      case selected of
        Just check -> check
        Nothing -> do
          open_dict <- if ordinary_call && isJust (isClassOpId_maybe fun)
                          && (any open_mapping_operand args || all isClosedConTypeOperand args)
                         then tcTryOpenDictMethodCall mode name fun args
                         else pure Nothing
          case open_dict of
            Just (result, result_kind) -> checkExpKind mode rn_ty result result_kind exp_kind
            Nothing
              | not is_local -> do
                  let run_closed = run_closed_tree (RunClosedFunction fun) args
                  can_uninterp <- if ordinary_call then canUseUninterpretedTerm fun else pure False
                  staging <- if can_uninterp then closedOperandStaging args else pure (Just True)
                  case staging of
                    Nothing -> stage4_uninterp fun run_closed
                    Just True -> run_closed
                    Just False
                      | can_uninterp -> tryTcDiscardingErrs (stage4_uninterp fun run_closed) run_closed
                      | otherwise    -> run_closed
              | otherwise -> do
                  staging <- closedOperandStaging args
                  if isJust staging
                    then run_closed_tree (RunClosedFunction fun) args
                    else stage4_uninterp fun $
                           if lengthIs args 1 && is_closed_retry_head (varType fun)
                             then run_closed_tree (RunClosedFunction fun) args
                             else unsupported name

    is_closed_retry_head fun_ty
      | (_, binders, rho) <- splitInvisPiTys fun_ty
      , all isTyVar (mapMaybe namedPiTyBinder_maybe binders)
      , let preds = mapMaybe anonPiTyBinderType_maybe binders
      , Just (FTF_T_T, _, _, domain, _) <- splitFunTy_maybe rho
      , let retry_tys = [charTy, mkListTy naturalTy]
      = any (tcEqType domain) retry_tys
        || (not (null preds) && any (isJust . tcMatchTy domain) retry_tys)
      | otherwise = False

    stage4_uninterp fun on_miss
      | not ordinary_call = on_miss
      | otherwise = do
          can_uninterp <- canUseUninterpretedTerm fun
          if not can_uninterp
            then on_miss
            else tcUninterpretedTermCall mode rn_ty exp_kind (length val_args_only) fun
                   >>= maybe on_miss pure

    use_source_table table = do
      sourceTableReferenceUse table
      tc_app_ty_with_head mode rn_ty exp_kind (mkTyConTy table)

    defer_pending_call name fun arg
      | Just (wildcards, call_owner, pending) <- mode_pending_calls mode
      , Just (owner_level, HM_Sig) <- mode_holes mode
      , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ operand))) <- ignoreParens arg
      , Just _ <- lookupNameEnv wildcards operand
      = Just $ do
          (domain, result_kind) <- retryClosedCallKinds name (varType fun)
          tcCheckClosedCallContext name
          case call_owner of
            RetrySigCalls  -> pure ()
            RejectSigCalls -> failWithTc (TcRnClosedTypeEval name
              ("This binding group has a pattern binding with an explicit multiplicity. " ++
               "Give the binding that uses this call a complete type signature."))
          checked <- tc_check_lhs_type mode arg domain
          result <- setTcLevel owner_level (newFlexiTyVarTy result_kind)
          call_span <- getSrcSpanM
          updTcRef pending (TcPendingTypeCall call_span fun checked result :)
          checkExpKind mode rn_ty result result_kind exp_kind
      | otherwise = Nothing

    run_closed_tree root args = do
      hint <- closedCallResultHint ordinary_call exp_kind
      (kind, value) <- tcPlanClosedTree mode ordinary_call hint root args
      checkClosedCallExpectedKind rn_ty exp_kind kind
      observe <- prepareClosedCallResult kind value
      result <- observe
      checkExpKind mode rn_ty result kind exp_kind

    partial_source_callback fun args
      | isJust (mode_complete_type_arg mode)
      , Just table <- sourceTableId_maybe fun
      , Just arity <- sourceCaseVisibleArity table <|> ordinary_source_arity fun table
      , not (args `lengthAtLeast` arity)
      = Just table
      | otherwise = Nothing

    ordinary_source_arity fun table
      | Just _ <- sourceTableId_maybe fun
      , let vis = count isVisibleTyConBinder (tyConBinders table)
      , vis > 0
      = Just vis
      | otherwise = Nothing

    checked_callback table = do
      let head_ty = mkTyConTy table
      sourceTableTypeUses head_ty
      tc_app_ty_with_head mode rn_ty exp_kind head_ty

    is_local_open_operand arg = isOpenTypeOperand mode arg || is_type_constructor_app arg

    is_type_constructor_app arg = case unLoc (ignoreParens arg) of
      HsTyVar _ NotPromoted (L _ (WithUserRdr _ nm)) -> isTyConName nm
      HsAppTy _ f operand -> is_type_constructor_app f && is_type_constructor_app operand
      _ -> False

    retained_identity_operand arg
      | L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ operand))) <- ignoreParens arg
      = operand `elemNameEnv` mode_indices mode
      | Just (_, _, head_name, [operand]) <- nestedClosedCall arg
      , isVarName head_name = retained_identity_operand operand
      | otherwise = False

    indexed_variable nm = isTyVarName nm || isJust (lookupNameEnv (mode_indices mode) nm)
    open_indexed_operand ty = case unLoc (indexed_operand ty) of
      HsTyVar _ NotPromoted (L _ (WithUserRdr _ nm)) -> indexed_variable nm
      HsAppTy _ f a -> open_indexed_operand f || open_indexed_operand a
      HsExplicitTupleTy _ _ fields -> any open_indexed_operand fields
      HsExplicitListTy _ _ fields -> any open_indexed_operand fields
      _ -> False

    known_constructor operand =
      case splitHsAppTys (unLoc (indexed_operand operand)) of
        (L _ (HsTyVar _ _ (L _ (WithUserRdr _ nm))), _) -> isDataConName nm
        _ -> False

    indexed_operand ty = case ignoreParens ty of
      L _ (HsKindSig _ operand _) -> indexed_operand operand
      operand -> operand

    checkedClassMethod fun args@[_, _, receiver]
      | Just _ <- isClassOpId_maybe fun
      , any (isOpenTypeOperand mode) args = do
          (_, kind) <- tc_infer_lhs_type mode receiver
          actual <- zonkTcTypeIfNeeded kind
          case splitTyConApp_maybe actual of
            Just (tc, [_]) | tc == listTyCon ->
              fmap (checkTableMethod fun) <$> lookupSourceContainerMethod fun (mkTyConTy tc)
            _ -> pure Nothing
    checkedClassMethod fun args@[left, right]
      | sourceMapSelector fun, any open_mapping_operand args
      , Just cls <- isClassOpId_maybe fun = do
          (receiver, kind) <- tc_infer_lhs_type mode right
          actual <- zonkTcTypeIfNeeded kind
          case splitAppTy_maybe actual of
            Just (domain, _) -> do
              table <- lookupSourceCheckedMapMethod cls fun domain
              case table of
                Just (dfun, predicate, worker, checked) ->
                  pure (Just (checkInstantiatedMethod fun predicate (dfun, worker, checked)))
                Nothing -> do
                  ground <- lookupSourceContainerMethod fun domain
                  case ground of
                    Just checked -> pure (Just (checkTableMethod fun checked))
                    Nothing | isOpenTypeOperand mode left
                            , isOpenTypeOperand mode right || promotedReceiver receiver ->
                      fmap (checkOpenMethod fun left receiver) <$> lookupSourceGroundDFun cls domain
                    Nothing -> pure Nothing
            Nothing -> pure Nothing
      | Just _ <- isClassOpId_maybe fun
      , Just domain <- retainedDomain left <|> retainedDomain right
      = do actual <- zonkTcTypeIfNeeded domain
           fmap (checkTableMethod fun) <$> lookupSourceBinaryMethod fun actual
      | Just _ <- isClassOpId_maybe fun
      , any open_mapping_operand args || all isClosedConTypeOperand args
      = do mb_dom <- tryTcDiscardingErrs (pure Nothing) (Just . snd <$> tc_infer_lhs_type mode left)
           case mb_dom of
             Just domain -> do
               actual <- zonkTcTypeIfNeeded domain
               fmap (checkTableMethod fun) <$> lookupSourceBinaryMethod fun actual
             Nothing -> pure Nothing
    checkedClassMethod fun [arg]
      | Just _ <- isClassOpId_maybe fun
      , Just domain <- retainedDomain arg
      = do actual <- zonkTcTypeIfNeeded domain
           fmap (checkTableMethod fun) <$> lookupSourceUnaryMethod fun actual
      | Just _ <- isClassOpId_maybe fun
      , open_mapping_operand arg || isClosedConTypeOperand arg
      = do mb_dom <- tryTcDiscardingErrs (pure Nothing) (Just . snd <$> tc_infer_lhs_type mode arg)
           case mb_dom of
             Just domain -> do
               actual <- zonkTcTypeIfNeeded domain
               fmap (checkTableMethod fun) <$> lookupSourceUnaryMethod fun actual
             Nothing -> pure Nothing
    checkedClassMethod _ _ = pure Nothing

    open_mapping_operand = isOpenTypeOperandWith (\(WithUserRdr _ n) -> indexed_variable n)
                                                 (\(WithUserRdr _ n) -> isDataConName n)

    checkTableMethod fun selected@(dfun, _, _) =
      checkInstantiatedMethod fun (varType dfun) selected

    checkInstantiatedMethod fun predicate (dfun, target, table) = do
      recordSourceMethodUse dfun fun target
      checkSourceMethodSelection mode dfun predicate (idName fun)
      (ty, kind) <- tcInferTyApps mode hs_fun_ty (mkTyConTy table) hs_args
      checkExpKind mode rn_ty ty kind exp_kind

    checkOpenMethod fun left receiver dfun = do
      let name = idName fun
      local <- tcOpenTypeContext mode name
      checkSourceMethodSelection mode dfun (varType dfun) name
      worker <- readOpenMethodWorker dfun fun >>= either (reportOpenTypeFailure name) pure
      recordSourceMethodUse dfun fun worker
      (callback, _) <- tc_infer_lhs_type mode left
      checked_cb <- zonkTcTypeIfNeeded callback
      checked_receiver <- zonkTcTypeIfNeeded receiver
      case instantiateOpenMethod worker (typeKind checked_cb) (typeKind checked_receiver) of
        Nothing -> reportOpenTypeFailure name InvalidInput
        Just (method, result_kind) -> do
          observed <- tcObserveOpenTypeCall local False method result_kind [checked_cb, checked_receiver]
          result <- either (reportOpenTypeFailure name) pure observed
          checkExpKind mode rn_ty result result_kind exp_kind

    promotedReceiver ty = case splitTyConApp_maybe ty of
      Just (tc, _) -> isJust (isPromotedDataCon_maybe tc)
      Nothing      -> False

    retainedDomain arg
      | L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ nm))) <- indexed_operand arg
      , Just index <- lookupNameEnv (mode_indices mode) nm
      , isNativeOpaqueValueId index
      = Just (varType index)
      | otherwise = Nothing

tc_app_ty_runtime_head :: TcTyMode -> HsType GhcRn -> ExpKind -> Id -> TcM TcType
tc_app_ty_runtime_head mode rn_ty exp_kind head_id
  = do { (ty, kind) <- foldM apply (TermVarTy head_id, varType head_id) hs_args
       ; checkExpKind mode rn_ty ty kind exp_kind }
    -- See Note [Source opaque call indices].
  where
    apply :: (TcType, TcKind) -> LHsTypeArg GhcRn -> TcM (TcType, TcKind)
    apply checked HsArgPar{} = return checked
    apply _ (HsTypeArg _ arg) = setSrcSpanA (getLoc arg) invalid_call
    apply (fun, kind) (HsValArg _ arg) = setSrcSpanA (getLoc arg) $
      do { (fun', kind') <- case tcSplitTyConApp_maybe kind of
               Just (tc, _) | isTypeFamilyTyCon tc -> do
                 co <- matchExpectedFunKind (TypeThing fun)
                         (Just (pure unmatchableDataConTy)) 1 kind
                 exposed <- liftZonkM (zonkTcType (mkCastTy fun co))
                 return (exposed, typeKind exposed)
               _ -> return (fun, kind)
         ; arrow <- inferRetainedCallbackKinds arg kind'
         ; case nativeIndexFunctionTypeTys_maybe arrow of
             Just (domain, result) -> do { ty <- tc_check_lhs_type mode arg domain
                                         ; return (mkAppTy fun' ty, result) }
             Nothing | DepFunTy DepFunVisible bndr body <- coreFullView arrow
                     , isManyTy (idMult bndr)
                     , isNativeOpaqueValueType (varType bndr)
                     , tcIsLiftedTypeKind (typeKind body)
                     -> do { ty <- tc_check_lhs_type mode arg (varType bndr)
                           ; return (mkAppTy fun' ty, replaceTermVarInType bndr ty body) }
                     | FunTy { ft_mult = mult } <- coreFullView arrow
                     -> failWithTc $ TcRnRetainedQuantificationError $
                          if isManyTy mult then RetainedUnsupportedDomain arrow
                                           else RetainedIndexMultiplicity mult
                     | otherwise -> invalid_call }
    invalid_call :: TcM a
    invalid_call = failWithTc $ TcRnRetainedQuantificationError RetainedCallIndex
    (_, hs_args) = splitHsAppTys rn_ty

{- Note [Captured term names in ordinary types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under 'RequiredTypeArguments' (GHC Proposal #281, Eisenberg, 2020), a name in a
type can resolve to a term. Without 'DependentTypes', keep that
occurrence on 'tc_app_ty_generic' so it reports the standard term-capture
diagnostic.

With 'DependentTypes', 'tcTermAppInType' routes applications in
four deterministic stages without speculative backtracking:
  1. Scoped dictionary selectors ('ClassAppTy') and registered runtime function
     indices ('TermVarTy' / 'TermAppTy'),
  2. Certified 'SourceTable' families ('TyConApp'), promoted class methods, and
     partial-signature wildcard deferrals ('TcPendingTypeCall'),
  3. Symbolic 'OpenTypeEval' reduction (Eisenberg and Stolarek, Haskell
     Symposium 2014) with a single 'ClosedEval' bytecode fallback when all
     operands are closed ground values,
  4. Uninterpreted closed families ('U:f') when a 'TcPlugin' is active.
-}

-- | Add the written constraints to the scope of a promoted method call.
sourceMethodContext :: TcTyMode -> [PredType] -> TcTyMode
sourceMethodContext mode theta = mode { mode_source_givens = theta ++ mode_source_givens mode }

-- | Require the ordinary solver to select the exact compiled dictionary.
-- See Note [Select the dictionary for a promoted method] in GHC.Tc.Gen.SourceTable.
checkSourceMethodSelection :: TcTyMode -> Id -> PredType -> Name -> TcM ()
checkSourceMethodSelection mode dfun predicate selector = do
  lexical <- getLexicalGivens
  written <- newEvVars (mode_source_givens mode)
  (_, constraints) <- captureConstraints $
    checkConstraints (SigTypeSkol (ExprSigCtxt NoRRC)) [] (written ++ lexical) $ do
      wanted <- newWanted (OccurrenceOf selector) Nothing predicate
      emitSimple (mkNonCanonical wanted)
      emitDelayedErrors (unitBag (DE_SourceMethod (ctEvEvId wanted) dfun selector (ctEvLoc wanted)))
  discardResult (unsetGOptM Opt_SolveConstantDicts (simplifyTopNoDefaulting constraints))

tc_app_ty_generic :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
tc_app_ty_generic mode rn_ty exp_kind
  = do { (fun_ty, _) <- tcInferTyAppHead mode hs_fun_ty hs_args
       ; tc_app_ty_with_head mode rn_ty exp_kind fun_ty }
  where
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty

-- | Infer the arguments of an already resolved type application.
tc_app_ty_with_head :: TcTyMode -> HsType GhcRn -> ExpKind -> TcType -> TcM TcType
tc_app_ty_with_head mode rn_ty exp_kind fun_ty
       -- See Note [Owning omitted arrows in kind-valued arguments] in GHC.Tc.Solver.
  = do { let index_level = case exp_kind of
               Check expected | isTypeLikeKind expected -> KindLevel
               _ -> TypeLevel
             expected_kind = checkingExpType_maybe exp_kind
       ; (prefix, prefix_kind) <- tcInferTyApps_nosat index_level expected_kind mode hs_fun_ty fun_ty hs_args
       -- See Note [saturateFamApp] for a checked partial family.
       ; allow_partial <- unsaturatedTypeFamiliesEnabled
       ; (ty, inferred_kind) <- case (exp_kind, tcSplitTyConApp_maybe prefix) of
           (Check expected, Just (tc, args))
             | allow_partial, isPartialTypeFunction tc args
             , let n_expected = invisibleBndrCount expected
             , expected `tcEqType` prefix_kind
               || (n_expected > 0 && n_expected <= invisibleBndrCount prefix_kind)
             -> return (prefix, prefix_kind)
           _ -> saturateFamApp mode prefix prefix_kind
       ; checkExpKind mode rn_ty ty inferred_kind exp_kind }
  where
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty

-- | Check named rigid operands and attempt pure symbolic reduction.
-- See Note [Open source calls] in GHC.Tc.Gen.OpenTypeEval.
tcTryOpenTypeCall
  :: TcTyMode -> Name -> Id
  -> ([TyVar], [Kind], Kind, Core.CoreExpr -> Core.CoreExpr)
  -> [LHsType GhcRn]
  -> TcM (Either OpenTypeFailure (Type, Kind))
tcTryOpenTypeCall mode name fun (tvs, domains, result_tpl, wrap) args = do
  local <- tcOpenTypeContext mode name
  (meta_tvs, res_k0, raw_ops) <- solveEqualities "tcTryOpenTypeCall" $ do
    (subst, mvs) <- newMetaTyVars tvs
    let exp_doms = map (Subst.substTy subst) domains
    ops <- zipWithM (\arg kd -> setSrcSpanA (getLoc arg) (tc_check_lhs_type mode arg kd)) args exp_doms
    pure (mvs, Subst.substTy subst result_tpl, ops)
  ty_args0  <- liftZonkM (zonkTcTypes (mkTyVarTys meta_tvs))
  res_k1    <- liftZonkM (zonkTcType res_k0)
  operands1 <- liftZonkM (zonkTcTypes raw_ops)
  let kind_Pool ts ops =
        ts ++ map typeKind ops ++ [varType v | v <- tyCoVarsOfTypesList ops]
      rep_metas =
        [ mv | mv <- tyCoVarsOfTypesList (kind_Pool (res_k1 : ty_args0) operands1)
             , isTcTyVar mv, isMetaTyVar mv, not (isTyVarTyVar mv) ]
  forM_ rep_metas $ \mv -> do
    cts <- readMetaTyVar mv
    case cts of
      Flexi | tyVarKind mv `tcEqType` runtimeRepTy ->
        liftZonkM (writeMetaTyVar mv liftedRepTy)
      _ -> pure ()
  ty_args1  <- liftZonkM (zonkTcTypes ty_args0)
  res_k2    <- liftZonkM (zonkTcType res_k1)
  operands2 <- liftZonkM (zonkTcTypes operands1)
  let def_metas =
        [ mv | mv <- tyCoVarsOfTypesList (kind_Pool (res_k2 : ty_args1) operands2)
             , isTcTyVar mv, isMetaTyVar mv, not (isTyVarTyVar mv) ]
  forM_ def_metas $ \mv -> do
    cts <- readMetaTyVar mv
    case cts of
      Flexi | tcIsLiftedTypeKind (tyVarKind mv) ->
        liftZonkM (writeMetaTyVar mv liftedTypeKind)
      _ -> pure ()
  ty_args     <- liftZonkM (zonkTcTypes ty_args1)
  result_kind <- liftZonkM (zonkTcType res_k2)
  operands    <- liftZonkM (zonkTcTypes operands2)
  if anyVarSet isMetaTyVar (tyCoVarsOfTypes (result_kind : ty_args))
     || not (all (\t -> tcIsLiftedTypeKind (typeKind t) || tcIsLiftedTypeKind (typeKind (typeKind t))) ty_args)
    then pure (Left InvalidInput)
    else do
      let applied = wrap (foldl' Core.App (Core.Var fun) (map Core.Type ty_args))
      fmap (, result_kind) <$> tcObserveOpenTypeCall local False applied result_kind operands

-- | Specialize a class method dictionary via 'InstEnv' and evaluate open arguments in Core.
tcTryOpenDictMethodCall
  :: TcTyMode -> Name -> Id -> [LHsType GhcRn] -> TcM (Maybe (Type, Kind))
tcTryOpenDictMethodCall mode name fun args
  | let (ma_tvs, theta, rho) = tcSplitSigmaTy (varType fun)
        tvs                  = map snd ma_tvs
  , not (null theta)
  , all (tcIsLiftedTypeKind . typeKind . tyVarKind) tvs
  , Just (domains, result_tpl) <- peel_arrows (length args) rho
  = tryTcDiscardingErrs (pure Nothing) $ do
      _ <- tcOpenTypeContext mode name
      (meta_tvs, doms0, res_k0, raw_ops) <- do
        (subst, mvs) <- newMetaTyVars tvs
        let exp_doms = map (Subst.substTy subst) domains
            has_ret_idx arg = not (isEmptyNameEnv (mode_indices mode)) &&
              isOpenTypeOperandWith
                (\(WithUserRdr _ n) -> isJust (lookupNameEnv (mode_indices mode) n))
                (\(WithUserRdr _ n) -> isDataConName n)
                arg
        -- Check arguments without retained runtime indices first and solve equalities
        -- so retained operands see concrete domains rather than unfilled metavariables.
        pass1 <- solveEqualities "tcTryOpenDictMethodCall.pass1" $
          forM (zip args exp_doms) $ \(arg, kd) ->
            if has_ret_idx arg
              then pure Nothing
              else Just <$> setSrcSpanA (getLoc arg) (tc_check_lhs_type mode arg kd)
        doms_z <- liftZonkM (zonkTcTypes exp_doms)
        ops <- solveEqualities "tcTryOpenDictMethodCall.pass2" $
          forM (zip3 args doms_z pass1) $ \(arg, kd, mb_done) ->
            case mb_done of
              Just op -> pure op
              Nothing -> do
                (op_ty, op_kind) <- setSrcSpanA (getLoc arg) (tc_infer_lhs_type mode arg)
                when (anyFreeVarsOfType isMetaTyVar kd && not (anyFreeVarsOfType isId op_kind)) $
                  discardResult (unifyKind Nothing op_kind kd)
                kd' <- liftZonkM (zonkTcType kd)
                if op_kind `tcEqType` kd'
                  then pure op_ty
                  else setSrcSpanA (getLoc arg) (tc_check_lhs_type mode arg kd')
        pure (mvs, doms_z, Subst.substTy subst result_tpl, ops)
      ty_args0  <- liftZonkM (zonkTcTypes (mkTyVarTys meta_tvs))
      doms1     <- liftZonkM (zonkTcTypes doms0)
      res_k1    <- liftZonkM (zonkTcType res_k0)
      operands1 <- liftZonkM (zonkTcTypes raw_ops)
      let all_tys ts ds rk ops = ts ++ ds ++ [rk] ++ map typeKind ops ++ [varType v | v <- tyCoVarsOfTypesList ops]
      forM_ [ mv | mv <- tyCoVarsOfTypesList (all_tys ty_args0 doms1 res_k1 operands1)
                 , isTcTyVar mv, isMetaTyVar mv, not (isTyVarTyVar mv) ] $ \mv -> do
        cts <- readMetaTyVar mv
        case cts of
          Flexi | tyVarKind mv `tcEqType` runtimeRepTy -> liftZonkM (writeMetaTyVar mv liftedRepTy)
          _ -> pure ()
      ty_args1  <- liftZonkM (zonkTcTypes ty_args0)
      doms2     <- liftZonkM (zonkTcTypes doms1)
      res_k2    <- liftZonkM (zonkTcType res_k1)
      operands2 <- liftZonkM (zonkTcTypes operands1)
      forM_ [ mv | mv <- tyCoVarsOfTypesList (all_tys ty_args1 doms2 res_k2 operands2)
                 , isTcTyVar mv, isMetaTyVar mv, not (isTyVarTyVar mv) ] $ \mv -> do
        cts <- readMetaTyVar mv
        case cts of
          Flexi | tcIsLiftedTypeKind (tyVarKind mv) -> liftZonkM (writeMetaTyVar mv liftedTypeKind)
          _ -> pure ()
      ty_args     <- liftZonkM (zonkTcTypes ty_args1)
      doms        <- liftZonkM (zonkTcTypes doms2)
      result_kind <- liftZonkM (zonkTcType res_k2)
      operands    <- liftZonkM (zonkTcTypes operands2)
      if anyVarSet isMetaTyVar (tyCoVarsOfTypes (result_kind : ty_args ++ doms))
        then pure Nothing
        else do
          mb_spec <- instantiateOpenDictMethod fun ty_args doms result_kind
          case mb_spec of
            Nothing -> pure Nothing
            Just specialized -> do
              observed <- tcObserveOpenTypeCall True True specialized result_kind operands
              case observed of
                Right ty -> pure (Just (ty, result_kind))
                Left (CannotReduce Exhausted) -> reportOpenTypeFailure name (CannotReduce Exhausted)
                Left _ -> pure Nothing
  | otherwise = pure Nothing
  where
    peel_arrows 0 rest = Just ([], rest)
    peel_arrows n ty
      | n > 0
      , Just (FTF_T_T, ma, mult, dom, rest) <- splitFunTy_maybe ty
      , ma `tcEqType` unmatchableDataConTy
      , isManyTy mult
      = do (doms, res) <- peel_arrows (n - 1) rest
           pure (dom : doms, res)
      | otherwise = Nothing

-- | Find the erased telescope of an indexed source call with ordinary arrows.
-- See Note [Known indexed operands in source calls].
indexedOpenTypeFunction :: Id -> Maybe ([TyVar], [Kind], Kind)
indexedOpenTypeFunction fun = do
    let (_, binders, rho) = splitInvisPiTys (varType fun)
        (_, arrows, result) = splitFunTys rho
        domains = map scaledThing arrows
    tvs <- traverse namedPiTyBinder_maybe binders
    guard (all isTyVar tvs && any (not . isLiftedTypeKind . varType) tvs &&
           not (null domains) &&
           rho `tcEqType` foldr mkVisFunTyManyU result domains)
    guard (any indexed_domain domains)
    pure (tvs, domains, result)
  where
    indexed_domain ty
      | Just (tc, args) <- splitTyConApp_maybe ty
      , isBoxedDataTyCon tc
      = any (not . isVanillaDataCon) (tyConDataCons tc) ||
        any (not . isLiftedTypeKind . typeKind) args
      | otherwise = False

-- | Solve the operand kinds before interpreting one indexed observation.
tcIndexedOpenTypeCall :: TcTyMode -> Id -> ([TyVar], [Kind], Kind) -> [LHsType GhcRn]
                      -> TcM (Maybe (Type, Kind))
tcIndexedOpenTypeCall mode fun (tvs, domains, result) args = do
    let name = idName fun
    local <- tcOpenTypeContext mode name
    (variables, result_kind, operands) <- solveEqualities "indexed source call" $ do
      (subst, variables) <- newMetaTyVars tvs
      let inputs = map (Subst.substTy subst) domains
      operands <- zipWithM (tc_check_lhs_type mode) args inputs
      pure (mkTyVarTys variables, Subst.substTy subst result, operands)
    checked <- liftZonkM (zonkTcTypes variables)
    kind <- liftZonkM (zonkTcType result_kind)
    checked_operands <- liftZonkM (zonkTcTypes operands)
    if anyVarSet isMetaTyVar (tyCoVarsOfTypes checked)
      then return Nothing
      else do
        let applied = foldl' Core.App (Core.Var fun) (map Core.Type checked)
        observed <- tcObserveOpenTypeCall local True applied kind checked_operands
        case observed of
          Left InvalidInput -> return Nothing
          Left (UnavailableDefinition owner MissingTemplate)
            | not local || (owner == name && isJust (sourceTableId_maybe fun)) -> return Nothing
          Left reason -> reportOpenTypeFailure name reason
          Right ty -> return (Just (ty, kind))

{- Note [Known indexed operands in source calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'instantiateOpenTypeFunction' instantiates universal type parameters at 'Type'.
For unary or binary functions over indexed algebraic datatypes, use the checked
operand kinds to instantiate the invisible telescope before calling
'observeOpenType'. When the operand cannot be represented symbolically or a
checked table owner lacks a Core unfolding outside its table rows, return
'Nothing' so 'tcTermAppInType' falls through to 'ClosedEval' or 'TcPlugin'
elaboration. See Note [Observing indexed constructor cases] in
"GHC.Core.OpenWhnf".
-}

-- | Check the staging restrictions and tell whether the call is local.
tcOpenTypeContext :: TcTyMode -> Name -> TcM Bool
tcOpenTypeContext mode name = do
  when (mode_complete_type_arg mode == Just False) $
    reject "A visible type argument containing an open call must be complete. Replace its wildcards with types."
  level <- getThLevel
  bootOrSig <- tcIsHsBootOrSig
  explicitLevels <- xoptM LangExt.ExplicitLevelImports
  case level of
    Comp | not bootOrSig && not explicitLevels -> pure ()
    _ -> reject "This call requires an ordinary module outside quotes and splices, without ExplicitLevelImports."
  env <- getGblEnv
  pure (nameIsLocalOrFrom (tcg_semantic_mod env) name)
  where
    reject :: String -> TcM a
    reject = failWithTc . TcRnOpenTypeEval name Nothing

-- | Observe checked operands and record every definition used for readback.
tcObserveOpenTypeCall :: Bool -> Bool -> Core.CoreExpr -> Kind -> [Type]
                      -> TcM (Either OpenTypeFailure Type)
tcObserveOpenTypeCall local indexed instantiated result_kind operands = do
  env <- getGblEnv
  outcome <- observeOpenType local indexed instantiated result_kind operands
  case outcome of
    Left reason -> return (Left reason)
    Right (result, used, constructors) -> do
      let locals = tcg_local_open_defs env
          lookup_owner n = case lookupNameEnv locals n of
            Just (owner, _) -> pure owner
            Nothing -> case lookupTypeEnv (tcg_type_env env) n of
              Just (AnId v) -> pure v
              _             -> tcLookupId n
      dependencies <- forM used $ \(n, first) -> (, first) <$> lookup_owner n
      recordUnfoldedSourceUses dependencies
      updTcRef (tcg_source_check env) (\s -> s { scs_evaluated_uses = scs_evaluated_uses s `unionNameSet` constructors `unionNameSet` mkNameSet (map fst used) })
      pure (Right result)

-- | Report an unavailable observation at the source call.
reportOpenTypeFailure :: Name -> OpenTypeFailure -> TcM a
reportOpenTypeFailure name reason =
  let (definition, message) = openTypeFailureReason reason
  in failWithTc (TcRnOpenTypeEval name definition message)

-- | Admit a rigid variable or a promoted constructor with symbolic fields.
isOpenTypeOperand :: TcTyMode -> LHsType GhcRn -> Bool
isOpenTypeOperand mode =
  isOpenTypeOperandWith
    (\(WithUserRdr _ name) -> isTyVarName name || isJust (lookupNameEnv (mode_indices mode) name))
    (\(WithUserRdr _ name) -> isDataConName name)

isClosedConTypeOperand :: LHsType GhcRn -> Bool
isClosedConTypeOperand ty = case unLoc (ignoreParens ty) of
  HsKindSig _ operand _ -> isClosedConTypeOperand operand
  HsExplicitTupleTy _ _ elems -> length elems /= 1 && all isClosedConTypeOperand elems
  HsTupleTy _ _ elems -> length elems /= 1 && all isClosedConTypeOperand elems
  HsExplicitListTy _ _ elems -> all isClosedConTypeOperand elems
  HsOpTy _ lhs op rhs
    | HsTyVar _ _ (L _ (WithUserRdr _ nm)) <- unLoc (ignoreParens op) ->
        isDataConName nm && all isClosedConTypeOperand [lhs, rhs]
    | otherwise -> False
  _ -> case splitHsAppTys (unLoc (ignoreParens ty)) of
    (L _ (HsTyVar _ _ (L _ (WithUserRdr _ nm))), args)
      | isDataConName nm, Just fields <- hsValArgsOnly args ->
          all isClosedConTypeOperand fields
    _ -> False

isHsArgPar :: HsArg p tm ty -> Bool
isHsArgPar HsArgPar{} = True
isHsArgPar _          = False

hsValArgsOnly :: [HsArg p tm ty] -> Maybe [tm]
hsValArgsOnly []                    = Just []
hsValArgsOnly (HsArgPar{} : rest)   = hsValArgsOnly rest
hsValArgsOnly (HsValArg _ x : rest) = (x :) <$> hsValArgsOnly rest
hsValArgsOnly _                     = Nothing

-- | Find a resolved term call or a checked source family call.
nestedClosedCall :: LHsType GhcRn -> Maybe (SrcSpan, RdrName, Name, [LHsType GhcRn])
nestedClosedCall arg
  | L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr rdr name))) <- head_ty
  , isExternalName name
  , Just operands <- hsValArgsOnly args
  , not (null operands)
  , isVarName name || isSourceFamilyName name
  = Just (locA loc, rdr, name, operands)
  | otherwise = Nothing
  where
    (head_ty, args) = splitHsAppTys (unLoc (ignoreParens arg))

-- | Recognize the internal Name selected for a checked source family.
isSourceFamilyName :: Name -> Bool
isSourceFamilyName = isJust . sourceTableTermOcc . nameOccName

-- | Distinguish equally spelled input types in the caller's import context.
closedOperandMismatch :: Name -> Kind -> Type -> TcM a
closedOperandMismatch owner actual expected = do
  flags <- getDynFlags
  let short_actual = showPpr flags actual
      short_expected = showPpr flags expected
  (a, e) <- if short_actual /= short_expected
            then return (short_actual, short_expected)
            else do ctx <- getNamePprCtx
                    units <- hsc_units <$> getTopEnv
                    let qualify = showSDocForUser flags units ctx . ppr
                    return (qualify actual, qualify expected)
  failWithTc (TcRnClosedTypeEval owner
    ("The operand has kind " ++ a ++ ", which does not match the declared input type " ++ e ++ "."))

-- | Validate the monomorphic domain and codomain of a deferred partial-signature call.
retryClosedCallKinds :: Name -> Type -> TcM (Type, Type)
retryClosedCallKinds name fun_ty
  | Just (FTF_T_T, _, _, domain, codomain) <- splitFunTy_maybe rho
  , all isTyVar tvs
  = do input <- if any (tcEqType domain) retry_inputs
                  then pure domain
                  else if not (null predicates) && any (isJust . tcMatchTy domain) retry_inputs
                    then failWithTc (TcRnClosedTypeEval name
                           "The argument must be a closed Char literal or a closed list of Natural literals.")
                    else unsupported
       subst <- maybe unsupported pure (tcMatchTy domain input)
       let result = Subst.substTy subst codomain
           undetermined tv = maybe True (not . noFreeVarsOfType) (Subst.lookupTyVar subst tv)
           ground_class p = let pred_ty = Subst.substTy subst p
                            in noFreeVarsOfType pred_ty && isJust (getClassPredTys_maybe pred_ty)
       unless (not (any undetermined tvs) && all ground_class predicates && noFreeVarsOfType result &&
               Subst.substTy subst rho `tcEqType` mkVisFunTyManyU input result) unsupported
       pure (input, result)
  | otherwise = unsupported
  where
    (_, binders, rho) = splitInvisPiTys fun_ty
    tvs = mapMaybe namedPiTyBinder_maybe binders
    predicates = mapMaybe anonPiTyBinderType_maybe binders
    retry_inputs = [charTy, mkListTy naturalTy]
    unsupported :: TcM a
    unsupported = failWithTc (TcRnNoSourceTable name)

-- | Use an already known expected kind as an input to ordinary specialization.
closedCallResultHint :: Bool -> ExpKind -> TcM (Maybe Kind)
closedCallResultHint True (Check expected) = do
  kind <- liftZonkM (zonkTcType expected)
  return (nestedClosedResultHint True kind)
closedCallResultHint _ _ = return Nothing

nestedClosedResultHint :: Bool -> Kind -> Maybe Kind
nestedClosedResultHint ordinary kind =
  kind <$ guard (ordinary && noFreeVarsOfType kind && isTyFamFree kind)

{- Note [Expected result kinds for closed calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Take a result-kind hint only from a ground, family-free 'Check' context.
'tcPlanClosedTree' instantiates each application in the closed call tree once,
solves operand and result kinds together, defaults unconstrained empty lists to
@[Natural]@, normalizes ground family applications in the inferred result kind,
and validates all class dictionaries before evaluating any child expression.
-}

data ClosedCallValue
  = OrdinaryClosedCall SrcSpan Name (TcM Core.CoreExpr)
  | ClosedFamilyValue SrcSpan (TcM Type)

prepareClosedCallResult :: Kind -> ClosedCallValue -> TcM (TcM Type)
prepareClosedCallResult kind (OrdinaryClosedCall loc name expression) = setSrcSpan loc $ do
  reify <- tcPrepareClosedResult name kind
  return (expression >>= reify)
prepareClosedCallResult _ (ClosedFamilyValue _ action) = return action

closedCallRuntimeOperand :: Name -> ClosedCallValue -> TcM Core.CoreExpr
closedCallRuntimeOperand _ (OrdinaryClosedCall _ _ expression) = expression
closedCallRuntimeOperand name (ClosedFamilyValue loc action) = do
  checked <- action
  setSrcSpan loc $ do
    value <- tcDecodeClosedCallOperand name checked
    case value of
      Just argument -> return argument
      Nothing -> failWithTc (TcRnClosedTypeEval name
        "The checked operand does not represent a supported closed value.")

data ClosedCallTree
  = ClosedLiteral Name (LHsType GhcRn) Type
  | ClosedBoundary (LHsType GhcRn) Kind
  | CheckedBoundary ClosedCallValue
  | ClosedCallback SrcSpan Id Type
  | ClosedFamilyApplication SrcSpan Name TyCon [Kind] [Kind] Kind [ClosedCallTree]
  | ClosedApplication SrcSpan Id [(TyVar, Type)] [Type] [Type] Kind [ClosedCallTree]

closedCallArityError :: Name -> [Type] -> [LHsType GhcRn] -> TcM a
closedCallArityError name domains args =
  failWithTc (TcRnClosedTypeEval name
    ("Expected " ++ show (length domains) ++
     (if lengthIs domains 1 then " value argument, but got " else " value arguments, but got ") ++
     show (length args) ++ ". Closed calls must be fully applied."))

data ClosedCallHead = RunClosedFunction Id | CheckClosedCase TyCon | ReduceClosedCase TyCon

-- | Solve one kind problem for the closed call tree before preparing evidence.
-- See Note [Closed source calls] and Note [Same-module closed term evaluation].
tcPlanClosedTree :: TcTyMode -> Bool -> Maybe Kind -> ClosedCallHead -> [LHsType GhcRn]
                 -> TcM (Kind, ClosedCallValue)
tcPlanClosedTree mode ordinary hint root arguments = do
  deferred_ref <- newTcRef []
  tree <- solveEqualities "tcPlanClosedTree" $ case root of
    RunClosedFunction fun -> collect deferred_ref True fun hint arguments
    CheckClosedCase table -> collect_family deferred_ref False table hint arguments
    ReduceClosedCase table -> collect_family deferred_ref True table hint arguments
  checked <- solveEqualities "tcPlanClosedTree: checked families" (prepare_boundaries True tree)
  solveEqualities "tcPlanClosedTree: default empty lists" (default_empty checked)
  deferred <- readTcRef deferred_ref
  solveEqualities "tcPlanClosedTree: deferred operand kinds" $
    forM_ (reverse deferred) check_deferred_match
  ground <- verify True checked
  case ground of
    ClosedApplication _ _ _ _ _ result _ -> (result,) <$> prepare ground
    ClosedFamilyApplication loc owner table kinds [input] result [child]
      | CheckClosedCase{} <- root -> setSrcSpan loc $ do
          run <- case child of
            ClosedApplication call_loc fun vars _ domains _ args -> do
              selected <- closedPairProjectionField table kinds input result
              case (selected, sourceTableId_maybe fun >>= sourcePairBuilderField) of
                (Just side, Just (copied, copied_arg))
                  | side == copied
                  , Just builder <- sourceTableId_maybe fun
                  , and [i == copied_arg || is_literal child_arg | (i, child_arg) <- zip [0..] args] -> do
                      operands <- zipWithM prepare_family_operand domains args
                      pure $ do
                        values <- zipWithM (checkClosedFamilyOperand (getName fun)) domains operands
                        pure (mkTyConApp builder (map snd vars ++ values))
                (Just side, _) -> do
                  observed <- prepareObservedPair side call_loc fun (map snd vars) domains args input result
                  maybe (prepare_family_operand input child) pure observed
                _ -> prepare_family_operand input child
            _ -> prepare_family_operand input child
          let application = do
                arg <- checkClosedFamilyOperand owner input run
                return (mkTyConApp table (kinds ++ [arg]))
          return (result, ClosedFamilyValue loc application)
    ClosedFamilyApplication _ _ _ _ _ result _ -> (result,) <$> prepare ground
    _ -> panic "tcPlanClosedTree: missing call root"
  where
    is_literal ClosedLiteral{} = True
    is_literal _               = False

    meta_bind tv _
      | isTyVar tv, isMetaTyVar tv = BindMe
      | otherwise                  = DontBindMe

    tree_result (ClosedApplication _ _ _ _ _ r _)       = Just r
    tree_result (ClosedFamilyApplication _ _ _ _ _ r _) = Just r
    tree_result _                                       = Nothing

    collect deferred_ref outer_call fun expected args = do
      loc <- getSrcSpanM
      let name = getName fun
          (_, binders, rho) = splitInvisPiTys (varType fun)
          tvs = mapMaybe namedPiTyBinder_maybe binders
          predicates = mapMaybe anonPiTyBinderType_maybe binders
          (_, arrows, codomain) = splitFunTys rho
          domains = map scaledThing arrows
      checkCompleteClosedCall mode name
      tcPreflightClosedCall name
      unless (equalLength domains args) (closedCallArityError name domains args)
      unless (ordinary || lengthIs domains 1) (failWithTc (TcRnNoSourceTable name))
      unless (all isTyVar tvs && rho `tcEqType` foldr mkVisFunTyManyU codomain domains) $
        unsupported name
      (subst, variables) <- newMetaTyVars tvs
      let inputs = map (Subst.substTy subst) domains
          result = Subst.substTy subst codomain
          constraints = map (Subst.substTy subst) predicates
      forM_ expected $ \kind ->
        when (isJust (tcUnifyTys meta_bind [result] [kind])) $
          discardResult (unifyKind Nothing result kind)
      children <- sequence
        [ collect_arg deferred_ref outer_call (lengthIs args 1) name raw_dom arg inp
        | (raw_dom, arg, inp) <- zip3 domains args inputs ]
      return (ClosedApplication loc fun (zip tvs (mkTyVarTys variables))
                constraints inputs result children)

    collect_family deferred_ref allow_unary_table table expected args = do
      loc <- getSrcSpanM
      owner <- closedFamilyOwner (getName table)
      let (invisible, visible) = span isInvisibleTyConBinder (tyConBinders table)
          tvs = [tv | Bndr tv _ <- invisible]
          domain (Bndr tv AnonTCB) = Just (tyVarKind tv)
          domain _ = Nothing
          unary_table = allow_unary_table && lengthIs args 1 && lengthIs visible 1
      checkCompleteClosedCall mode owner
      when (allow_unary_table && not (isSourceConstructorCase table)) $
        sourceTableFamilyUse table
      case traverse domain visible of
        Just domains
          | supportsClosedFamilyTree table args || unary_table
          , all isTyVar tvs
          , tyConArity table == length tvs + length domains -> do
              (subst, variables) <- newMetaTyVars tvs
              let inputs = map (Subst.substTy subst) domains
                  result = Subst.substTy subst (tyConResKind table)
              forM_ expected $ \kind ->
                when (isJust (tcUnifyTys meta_bind [result] [kind])) $
                  discardResult (unifyKind Nothing result kind)
              children <- sequence
                [ collect_arg deferred_ref False (lengthIs args 1) owner raw_dom arg inp
                | (raw_dom, arg, inp) <- zip3 domains args inputs ]
              return (ClosedFamilyApplication loc owner table (mkTyVarTys variables) inputs result children)
        _ -> bad_family owner

    record_child_kind deferred_ref outer_call is_unary name raw_dom arg expected child =
      case tree_result child of
        Nothing -> pure child
        Just child_res
          | outer_call, not (isTyVarTy raw_dom) -> do
              updTcRef deferred_ref ((getLocA arg, unLoc arg, name, raw_dom, child_res, expected) :)
              pure child
          | outer_call && is_unary -> do
              updTcRef deferred_ref ((getLocA arg, unLoc arg, name, raw_dom, child_res, expected) :)
              pure child
          | otherwise -> do
              setSrcSpanA (getLoc arg) $
                discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) child_res expected)
              pure child

    child_hint outer_call is_unary raw_dom expected
      | outer_call && is_unary && not (isTyVarTy raw_dom) = do
          exp' <- liftZonkM (zonkTcType expected)
          pure (nestedClosedResultHint ordinary exp')
      | otherwise = pure (Just expected)

    collect_arg deferred_ref outer_call is_unary name raw_dom arg expected
      | ordinary, Just (loc, rdr, child) <- bareClosedCallback arg
      = setSrcSpan loc $ do
          callback <- tcClosedCallback rdr child
          let (_, binders, rho) = splitInvisPiTys (varType callback)
              tvs = mapMaybe namedPiTyBinder_maybe binders
              (_, arrows, result) = splitFunTys rho
              domains = map scaledThing arrows
          unless (all isTyVar tvs && equalLength tvs binders &&
                  not (null domains) && rho `tcEqType` foldr mkVisFunTyManyU result domains) $
            failWithTc (TcRnClosedTypeEval (getName callback)
              "A closed callback must have an ordinary function type without constraints.")
          (subst, _) <- newMetaTyVars tvs
          let actual = Subst.substTy subst rho
          discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) actual expected)
          return (ClosedCallback loc callback actual)
      | Just (loc, rdr, child, args) <- nestedClosedCall arg
      , isSourceFamilyName child, ordinary
      = setSrcSpan loc $ do
          fallback <- localFallbackFamilyOwner child
          case fallback of
            Just fun -> do
              staging <- closedOperandStaging args
              if isJust staging
                then if any bare_empty args
                       then return (ClosedBoundary arg expected)
                       else do
                         ch_hint <- child_hint outer_call is_unary raw_dom expected
                         c <- collect deferred_ref False fun ch_hint args
                         record_child_kind deferred_ref outer_call is_unary name raw_dom arg expected c
                else failWithTc (TcRnNoSourceTable (getName fun))
            Nothing -> do
              staging <- closedOperandStaging args
              table <- tcLookupTyCon child
              if admissibleFamilyOperands args staging && supportsClosedFamilyTree table args
                then do
                  executable <- tcClosedCaseExecutableOwner True rdr child args staging
                  ch_hint <- child_hint outer_call is_unary raw_dom expected
                  c <- case executable of
                    Just fun -> collect deferred_ref False fun ch_hint args
                    Nothing  -> collect_family deferred_ref False table ch_hint args
                  record_child_kind deferred_ref outer_call is_unary name raw_dom arg expected c
                else return (ClosedBoundary arg expected)
      | Just (loc, _, child, args) <- nestedClosedCall arg
      = if isSourceFamilyName child || any bare_empty args
          then return (ClosedBoundary arg expected)
          else setSrcSpan loc $ do
            env <- getGblEnv
            let inter_child = isInteractiveStmtTerm env child
            fun <- if not inter_child && nameIsLocalOrFrom (tcg_semantic_mod env) child
              then case lookupNameEnv (tcg_local_open_defs env) child of
                     Just (f, _) -> pure f
                     Nothing     -> failWithTc (TcRnNoSourceTable child)
              else case lookupTypeEnv (tcg_type_env env) child of
                     Just (AnId f) -> pure f
                     _ -> do
                       thing <- tcLookupGlobal child
                       case thing of
                         AnId f -> pure f
                         _      -> failWithTc (TcRnNoSourceTable child)
            ch_hint <- child_hint outer_call is_unary raw_dom expected
            c <- collect deferred_ref False fun ch_hint args
            record_child_kind deferred_ref outer_call is_unary name raw_dom arg expected c
      | otherwise = do
          call_loc <- getSrcSpanM
          setSrcSpanA (getLoc arg) $ do
            let lit_kind = closedCallLiteralKind arg
            unless (ordinary || isJust lit_kind) (failWithTc (TcRnNoSourceTable name))
            eligible <- if closedConstructorLiteral arg
                          then pure True
                          else closedConstructorAlias arg
            unless eligible $ do
              declared <- liftZonkM (zonkTcType expected)
              let scalars = [naturalTy, charTy, boolTy]
                  cand_kinds = scalars ++ map mkListTy scalars
                  matched_kinds = filter (isJust . tcMatchTy raw_dom) cand_kinds
                  has_con_head = case splitHsAppTys (unLoc (ignoreParens arg)) of
                    (L _ (HsTyVar _ _ (L _ (WithUserRdr _ c))), _) -> isDataConName c
                    _ -> False
                  var_or_field_reason
                    | L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ var))) <- ignoreParens arg
                    , isTyVarName var
                    = "This argument is a type variable. Closed evaluation requires a known value."
                    | otherwise
                    = "Each constructor field must be a closed literal. This operand contains an open or unsupported field."
              if outer_call && is_unary && declared `tcEqType` charTy
                then setSrcSpan call_loc $
                       failWithTc (TcRnClosedTypeEval name "The argument must be a closed Char literal, such as 'x'.")
                else if outer_call && is_unary && declared `tcEqType` mkListTy naturalTy
                then setSrcSpan call_loc $
                       failWithTc (TcRnClosedTypeEval name "The argument must be a closed list of Natural literals, such as '[1, 2, 3].")
                else if outer_call && is_unary && isNothing lit_kind
                then case matched_kinds of
                       [inp] -> do
                         dflags <- getDynFlags
                         failWithTc (TcRnClosedTypeEval name ("The argument must be a closed literal of kind " ++ showPpr dflags inp ++ "."))
                       [] | has_con_head -> failWithTc (TcRnClosedTypeEval name var_or_field_reason)
                          | otherwise    -> setSrcSpan call_loc (failWithTc (TcRnNoSourceTable name))
                       inps -> do
                         dflags <- getDynFlags
                         failWithTc (TcRnClosedTypeEval name ("The argument must be a closed literal of one of these kinds: " ++ intercalate ", " (map (showPpr dflags) inps) ++ "."))
                else failWithTc (TcRnClosedTypeEval name var_or_field_reason)
            unless (bare_empty arg) $
              forM_ lit_kind $ \actual -> do
                when outer_call $ do
                  declared <- liftZonkM (zonkTcType expected)
                  when (noFreeVarsOfType declared && isTyFamFree declared &&
                        not (actual `tcEqType` declared)) $
                    closedOperandMismatch name actual declared
                discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) actual expected)
            literal <- tc_check_lhs_type mode arg expected
            return (ClosedLiteral name arg literal)

    check_deferred_match (arg_span, arg_ty, owner, raw_dom, actual, expected) = do
      actual' <- liftZonkM (zonkTcType actual)
      expected' <- liftZonkM (zonkTcType expected)
      if not (isTyVarTy raw_dom) && isNothing (tcUnifyTys meta_bind [actual'] [expected'])
        then setSrcSpan arg_span (closedOperandMismatch owner actual' raw_dom)
        else setSrcSpan arg_span $
               discardResult (unifyKind (Just (HsTypeRnThing arg_ty)) actual' expected')

    prepare_boundaries _ (ClosedBoundary arg expected)
      | Just (loc, rdr, name, args) <- nestedClosedCall arg = do
        expected' <- liftZonkM (zonkTcType expected)
        let hint' = nestedClosedResultHint ordinary expected'
        (actual, value) <- setSrcSpan loc (tcPlanClosedHead mode ordinary hint' rdr name args)
        setSrcSpanA (getLoc arg) $
          discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) actual expected)
        return (CheckedBoundary value)
    prepare_boundaries True (ClosedApplication loc fun vars predicates inputs result [ClosedBoundary arg expected])
      | Just (b_loc, rdr, name, args) <- nestedClosedCall arg = do
        expected' <- liftZonkM (zonkTcType expected)
        let hint' = nestedClosedResultHint ordinary expected'
            (_, _, rho) = splitInvisPiTys (varType fun)
            (_, arrows, _) = splitFunTys rho
        (actual, value) <- setSrcSpan b_loc (tcPlanClosedHead mode ordinary hint' rdr name args)
        case map scaledThing arrows of
          [raw_dom] | not (isTyVarTy raw_dom) && isNothing (tcUnifyTys meta_bind [actual] [expected']) ->
            setSrcSpanA (getLoc arg) (closedOperandMismatch (getName fun) actual raw_dom)
          _ -> setSrcSpanA (getLoc arg) $
                 discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) actual expected)
        return (ClosedApplication loc fun vars predicates inputs result [CheckedBoundary value])
    prepare_boundaries _ (ClosedApplication loc fun vars predicates inputs result children) =
      ClosedApplication loc fun vars predicates inputs result <$> traverse (prepare_boundaries False) children
    prepare_boundaries _ (ClosedFamilyApplication loc owner table kinds inputs result children) =
      ClosedFamilyApplication loc owner table kinds inputs result <$> traverse (prepare_boundaries False) children
    prepare_boundaries _ tree = return tree

    default_empty (ClosedLiteral _ arg literal)
      | bare_empty arg = do
          actual <- liftZonkM (zonkTcType (typeKind literal))
          case splitTyConApp_maybe actual of
            Just (tc, [element]) | tc == listTyCon
                                 , Just tv <- getTyVar_maybe element
                                 , isMetaTyVar tv ->
              setSrcSpanA (getLoc arg) (discardResult (unifyKind Nothing element naturalTy))
            _ -> return ()
    default_empty (ClosedApplication _ _ _ _ _ _ children) = mapM_ default_empty children
    default_empty (ClosedFamilyApplication _ _ _ _ _ _ children) = mapM_ default_empty children
    default_empty _ = return ()

    verify _ (ClosedLiteral name arg literal) = do
      checked <- liftZonkM (zonkTcType literal)
      unless (noFreeVarsOfType checked) $ setSrcSpanA (getLoc arg) $
        failWithTc (TcRnClosedTypeEval name
          "The constructor literal does not determine a closed operand kind.")
      return (ClosedLiteral name arg checked)
    verify _ (ClosedCallback loc fun ty) = setSrcSpan loc $ do
      checked <- liftZonkM (zonkTcType ty)
      unless (noFreeVarsOfType checked && isTyFamFree checked) $
        failWithTc (TcRnClosedTypeEval (getName fun)
          "The enclosing call does not determine the callback's ordinary type.")
      return (ClosedCallback loc fun checked)
    verify _ tree@CheckedBoundary{} = return tree
    verify _ ClosedBoundary{} = panic "tcPlanClosedTree: unprepared boundary"
    verify _ (ClosedApplication loc fun vars constraints inputs result children) = setSrcSpan loc $ do
      let name = getName fun
          ground ty = noFreeVarsOfType ty && isTyFamFree ty
          child_app_first = case children of { [ClosedApplication{}] -> True; _ -> False }
      pre_children <- if child_app_first then Just <$> traverse (verify False) children else pure Nothing
      values <- liftZonkM (zonkTcTypes (map snd vars))
      let unresolved = [getOccString tv | ((tv, _), value) <- zip vars values, not (ground value)]
          subject = case children of
            [ClosedLiteral _ syntax _] | isJust (closedCallLiteralKind syntax) -> "The literal argument does"
            [ClosedApplication{}]       -> "The checked operand does"
            [ClosedFamilyApplication{}] -> "The checked operand does"
            [CheckedBoundary{}]         -> "The checked operand does"
            _                           -> "The operands do"
      unless (null unresolved) $
        failWithTc (TcRnClosedTypeEval name
          (subject ++ " not determine the following type parameters: " ++ unwords unresolved ++ "."))
      constraints' <- liftZonkM (zonkTcTypes constraints)
      inputs' <- liftZonkM (zonkTcTypes inputs)
      unless (all ground inputs' &&
              all (\p -> ground p && isJust (getClassPredTys_maybe p)) constraints') $
        unsupported name
      zonked_result <- liftZonkM (zonkTcType result)
      result' <- if noFreeVarsOfType zonked_result && not (isTyFamFree zonked_result)
        then do norm_loc <- getCtLocM AnnOrigin Nothing
                reductionReducedType <$> tcRewriteFamilyAxioms norm_loc zonked_result
        else return zonked_result
      unless (ground result') $ unsupported name
      checked_children <- maybe (traverse (verify False) children) pure pre_children
      pure (ClosedApplication loc fun (zip (map fst vars) values) [] inputs' result' checked_children)

    verify _ (ClosedFamilyApplication loc owner table kinds inputs result children) = setSrcSpan loc $ do
      checked_children <- traverse (verify False) children
      kinds' <- liftZonkM (zonkTcTypes kinds)
      inputs' <- liftZonkM (zonkTcTypes inputs)
      result' <- liftZonkM (zonkTcType result)
      unless (all (\ty -> noFreeVarsOfType ty && isTyFamFree ty) (result' : inputs' ++ kinds')) $
        bad_family owner
      pure (ClosedFamilyApplication loc owner table kinds' inputs' result' checked_children)

    bad_family owner = failWithTc (TcRnClosedTypeEval owner
      "The operands do not determine the checked family's input and result kinds.")

    prepare (ClosedCallback loc fun ty) = setSrcSpan loc $ do
      core <- tcPrepareClosedFunctionValue fun ty
      return (OrdinaryClosedCall loc (getName fun) (return core))
    prepare (CheckedBoundary value) = return value
    prepare (ClosedLiteral name arg literal) = setSrcSpanA (getLoc arg) $ do
      loc <- getSrcSpanM
      value <- tcDecodeClosedCallOperand name literal
      case value of
        Just core -> return (OrdinaryClosedCall loc name (return core))
        Nothing -> failWithTc (TcRnClosedTypeEval name
          "The checked argument does not represent a supported closed value.")
    prepare (ClosedApplication loc fun _ _ inputs result children) = setSrcSpan loc $ do
      let owner = getName fun
          rigid (ClosedLiteral _ syntax _) = closedConstructorSyntax False syntax
          rigid _ = False
      apply <- tcPrepareClosedApplication fun inputs result (map rigid children)
      operands <- mapM (fmap (closedCallRuntimeOperand owner) . prepare) children
      return (OrdinaryClosedCall loc owner (sequence operands >>= apply))
    prepare (ClosedFamilyApplication loc owner table kinds [input] result [child]) =
      setSrcSpan loc $ case child of
        ClosedLiteral _ _ literal -> nominal (pure literal)
        _ -> do
          value <- prepare child
          projection <- prepareClosedPairProjection owner table kinds input result value
          case projection of
            Just projected -> pure projected
            Nothing -> prepareClosedCallResult input value >>= nominal
      where
        nominal operand =
          ClosedFamilyValue loc <$> prepareClosedFamilyResult owner table kinds [input] result [operand]
    prepare (ClosedFamilyApplication loc owner table kinds inputs result children) = setSrcSpan loc $ do
      operands <- zipWithM prepare_family_operand inputs children
      ClosedFamilyValue loc <$> prepareClosedFamilyResult owner table kinds inputs result operands
    prepare ClosedBoundary{} = panic "tcPlanClosedTree: unprepared boundary"

    prepareObservedPair side loc fun params domains args input result = runMaybeT $ do
      (tc, [left, right]) <- MaybeT (pure (splitTyConApp_maybe input))
      guard (tc == pairTyCon)
      field <- MaybeT (lookupObservedPairField side fun params (pickLR side (right, left)))
      observer <- MaybeT (lookupPairObserver side fun (length domains))
      let owner = getName fun
      available <- lift (tcHasClosedCallExecutable owner)
      permitted <- lift (canObservePairSource fun)
      guard (available && permitted)
      lift $ setSrcSpan loc $ do
        apply <- tcPrepareClosedApplication fun domains input []
        operands <- mapM (fmap (closedCallRuntimeOperand owner) . prepare) args
        select <- closedPairProjectionExpression side input left right result
        reify <- tcPrepareClosedResult owner input
        reify_selected <- tcPrepareClosedResult owner result
        env <- getGblEnv
        pure $ do
          partial <- readPairRecipe owner emptyNameSet observer
          expression <- sequence operands >>= apply
          if partial then do
            selected <- reify_selected (select expression)
            recordSourceUses [(fun, Nothing)]
            ordinal <- readTcRef (tcg_zany_n env)
            updTcRef (tcg_zany_n env) (+ 1)
            updTcRef (tcg_source_check env) (\s -> s { scs_evaluated_uses = extendNameSet (scs_evaluated_uses s) (tyConName field) })
            let mod = tcg_semantic_mod env
                key = mkStrLitTy (fsLit (show (unitString (moduleUnit mod),
                                               moduleNameString (moduleName mod), ordinal)))
                unread = mkTyConApp field (params ++ [key])
                (x, y) = pickLR side ((selected, unread), (unread, selected))
            pure (mkPromotedPairTy left right x y)
          else reify expression

    prepare_family_operand _ (ClosedLiteral _ _ literal) = return (return literal)
    prepare_family_operand kind child = prepare child >>= prepareClosedCallResult kind

    unsupported name = failWithTc (TcRnClosedTypeEval name
      "The call must have a fully instantiated ordinary function type.")

    bare_empty arg = case ignoreParens arg of
      L _ (HsExplicitListTy _ _ []) -> True
      _ -> False

readPairRecipe :: Name -> NameSet -> Id -> TcM Bool
readPairRecipe owner seen entry
  | name `elemNameSet` seen = pure False
  | otherwise = do
      recipe <- readRecipeEntry owner entry
      maybe (pure False) (readRecipeValue owner (extendNameSet seen name)) recipe
  where
    name = getName entry

readRecipeEntry :: Name -> Id -> TcM (Maybe String)
readRecipeEntry owner entry = do
  observe <- tcPrepareClosedObservation owner stringTy (getName entry)
  quoted <- observe (Core.Var entry)
  pure (quoted >>= readbackString)

readRecipeValue :: Name -> NameSet -> String -> TcM Bool
readRecipeValue _ _ "!" = pure True
readRecipeValue owner seen reference
  | Just (unit, mod, occurrence, arity) <- readMaybe reference = do
      let defining = mkModule (stringToUnit unit) (mkModuleName mod)
      (table, legacy) <- lookupPairRecipe defining (mkVarOcc occurrence) arity
      rows <- maybe (pure Nothing) (readRecipeEntry owner) table
      case rows of
        Nothing -> maybe (pure False) (readPairRecipe owner seen) legacy
        Just contents -> case legacy of
          Just key
            | let name = getName key
            , not (name `elemNameSet` seen)
            , Just entries <- readMaybe contents
            , [recipe] <- [row | (occ, n, row) <- entries, occ == occurrence, n == arity]
            -> readRecipeValue owner (extendNameSet seen name) recipe
          _ -> pure False
  | otherwise = pure False

readbackString :: Type -> Maybe String
readbackString ty = case splitTyConApp_maybe ty of
  Just (con, [_]) | con == promoteDataCon nilDataCon -> Just []
  Just (con, [_, char, tail_ty]) | con == promoteDataCon consDataCon ->
    (:) <$> isCharLitTy char <*> readbackString tail_ty
  _ -> Nothing

prepareClosedPairProjection :: Name -> TyCon -> [Kind] -> Kind -> Kind
                            -> ClosedCallValue -> TcM (Maybe ClosedCallValue)
prepareClosedPairProjection owner table kinds input result (OrdinaryClosedCall _ _ expression)
  | Just (_, [left_kind, right_kind]) <- splitTyConApp_maybe input = do
      field <- closedPairProjectionField table kinds input result
      case field of
        Just side -> do
          loc <- getSrcSpanM
          sourceCaseKindUse table kinds
          observe <- closedPairProjectionExpression side input left_kind right_kind result
          return (Just (OrdinaryClosedCall loc owner (observe <$> expression)))
        Nothing -> return Nothing
prepareClosedPairProjection _ _ _ _ _ _ = return Nothing

closedPairProjectionExpression :: LeftOrRight -> Kind -> Kind -> Kind -> Kind
                               -> TcM (Core.CoreExpr -> Core.CoreExpr)
closedPairProjectionExpression side input left right result = do
  binder <- mkSysLocalM (fsLit "closed_pair") ManyTy input
  a <- mkSysLocalM (fsLit "closed_first") ManyTy left
  b <- mkSysLocalM (fsLit "closed_second") ManyTy right
  pure $ \scrutinee -> Core.Case scrutinee binder result
    [Core.Alt (Core.DataAlt (tupleDataCon Boxed 2)) [a, b] (Core.Var (pickLR side (a, b)))]

closedPairProjectionField :: TyCon -> [Kind] -> Kind -> Kind -> TcM (Maybe LeftOrRight)
closedPairProjectionField table kinds input result
  | sourceCaseVisibleArity table == Just 1
  , Just (tc, [left_kind, right_kind]) <- splitTyConApp_maybe input
  , tc == tupleTyCon Boxed 2 = do
      loc <- getSrcSpanM
      let fresh_field kind = do
            name <- mkInternalName <$> newUnique <*> pure (mkTyVarOcc "field") <*> pure loc
            return (mkTyVarTy (mkTyVar name kind))
      left <- fresh_field left_kind
      right <- fresh_field right_kind
      envs <- tcGetFamInstEnvs
      let pair = mkPromotedPairTy left_kind right_kind left right
      return $ do
        reduction <- reduceTyFamApp_maybe envs Nominal table (kinds ++ [pair])
        let field = reductionReducedType reduction
        if field `tcEqType` left && result `tcEqType` left_kind then Just CLeft
        else if field `tcEqType` right && result `tcEqType` right_kind then Just CRight
        else Nothing
closedPairProjectionField _ _ _ _ = return Nothing

checkClosedCallExpectedKind :: HsType GhcRn -> ExpKind -> Kind -> TcM ()
checkClosedCallExpectedKind rn_ty (Check expected) actual = do
  expected' <- liftZonkM (zonkTcType expected)
  when (noFreeVarsOfType expected' && isTyFamFree expected' && not (actual `tcEqType` expected')) $
    solveEqualities "checkClosedCallExpectedKind" $
      discardResult (unifyKind (Just (HsTypeRnThing rn_ty)) actual expected')
checkClosedCallExpectedKind _ (Infer _) _ = return ()

tcPlanClosedHead :: TcTyMode -> Bool -> Maybe Kind -> RdrName -> Name
                 -> [LHsType GhcRn] -> TcM (Kind, ClosedCallValue)
tcPlanClosedHead mode ordinary_call hint rdr name args
  | isSourceFamilyName name = do
      fallback <- localFallbackFamilyOwner name
      case fallback of
        Just fun -> do
          staging <- closedOperandStaging args
          if isJust staging
            then tcPlanClosedTree mode ordinary_call hint (RunClosedFunction fun) args
            else failWithTc (TcRnNoSourceTable (getName fun))
        Nothing -> do
          staging <- closedOperandStaging args
          executable <- tcClosedCaseExecutableOwner True rdr name args staging
          case executable of
            Just owner -> tcPlanClosedTree mode ordinary_call hint (RunClosedFunction owner) args
            Nothing -> do
              table <- tcLookupTyCon name
              if (ordinary_call && admissibleFamilyOperands args staging && supportsClosedFamilyTree table args)
                 || lengthIs args 1
                then tcPlanClosedTree mode ordinary_call hint (ReduceClosedCase table) args
                else failWithTc (TcRnNoSourceTable name)
  | otherwise = do
      env <- getGblEnv
      let inter_term = isInteractiveStmtTerm env name
      if not inter_term && nameIsLocalOrFrom (tcg_semantic_mod env) name
        then case lookupNameEnv (tcg_local_open_defs env) name of
               Just (fun, _) -> do
                 staging <- closedOperandStaging args
                 if isJust staging
                   then tcPlanClosedTree mode ordinary_call hint (RunClosedFunction fun) args
                   else failWithTc (TcRnNoSourceTable name)
               Nothing -> failWithTc (TcRnNoSourceTable name)
        else case lookupTypeEnv (tcg_type_env env) name of
               Just (AnId fun) -> tcPlanClosedTree mode ordinary_call hint (RunClosedFunction fun) args
               _ -> do
                 thing <- tcLookupGlobal name
                 case thing of
                   AnId fun -> tcPlanClosedTree mode ordinary_call hint (RunClosedFunction fun) args
                   _ -> failWithTc (TcRnNoSourceTable name)

isInteractiveStmtTerm :: TcGblEnv -> Name -> Bool
isInteractiveStmtTerm env nm =
  isInteractiveModule (tcg_semantic_mod env)
  && case lookupTypeEnv (tcg_type_env env) nm of
       Just (AnId _) -> isInternalName nm || nameModule_maybe nm /= Just (tcg_semantic_mod env)
       _             -> False

-- | Resolve a local @T:f@ family occurrence when @f@ fell back to
-- @tcg_local_open_defs@ for same-module closed evaluation.
-- See Note [Same-module closed term evaluation].
localFallbackFamilyOwner :: Name -> TcM (Maybe Id)
localFallbackFamilyOwner name
  | not (isSourceFamilyName name) = pure Nothing
  | otherwise = do
      env <- getGblEnv
      if nameIsLocalOrFrom (tcg_semantic_mod env) name
         && isNothing (lookupTypeEnv (tcg_type_env env) name)
        then do
          owner <- closedFamilyOwner name
          exec_ok <- tcHasClosedCallExecutable owner
          pure $ if exec_ok
            then fst <$> lookupNameEnv (tcg_local_open_defs env) owner
            else Nothing
        else pure Nothing

{- Note [Same-module closed term evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an earlier top-level value binding group in the current module is
referenced in a later type signature or declaration on concrete closed
arguments without a certified 'SourceTable', GHC evaluates the closed call
in-module through the target interpreter:
  1. Type-level lookup records local value demands in 'tcg_local_open_demands',
     and "GHC.Tc.Module" typechecks and desugars the earlier binding group into
     'tcg_local_open_defs'. Open operands without a 'SourceTable' report
     'TcRnNoSourceTable'.
  2. 'GHC.Tc.Gen.ClosedEval.rewriteSameModuleClosedExpr' rewrites references in
     'tcg_local_open_defs' to interactive BCO 'GlobalId's, and
     'GHC.Driver.Main.Passes.hscLoadSameModuleClosedBcos' loads them into the
     active interpreter so evaluation works under @-fno-code@ without reading a
     stale object file. Core plugin passes that mutate evaluated bindings are
     rejected after simplification.
-}

tcClosedCaseExecutableOwner :: Bool -> RdrName -> Name -> [LHsType GhcRn] -> Maybe Bool
                            -> TcM (Maybe Id)
tcClosedCaseExecutableOwner nested rdr name args staging
  | isQual rdr || isUnqual rdr
  , Just needs_execution <- staging
  = do
      env <- getGblEnv
      table <- tcLookupTyCon name
      let expect_eager = monomorphicEnumCaseArity (tyConKind table) == Just (length args)
          visible = filterOut isInvisibleTyConBinder (tyConBinders table)
          nominal_prefix = length args < length visible && all nominalCallback args
          staged = needs_execution &&
                   (sourceCaseVisibleArity table == Just (length args) ||
                    (not nominal_prefix && any (isJust . bareClosedCallback) args))
          nested_case = nested && lengthIs args 2 && sourceCaseVisibleArity table == Just 2
      owner <- if expect_eager || staged || nested_case then sourceTableOwner table else pure Nothing
      case owner of
        Just fun
          | not (nameIsLocalOrFrom (tcg_semantic_mod env) (getName fun))
          , staged || (expect_eager && sourceTableKeepsExecutable fun) ||
                      (nested_case && isFinalStructuralSource fun) -> do
          imported <- isClosedOrdinaryImport rdr (getName fun)
          if imported
            then do
              executable <- tcHasClosedCallExecutable (getName fun)
              pure (fun <$ guard executable)
            else pure Nothing
        _ -> pure Nothing
  | otherwise = pure Nothing

bareClosedCallback :: LHsType GhcRn -> Maybe (SrcSpan, RdrName, Name)
bareClosedCallback arg
  | L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr rdr name))) <- ignoreParens arg
  , isExternalName name, isVarName name || isSourceFamilyName name
  = Just (locA loc, rdr, name)
  | otherwise = Nothing

nominalCallback :: LHsType GhcRn -> Bool
nominalCallback arg
  | Just (_, _, name) <- bareClosedCallback arg = isSourceFamilyName name
  | otherwise = False

checkOpenCallbackInterfaces :: TyCon -> [LHsType GhcRn] -> TcM ()
checkOpenCallbackInterfaces table args
  | lengthIs visible (length args) = zipWithM_ check visible args
  | otherwise = pure ()
  where
    visible = dropWhile isInvisibleTyConBinder (tyConBinders table)
    check (Bndr binder AnonTCB) operand
      | Just (loc, rdr, name) <- bareClosedCallback operand
      , isVarName name
      , Just (FTF_T_T, ma, mult, _, _) <- splitFunTy_maybe (varType binder)
      , ma `tcEqType` unmatchableDataConTy, isManyTy mult
      = setSrcSpan loc (failWithTc (TcRnNoSourceCallback rdr))
    check _ _ = pure ()

checkNominalCallback :: LHsType GhcRn -> TcM ()
checkNominalCallback arg
  | Just (loc, rdr, name) <- bareClosedCallback arg, isSourceFamilyName name
  = setSrcSpan loc $ do
      fun <- tcClosedCallback rdr name
      fallback <- localFallbackFamilyOwner name
      when (isNothing fallback) $ do
        table <- tcLookupTyCon name
        owner <- sourceTableOwner table
        unless (owner == Just fun) $
          failWithTc (TcRnNoSourceTable (getName fun))
  | otherwise = return ()

tcClosedCallback :: RdrName -> Name -> TcM Id
tcClosedCallback rdr name = do
  owner <- closedFamilyOwner name
  imported <- isClosedOrdinaryImport rdr owner
  unless imported $ failWithTc (TcRnClosedTypeEval owner
    "A closed callback needs an ordinary non-SOURCE import.")
  env <- getGblEnv
  let inter_owner = isInteractiveStmtTerm env owner
  if not inter_owner && nameIsLocalOrFrom (tcg_semantic_mod env) owner
    then case lookupNameEnv (tcg_local_open_defs env) owner of
           Just (fun, _) -> return fun
           Nothing -> case lookupTypeEnv (tcg_type_env env) owner of
             Just (AnId fun) -> return fun
             _ -> failWithTc (TcRnNoSourceTable owner)
    else case lookupTypeEnv (tcg_type_env env) owner of
           Just (AnId fun) -> return fun
           _ -> do
             thing <- tcLookupGlobal owner
             case thing of
               AnId fun -> return fun
               _ -> failWithTc (TcRnNoSourceTable owner)

isClosedOrdinaryImport :: RdrName -> Name -> TcM Bool
isClosedOrdinaryImport rdr name = do
  env <- getGblEnv
  rdr_env <- getGlobalRdrEnv
  let occurrence = nameOccName name
      term_rdr = case isQual_maybe rdr of
        Just (qualifier, _) -> mkRdrQual qualifier occurrence
        Nothing -> mkRdrUnqual occurrence
      local_certified =
        name `elemNameEnv` tcg_local_open_defs env ||
        case lookupTypeEnv (tcg_type_env env) name of
          Just (AnId fun) -> isJust (sourceTableId_maybe fun)
          _ -> False
      imported gre = greName gre == name &&
                     (any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre) ||
                      maybe False isInteractiveModule (nameModule_maybe name) ||
                      (isLocalGRE gre && local_certified))
  return (any imported (lookupGRE rdr_env (LookupRdrName term_rdr (RelevantGREsFOS WantNormal))))

admissibleFamilyOperands :: [LHsType GhcRn] -> Maybe Bool -> Bool
admissibleFamilyOperands args = maybe False (|| lengthIs args 2)

closedOperandStaging :: [LHsType GhcRn] -> TcM (Maybe Bool)
closedOperandStaging = runMaybeT . fmap or . traverse contains_staged_call
  where
    contains_staged_call operand
      | Just _ <- bareClosedCallback operand = return True
      | Just (_, _, head_name, args) <- nestedClosedCall operand = do
          children <- traverse contains_staged_call args
          return (isVarName head_name || or children)
      | closedConstructorLiteral operand = return False
      | otherwise = do
          rhs <- MaybeT (closedConstructorAliasRhs operand)
          guard (closed_value rhs)
          return False

    closed_value ty = case splitTyConApp_maybe ty of
      Just (con, args)
        | Just dc <- isPromotedDataCon_maybe con
        -> let (_, fields) = splitAt (length (dataConUserTyVars dc)) args
           in all closed_value fields
      _ -> isJust (isNumLitTy ty) || isJust (isCharLitTy ty)

supportsClosedFamilyTree :: TyCon -> [LHsType GhcRn] -> Bool
supportsClosedFamilyTree table args =
  let arity = length args
  in arity <= 2 && sourceCaseVisibleArity table == Just arity

closedFamilyOwner :: Name -> TcM Name
closedFamilyOwner name = case sourceTableTermOcc (nameOccName name) of
  Just occ -> lookupOrig (nameModule name) occ
  Nothing -> return name

checkClosedFamilyOperand :: Name -> Kind -> TcM Type -> TcM Type
checkClosedFamilyOperand owner kind run = do
  value <- run
  unless (typeKind value `tcEqType` kind && noFreeVarsOfType value) $
    failWithTc (TcRnClosedTypeEval owner "The checked family operand does not have the planned closed kind.")
  return value

prepareClosedFamilyResult :: Name -> TyCon -> [Kind] -> [Kind] -> Kind
                          -> [TcM Type] -> TcM (TcM Type)
prepareClosedFamilyResult owner table kind_args input_kinds result_kind run_args = do
  when (isSourceConstructorCase table) (sourceCaseKindUse table kind_args)
  loc <- getSrcSpanM
  let bad :: String -> TcM a
      bad = failWithTc . TcRnClosedTypeEval owner
      checked = checkClosedFamilyOperand owner
      reduce args = do
        norm_loc <- getCtLocM AnnOrigin Nothing
        reductionReducedType <$> tcRewriteFamilyAxioms norm_loc
          (mkTyConApp table (kind_args ++ args))
  return $ setSrcSpan loc $ do
    result <- case (input_kinds, run_args) of
      ([domain], [run]) -> checked domain run >>= reduce . (:[])
      ([domain, fallback_kind], [run, fallback]) -> do
        input <- checked domain run
        uniq <- newUnique
        let marker = mkTyVarTy (mkTyVar (mkInternalName uniq (mkTyVarOcc "fallback") loc) fallback_kind)
        reduced <- reduce [input, marker]
        if reduced `tcEqType` marker
          then checked fallback_kind fallback
          else if noFreeVarsOfType reduced
            then return reduced
            else bad "The checked family did not select a closed result or its fallback."
      _ -> bad "The checked family does not have a supported operand count."
    unless (typeKind result `tcEqType` result_kind && noFreeVarsOfType result) $
      bad "The checked family result does not have the planned closed kind."
    return result

checkCompleteClosedCall :: TcTyMode -> Name -> TcM ()
checkCompleteClosedCall mode name =
  when (mode_complete_type_arg mode == Just False) $
    failWithTc (TcRnClosedTypeEval name
      "A visible type argument containing a closed call must be complete. Replace its wildcards with types.")

closedConstructorLiteral :: LHsType GhcRn -> Bool
closedConstructorLiteral = closedConstructorSyntax True

closedConstructorSyntax :: Bool -> LHsType GhcRn -> Bool
closedConstructorSyntax allow_literals arg = case unLoc (ignoreParens arg) of
  HsExplicitListTy _ _ elements -> all constructor elements
  HsExplicitTupleTy _ _ fields -> lengthIs fields 2 && all constructor fields
  HsTyLit _ HsNatural{} -> allow_literals
  HsTyLit _ HsChar{} -> allow_literals
  syntax -> case splitHsAppTys syntax of
    (L _ (HsTyVar _ _ (L _ (WithUserRdr _ con))), args)
      | isDataConName con -> all closed_argument args
    _ -> False
  where
    constructor = closedConstructorSyntax allow_literals
    closed_argument HsArgPar{} = True
    closed_argument (HsValArg _ operand) = constructor operand
    closed_argument _ = False

closedConstructorAlias :: LHsType GhcRn -> TcM Bool
closedConstructorAlias = fmap isJust . closedConstructorAliasRhs

closedConstructorAliasRhs :: LHsType GhcRn -> TcM (Maybe Type)
closedConstructorAliasRhs arg
  | L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))) <- ignoreParens arg
  , isExternalName name, isTyConName name = do
      env <- getGblEnv
      if nameIsLocalOrFrom (tcg_semantic_mod env) name
        then return $ case lookupTypeEnv (tcg_type_env env) name of
          Just (ATyCon tc) | tyConArity tc == 0 -> synTyConRhs_maybe tc
          _ -> Nothing
        else do
          thing <- tcLookupGlobal name
          return $ case thing of
            ATyCon tc | tyConArity tc == 0 -> synTyConRhs_maybe tc
            _ -> Nothing
  | otherwise = return Nothing

closedCallLiteralKind :: LHsType GhcRn -> Maybe Kind
closedCallLiteralKind arg
  | L _ (HsExplicitListTy _ _ elements) <- ignoreParens arg =
      case elements of
        [] -> Just (mkListTy naturalTy)
        first : rest
          | all (isJust . scalarKind) rest -> mkListTy <$> scalarKind first
        _ -> Nothing
  | otherwise = scalarKind arg
  where
    scalarKind operand = case ignoreParens operand of
      L _ (HsTyLit _ HsNatural{}) -> Just naturalTy
      L _ (HsTyLit _ HsChar{}) -> Just charTy
      L _ (HsTyVar _ _ (L _ (WithUserRdr _ con)))
        | con == trueDataConName || con == falseDataConName -> Just boolTy
      _ -> Nothing

{- Note [Closed source calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a resolved term call in a type has closed literal, constructor, callback,
or nested-call operands, 'tcPlanClosedTree' instantiates all producer
telescopes in one constraint scope, checks operand kinds and dictionaries
before executing any child, composes adjacent ordinary calls into a single
Core expression to preserve laziness of unread intermediate fields, and
decodes the result via "GHC.Tc.Gen.ClosedEval" (see Note [Closed literal type
evaluation] in "GHC.Tc.Gen.ClosedEval"). Empty promoted lists default their
element kind to 'Natural' only when surrounding operands and expected kinds
leave it unconstrained.
-}

{- Note [Source opaque call indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A complete local type can apply a registered lexical function 'Id' of
unrestricted arrow type to value arguments, for example @Vec Int (f n)@ (GHC
Proposal #378). Check each argument against its arrow domain before advancing
to the codomain; when a family-defined codomain hides a subsequent arrow, use
'matchExpectedFunKind' to cast the prefix before applying the next argument.
The first application forms 'TermAppTy', and further arguments use 'AppTy'.
-}

---------------------------
-- | Apply a type of a given kind to a list of arguments. This instantiates
-- invisible parameters as necessary. Always consumes all the arguments,
-- using matchExpectedFunKind as necessary.
-- This takes an optional @VarEnv Kind@ which maps kind variables to kinds.-
-- These kinds should be used to instantiate invisible kind variables;
-- they come from an enclosing class for an associated type/data family.
--
-- tcInferTyApps also arranges to saturate any trailing invisible arguments
--   of a type-family application, which is usually the right thing to do
-- tcInferTyApps_nosat preserves the remaining invisible binders for :kind
-- and for checking an application against an expected kind.
tcInferTyApps
    :: TcTyMode
    -> LHsType GhcRn        -- ^ Function (for printing only)
    -> TcType               -- ^ Function
    -> [LHsTypeArg GhcRn]   -- ^ Args
    -> TcM (TcType, TcKind) -- ^ (f args, result kind)
{- Note [Inferred predicate kinds in source applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A promoted constructor can quantify over the matchability of a
predicate. For example, Here can take a proof of p x. In a family
pattern for membership, the expected kind is Any ((:~:) witness) xs.
Keep the inferred matchability of Here flexible so the predicate can
select Matchable. The same applies when a visible type application
specifies the predicate. A generated source family can abstract over
the same predicate when it checks a call on the promoted package.
Keep its inferred matchability flexible as well. In each case, the
binder must occur in a quantified predicate kind.
-}

{- Note [Implicit ordinary-arrow matchability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FUN has a specified invisible matchability parameter before multiplicity.
When a source application must instantiate an omitted Matchability binder,
use U at TypeLevel and M at KindLevel. This rule also applies to user-defined
Matchability binders. An explicit @ argument is checked normally.

Checked declaration kinds defer omitted metadata until the declaration prefix
has supplied its constraints. See Note [Checked declaration-kind matchability].

Do not instantiate trailing binders in the application loop. First compare
actual and expected kinds in checkExpectedKind. Instantiate only the excess
invisible binders. This preserves a polymorphic argument when the expected
kind has the same invisible forall prefix. For example, P must retain its
binder when P :: forall (m :: Matchability). Type is passed to a constructor
that expects this kind. The same rule preserves bare FUN in that position.

Use the same mode-aware instantiation in the application loop when a real
source argument requires it, and when family saturation requires it. The
fully instantiated inference entry point tcInferLHsTypeKind uses TypeLevel.
The unsaturated inference entry point keeps its invisible forall binders.
See Note [Do not always instantiate eagerly in types].

This is the initial concrete policy for the ordinary-arrow port. It does not
infer or generalize a new matchability quantifier. The forall stage must
revisit the broader polymorphic inference policy from proposal #242. See
Note [Ordinary arrow matchability] in GHC.Core.TyCo.Rep.
-}

tcInferTyApps mode hs_ty fun hs_args
  = do { (f_args, res_k) <- tcInferTyApps_nosat TypeLevel Nothing mode hs_ty fun hs_args
       ; saturateFamApp mode f_args res_k }

-- | Apply the arguments while retaining the remaining invisible binders.
tcInferTyApps_nosat :: TypeOrKind -> Maybe Kind -> TcTyMode -> LHsType GhcRn -> TcType
                    -> [LHsTypeArg GhcRn] -> TcM (TcType, TcKind)
tcInferTyApps_nosat index_level expected_kind mode orig_hs_ty fun orig_hs_args
  = do { traceTc "tcInferTyApps {" (ppr orig_hs_ty $$ ppr orig_hs_args)
       ; keep_inferred_prefix <- case (expected_kind, fun) of
           (Just expected, TyConApp tc [])
             | hasInferredMatchability tc
             , isUnsaturatedUserFamily tc []
             , all isHsArgPar orig_hs_args
             , invisibleBndrCount expected == invisibleBndrCount (typeKind fun)
             -> unsaturatedTypeFamiliesEnabled
           _ -> pure False
       ; fun <- case (mode_synonym_metadata mode, fun) of
           _ | keep_inferred_prefix -> pure fun
           (Just (get_indices, _), TyConApp tc []) | hasInferredMatchability tc -> do
             let binders = tyConBinders tc
                 indices = takeWhile inferredMatchabilityIndex binders
                 domains = [tyVarKind (binderVar b) | b <- binders, isVisibleTyConBinder b]
                 from_argument b
                   | TypeSynonymFlavour <- tyConFlavour tc =
                       any (synonymCallbackIndex (binderVar b)) domains
                   | otherwise = False
             args <- get_indices (tyConName tc) (map from_argument indices)
             pure (mkTyConApp tc args)
           (Nothing, TyConApp tc [])
             | TypeLevel <- mode_tyki mode
             , isTypeFamilyTyCon tc ||
               (isTcTyCon tc && is_closed_family tc && infer_family_arrows)
             , hasInferredMatchability tc
             , let indices = takeWhile inferredMatchabilityIndex (tyConBinders tc)
             , any (familyKindUsesMatchability tc) indices -> do
                 defaults <- tcl_kind_arrow_defaults <$> getLclEnv
                 let policy = mode_callback_arrow mode
                     default_level = case policy of
                       DefaultUnsignedFamilyArrows -> KindLevel
                       _ -> index_level
                     choose b
                       | not (familyKindUsesMatchability tc b) = pure unmatchableDataConTy
                       | infer_family_arrows = newFlexiTyVarTy matchabilityTy
                       | Just (owner, _) <- defaults = fresh_kind_arrow default_level owner
                       | otherwise = pure (defaultForAllMatchability default_level)
                 case (policy, defaults, index_level) of
                   (OrdinaryArrowChecking, Nothing, TypeLevel) -> pure fun
                   _ -> mkTyConApp tc <$> traverse choose indices
           _ -> pure fun
       ; (f_args, res_k) <- go_init 1 fun orig_hs_args Nothing
       ; traceTc "tcInferTyApps }" (ppr f_args <+> dcolon <+> ppr res_k)
       ; return (f_args, res_k) }
  where

    infer_family_arrows
      | InferUnsignedFamilyArrows <- mode_callback_arrow mode = True
      | otherwise = False

    is_closed_family tc = case tyConFlavour tc of
      ClosedTypeFamilyFlavour -> True
      _ -> False

    -- go_init just initialises the auxiliary
    -- arguments of the 'go' loop
    go_init n fun all_args checked_arg
      = go n fun empty_subst fun_ki all_args checked_arg
      where
        fun_ki = typeKind fun
           -- We do (typeKind fun) here, even though the caller
           -- knows the function kind, to absolutely guarantee
           -- INVARIANT for 'go'
           -- Note that in a typical application (F t1 t2 t3),
           -- the 'fun' is just a TyCon, so typeKind is fast

        empty_subst = mkEmptySubst $ mkInScopeSet $
                      tyCoVarsOfType fun_ki

    go :: Int             -- The # of the next argument
       -> TcType          -- Function applied to some args
       -> Subst        -- Applies to function kind
       -> TcKind          -- Function kind
       -> [LHsTypeArg GhcRn]    -- Un-type-checked args
       -> Maybe (TcType, TcKind) -- Checked current visible argument
       -> TcM (TcType, TcKind)  -- Result type and its kind
    -- INVARIANT: in any call (go n fun subst fun_ki args checked_arg)
    --               typeKind fun  =  subst(fun_ki)
    -- So the 'subst' and 'fun_ki' arguments are simply
    -- there to avoid repeatedly calling typeKind.
    --
    -- Reason for INVARIANT: to support the Purely Kinded Type Invariant
    -- it's important that if fun_ki has a forall, then so does
    -- (typeKind fun), because the next thing we are going to do
    -- is apply 'fun' to an argument type.

    -- Dispatch on all_args first, for performance reasons
    go n fun subst fun_ki all_args checked_arg = case (all_args, tcSplitPiTy_maybe fun_ki) of

      ---------------- No user-written args left
      ([], Just (_, Named (Bndr kv flag), inner_ki))
        | isInvisibleForAllTyFlag flag
        , expects_promoted_callback fun
        -> instantiate kv flag inner_ki

      ([], _) -> return (fun, substTy subst fun_ki)

      ---------------- HsArgPar: We don't care about parens here
      (HsArgPar _ : args, _) -> go n fun subst fun_ki args checked_arg

      ---------------- HsTypeArg: a kind application (fun @ki)
      (HsTypeArg _ hs_ki_arg : hs_args, Just (_ma, ki_binder, inner_ki)) ->
        case ki_binder of

        -- FunTy with PredTy on LHS, or ForAllTy with Inferred
        Named (Bndr kv Inferred)         -> instantiate kv Inferred inner_ki

        Named (Bndr _ Specified) ->  -- Visible kind application
          do { traceTc "tcInferTyApps (vis kind app)"
                       (vcat [ ppr ki_binder, ppr hs_ki_arg
                             , ppr (piTyBinderType ki_binder)
                             , ppr subst ])

             ; let exp_kind = substTy subst $ piTyBinderType ki_binder
             ; arg_mode <- mkHoleMode KindLevel HM_VTA
                   -- HM_VKA: see Note [Wildcards in visible kind application]
             ; ki_arg <- addErrCtxt (FunAppCtxt (FunAppCtxtTy orig_hs_ty hs_ki_arg) n) $
                         tc_check_lhs_type (arg_mode { mode_indices = mode_indices mode })
                                           hs_ki_arg exp_kind

             ; traceTc "tcInferTyApps (vis kind app)" (ppr exp_kind)
             ; (subst', fun') <- mkAppTyM subst fun ki_binder ki_arg
             ; go (n+1) fun' subst' inner_ki hs_args Nothing }

        -- Attempted visible kind application (fun @ki), but fun_ki is
        --   forall k -> blah   or   k1 -> k2
        -- So we need a normal application.  Error.
        _ -> ty_app_err hs_ki_arg $ substTy subst fun_ki

      -- No binder; try applying the substitution, or fail if that's not possible
      (HsTypeArg _ ki_arg : _, Nothing) -> try_again_after_substing_or $
                                           ty_app_err ki_arg substed_fun_ki

      ---------------- HsValArg: a normal argument (fun ty)
      (HsValArg _ arg : args, Just (_ma, ki_binder, inner_ki))
        -- next binder is invisible; need to instantiate it
        | Named (Bndr kv flag) <- ki_binder
        , isInvisibleForAllTyFlag flag   -- ForAllTy with Inferred or Specified
         -> if not (isEmptyNameEnv (mode_indices mode)) && mentions_index arg
            then do { (arg_ty, arg_kind) <- case checked_arg of
                        Just checked -> return checked
                        Nothing ->
                          do { (arg_ty, arg_kind) <-
                                 addErrCtxt (FunAppCtxt (FunAppCtxtTy orig_hs_ty arg) n) $
                                   tc_infer_lhs_type mode arg
                             ; [arg_ty, arg_kind] <- liftZonkM $ zonkTcTypes [arg_ty, arg_kind]
                             ; return (arg_ty, arg_kind) }
                    ; let checked = Just (arg_ty, arg_kind)
                    ; case native_choices subst fun_ki arg_kind of
                        Just (binders, choices, result_kind) ->
                          do { (subst', fun') <- foldM
                                 (\(s, f) (binder, choice) -> mkAppTyM s f binder choice)
                                 (subst, fun) (zipEqual binders choices)
                             ; valid <- native_scope
                                 (arg_ty : arg_kind : fun' : substTy subst' result_kind : choices)
                             ; if valid
                               then go n fun' subst' result_kind all_args checked
                               else instantiate_checked kv flag inner_ki checked }
                        Nothing -> instantiate_checked kv flag inner_ki checked }
            else instantiate kv flag inner_ki

        -- "normal" case
        | otherwise
         -> do { traceTc "tcInferTyApps (vis normal app)"
                          (vcat [ ppr ki_binder
                                , ppr arg
                                , ppr (piTyBinderType ki_binder)
                                , ppr subst ])
                ; let exp_kind = substTy subst $ piTyBinderType ki_binder
                ; arg' <- addErrCtxt (FunAppCtxt (FunAppCtxtTy orig_hs_ty arg) n) $
                          case checked_arg of
                            Nothing -> check_kind_value fun ki_binder inner_ki args arg exp_kind
                            Just (arg_ty, arg_kind) ->
                              checkExpectedKind mode (unLoc arg) arg_ty arg_kind exp_kind
                ; traceTc "tcInferTyApps (vis normal app) 2" (ppr exp_kind)
                ; (subst', fun') <- mkAppTyM subst fun ki_binder arg'
                ; go (n+1) fun' subst' inner_ki args Nothing }

          -- no binder; try applying the substitution, or infer another arrow in fun kind
      (HsValArg _ _ : _, Nothing)
        -> try_again_after_substing_or $
           do { let arrows_needed = n_initial_val_args all_args
              ; co <- matchExpectedFunKind (HsTypeRnThing $ unLoc hs_ty)
                          missing_arrow arrows_needed substed_fun_ki

              ; fun' <- liftZonkM $ zonkTcType (fun `mkCastTy` co)
                     -- This zonk is essential, to expose the fruits
                     -- of matchExpectedFunKind to the 'go' loop

              ; traceTc "tcInferTyApps (no binder)" $
                   vcat [ ppr fun <+> dcolon <+> ppr fun_ki
                        , ppr arrows_needed
                        , ppr co
                        , ppr fun' <+> dcolon <+> ppr (typeKind fun')]
              ; go_init n fun' all_args checked_arg }
                -- Use go_init to establish go's INVARIANT
      where
        instantiate kv flag inner_ki
          = instantiate_checked kv flag inner_ki checked_arg

        instantiate_checked kv flag inner_ki checked
          = do { traceTc "tcInferTyApps (need to instantiate)"
                         (vcat [ ppr kv, ppr subst])
               ; (subst', arg') <- choose_matchability kv flag inner_ki
               ; go n (mkAppTy fun arg') subst' inner_ki all_args checked }
                 -- Because tcInvisibleTyBinder instantiate ki_binder,
                 -- the kind of arg' will have the same shape as the kind
                 -- of ki_binder.  So we don't need mkAppTyM here.

        choose_matchability tv flag inner_ki
          -- See Note [Callback arrows in inferred synonym kinds] in GHC.Tc.TyCl.
          | Inferred <- flag
          , tyVarKind tv `tcEqType` matchabilityTy
          , Just (tc, _) <- tcSplitTyConApp_maybe fun
          , TypeSynonymFlavour <- tyConFlavour tc
          , any (synonymCallbackIndex tv) (take supplied_args (visible_domains inner_ki))
          = do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
               ; case owner of
                   Just (defaults, _) -> bind <$> fresh_kind_arrow KindLevel defaults
                   Nothing -> fresh }
          -- See Note [Inferred datatype callback arrows].
          | Inferred <- flag
          , tyVarKind tv `tcEqType` matchabilityTy
          , Just (tc, _) <- tcSplitTyConApp_maybe fun
          , case tyConFlavour tc of
              DataTypeFlavour -> True
              NewtypeFlavour -> True
              _ -> False
          , any (callback_arrow tv) (take supplied_args (visible_domains inner_ki))
          = case mode_callback_arrow mode of
              InferUnsignedFamilyArrows -> fresh
              InferDatatypeArrows {} -> fresh
              _ -> do { defaults <- tcl_kind_arrow_defaults <$> getLclEnv
                      ; case defaults of
                          Just (owner, _) ->
                            bind <$> fresh_kind_arrow (mode_tyki mode) owner
                          Nothing -> default_binder tv }
          -- See Note [Inferred predicate kinds in source applications].
          | Inferred <- flag
          , TypeLevel <- mode_tyki mode
          , tyVarKind tv `tcEqType` matchabilityTy
          , Just (tc, _) <- tcSplitTyConApp_maybe fun
          , (isPromotedDataCon tc &&
             any (curried_callback_index tv . varType . binderVar) (tyConBinders tc)) ||
            (isSourceFamilyName (tyConName tc) && quantified_callback tv inner_ki)
          = fresh
          -- See Note [Omitted matchability in higher-order arguments].
          | Specified <- flag
          , TypeLevel <- mode_tyki mode
          , Nothing <- mode_holes mode
          , tyVarKind tv `tcEqType` matchabilityTy
          , Just (tc, _) <- tcSplitTyConApp_maybe fun
          , isPromotedDataCon tc
          , let from_expected = null all_args && expects_promoted_callback fun
                visible = if from_expected then 1 else supplied_args
          , any (curried_callback_index tv) (take visible (visible_domains inner_ki))
          = case mode_synonym_metadata mode of
              Just (_, Just outer_arrow) | from_expected -> bind <$> outer_arrow TypeLevel
              _ -> do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
                      ; case owner of
                          Just (defaults, _) -> bind <$> fresh_kind_arrow TypeLevel defaults
                          Nothing -> default_binder tv }
          | Specified <- flag
          , TypeLevel <- mode_tyki mode
          , Nothing <- mode_holes mode
          , HsValArg{} : _ <- dropWhile isHsArgPar all_args
          , substTy subst (tyVarKind tv) `tcEqType` matchabilityTy
          = do { enabled <- unsaturatedTypeFamiliesEnabled
               ; if not enabled
                 then default_binder tv
                 else do
                   { owner <- tcl_kind_arrow_defaults <$> getLclEnv
                   ; case owner of
                       Just (defaults, _)
                         | Just (tc, _) <- tcSplitTyConApp_maybe fun
                         , isTypeFamilyTyCon tc || isAlgTyCon tc
                         , higher_order_use tv inner_ki
                         -> do { ma <- fresh_kind_arrow TypeLevel defaults
                               ; return (extendTvSubstAndInScope subst tv ma, ma) }
                       _ -> default_binder tv } }
          | otherwise = default_binder tv
          where
            bind ma = (extendTvSubstAndInScope subst tv ma, ma)
            fresh = bind <$> newFlexiTyVarTy matchabilityTy

        callback_arrow tv domain
          | Just (arrow, _, _) <- tcSplitFunTy_maybe domain = getTyVar_maybe arrow == Just tv
          | otherwise = False

        curried_callback_index tv domain
          | Just (arrow, _, result) <- tcSplitFunTy_maybe domain
          = getTyVar_maybe arrow == Just tv || curried_callback_index tv result
          | otherwise = False

        quantified_callback tv kind
          | Just (_, binder, rest) <- tcSplitPiTy_maybe kind
          , isInvisiblePiTyBinder binder
          = curried_callback_index tv (piTyBinderType binder) || quantified_callback tv rest
          | otherwise = False

        supplied_args = length [() | HsValArg{} <- all_args]

        higher_order_use tv inner_ki =
          any (isJust . tcSplitFunTy_maybe) (filter uses_index domains) &&
          (maybe False (isJust . tcSplitFunTy_maybe) expected_kind ||
           lengthAtLeast (filter uses_index (take supplied_args domains)) 2)
          where
            domains = visible_domains inner_ki
            uses_index domain = tv `elemVarSet` tyCoVarsOfType domain

        default_binder =
          tcInstInvisibleTyBinder (matchabilityInstantiationMode mode) subst

        -- See Note [Omitted matchability in higher-order arguments].
        visible_domains kind
          | Just (_, binder, rest) <- tcSplitPiTy_maybe kind
          , isInvisiblePiTyBinder binder = visible_domains rest
          | Just (_, binder, rest) <- tcSplitPiTy_maybe kind
          = piTyBinderType binder : visible_domains rest
          | otherwise = []

        try_again_after_substing_or fallthrough
          | not (isEmptyTCvSubst subst)
          = go n fun zapped_subst substed_fun_ki all_args checked_arg
          | otherwise
          = fallthrough

        zapped_subst   = zapSubst subst
        substed_fun_ki = substTy subst fun_ki
        hs_ty          = appTypeToArg orig_hs_ty (take (n-1) orig_hs_args)

    check_kind_value (TyConApp tc []) _ _ _ arg expected
      | Just (level, HM_FamPat True) <- mode_holes mode
      , Just field_kind <- primFamilyPatFieldKind (tyConName tc)
      , expected `tcEqType` field_kind
      = check_prim_field level arg field_kind

    -- A visible argument is used as a kind when it supplies the whole kind
    -- of the next visible argument. Keep this limited to the checked telescope.
    -- An indexed family may expose its metadata on reduction, as in
    -- Bridge (K 'True) Maybe. Use the same owned default as for a bare
    -- arrow so the equality solver can select M before the U default.
    check_kind_value _ binder inner args arg expected
      | TypeLevel <- mode_tyki mode
      , isEmptyNameEnv (mode_indices mode)
      , Named (Bndr tv _) <- binder
      , isLiftedTypeKind expected
      , HsValArg{} : _ <- dropWhile isHsArgPar args
      , Just (_, next, _) <- tcSplitPiTy_maybe inner
      , not (isInvisiblePiTyBinder next)
      , Just kind_var <- getTyVar_maybe (piTyBinderType next)
      , kind_var == tv
      = do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
           ; case owner of
               Nothing -> tc_check_lhs_type mode arg expected
               Just (defaults, _) -> check_arrows defaults arg expected }
      | otherwise = do
          mb_forgot <- case (mode_tyki mode, tcSplitTyConApp_maybe fun, tcSplitFunTy_maybe expected) of
            (TypeLevel, Just (tc, _), Just (ma, _, _))
              | isSourceFamilyName (tyConName tc)
              , ma `tcEqType` unmatchableDataConTy
              , is_tycon_app_hs_ty arg -> do
                  (arg_ty, arg_kind) <- tc_infer_lhs_type mode arg
                  let orig = TypeEqOrigin { uo_actual    = arg_kind
                                          , uo_expected  = expected
                                          , uo_thing     = Just (HsTypeRnThing (unLoc arg))
                                          , uo_invisible = Nothing }
                  tcForgetMatchabilityArrows orig arg_ty arg_kind expected
            _ -> pure Nothing
          case mb_forgot of
            Just forgotten -> pure forgotten
            Nothing        -> tc_check_lhs_type mode arg expected

    is_tycon_app_hs_ty (L _ (HsParTy _ inner))  = is_tycon_app_hs_ty inner
    is_tycon_app_hs_ty (L _ (HsAppTy _ f _))    = is_tycon_app_hs_ty f
    is_tycon_app_hs_ty (L _ (HsAppKindTy _ f _)) = is_tycon_app_hs_ty f
    is_tycon_app_hs_ty (L _ (HsTyVar _ _ (L _ (WithUserRdr _ nm)))) = isTyConName nm || isTyVarName nm
    is_tycon_app_hs_ty _ = False

    -- This slot supplies the kind for a wildcard or an exact annotation.
    -- See Note [Primitive family pattern binders] in GHC.Core.Type.
    check_prim_field level arg field_kind = case unLoc (ignoreParens arg) of
      HsWildCardTy{} -> do
        name <- newMetaTyVarName (fsLit "_")
        details <- newTauTvDetailsAtLevel level
        return (mkTyVarTy (mkTcTyVar name field_kind details))
      HsKindSig _ field sig
        | isPrimKindAnnotation field_kind sig -> check_prim_field level field field_kind
      _ -> tc_check_lhs_type mode arg field_kind

    -- See Note [Omitted matchability in higher-order arguments].
    expects_promoted_callback ty
      | TypeLevel <- mode_tyki mode
      , Nothing <- mode_holes mode
      , Just expected <- expected_kind
      , Just {} <- tcSplitFunTy_maybe expected
      , Just (tc, _) <- tcSplitTyConApp_maybe ty
      = isPromotedDataCon tc
      | otherwise = False

    check_arrows defaults (L _ (HsParTy _ arg)) expected =
      check_arrows defaults arg expected
    check_arrows defaults (L loc (HsFunTy _ mult domain range)) expected =
      setSrcSpanA loc $
      tc_fun_type_x (check_arrows defaults) (fresh_kind_arrow TypeLevel defaults)
                    mode mult domain range (Check expected)
    check_arrows defaults arg expected =
      let deferred_mode = case mode_synonym_metadata mode of
            Just _ -> mode
            Nothing -> mode {mode_synonym_metadata =
                       Just (\_ callbacks -> traverse (const (fresh_kind_arrow TypeLevel defaults)) callbacks
                            , Nothing)}
      in tc_check_lhs_type deferred_mode arg expected

    missing_arrow
      | infer_family_arrows =
          Just (newFlexiTyVarTy matchabilityTy)
      | CheckFamilyRhsArrows <- mode_callback_arrow mode =
          Just $ do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
                    ; case owner of
                        Just (defaults, _) -> fresh_kind_arrow KindLevel defaults
                        Nothing -> pure matchableDataConTy }
      | InferDatatypeArrows parameters <- mode_callback_arrow mode
      , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))) <- ignoreParens orig_hs_ty
      , name `elemNameSet` parameters =
          Just (newFlexiTyVarTy matchabilityTy)
      | Just (_, callback_arrow) <- mode_synonym_metadata mode
      , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))) <- ignoreParens orig_hs_ty
      , isTyVarName name = ($ KindLevel) <$> callback_arrow
      | otherwise = Nothing

    fresh_kind_arrow level defaults =
      do { ma <- newFlexiTyVar matchabilityTy
         ; updTcRef defaults ((ma, level) :)
         ; return (mkTyVarTy ma) }

    -- Detect a registered runtime Name in a source argument.
    -- See Note [Immutable native kind arguments].
    mentions_index :: Data a => a -> Bool
    mentions_index thing
      | isAstLeafAnnotation thing = False
      | Just (L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))))
          <- (cast thing :: Maybe (LHsType GhcRn))
      = isJust (lookupNameEnv (mode_indices mode) name)
      | otherwise = or (gmapQ mentions_index thing)

    -- Match complete invisible choices from an actual native kind.
    native_choices :: Subst -> TcKind -> TcKind
                   -> Maybe ([PiTyBinder], [TcType], TcKind)
    native_choices subst kind actual
      | hasNativeDependentExpr (Core.Type actual)
      , let (binders, result) = invisible_prefix kind
            tvs = [tv | Named (Bndr tv _) <- binders]
      , not (null tvs), all isTyVar tvs
      , Just (_ma, visible, _) <- tcSplitPiTy_maybe result
      , let formal = substTy subst (piTyBinderType visible)
            bind_outer tv _ | tv `elem` tvs = BindMe
                            | otherwise = DontBindMe
            target_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType actual))
            match_subst = Subst.extendSubstInScopeSetFromSource target_subst
                            (tyCoVarsOfTypes [formal, substTy subst kind])
      , Just matched <- tcMatchTyX_BM bind_outer match_subst formal actual
      , Just (complete, choices) <- choose matched tvs tvs
      , substTy complete formal `tcEqType` actual
      = Just (binders, choices, result)
      | otherwise = Nothing
      where
        choose matched _ [] = Just (matched, [])
        choose matched all_tvs (tv : tvs) = do
          let expected_kind = substTy matched (substTy subst (tyVarKind tv))
          choice <- if expected_kind `tcEqType` matchabilityTy
                    then do { tyki <- matchabilityInstantiationMode mode
                            ; let ma = defaultForAllMatchability tyki
                            ; guard (maybe True (`tcEqType` ma) (Subst.lookupTyVar matched tv))
                            ; return ma }
                    else Subst.lookupTyVar matched tv
          guard (typeKind choice `tcEqType` expected_kind)
          guard (not (any (`elemVarSet` tyCoVarsOfType choice) all_tvs))
          (complete, choices) <- choose (extendTvSubstAndInScope matched tv choice) all_tvs tvs
          return (complete, choice : choices)

    -- Check runtime metadata against the source-owned lexical context.
    native_scope :: [TcType] -> TcM Bool
    native_scope tys = do
      bindings <- mapM (liftZonkM . zonkId) (nonDetNameEnvElts (mode_indices mode))
      let metadata = tyCoVarsOfTypes (concatMap (\v -> [varType v, idMult v]) bindings)
          in_scope = extendInScopeSetList (mkInScopeSet metadata) bindings
          valid v = isNativeOpaqueValueId v
                 && maybe False (\binding -> isNativeOpaqueValueId binding
                                 && varType binding `tcEqType` varType v)
                                (lookupInScope in_scope v)
      indices <- mapM (liftZonkM . zonkId)
                   (filter isNonCoVarId (tyCoVarsOfTypesList tys))
      return (all valid indices)

    -- Observe the invisible prefix without rebuilding its annotations.
    invisible_prefix kind
      | Just (_ma, binder@(Named (Bndr _ flag)), inner) <- tcSplitPiTy_maybe kind
      , isInvisibleForAllTyFlag flag
      = let (binders, result) = invisible_prefix inner
        in (binder : binders, result)
      | otherwise = ([], kind)

    n_initial_val_args :: [HsArg p tm ty] -> Arity
    -- Count how many leading HsValArgs we have
    n_initial_val_args (HsValArg {} : args) = 1 + n_initial_val_args args
    n_initial_val_args (HsArgPar {} : args) = n_initial_val_args args
    n_initial_val_args _                    = 0

    ty_app_err arg ty
      = failWith $ TcRnInvalidVisibleKindArgument arg ty

{- Note [Omitted matchability in higher-order arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider Receiver (Select 'True) 'Match. Both Receiver and Select have a
specified invisible matchability parameter. The kind of the last argument
of Receiver fixes M. When checking the family prefix first, defaulting
either index to U would lose this choice.

In an experimental type application, defer an omitted index used by
a function-valued domain when another supplied domain shares that index.
When an argument is checked against a function kind, allow its omitted
index to agree with the expected kind. This also checks the inner family
prefix in the example. The enclosing declaration solver collects those
equalities and defaults any remaining index to U.

A promoted constructor can quantify over the matchability of a callback
field. For example, a constructor with a field of kind `a %m -> b` can
take a matchable type constructor after promotion. Defer an omitted
specified `m` if a supplied field has an arrow annotated with it. Its
checked argument may determine the index. A receiver such as `Pure1`
can also take the promoted constructor itself. When the expected kind
is an arrow, instantiate the remaining invisible binders here, so the
callback domain can use the same deferred default. The equality between
the actual and expected kinds may determine the index, including through
another argument of the receiver. In the first pass over a synonym RHS,
register these choices with the enclosing declaration solver. The inner
solver leaves unresolved metadata for synonym generalisation, but
`type Lift = Pure1 'Endo` must default the omitted constructor indices
to U. The enclosing solver applies this default after solving the
kind equalities.

An isolated partial application with an inferred kind keeps its original
default. Otherwise its synonym kind could gain an extra invisible binder.
An explicit kind argument and a family pattern keep their usual paths.
-}

{- Note [Immutable native kind arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an argument mentions a registered runtime index in 'mode_indices' (GHC
Proposal #378; Gundry, PhD thesis, 2013, §6.2), instantiating an invisible kind
parameter with a mutable metavariable would fail the runtime-index escape check
(for example @Count xs@ with @xs :: List (Vec Int n)@). Instead, infer and cache
the argument once, match the pending invisible prefix against the checked kind
via 'tcMatchTyX_BM', validate 'native_scope', and apply the rigid choices with
'mkAppTyM' while preserving the Purely Kinded Type Invariant.
-}

mkAppTyM :: Subst
         -> TcType -> PiTyBinder    -- fun, plus its top-level binder
         -> TcType                  -- arg
         -> TcM (Subst, TcType)  -- Extended subst, plus (fun arg)
-- Precondition: the application (fun arg) is well-kinded after zonking
--               That is, the application makes sense
--
-- Precondition: for (mkAppTyM subst fun bndr arg)
--       typeKind fun  =  Pi bndr. body
--  That is, fun always has a ForAllTy or FunTy at the top
--           and 'bndr' is fun's pi-binder
--
-- Postcondition: if fun and arg satisfy (PKTI), the purely-kinded type
--                invariant, then so does the result type (fun arg)
--
-- We do not require that
--    typeKind arg = tyVarKind (binderVar bndr)
-- This must be true after zonking (precondition 1), but it's not
-- required for the (PKTI).
mkAppTyM subst fun ki_binder arg
  | -- See Note [mkAppTyM]: Nasty case 2
    TyConApp tc args <- fun
  , isTypeSynonymTyCon tc
  , args `lengthIs` (tyConArity tc - 1)
  , any isTrickyTvBinder (tyConTyVars tc) -- We could cache this in the synonym
  = do { (arg':args') <- liftZonkM $ zonkTcTypes (arg:args)
       ; let subst' = case ki_binder of
                        Anon {}           -> subst
                        Named (Bndr tv _) -> extendTvSubstAndInScope subst tv arg'
       ; return (subst', expandLocalSynApp (mkTyConApp tc (args' ++ [arg']))) }


mkAppTyM subst fun (Anon {}) arg
   = return (subst, expandLocalSynApp (mk_app_ty fun arg))

mkAppTyM subst fun (Named (Bndr tv _)) arg
  = do { arg' <- if isTrickyTvBinder tv
                 then -- See Note [mkAppTyM]: Nasty case 1
                      liftZonkM $ zonkTcType arg
                 else return     arg
       ; return ( extendTvSubstAndInScope subst tv arg'
                , expandLocalSynApp (mk_app_ty fun arg') ) }

-- | Expand a saturated local @type@ synonym application immediately so no
-- internal 'SynonymTyCon' escapes 'GHC.Tc.Gen.HsType'.
expandLocalSynApp :: TcType -> TcType
expandLocalSynApp ty@(TyConApp tc _)
  | isTypeSynonymTyCon tc, isInternalName (tyConName tc)
  , Just rhs <- coreView ty
  = rhs
expandLocalSynApp ty = ty

mk_app_ty :: TcType -> TcType -> TcType
-- This function just adds an ASSERT for mkAppTyM's precondition
mk_app_ty fun arg
  = assertPpr (isPiTy fun_kind)
              (ppr fun <+> dcolon <+> ppr fun_kind $$ ppr arg) $
    mkAppTy fun arg
  where
    fun_kind = typeKind fun

isTrickyTvBinder :: TcTyVar -> Bool
-- NB: isTrickyTvBinder is just an optimisation
-- It would be absolutely sound to return True always
isTrickyTvBinder tv = isPiTy (tyVarKind tv)

{- Note [The Purely Kinded Type Invariant (PKTI)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type inference, we maintain this invariant

 (PKTI) It is legal to call 'typeKind' on any Type ty,
        on any sub-term of ty, /without/ zonking ty

        Moreover, any such returned kind
        will itself satisfy (PKTI)

By "legal to call typeKind" we mean "typeKind will not crash".
The way in which typeKind can crash is in applications
    (a t1 t2 .. tn)
if 'a' is a type variable whose kind doesn't have enough arrows
or foralls.  (The crash is in piResultTys.)

The loop in tcInferTyApps has to be very careful to maintain the (PKTI).
For example, suppose
    kappa is a unification variable
    We have already unified kappa := Type
      yielding    co :: Refl (Type -> Type)
    a :: kappa
then consider the type
    (a Int)
If we call typeKind on that, we'll crash, because the (un-zonked)
kind of 'a' is just kappa, not an arrow kind.  So we must zonk first.

So the type inference engine is very careful when building applications.
This happens in tcInferTyApps. Suppose we are kind-checking the type (a Int),
where (a :: kappa).  Then in tcInferApps we'll run out of binders on
a's kind, so we'll call matchExpectedFunKind, and unify
   kappa := kappa1 -> kappa2,  with evidence co :: kappa ~ (kappa1 ~ kappa2)
At this point we must zonk the function type to expose the arrrow, so
that (a Int) will satisfy (PKTI).

The absence of this caused #14174 and #14520.

The calls to mkAppTyM is the other place we are very careful; see Note [mkAppTyM].

Wrinkle around FunTy:
Note that the PKTI does *not* guarantee anything about the shape of FunTys.
Specifically, when we have (FunTy vis mult arg res), it should be the case
that arg :: TYPE rr1 and res :: TYPE rr2, for some rr1 and rr2. However, we
might not have this. Example: if the user writes (a -> b), then we might
invent a :: kappa1 and b :: kappa2. We soon will check whether kappa1 ~ TYPE rho1
(for some rho1), and that will lead to kappa1 := TYPE rho1 (ditto for kappa2).
However, when we build the FunTy, we might not have zonked `a`, and so the
FunTy will be built without being able to purely extract the RuntimeReps.

Because the PKTI does not guarantee that the RuntimeReps are available in a FunTy,
we must be aware of this when splitting: splitTyConApp and splitAppTy will *not*
split a FunTy if the RuntimeReps are not available. See also Note [Decomposing FunTy]
in GHC.Tc.Solver.Equality.

Note [mkAppTyM]
~~~~~~~~~~~~~~~
mkAppTyM is trying to guarantee the Purely Kinded Type Invariant
(PKTI) for its result type (fun arg).  There are two ways it can go wrong:

* Nasty case 1: forall types (polykinds/T14174a)
    T :: forall (p :: *->*). p Int -> p Bool
  Now kind-check (T x), where x::kappa.
  Well, T and x both satisfy the PKTI, but
     T x :: x Int -> x Bool
  and (x Int) does /not/ satisfy the PKTI.

* Nasty case 2: type synonyms
    type S f a = f a
  Even though (S ff aa) would satisfy the (PKTI) if S was a data type
  (i.e. nasty case 1 is dealt with), it might still not satisfy (PKTI)
  if S is a type synonym, because the /expansion/ of (S ff aa) is
  (ff aa), and /that/ does not satisfy (PKTI).  E.g. perhaps
  (ff :: kappa), where 'kappa' has already been unified with (*->*).

  We check for nasty case 2 on the final argument of a type synonym.

Notice that in both cases the trickiness only happens if the
bound variable has a pi-type.  Hence isTrickyTvBinder.
-}


saturateFamApp :: TcTyMode -> TcType -> TcKind -> TcM (TcType, TcKind)
-- Precondition for (saturateFamApp mode ty kind):
--     typeKind ty = kind
--
-- If 'ty' is an unsaturated family application with trailing
-- invisible arguments, instantiate them.
-- See Note [saturateFamApp]

saturateFamApp mode ty kind
  | Just (tc, args) <- tcSplitTyConApp_maybe ty
  , tyConMustBeSaturated tc
  , let n_to_inst = tyConArity tc - length args
  = do { (extra_args, ki') <- tcInstInvisibleTyBindersN (matchabilityInstantiationMode mode) n_to_inst kind
       ; return (ty `mkAppTys` extra_args, ki') }
  | otherwise
  = return (ty, kind)

{- Note [saturateFamApp]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   type family F :: Either j k
   type instance F @Type = Right Maybe
   type instance F @Type = Right Either```

Then F :: forall {j,k}. Either j k

The two type instances do a visible kind application that instantiates
'j' but not 'k'.  But we want to end up with instances that look like
  type instance F @Type @(*->*) = Right @Type @(*->*) Maybe

so that F has arity 2.  We must instantiate that trailing invisible
binder. In general, Invisible binders precede Specified and Required,
so this is only going to bite for apparently-nullary families.

Note that
  type family F2 :: forall k. k -> *
is quite different and really does have arity 0.

It's not just type instances where we need to saturate those
unsaturated arguments: see #11246.  Hence doing this in tcInferApps.

An expected kind can ask for a suffix of the invisible binders of a
partial user family. For example, F True might have kind
  forall a. forall b. a
where the receiver expects forall b. Bool. Keep the partial application
until checkExpectedKind. It instantiates a, checks the remaining forall,
and learns a ~ Bool. Saturating both binders here would discard b before
the receiver could use it. If a bare family has inferred matchability
parameters and the expected kind needs its whole invisible telescope,
keep those parameters before the argument loop as well. For example,
a receiver can take the polymorphic Map itself before applying it at a
particular callback matchability. Inference without an expected kind
keeps the ordinary saturation rule. See Note [Partial type family applications]
in GHC.Core.Type for the eligible families.
-}

appTypeToArg :: LHsType GhcRn -> [LHsTypeArg GhcRn] -> LHsType GhcRn
appTypeToArg f []                       = f
appTypeToArg f (HsValArg _ arg   : args) = appTypeToArg (mkHsAppTy f arg) args
appTypeToArg f (HsArgPar _       : args) = appTypeToArg f                 args
appTypeToArg f (HsTypeArg _ arg  : args)
  = appTypeToArg (mkHsAppKindTy noExtField f arg) args


{- *********************************************************************
*                                                                      *
                checkExpectedKind
*                                                                      *
********************************************************************* -}

-- | This instantiates invisible arguments for the type being checked if it must
-- be saturated and is not yet saturated. It then calls and uses the result
-- from checkExpectedKindX to build the final type
checkExpectedKind :: HasDebugCallStack
                  => TcTyMode
                  -> HsType GhcRn       -- ^ type we're checking (for printing)
                  -> TcType             -- ^ type we're checking
                  -> TcKind             -- ^ the known kind of that type
                  -> TcKind             -- ^ the expected kind
                  -> TcM TcType
-- Just a convenience wrapper to save calls to 'ppr'
checkExpectedKind mode hs_ty ty act_kind exp_kind
  = do { traceTc "checkExpectedKind" (ppr ty $$ ppr act_kind)

       ; (new_args, act_kind') <- tcInstInvisibleTyBindersN (matchabilityInstantiationMode mode) n_to_inst act_kind

       ; let origin = TypeEqOrigin { uo_actual    = act_kind'
                                   , uo_expected  = exp_kind
                                   , uo_thing     = Just (HsTypeRnThing hs_ty)
                                   , uo_invisible = Nothing } -- the hs_ty is visible

       ; traceTc "checkExpectedKindX" $
         vcat [ ppr hs_ty
              , text "act_kind':" <+> ppr act_kind'
              , text "exp_kind:" <+> ppr exp_kind ]

       ; let res_ty = expandLocalSynApp (ty `mkAppTys` new_args)
       ; case res_ty of
           TyConApp tc args
             | isTypeSynonymTyCon tc, isInternalName (tyConName tc)
             , args `lengthLessThan` tyConArity tc
             -> let tc_bndrs = tyConBinders tc
                    vis_req  = count isVisibleTyConBinder tc_bndrs
                    vis_got  = count (isVisibleTyConBinder . fst) (zip tc_bndrs args)
                in failWithTc (TcRnArityMismatch (ATyCon tc) vis_req vis_got)
           _ -> pure ()

       ; if act_kind' `tcEqType` exp_kind
         then return res_ty  -- This is very common
         else do { mb_forget <- case mode_holes mode of
                     Just (_, HM_VTA) -> tcForgetMatchabilityArrows origin res_ty act_kind' exp_kind
                     _                -> pure Nothing
                 ; case mb_forget of
                     Just forgotten -> return forgotten
                     Nothing ->
                       do { co_k <- unifyTypeAndEmit KindLevel origin act_kind' exp_kind
                          ; traceTc "checkExpectedKind" (vcat [ ppr act_kind
                                                              , ppr exp_kind
                                                              , ppr co_k ])
                          ; return (res_ty `mkCastTy` co_k) } } }
    where
      -- We need to make sure that both kinds have the same number of implicit
      -- foralls and constraints out front. If the actual kind has more, instantiate
      -- accordingly. Otherwise, just pass the type & kind through: the errors
      -- are caught in unifyType.
      n_exp_invis_bndrs = invisibleBndrCount exp_kind
      n_act_invis_bndrs = invisibleBndrCount act_kind
      n_to_inst         = n_act_invis_bndrs - n_exp_invis_bndrs


-- tyr <- checkExpKind mode hs_ty ty (act_ki :: Kind) (exp_ki :: ExpKind)
--     requires that `ty` has kind `act_ki`
-- It checks that the actual kind `act_ki` matches the expected kind `exp_ki`
-- and returns `tyr`, a possibly-casted form of `ty`, that has precisely kind `exp_ki`
-- `hs_ty` is purely for error messages
checkExpKind :: TcTyMode -> HsType GhcRn -> TcType -> TcKind -> ExpKind -> TcM TcType
checkExpKind mode rn_ty ty ki (Check ki') =
  checkExpectedKind mode rn_ty ty ki ki'
checkExpKind _mode _rn_ty ty ki (Infer cell) = do
  -- NB: do not instantiate.
  -- See Note [Do not always instantiate eagerly in types]
  co <- fillInferResultNoInst ki cell
  pure (ty `mkCastTy` co)

{- Note [Matchability forgetfulness in visible type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Kiss et al. (ICFP 2019, "Higher-Order Type-Level Programming in Haskell"),
every matchable type constructor f :: k1 %M -> k2 can be eta-expanded to an
unmatchable type function (\x -> f x) :: k1 %U -> k2. Forgetting Matchable
drops generativity and injectivity assumptions without inventing new ones,
whereas converting (k1 %U -> k2) to (k1 %M -> k2) is unsound and remains
rejected.

When a visible type argument (@ty or required type argument) supplies a
partially-applied matchable type constructor tc args whose actual kind has
concrete Matchable arrows where the expected kind has concrete Unmatchable
arrows (for example @(EqP x) for p :: a %U -> Type in Leibniz equality),
tcForgetMatchabilityArrows wraps the application in a module-local 1-equation
first-order closed type family specialized to tc's prefix binders:

  type family $eta$EqP$2$M (a :: Type) (x :: a) (z :: a) :: Type where
    $eta$EqP$2$M a x z = EqP @a x z

Partially applied to [a, x], $eta$EqP$2$M has kind a %U -> Type with no
higher-order Matchable binder in the module interface, and saturating it at z
reduces to EqP @a x z via a closed family axiom. Do not trigger when either
matchability is an unfilled metavariable or when any arrow requires U -> M.
-}

-- | Extract @(kind, ty)@ when a type is @TypeRep \@kind ty@, peeling outer
-- synonyms, casts, and filled metavariables without zonking unrelated types.
tcTypeRepTarget_maybe :: TcType -> TcM (Maybe (TcKind, TcType))
tcTypeRepTarget_maybe ty0 = do
  mb_app <- split_head [] ty0
  case mb_app of
    Just (tc, [raw_kind, raw_target]) -> do
      selector <- tcLookupKnownOccId typeRepIdOcc
      kind   <- zonkTcTypeIfNeeded raw_kind
      target <- zonkTcTypeIfNeeded raw_target
      let (_, rep) = tcSplitPhiTy (piResultTys (varType selector) [kind, target])
      pure $ case tcSplitTyConApp_maybe rep of
        Just (rep_tc, _) | tc == rep_tc -> Just (kind, target)
        _                               -> Nothing
    _ -> pure Nothing
  where
    split_head :: [TcType] -> TcType -> TcM (Maybe (TyCon, [TcType]))
    split_head args ty
      | Just ty' <- coreView ty = split_head args ty'
    split_head args (CastTy ty _) = split_head args ty
    split_head args (AppTy fun arg) = split_head (arg : args) fun
    split_head args (TyVarTy tv)
      | isTcTyVar tv, isMetaTyVar tv = do
          cts <- readMetaTyVar tv
          case cts of
            Indirect ty' -> split_head args ty'
            Flexi        -> pure Nothing
      | otherwise = pure Nothing
    split_head args (TyConApp tc tc_args)
      | not (null args)
      , Just ty' <- coreView (mkTyConApp tc (tc_args ++ args))
      = split_head [] ty'
      | occNameFS (nameOccName (tyConName tc)) == fsLit "TypeRep"
      = pure (Just (tc, tc_args ++ args))
    split_head _ _ = pure Nothing

-- | Substitute 'Type' replacements for captured runtime 'Id' occurrences,
-- converting 'TermAppTy' heads when replaced and updating remaining variable
-- kinds and multiplicities.
replaceTypeTermIds :: VarEnv Type -> Type -> Type
replaceTypeTermIds env
  | isEmptyVarEnv env = id
  | otherwise         = go
  where
    go (TyVarTy tv)       = TyVarTy (setTyVarKind tv (go (tyVarKind tv)))
    go (TermVarTy v)      = lookupVarEnv env v `orElse` TermVarTy (updateIdTypeAndMult go v)
    go (TermAppTy f a)    = case lookupVarEnv env f of
                              Just f_ty -> mkAppTy f_ty (go a)
                              Nothing   -> TermAppTy (updateIdTypeAndMult go f) (go a)
    go (AppTy f a)        = mkAppTy (go f) (go a)
    go (TyConApp tc args) = mkTyConApp tc (map go args)
    go (FunTy af ma m a r) = FunTy af (go ma) (go m) (go a) (go r)
    go (ForAllTy ma (Bndr b vis) r) =
      ForAllTy (go ma) (Bndr (updateVarType go b) vis) (go r)
    go (DepFunTy vis b r) = DepFunTy vis (updateIdTypeAndMult go b)
                              (replaceTypeTermIds (delVarEnv env b) r)
    go (ExistsTy vis b r) = ExistsTy vis (setTyVarKind b (go (tyVarKind b))) (go r)
    go (DepDictTy b r)    = DepDictTy (updateIdTypeAndMult go b)
                              (replaceTypeTermIds (delVarEnv env b) r)
    go (ClassAppTy s a d x k) =
      ClassAppTy (updateIdTypeAndMult go s) (go a) (go d) (go x) (go k)
    go (CastTy t co)      = CastTy (go t) (go_co co)
    go (CoercionTy co)    = CoercionTy (go_co co)
    go t                  = t

    go_mco MRefl    = MRefl
    go_mco (MCo co) = MCo (go_co co)

    go_co co
      | isEmptyVarSet (filterVarSet (`elemVarEnv` env) (tyCoVarsOfCo co)) = co
    go_co (Refl ty)              = Refl (go ty)
    go_co (GRefl r ty mco)       = GRefl r (go ty) (go_mco mco)
    go_co (TyConAppCo r tc cos)  = TyConAppCo r tc (map go_co cos)
    go_co (AppCo c1 c2)          = AppCo (go_co c1) (go_co c2)
    go_co (SymCo c)              = SymCo (go_co c)
    go_co (TransCo c1 c2)        = TransCo (go_co c1) (go_co c2)
    go_co (SelCo sel c)          = SelCo sel (go_co c)
    go_co (LRCo lr c)            = LRCo lr (go_co c)
    go_co (InstCo c1 c2)         = InstCo (go_co c1) (go_co c2)
    go_co (KindCo c)             = KindCo (go_co c)
    go_co (SubCo c)              = SubCo (go_co c)
    go_co (AxiomCo ax cos)       = AxiomCo ax (map go_co cos)
    go_co (FunCo r af1 af2 ma w c1 c2) =
      FunCo r af1 af2 (go_co ma) (go_co w) (go_co c1) (go_co c2)
    go_co co                     = co

-- | Per-module state for synthesized closed type families, synonyms, and
-- retained function helpers, stored in 'tcg_th_state' and mirrored to
-- 'tcg_type_env_var' so no global 'IORef's are used.
data SynthTypeState = SynthTypeState
  { sts_tycons         :: !(NameEnv TyCon)
  , sts_source_funs    :: !(NameEnv (Id, LHsBind GhcTc, TyCon))
  , sts_simple_funs    :: !(Maybe (NameEnv (Maybe (LocatedN Name), LHsExpr GhcRn)))
  , sts_ordinals       :: !(OccEnv Int)
  , sts_sig_cases      :: !(NameEnv [(Bool, [TyVar], Type, [([Name], LHsType GhcRn, LHsType GhcRn)], TyCon)])
  , sts_pending_checks :: ![TcM ()]
  , sts_synth_fun_ids  :: ![Id]
  }

emptySynthTypeState :: SynthTypeState
emptySynthTypeState =
  SynthTypeState emptyNameEnv emptyNameEnv Nothing emptyOccEnv emptyNameEnv [] []

getSynthTypeState :: TcM SynthTypeState
getSynthTypeState = do
  gbl <- getGblEnv
  st_map <- readTcRef (tcg_th_state gbl)
  let rep = typeRep (Proxy :: Proxy SynthTypeState)
  pure $ fromMaybe emptySynthTypeState (Map.lookup rep st_map >>= fromDynamic)

modifySynthTypeState :: Module -> (SynthTypeState -> (SynthTypeState, a)) -> a -> TcM a
modifySynthTypeState mod_key f def_val = do
  gbl <- getGblEnv
  if tcg_mod gbl /= mod_key
    then pure def_val
    else do
      let ref = tcg_th_state gbl
          rep = typeRep (Proxy :: Proxy SynthTypeState)
      st_map <- readTcRef ref
      let cur = fromMaybe emptySynthTypeState (Map.lookup rep st_map >>= fromDynamic)
          (!next, res) = f cur
      writeTcRef ref (Map.insert rep (toDyn next) st_map)
      pure res

-- | Snapshot 'SynthTypeState' across an attempted declaration group and restore
-- it if the action fails or emits errors so retried groups start from a clean
-- ordinal state.
withRollbackSynthTypeState :: TcM a -> TcM a
withRollbackSynthTypeState act = do
  cur_mod <- tcg_mod <$> getGblEnv
  saved <- getSynthTypeState
  let rollback = modifySynthTypeState cur_mod (const (saved, ())) ()
  (mb_res, had_errs) <- askErrsFound (attemptM act)
  case mb_res of
    Just res -> do
      when had_errs rollback
      pure res
    Nothing  -> rollback >> failM

allocSynthOrdinal :: OccName -> TcM Int
allocSynthOrdinal owner_occ = do
  cur_mod <- tcg_mod <$> getGblEnv
  modifySynthTypeState cur_mod
    (\st ->
       let n = fromMaybe 0 (lookupOccEnv (sts_ordinals st) owner_occ)
       in (st { sts_ordinals = extendOccEnv (sts_ordinals st) owner_occ (n + 1) }, n))
    0

-- | Look up a synthesized 'TyCon' in 'tcg_type_env' or the current group's
-- 'SynthTypeState'.
lookupPendingSynthTyCon :: Name -> TcM (Maybe TyCon)
lookupPendingSynthTyCon nm
  | not (isTcClsNameSpace (nameNameSpace nm)) = pure Nothing
  | otherwise = do
      gbl <- getGblEnv
      case lookupTypeEnv (tcg_type_env gbl) nm of
        Just (ATyCon tc) -> pure (Just tc)
        _ | nameModule_maybe nm /= Just (tcg_mod gbl) -> pure Nothing
          | otherwise -> do
              st <- getSynthTypeState
              case lookupNameEnv (sts_tycons st) nm of
                Just tc -> pure (Just tc)
                Nothing -> pure $ listToMaybe
                  [ ftc | (_, _, ftc) <- nonDetNameEnvElts (sts_source_funs st)
                        , tyConName ftc == nm ]

registerSynthTyCon :: TyCon -> TcM TyCon
registerSynthTyCon tc = do
  cur_mod <- tcg_mod <$> getGblEnv
  modifySynthTypeState cur_mod
    (\st -> (st { sts_tycons = extendNameEnv (sts_tycons st) (tyConName tc) tc }, tc))
    tc

-- | Flavour of a synthesized closed type family.
data SynthClosedFamFlav
  = SynthAbstractFam
  | SynthUninterpFam
  | SynthClosedEqns (Maybe (Name -> (Name, Bool))) [([TyVar], [Type], Type, SrcSpan)]

-- | Construct a closed 'FamilyTyCon' from explicit 'TyConBinder's.
mkSynthClosedFamilyTcbs :: Name -> [TyConBinder] -> Kind -> SynthClosedFamFlav -> TcM TyCon
mkSynthClosedFamilyTcbs fam_name tc_binders res_kind flav = do
  let tc_kind      = mkTyConKindWithMatchability unmatchableDataConTy tc_binders res_kind
      mk_tc inj ctf = mkFamilyTyCon fam_name tc_kind tc_binders 0 res_kind Nothing
                        (ClosedTypeFamilyTyCon ctf) Nothing inj
  case flav of
    SynthAbstractFam -> pure (mk_tc NotInjective CTF_Abstract)
    SynthUninterpFam -> pure (mk_tc NotInjective (CTF Nothing))
    SynthClosedEqns mb_scope [] -> pure (mk_tc NotInjective (CTF Nothing))
      where _ = mb_scope
    SynthClosedEqns mb_scope rows -> do
      ax_name <- newImplicitBinder fam_name mkInstTyCoOcc
      let branches = [ mkCoAxBranch qtvs [] [] lhs rhs (map (const Nominal) qtvs) loc
                     | (qtvs, lhs, rhs, loc) <- rows ]
          inj = case rows of
            [(qtvs, lhs, rhs, _)]
              | Just lhs_tvs <- traverse getTyVar_maybe lhs
              , lhs_tvs == qtvs
              , equalLength qtvs tc_binders
              -> let det   = injectiveVarsOfType False rhs
                     flags = [binderVar tcb `elemVarSet` det | tcb <- tc_binders]
                 in if or flags then Injective flags else NotInjective
            _ -> NotInjective
          ctf = case mb_scope of
            Nothing      -> CTF (Just axiom)
            Just mk_root -> let (owner, is_sum) = mk_root fam_name
                            in CTF_Scoped owner is_sum axiom
          fam_tc = mk_tc inj ctf
          axiom  = mkBranchedCoAxiom Nominal ax_name fam_tc branches
      pure fam_tc

-- | Construct a closed 'FamilyTyCon' with specified invisible binders and
-- non-dependent visible binders.
mkSynthClosedFamily :: Name -> [TyVar] -> [TyVar] -> Kind -> SynthClosedFamFlav -> TcM TyCon
mkSynthClosedFamily fam_name invis_tvs vis_tvs res_kind flav =
  mkSynthClosedFamilyTcbs fam_name
    (mkNamedTyConBinders (Invisible SpecifiedSpec) invis_tvs ++ mkAnonTyConBinders vis_tvs)
    res_kind flav

-- | Build 'TyConBinder's from invisible/visible binder groups, marking visible
-- binders used in subsequent kinds as 'Required' rather than 'AnonTCB'.
mkDependentTyConBinders
  :: [(TyVar, Specificity)]
  -> [TyVar]
  -> [(TyVar, Specificity)]
  -> [TyVar]
  -> Kind
  -> [TyConBinder]
mkDependentTyConBinders pre_invis pre_vis mid_invis tail_vis res_kind =
  let raw = [ (tv, Just sp) | (tv, sp) <- pre_invis ]
         ++ [ (tv, Nothing) | tv       <- pre_vis ]
         ++ [ (tv, Just sp) | (tv, sp) <- mid_invis ]
         ++ [ (tv, Nothing) | tv       <- tail_vis ]
      build [] = ([], tyCoVarsOfType res_kind)
      build ((tv, mb_sp) : rest) =
        let (rest_tcbs, later_fvs) = build rest
            tcb = case mb_sp of
              Just sp -> Bndr tv (NamedTCB (Invisible sp))
              Nothing | tv `elemVarSet` later_fvs -> Bndr tv (NamedTCB Required)
                      | otherwise                 -> Bndr tv AnonTCB
            fvs' = (later_fvs `delVarSet` tv) `unionVarSet` tyCoVarsOfType (tyVarKind tv)
        in (tcb : rest_tcbs, fvs')
  in fst (build raw)

-- | Intern a synthesized closed 'FamilyTyCon' in the current module's
-- 'SynthTypeState', reusing an existing entry when 'fam_name' is already bound.
tcInternSynthClosedFamily
  :: Name -> (TyCon -> Bool) -> TcM (Maybe ([TyVar], [TyVar], Kind, SynthClosedFamFlav))
  -> TcM (Maybe TyCon)
tcInternSynthClosedFamily fam_name valid build_spec = do
  mb_existing <- lookupPendingSynthTyCon fam_name
  case mb_existing of
    Just existing -> pure (if valid existing then Just existing else Nothing)
    Nothing -> do
      mb_spec <- build_spec
      case mb_spec of
        Nothing -> pure Nothing
        Just (invis_tvs, vis_tvs, res_kind, flav) -> do
          tc <- mkSynthClosedFamily fam_name invis_tvs vis_tvs res_kind flav
          Just <$> registerSynthTyCon tc

-- | Synthesize a 1-equation closed 'FamilyTyCon' @$motive@ for higher-order
-- Miller pattern unification @?m s_1 .. s_k ~ rhs@, abstracting ambient free
-- type variables and local runtime index 'Id's in dependency order.
tcSynthesizeMillerMotive
  :: SrcSpan -> [Var] -> Kind -> Type
  -> TcM (Maybe (Type, CoAxiom Branched, [Type]))
tcSynthesizeMillerMotive loc spine_vars res_kind0 rhs_ty0 = runMaybeT $ do
  guard (not (null spine_vars) && not (any isCoVar spine_vars))
  let spine_set = mkVarSet spine_vars
      all_inputs = rhs_ty0 : res_kind0 : map varType spine_vars
      seed_fvs  = filterOut (`elemVarSet` spine_set) $
                    tyCoVarsOfTypesList all_inputs ++ concatMap collect_global_term_ids all_inputs
      close_caps vs =
        let k_fvs = filterOut (`elemVarSet` spine_set) $
                      tyCoVarsOfTypesList (map varType vs)
                        ++ concatMap (collect_global_term_ids . varType) vs
            more  = filterOut (`elemVarSet` mkVarSet vs) k_fvs
        in if null more then vs else close_caps (more ++ vs)
      cap_vars = scopedSort (close_caps (dedup emptyVarSet seed_fvs))
  guard (not (any isCoVar cap_vars))
  guard (not (any (\v -> anyFreeVarsOfType (`elemVarSet` spine_set) (varType v)) cap_vars))
  (_, all_pairs, subst_all) <- liftMaybeT (abstractSynthCaptures (cap_vars ++ spine_vars) [])
  gbl <- liftMaybeT getGblEnv
  let res_rec = resolveRecTcTypes (tcg_type_env gbl)
      (cap_pairs, spine_pairs) = splitAt (length cap_vars) all_pairs
      invis_tvs = [setTyVarKind tv (res_rec (tyVarKind tv)) | (tv, _) <- cap_pairs]
      vis_tvs   = [setTyVarKind tv (res_rec (tyVarKind tv)) | (tv, _) <- spine_pairs]
      all_tvs   = invis_tvs ++ vis_tvs
      res_kind' = res_rec (subst_all res_kind0)
      rhs_ty'   = res_rec (subst_all rhs_ty0)
      tcbs      = mkDependentTyConBinders (map (, SpecifiedSpec) invis_tvs) vis_tvs [] [] res_kind'
      cap_args  = [ res_rec (if isTyVar orig_v then mkTyVarTy orig_v else TermVarTy orig_v)
                  | (_, orig_v) <- cap_pairs ]
      cur_mod   = tcg_mod gbl
  uniq   <- liftMaybeT getUniqueM
  fam_nm <- liftMaybeT (newGlobalBinder cur_mod (mkTcOcc ("$motive$" ++ show uniq)) Nothing loc)
  fam_tc <- liftMaybeT $ mkSynthClosedFamilyTcbs fam_nm tcbs res_kind'
              (SynthClosedEqns (Just (\nm -> (nm, False)))
                [(all_tvs, mkTyVarTys all_tvs, rhs_ty', loc)])
  _      <- liftMaybeT (registerSynthTyCon fam_tc)
  ax     <- MaybeT (pure (closedFamilyTyConCoAxiom_maybe fam_tc))
  pure (mkTyConApp fam_tc cap_args, ax, cap_args)
  where
    dedup _ [] = []
    dedup seen (v : vs)
      | v `elemVarSet` seen = dedup seen vs
      | otherwise           = v : dedup (extendVarSet seen v) vs

    collect_global_term_ids (TermVarTy v)
      | isGlobalId v = [v]
      | otherwise    = []
    collect_global_term_ids (TermAppTy f a) =
      (if isGlobalId f then [f] else []) ++ collect_global_term_ids a
    collect_global_term_ids (AppTy f a) =
      collect_global_term_ids f ++ collect_global_term_ids a
    collect_global_term_ids (TyConApp _ as) =
      concatMap collect_global_term_ids as
    collect_global_term_ids (FunTy _ ma m a r) =
      concatMap collect_global_term_ids [ma, m, a, r]
    collect_global_term_ids (ForAllTy ma b r) =
      collect_global_term_ids ma ++ collect_global_term_ids (binderType b)
        ++ collect_global_term_ids r
    collect_global_term_ids (DepFunTy _ b r) =
      collect_global_term_ids (varType b) ++ filter (/= b) (collect_global_term_ids r)
    collect_global_term_ids (ExistsTy _ b r) =
      collect_global_term_ids (varType b) ++ collect_global_term_ids r
    collect_global_term_ids (DepDictTy b r) =
      collect_global_term_ids (varType b) ++ filter (/= b) (collect_global_term_ids r)
    collect_global_term_ids (CastTy t _) =
      collect_global_term_ids t
    collect_global_term_ids _ = []

freshAnonArgTyVars :: [Kind] -> TcM [TyVar]
freshAnonArgTyVars = zipWithM
  (\i dom -> (`mkTyVar` dom) <$> newSysName (mkTyVarOcc ("arg" ++ show i)))
  [1 :: Int ..]

-- | Sort free 'TyVar's and runtime index 'Id's in dependency order with
-- 'scopedSort' and allocate fresh 'TyVar' parameters for a synthesized family.
abstractSynthCaptures :: [Var] -> [Var] -> TcM (Subst.Subst, [(TyVar, Var)], Type -> Type)
abstractSynthCaptures raw_fvs extra_in_scope = do
  let ordered_fvs = scopedSort raw_fvs
      init_subst  = Subst.mkEmptySubst (mkInScopeSet (mkVarSet (ordered_fvs ++ extra_in_scope)))
      alloc (subst, id_acc) v = do
        let ki = Subst.substTy subst (replaceTypeTermIds id_acc (varType v))
        nm <- newSysName (mkTyVarOccFS (occNameFS (nameOccName (getName v))))
        let tv      = mkTyVar nm ki
            subst'  = if isTyVar v
                        then Subst.extendTvSubstAndInScope subst v (mkTyVarTy tv)
                        else Subst.extendSubstInScope subst tv
            id_acc' = if isId v
                        then extendVarEnv id_acc v (mkTyVarTy tv)
                        else id_acc
        pure ((subst', id_acc'), (tv, v))
  ((subst1, id_map), pairs) <- mapAccumLM alloc (init_subst, emptyVarEnv) ordered_fvs
  let subst_all s ty = Subst.substTy s (replaceTypeTermIds id_map ty)
  pure (subst1, pairs, subst_all subst1)

-- | Queue a post-knot validity check for a synthesized family or synonym in the
-- current 'TyClGroup'.
deferSynthCheck :: TcM () -> TcM ()
deferSynthCheck act = do
  cur_mod <- tcg_mod <$> getGblEnv
  modifySynthTypeState cur_mod
    (\st -> (st { sts_pending_checks = act : sts_pending_checks st }, ()))
    ()

-- | Run and clear all deferred validity checks queued during the current
-- 'TyClGroup' knot.
runPendingSynthChecks :: TcM ()
runPendingSynthChecks = do
  cur_mod <- tcg_mod <$> getGblEnv
  checks <- modifySynthTypeState cur_mod
    (\st -> (st { sts_pending_checks = [] }, reverse (sts_pending_checks st)))
    []
  sequence_ checks

-- | Drain and return all synthesized 'TyCon's recorded for the given 'Module',
-- resetting per-group ordinal and intra-signature case caches.
drainForgetMTyCons :: Module -> TcM [TyCon]
drainForgetMTyCons mod_key =
  modifySynthTypeState mod_key
    (\st -> ( st { sts_tycons         = emptyNameEnv
                  , sts_ordinals       = emptyOccEnv
                  , sts_sig_cases      = emptyNameEnv
                  , sts_pending_checks = [] }
             , nonDetNameEnvElts (sts_tycons st) ))
    []

-- | Allocate a closed 1-equation source family and top-level helper 'Id' for a
-- retained @foreach (f :: a -> b)@ function argument.
tcRegisterSynthesizedSourceFun
  :: SrcSpan -> [TyVar] -> [Id] -> Id -> Type -> Type -> TcM (Id, TyCon)
tcRegisterSynthesizedSourceFun loc raw_tvs cap_val_ids x_id res_kind0 body_index0 = do
  cur_mod <- tcg_mod <$> getGblEnv
  let raw_vars = map (\v -> v :: Var) raw_tvs ++ map (\v -> v :: Var) (cap_val_ids ++ [x_id])
  (_, pairs, subst_all) <- abstractSynthCaptures raw_vars []
  let (p_tvs, val_tvs) = splitAt (length raw_tvs) (map fst pairs)
      res_kind' = subst_all res_kind0
      rhs_ty'   = subst_all body_index0
      all_tvs   = p_tvs ++ val_tvs
  uniq <- getUniqueM
  owner_name <- newGlobalBinder cur_mod (mkVarOcc ("$depfun$" ++ show uniq)) Nothing loc
  fam_name   <- newGlobalBinder cur_mod (mkSourceTableOcc (nameOccName owner_name)) Nothing loc
  fam_tc     <- mkSynthClosedFamily fam_name p_tvs val_tvs res_kind'
                  (SynthClosedEqns Nothing [(all_tvs, mkTyVarTys all_tvs, rhs_ty', loc)])
  let mono_ty = foldr mkVisFunTyManyU res_kind' (map tyVarKind val_tvs)
      fun_ty  = mkSpecForAllTys [(unmatchableDataConTy, tv) | tv <- p_tvs] mono_ty
      no_inl  = setInlinePragmaSpec neverInlinePragma NoInline
      raw_id  = mkLocalId owner_name ManyTy fun_ty `setInlinePragma` no_inl
      fun_id  = setIdDetails raw_id (SourceTableId owner_name fam_tc NominalSourceTable)
  pure (fun_id, fam_tc)

-- | Record a checked top-level helper binding for installation at the end of
-- the current declaration group.
tcCommitSynthesizedSourceFunBind :: Id -> LHsBind GhcTc -> TyCon -> TcM ()
tcCommitSynthesizedSourceFunBind fun_id bind fam_tc = do
  cur_mod <- tcg_mod <$> getGblEnv
  keepAlive (idName fun_id)
  modifySynthTypeState cur_mod
    (\st -> (st { sts_source_funs = extendNameEnv (sts_source_funs st) (idName fun_id) (fun_id, bind, fam_tc) }, ()))
    ()

-- | Drain and return all synthesized source function helpers for a 'Module'.
drainSynthesizedSourceFuns :: Module -> TcM [(Id, LHsBind GhcTc, TyCon)]
drainSynthesizedSourceFuns mod_key =
  modifySynthTypeState mod_key
    (\st -> (st { sts_source_funs = emptyNameEnv }, nonDetNameEnvElts (sts_source_funs st)))
    []

-- | Record a synthesized function 'Id' in 'SynthTypeState' so later family
-- rewrites in the same binding group can resolve its 'Name'.
recordLocalSynthFunId :: Id -> TcM ()
recordLocalSynthFunId fid = do
  cur_mod <- tcg_mod <$> getGblEnv
  modifySynthTypeState cur_mod
    (\st ->
       let cur  = sts_synth_fun_ids st
           cur' = if any ((== idName fid) . idName) cur then cur else fid : cur
       in (st { sts_synth_fun_ids = cur' }, ()))
    ()

-- | Extend the local value environment with synthesized function 'Id's that
-- are not yet installed in 'tcg_type_env'.
withLocalSynthFunIds :: TcM a -> TcM a
withLocalSynthFunIds action = do
  st <- getSynthTypeState
  case sts_synth_fun_ids st of
    []   -> action
    rids -> do
      gbl <- getGblEnv
      let fids = [ fid | fid <- rids
                       , isNothing (lookupTypeEnv (tcg_type_env gbl) (idName fid)) ]
      foldr tcExtendGeneratedIdEnv action fids

-- | Replace the active 1-argument renamed function table for a 'Module' and
-- return the previous table so nested calls can restore it.
setLocalSimpleFunBinds
  :: Module
  -> NameEnv (Maybe (LocatedN Name), LHsExpr GhcRn)
  -> TcM (Maybe (NameEnv (Maybe (LocatedN Name), LHsExpr GhcRn)))
setLocalSimpleFunBinds mod_key new_env =
  modifySynthTypeState mod_key
    (\st ->
       let next = if isEmptyNameEnv new_env then Nothing else Just new_env
       in (st { sts_simple_funs = next }, sts_simple_funs st))
    Nothing

-- | Look up a non-recursive 1-argument renamed function binding in the active
-- module declaration group.
lookupLocalSimpleFunBind :: Name -> TcM (Maybe (Maybe (LocatedN Name), LHsExpr GhcRn))
lookupLocalSimpleFunBind nm = do
  cur_mod <- tcg_mod <$> getGblEnv
  if nameModule_maybe nm /= Just cur_mod
    then pure Nothing
    else do
      st <- getSynthTypeState
      pure (sts_simple_funs st >>= (`lookupNameEnv` nm))

{- Note [Post-renaming lowering of lambdas, sums, and cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After 'GHC.Rename.HsType' renames type lambdas, dependent sums (@some@), and
type @case@ expressions in their lexical scope (GHC Proposals #281, #378, #425,
#473, and #523; Eisenberg and Stolarek, Haskell Symposium 2014; Danvy and
Nielsen, PPDP 2001), 'GHC.Tc.Gen.HsType' lowers them directly from typed
environments without pre-renaming 'GhcPs' closure conversion:

* Declaration kind-inference pass ('kcTyClGroup'): while the enclosing
  declaration 'Name' is not yet present in 'tcg_type_env', check all binders,
  scrutinees, patterns, and right-hand sides to record kind constraints, then
  return an @Any@ placeholder of the checked kind without registering
  'MonoTcTyCon'-bearing helpers. In the knot-tied declaration pass
  ('tcTyClDecls'), 'resolveRecTcTypes' replaces any 'TcTyCon' from the current
  recursive group with its knot-tied 'TyCon' using lazy 'TyConApp' nodes.
* Dependent sums ('tcSomeTypeInHsType'): synthesize @$lambda:<Owner>:<i>-sigma@
  with @CTF_Scoped root True@ (plus a @$signature-sum:@ or @$field-sum:@
  wrapper synonym on value signatures and constructor fields so Template Haskell
  'reify' sees an ordinary synonym application).
* Type lambdas ('tcHsTypeLambda'): coalesce duplicate-free curried binders,
  skolemise expected-kind @forall@ prefixes and unmatchable arrows, compile
  direct scrutinee @case@ bodies as multi-branch family equations, and attach
  @CTF_Scoped root False@ so nested lambdas share their outer scope root.
* Type cases ('tcHsTypeCase'): synthesize @$case:<Owner>:<i>@ (with
  @CTF_Scoped syn False@ on top-level value signatures for cross-module
  definitional equality) and compile overlapping, wildcard, or nested
  constructor patterns into ordered decision families via
  'compileOrderedSynthCase' (Maranget, ML 2008).
-}

data SynthSite
  = SynthInLambda Name Name
  | SynthInSigma Name Name
  | SynthTySyn Name
  | SynthClosedFam Name
  | SynthOpenFamInst Name
  | SynthConField Name (Maybe Name)
  | SynthTopSig Name
  | SynthLocalSig String
  | SynthExprT2T

classifySynthSite :: TcTyMode -> TcM (SynthSite, Bool)
classifySynthSite mode = do
  gbl <- getGblEnv
  lcl <- getLclEnv
  lcl_env <- getLclTypeEnv
  let ctxts = getLclEnvErrCtxt lcl
      is_real_decl_tc dt =
        isJust (lookupNameEnv lcl_env dt) || isJust (lookupTypeEnv (tcg_type_env gbl) dt)
      mb_decl_tc = listToMaybe [dt | TyConDeclCtxt dt _ <- ctxts, is_real_decl_tc dt]
      in_kc = case mb_decl_tc of
        Just dt -> isNothing (lookupTypeEnv (tcg_type_env gbl) dt)
        Nothing -> isJust (mode_synonym_metadata mode) && isNothing (mode_source_proof_owner mode)
      from_ctxts [] = SynthExprT2T
      from_ctxts (c : cs) = case c of
        DataConDefCtxt (L _ con_nm :| _) -> SynthConField con_nm mb_decl_tc
        TyConDeclCtxt tc_nm TypeSynonymFlavour | is_real_decl_tc tc_nm -> SynthTySyn tc_nm
        TyConDeclCtxt tc_nm ClosedTypeFamilyFlavour | is_real_decl_tc tc_nm -> SynthClosedFam tc_nm
        TyConInstCtxt fam_nm flav
          | OpenFamilyFlavour IAmType _ <- tyConInstFlavour flav -> SynthOpenFamInst fam_nm
        UserSigCtxt (FunSigCtxt val_nm _) _
          | isExternalName val_nm -> SynthTopSig val_nm
          | otherwise -> SynthLocalSig (occNameString (nameOccName val_nm))
        UserSigCtxt (ExprSigCtxt _) _ -> SynthLocalSig "$expression"
        _ -> from_ctxts cs
      site
        | Just (r, o) <- mode_lambda_root mode = SynthInLambda r o
        | Just (r, o) <- mode_sigma_root mode  = SynthInSigma r o
        | Just (_, HM_VTA) <- mode_holes mode  = SynthLocalSig "$type-argument"
        | otherwise                            = from_ctxts ctxts
  pure (site, in_kc)

-- | Replace 'TcTyCon' occurrences from the current recursive 'TyClGroup' with
-- their knot-tied 'TyCon's in 'tcg_type_env' without forcing the knot thunks.
resolveRecTcTypes :: TypeEnv -> Type -> Type
resolveRecTcTypes tenv = go
  where
    go (TyVarTy tv)       = TyVarTy (setTyVarKind tv (go (tyVarKind tv)))
    go (AppTy f a)        = AppTy (go f) (go a)
    go (TyConApp tc args)
      | isTcTyCon tc
      , Just (ATyCon real_tc) <- lookupTypeEnv tenv (tyConName tc)
      = TyConApp real_tc (map go args)
      | otherwise
      = TyConApp tc (map go args)
    go (ForAllTy ma (Bndr b vis) r) = ForAllTy (go ma) (Bndr (updateVarType go b) vis) (go r)
    go (FunTy af ma m a r) = FunTy af (go ma) (go m) (go a) (go r)
    go (CastTy t co)      = CastTy (go t) co
    go (TermVarTy v)      = TermVarTy (updateIdTypeAndMult go v)
    go (TermAppTy f a)    = TermAppTy (updateIdTypeAndMult go f) (go a)
    go (DepFunTy vis b r) = DepFunTy vis (updateIdTypeAndMult go b) (go r)
    go (ExistsTy vis b r) = ExistsTy vis (setTyVarKind b (go (tyVarKind b))) (go r)
    go (DepDictTy b r)    = DepDictTy (updateIdTypeAndMult go b) (go r)
    go (ClassAppTy s a d x k) = ClassAppTy (updateIdTypeAndMult go s) (go a) (go d) (go x) (go k)
    go t                  = t

isSigmaTyConName :: Name -> Bool
isSigmaTyConName nm =
  occNameFS (nameOccName nm) == fsLit "Sigma"
  && maybe False (\m -> moduleNameString (moduleName m) == "GHC.Sigma") (nameModule_maybe nm)

letUsedInCaseRn :: Name -> LHsType GhcRn -> Bool
letUsedInCaseRn target = go
  where
    go (L _ ty) = case ty of
      XHsType (HsTypeCaseRn scrut rows) ->
        mentions target scrut || any (\(ns, _, r) -> target `notElem` ns && mentions target r) rows
      XHsType (HsTypeLetRn _ (L _ b) r body) ->
        go r || (target /= b && (go body || (mentions target r && letUsedInCaseRn b body)))
      XHsType (HsTypeLambdaQuoteRn vs body) ->
        target `notElem` mapMaybe hsLTyVarName vs && go body
      XHsType (HsTypeLambdaCheckRn vs body helper) ->
        (target `notElem` mapMaybe hsLTyVarName vs && go body) || go helper
      HsParTy _ t            -> go t
      HsKindSig _ t k        -> go t || go k
      HsAppTy _ f a          -> go f || go a
      HsAppKindTy _ t k      -> go t || go k
      HsFunTy _ _ a b        -> go a || go b
      HsOpTy _ a _ b         -> go a || go b
      HsListTy _ t           -> go t
      HsTupleTy _ _ ts       -> any go ts
      HsSumTy _ ts           -> any go ts
      HsExplicitListTy _ _ ts -> any go ts
      HsExplicitTupleTy _ _ ts -> any go ts
      HsForAllTy _ _ b       -> go b
      HsForEachTy _ _ b      -> go b
      HsQualTy _ _ b         -> go b
      HsSpliceTy (HsUntypedSpliceTop _ inner) _ -> go inner
      _                      -> False

    mentions :: Data a => Name -> a -> Bool
    mentions nm thing
      | isAstLeafAnnotation thing = case cast thing :: Maybe Name of
          Just n  -> n == nm
          Nothing -> False
      | Just (L _ (HsSpliceTy (HsUntypedSpliceTop _ inner) _)) <- (cast thing :: Maybe (LHsType GhcRn))
      = mentions nm inner
      | Just (L _ HsSpliceTy{}) <- (cast thing :: Maybe (LHsType GhcRn)) = False
      | otherwise = or (gmapQ (mentions nm) thing)

ambientMetaTyVars :: ExpKind -> TcM TyCoVarSet
ambientMetaTyVars exp_kind = do
  lcl <- getLclTypeEnv
  let env_tys = concatMap thing_tys (nonDetNameEnvElts lcl)
      ek_tys  = maybeToList (checkingExpType_maybe exp_kind)
  zonked <- liftZonkM (zonkTcTypes (ek_tys ++ env_tys))
  pure (filterVarSet isMetaTyVar (tyCoVarsOfTypes zonked))
  where
    thing_tys (ATyVar _ tv)        = [mkTyVarTy tv, tyVarKind tv]
    thing_tys (ATcTypeLet _ ty ki) = [ty, ki]
    thing_tys (ATcId { tct_id = v }) = [varType v, idMult v]
    thing_tys _                    = []

-- | Partition captured 'Var's into invisible kind captures and visible type or
-- runtime-index captures, preserving explicit declaration header binders when
-- present.
partitionSynthCaptures
  :: Bool                     -- ^ Retain unused header binders for reification
  -> Bool                     -- ^ Under an inner explicit @forall@
  -> Maybe ([TyVar], [TyVar]) -- ^ Optional explicit declaration (invis, vis) header binders
  -> [Var]                    -- ^ Free variables referenced by the lowered construct
  -> TcM (Subst.Subst, [(TyVar, Specificity)], [TyVar], [Type], Type -> Type)
partitionSynthCaptures keep_unused_hdr under_forall mb_decl_hdr raw_fvs = do
  lcl_full <- getLclEnv
  lcl <- getLclTypeEnv
  let stack_vars =
        [ v
        | b <- reverse (getLclEnvBinderStack lcl_full)
        , Just v <- [case b of
                       TcTvBndr _ tv -> Just (tv :: Var)
                       TcIdBndr i _  -> Just (i :: Var)
                       _             -> Nothing] ]
      stack_pos = mkVarEnv (zip stack_vars [0 :: Int ..])
      lcl_spans = mkVarEnv [(tv :: Var, nameSrcSpan nm) | ATyVar nm tv <- nonDetNameEnvElts lcl]
      active_invis_specs = mkVarEnv
        [ (tv, SpecifiedSpec)
        | ATyVar _ tv <- nonDetNameEnvElts lcl ]
      lcl_tv_set = mkVarSet [tv | ATyVar _ tv <- nonDetNameEnvElts lcl]
      (hdr_invis, hdr_vis0) = fromMaybe ([], []) mb_decl_hdr
      raw_dedup     = dedup_vars raw_fvs
      raw_fv_set    = mkVarSet (close_kinds raw_dedup)
      hdr_vis       = if keep_unused_hdr
                        then hdr_vis0
                        else [tv | tv <- hdr_vis0, tv `elemVarSet` raw_fv_set]
      hdr_all_vis   = mkVarSet hdr_vis0
      hdr_vis_set   = mkVarSet hdr_vis
      hdr_invis_set = mkVarSet hdr_invis
      non_hdr_fvs   = sort_by_bind_loc stack_pos lcl_spans [v | v <- raw_dedup, not (v `elemVarSet` hdr_all_vis)]
      seed_vars     = dedup_vars (map (\v -> v :: Var) hdr_vis ++ non_hdr_fvs)
      base_vars     = scopedSort (close_kinds seed_vars)
      -- Distinguish kind-only variables and inferred metavariables from written visible variables:
      explicit_set  = mkVarSet seed_vars
      is_invis_var v
        | not (isTyVar v)                  = False
        | not (v `elemVarSet` lcl_tv_set)  = True
        | v `elemVarSet` hdr_invis_set     = True
        | isJust mb_decl_hdr, not under_forall = not (v `elemVarSet` hdr_vis_set)
        | not (v `elemVarSet` explicit_set) = True
        | otherwise                        = False
      (invis_orig, vis_orig) =
        ( [v | v <- base_vars, is_invis_var v]
        , [v | v <- base_vars, not (is_invis_var v)] )
      ordered = invis_orig ++ vis_orig
  (subst1, pairs, subst_fn) <- abstractSynthCaptures ordered []
  let (invis_pairs, vis_pairs) = splitAt (length invis_orig) pairs
      spec_of orig_v = lookupVarEnv active_invis_specs orig_v `orElse` InferredSpec
      invis_out = [ (p_tv, spec_of orig_v) | (p_tv, orig_v) <- invis_pairs ]
      vis_out   = map fst vis_pairs
      cap_args  = [ if isTyVar orig_v then mkTyVarTy orig_v else TermVarTy orig_v
                  | (_, orig_v) <- invis_pairs ++ vis_pairs ]
  pure (subst1, invis_out, vis_out, cap_args, subst_fn)
  where
    sort_by_bind_loc stack_pos lcl_spans =
      maybe [] (NE.toList . NE.sortBy (cmp_bind stack_pos lcl_spans)) . NE.nonEmpty
    bind_span lcl_spans v = lookupVarEnv lcl_spans v `orElse` nameSrcSpan (getName v)
    cmp_bind stack_pos lcl_spans v1 v2 = case (lookupVarEnv stack_pos v1, lookupVarEnv stack_pos v2) of
      (Just i1, Just i2) | i1 /= i2 -> compare i1 i2
      _ -> case (srcSpanStart (bind_span lcl_spans v1), srcSpanStart (bind_span lcl_spans v2)) of
        (RealSrcLoc l1 _, RealSrcLoc l2 _) -> compare l1 l2
        _ -> EQ
    dedup_vars = go emptyVarSet
      where
        go _ [] = []
        go seen (v : vs)
          | v `elemVarSet` seen = go seen vs
          | otherwise           = v : go (extendVarSet seen v) vs
    close_kinds vs =
      let k_fvs = tyCoVarsOfTypesList (map varType vs)
          more  = filterOut (`elemVarSet` mkVarSet vs) k_fvs
      in if null more then vs else close_kinds (more ++ vs)

lookupDeclHeaderTyVars :: SynthSite -> TcM (Maybe ([TyVar], [TyVar]))
lookupDeclHeaderTyVars (SynthTySyn syn_nm) = do
  gbl <- getGblEnv
  lcl <- getLclTypeEnv
  in_knot <- isInDeclKnotM
  let mb_tc = case lookupNameEnv lcl syn_nm of
        Just (ATcTyCon tc) -> Just tc
        _ | not in_knot
          , Just (ATyCon tc) <- lookupTypeEnv (tcg_type_env gbl) syn_nm
          -> Just tc
        _ -> Nothing
      resolve_lcl tv = case lookupNameEnv lcl (getName tv) of
        Just (ATyVar _ lcl_tv) -> Just lcl_tv
        _                      -> Nothing
  case mb_tc of
    Just poly_tc ->
      let bndrs = tyConBinders poly_tc
          invis = mapMaybe resolve_lcl [tv | Bndr tv vis <- bndrs, isInvisibleTyConBinder (Bndr tv vis)]
          vis   = mapMaybe resolve_lcl [tv | Bndr tv vis <- bndrs, isVisibleTyConBinder (Bndr tv vis)]
      in pure (Just (invis, vis))
    Nothing -> pure Nothing
lookupDeclHeaderTyVars _ = pure Nothing

checkSynthFamValidity :: TyCon -> [TyVar] -> [Type] -> Type -> SrcSpan -> Maybe SDoc -> TcM ()
checkSynthFamValidity fam_tc _qtvs lhs rhs loc _mb_override_err =
  setSrcSpan loc $
    setErrCtxt [] $
      addErrCtxt (TyConDeclCtxt (tyConName fam_tc) ClosedTypeFamilyFlavour) $
        addErrCtxt (ClosedFamEqnCtxt fam_tc) $
          checkValidTyFamEqn fam_tc lhs rhs

isInDeclKnotM :: TcM Bool
isInDeclKnotM = do
  lcl <- getLclEnv
  pure $ any (\case { TyConDeclCtxt{} -> True; DataConDefCtxt{} -> True; _ -> False })
             (getLclEnvErrCtxt lcl)

runOrDeferSynthCheck :: TcM () -> TcM ()
runOrDeferSynthCheck act = do
  in_decl <- isInDeclKnotM
  if in_decl then deferSynthCheck act else act

hasAmbientMetaKindsM :: TcM Bool
hasAmbientMetaKindsM = do
  lcl <- getLclTypeEnv
  let scoped = [tv | ATyVar _ tv <- nonDetNameEnvElts lcl]
      scoped_set = mkVarSet scoped
  ks <- liftZonkM $ zonkTcTypes (map tyVarKind scoped)
  pure (anyVarSet (\v -> isMetaTyVar v && not (v `elemVarSet` scoped_set)) (tyCoVarsOfTypes ks))

-- | Return the branch right-hand sides of all synthesized @CTF_Scoped@
-- families in the active declaration group for 'checkSynCyclesWithPayloads'.
pendingSynthSigmaPayloads :: TcM (NameEnv [Type])
pendingSynthSigmaPayloads = do
  st <- getSynthTypeState
  pure $ mkNameEnv
    [ (tyConName stc, [coAxBranchRHS b | b <- fromBranches (coAxiomBranches ax)])
    | stc <- nonDetNameEnvElts (sts_tycons st)
    , Just _ <- [closedFamilyScope_maybe stc]
    , Just ax <- [closedFamilyTyConCoAxiom_maybe stc] ]

-- | Lower @some (x :: A) | B@ (represented as @GHC.Sigma.Sigma A (\x -> B)@).
-- See Note [Post-renaming lowering of lambdas, sums, and cases].
tcSomeTypeInHsType
  :: TcTyMode -> HsType GhcRn -> Name -> LHsType GhcRn -> SrcSpan
  -> LocatedN Name -> LHsType GhcRn -> ExpKind -> TcM TcType
tcSomeTypeInHsType mode rn_ty sigma_nm dom_hs lam_loc bndr body_hs exp_kind = do
  enabled <- xoptM LangExt.DependentTypes
  unless enabled $
    failWithTc (TcRnRetainedQuantificationError RetainedSumContext)
  (site, in_kc) <- classifySynthSite mode
  has_kc_metas <- if in_kc then hasAmbientMetaKindsM else pure False
  if in_kc && not has_kc_metas
    then checkExpKind mode rn_ty (mkTyConApp anyTyCon [liftedTypeKind]) liftedTypeKind exp_kind
    else do
      sigma_tc <- tcLookupTyCon sigma_nm
      dom_ty0  <- tc_check_lhs_type mode dom_hs liftedTypeKind >>= liftZonkM . zonkTcType
      (wrap_syn_mb, fam_name, root_fam, next_owner) <- alloc_some_names in_kc site lam_loc
      let hs_tvb = HsTvb noAnn () (HsBndrVar noExtField bndr)
                     (HsBndrKind noExtField (L (l2l (getLoc bndr)) (XHsType (HsCoreTyRn dom_ty0))))
      skol <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn [hs_tvb]))
      x_skol <- newSkolemTyVar skol (unLoc bndr) dom_ty0
      let body_mode = mode { mode_sigma_root = Just (root_fam, next_owner) }
          solve_if_tc = if in_kc then id else solveEqualities "tcSomeTypeInHsType"
          wrap_ctxt = case site of
            SynthInSigma{} -> id
            _              -> addErrCtxt (TyConDeclCtxt fam_name ClosedTypeFamilyFlavour)
          body_check = tcExtendNameTyVarEnv [(unLoc bndr, x_skol)] $
                         tc_check_lhs_type body_mode body_hs liftedTypeKind
      body_ty0 <- solve_if_tc (wrap_ctxt body_check)
                    >>= liftZonkM . zonkTcType
      dom_ty   <- liftZonkM (zonkTcType dom_ty0)
      if in_kc
        then checkExpKind mode rn_ty (mkTyConApp anyTyCon [liftedTypeKind]) liftedTypeKind exp_kind
        else do
          gbl <- getGblEnv
          let raw_fvs = filterOut (== (x_skol :: Var)) $
                          tyCoVarsOfTypesList [dom_ty, body_ty0]
              cap_ids = [v | v <- raw_fvs, isId v]
          when (any isCoVar raw_fvs
                || not (null (findDupsEq (==) (map (nameOccName . getName) cap_ids)))
                || (not (null cap_ids) && not (allows_runtime_some site))) $
            failWithTc (TcRnRetainedQuantificationError RetainedSumContext)
          mb_hdr <- lookupDeclHeaderTyVars site
          (subst1, invis_caps, vis_caps, cap_args0, _) <- partitionSynthCaptures False (mode_under_forall mode) mb_hdr raw_fvs
          x_nm <- newSysName (mkTyVarOccFS (occNameFS (nameOccName (unLoc bndr))))
          let rec_tenv = tcg_type_env gbl
              res_rec  = resolveRecTcTypes rec_tenv
              id_map   = mkVarEnv [ (orig_v, cap_ty)
                                  | (orig_v, cap_ty) <- zip [v | v <- map snd_var raw_fvs] [] ]
              all_cap_tvs = map fst invis_caps ++ vis_caps
          let ordered_orig = [ v | arg <- cap_args0
                             , Just v <- [case arg of { TyVarTy tv -> Just (tv :: Var); TermVarTy i -> Just (i :: Var); _ -> Nothing }] ]
          (subst_c, cap_pairs, _) <- abstractSynthCaptures ordered_orig [x_skol]
          let p_all_tvs = map fst cap_pairs
              (p_invis_tvs, p_vis_tvs) = splitAt (length invis_caps) p_all_tvs
              p_invis = zip p_invis_tvs (map snd invis_caps)
              x_tv0   = mkTyVar x_nm (Subst.substTy subst_c ( replaceTypeTermIds
                          (mkVarEnv [(ov, mkTyVarTy pv) | (pv, ov) <- cap_pairs, isId ov]) dom_ty ))
              subst_x = Subst.extendTvSubstAndInScope subst_c x_skol (mkTyVarTy x_tv0)
              id_env  = mkVarEnv [(ov, mkTyVarTy pv) | (pv, ov) <- cap_pairs, isId ov]
              sub_ty  = res_rec . replaceTypeTermIds id_env . Subst.substTy subst_x
              x_tv    = setTyVarKind x_tv0 (res_rec (tyVarKind x_tv0))
              p_invis' = [(setTyVarKind tv (res_rec (tyVarKind tv)), sp) | (tv, sp) <- p_invis]
              p_vis'   = [setTyVarKind tv (res_rec (tyVarKind tv)) | tv <- p_vis_tvs]
              rhs_ty'  = sub_ty body_ty0
              dom_sub  = sub_ty dom_ty
              qtvs     = map fst p_invis' ++ p_vis' ++ [x_tv]
              tcbs     = mkDependentTyConBinders p_invis' p_vis' [] [x_tv] liftedTypeKind
              leaked   = filterOut (`elemVarSet` mkVarSet qtvs) $
                           tyCoVarsOfTypesList (rhs_ty' : map tyVarKind qtvs)
              _ = (subst1, id_map, all_cap_tvs)
          unless (null leaked && all (tcIsLiftedTypeKind . typeKind . tyVarKind) qtvs) $
            failWithTc (TcRnRetainedQuantificationError RetainedSumContext)
          fam_tc <- mkSynthClosedFamilyTcbs fam_name tcbs liftedTypeKind
                      (SynthClosedEqns (Just (const (root_fam, True)))
                        [(qtvs, mkTyVarTys qtvs, rhs_ty', lam_loc)])
          _ <- registerSynthTyCon fam_tc
          let cap_args = map res_rec cap_args0
              sigma_app = mkTyConApp sigma_tc [res_rec dom_ty, mkTyConApp fam_tc cap_args]
          res_ty <- case wrap_syn_mb of
            Just syn_name -> do
              let syn_tcbs  = mkDependentTyConBinders p_invis' p_vis' [] [] liftedTypeKind
                  syn_kind  = mkTyConKindWithMatchability unmatchableDataConTy syn_tcbs liftedTypeKind
                  syn_roles = map (const Nominal) syn_tcbs
                  fam_vars  = mkTyVarTys (map fst p_invis' ++ p_vis')
                  syn_rhs   = mkTyConApp sigma_tc [dom_sub, mkTyConApp fam_tc fam_vars]
                  syn_tc    = buildSynTyCon syn_name syn_kind syn_tcbs liftedTypeKind syn_roles syn_rhs
              _ <- registerSynthTyCon syn_tc
              pure (mkTyConApp syn_tc cap_args)
            Nothing -> pure sigma_app
          case site of
            SynthExprT2T ->
              setXOptM LangExt.UndecidableInstances $
                checkSynthFamValidity fam_tc qtvs (mkTyVarTys qtvs) rhs_ty' lam_loc Nothing
            SynthLocalSig{} ->
              setXOptM LangExt.UndecidableInstances $
                checkSynthFamValidity fam_tc qtvs (mkTyVarTys qtvs) rhs_ty' lam_loc Nothing
            _ -> do
              undec <- xoptM LangExt.UndecidableInstances
              runOrDeferSynthCheck $
                (if undec then setXOptM LangExt.UndecidableInstances else id) $
                  checkSynthFamValidity fam_tc qtvs (mkTyVarTys qtvs) rhs_ty' lam_loc Nothing
          checkExpKind mode rn_ty res_ty liftedTypeKind exp_kind
  where
    snd_var v = v
    allows_runtime_some SynthTopSig{}   = True
    allows_runtime_some SynthConField{} = True
    allows_runtime_some SynthInSigma{}  = True
    allows_runtime_some SynthExprT2T    = True
    allows_runtime_some SynthLocalSig{} = True
    allows_runtime_some _               = False

    alloc_some_names in_kc site loc = do
      cur_mod <- tcg_mod <$> getGblEnv
      let mk_g occ = newGlobalBinder cur_mod (mkTcOcc occ) Nothing loc
          next_ord occ = if in_kc then pure 0 else allocSynthOrdinal occ
      case site of
        SynthInSigma root_fam owner_nm -> do
          let o_occ = nameOccName owner_nm
          idx <- next_ord o_occ
          fn  <- mk_g ("$lambda:" ++ occNameString o_occ ++ ":" ++ show idx ++ "-sigma")
          pure (Nothing, fn, root_fam, owner_nm)
        SynthTopSig val_nm -> do
          let v_occ = nameOccName val_nm
          idx <- next_ord (mkTcOcc ("$signature-sum:" ++ occNameString v_occ))
          let syn_str = "$signature-sum:" ++ occNameString v_occ ++ ":" ++ show idx
          sn <- mk_g syn_str
          fn <- mk_g ("$lambda:" ++ syn_str ++ ":0-sigma")
          _  <- next_ord (nameOccName sn)
          pure (Just sn, fn, fn, sn)
        SynthConField con_nm _ -> do
          let c_occ = nameOccName con_nm
          idx <- next_ord (mkTcOcc ("$field-sum:" ++ occNameString c_occ))
          let syn_str = "$field-sum:" ++ occNameString c_occ ++ ":" ++ show idx
          sn <- mk_g syn_str
          fn <- mk_g ("$lambda:" ++ syn_str ++ ":0-sigma")
          _  <- next_ord (nameOccName sn)
          pure (Just sn, fn, fn, sn)
        SynthTySyn syn_nm -> do
          let s_occ = nameOccName syn_nm
          idx <- next_ord s_occ
          fn  <- mk_g ("$lambda:" ++ occNameString s_occ ++ ":" ++ show idx ++ "-sigma")
          pure (Nothing, fn, fn, syn_nm)
        SynthClosedFam cf_nm -> do
          let c_occ = nameOccName cf_nm
          idx <- next_ord c_occ
          fn  <- mk_g ("$lambda:" ++ occNameString c_occ ++ ":" ++ show idx ++ "-sigma")
          pure (Nothing, fn, fn, cf_nm)
        SynthOpenFamInst of_nm -> do
          u_occ <- chooseUniqueOccTc (mkInstTyTcOcc ("$sum:" ++ occNameString (nameOccName of_nm)))
          sn    <- newGlobalBinder cur_mod u_occ Nothing loc
          fn    <- mk_g ("$lambda:" ++ occNameString u_occ ++ ":0-sigma")
          _     <- next_ord u_occ
          pure (Nothing, fn, fn, sn)
        _ -> do
          uniq <- getUniqueM
          fn   <- mk_g ("$some$" ++ show uniq)
          pure (Nothing, fn, fn, fn)

-- | Lower a post-renaming type lambda ('HsTypeLambdaQuoteRn' or
-- 'HsTypeLambdaCheckRn') into a synthesized closed type family (and wrapper
-- synonym where required).
-- See Note [Post-renaming lowering of lambdas, sums, and cases].
tcHsTypeLambda
  :: Maybe (Name, [Name])
  -> TcTyMode -> HsType GhcRn -> [LHsTyVarBndr () GhcRn] -> LHsType GhcRn
  -> ExpKind -> TcM TcType
tcHsTypeLambda mb_pre_syn mode src binders0 body0 exp_kind = do
  enabled <- xoptM LangExt.DependentTypes
  unless enabled $ failWithTc TcRnTypeLambdaDisabled
  level <- getThLevel
  case (level, mb_pre_syn) of
    (Comp, _)                -> pure ()
    (Brack _ TcPending{}, Just _) -> pure ()
    _                        -> failWithTc TcRnTypeLambdaContext
  unless (isTypeLevel (mode_tyki mode) && not (null binders0)) $
    failWithTc TcRnTypeLambdaContext
  (site, in_kc) <- classifySynthSite mode
  when (disallow_lambda_site site) $ failWithTc TcRnTypeLambdaContext
  let (all_bndrs, inner_body) = coalesce_lambdas binders0 body0
      all_bndr_names = [ln | Just ln <- map hsLTyVarLocName all_bndrs]
  unless (equalLength all_bndrs all_bndr_names) $ failWithTc TcRnTypeLambdaContext
  lam_loc <- getSrcSpanM
  (wrap_syn_mb, fam_name, root_fam, next_owner) <- alloc_lambda_names in_kc site lam_loc
  ek0 <- expTypeToType exp_kind >>= liftZonkM . zonkTcType
  skol_invis_info <- mkSkolemInfo (TyConSkol ClosedTypeFamilyFlavour fam_name)
  (mid_invis_skols, ek_rho) <- tcSkolemiseInvisibleBndrs (getSkolemInfo skol_invis_info) ek0
  let mid_invis_specs = extract_invis_specs (length mid_invis_skols) ek0
  (dom_kinds_raw, res_kind0) <- peel_unmatchable_arrows src all_bndrs ek_rho
  let hs_tvbs0 =
        [ HsTvb noAnn () (HsBndrVar noExtField b)
            (HsBndrKind noExtField (L (l2l (getLoc b)) (XHsType (HsCoreTyRn dk))))
        | (b, dk) <- zip all_bndr_names dom_kinds_raw ]
  skol <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn hs_tvbs0))
  let kind_mode = mode { mode_tyki = KindLevel
                       , mode_pending_calls = Nothing
                       , mode_complete_type_arg = Nothing }
      check_bndr_tele [] [] = pure ([], [])
      check_bndr_tele (L _ (HsTvb _ () _ bkind) : bs) ((b, dk0) : dks) = do
        dk <- case bkind of
          HsBndrNoKind _ -> pure dk0
          HsBndrKind _ hs_k -> do
            sig_k <- tc_lhs_kind_sig kind_mode (TyVarBndrKindCtxt (unLoc b)) hs_k
            _ <- unifyKind (Just (HsTypeRnThing (unLoc hs_k))) sig_k dk0
            liftZonkM (zonkTcType sig_k)
        z_sk <- newSkolemTyVar skol (unLoc b) dk
        (z_rest, dk_rest) <- tcExtendNameTyVarEnv [(unLoc b, z_sk)] (check_bndr_tele bs dks)
        pure (z_sk : z_rest, dk : dk_rest)
      check_bndr_tele _ _ = failWithTc TcRnTypeLambdaContext
  (z_skols, dom_kinds0) <- tcExtendTyVarEnv mid_invis_skols $
                             check_bndr_tele all_bndrs (zip all_bndr_names dom_kinds_raw)
  let body_mode = mode { mode_lambda_root = Just (root_fam, next_owner) }
      solve_if_tc :: TcM a -> TcM a
      solve_if_tc = if in_kc then id else solveEqualities "tcHsTypeLambda"
      extend_lam :: TcM a -> TcM a
      extend_lam = tcExtendTyVarEnv mid_invis_skols
                 . tcExtendNameTyVarEnv (zip (map unLoc all_bndr_names) z_skols)
  case direct_lambda_case all_bndr_names inner_body of
    Just (scrut_col, scrut_nm, case_rows) -> do
      checked_rows <- addErrCtxt (TyConDeclCtxt fam_name ClosedTypeFamilyFlavour) $
        extend_lam $
          check_lambda_case_rows solve_if_tc body_mode fam_name
            mid_invis_skols z_skols dom_kinds0 scrut_col scrut_nm res_kind0 case_rows
      dom_kinds <- mapM (liftZonkM . zonkTcType) dom_kinds0
      res_kind  <- liftZonkM (zonkTcType res_kind0)
      mid_skols <- mapM (liftZonkM . zonkTyCoVarKind) mid_invis_skols
      if in_kc
        then do
          let full_k = mkSpecForAllTys [(unmatchableDataConTy, tv) | tv <- mid_skols]
                         (foldr mkVisFunTyManyU res_kind dom_kinds)
          checkExpKind mode src (mkTyConApp anyTyCon [full_k]) full_k exp_kind
        else build_case_lambda gbl_site site wrap_syn_mb fam_name root_fam lam_loc
               (zip mid_skols mid_invis_specs) z_skols dom_kinds res_kind scrut_col checked_rows
    Nothing -> do
      track_proofs <- xoptM LangExt.DependentTypes
      ax_probe <- newImplicitBinder fam_name mkInstTyCoOcc
      (body_ty0, proof_deps) <- solve_if_tc $ addErrCtxt (TyConDeclCtxt fam_name ClosedTypeFamilyFlavour) $
        extend_lam $
          if track_proofs
            then do
              r <- withSourceProofSignature (Just ax_probe) (tc_check_lhs_type body_mode inner_body res_kind0)
              ds <- takeSourceProofRulesFor ax_probe
              pure (r, ds)
            else (, []) <$> tc_check_lhs_type body_mode inner_body res_kind0
      dom_kinds <- mapM (liftZonkM . zonkTcType) dom_kinds0
      res_kind  <- liftZonkM (zonkTcType res_kind0)
      body_ty   <- liftZonkM (zonkTcType body_ty0)
      mid_skols <- mapM (liftZonkM . zonkTyCoVarKind) mid_invis_skols
      if in_kc
        then do
          let full_k = mkSpecForAllTys [(unmatchableDataConTy, tv) | tv <- mid_skols]
                         (foldr mkVisFunTyManyU res_kind dom_kinds)
          checkExpKind mode src (mkTyConApp anyTyCon [full_k]) full_k exp_kind
        else build_beta_lambda site wrap_syn_mb fam_name root_fam lam_loc
               (zip mid_skols mid_invis_specs) z_skols dom_kinds res_kind body_ty proof_deps
  where
    gbl_site = ()

    disallow_lambda_site (SynthLocalSig _) = mode_under_forall mode
    disallow_lambda_site _                 = False

    coalesce_lambdas bs (L _ (HsParTy _ inner)) = coalesce_lambdas bs inner
    coalesce_lambdas bs (L _ (XHsType (HsTypeLambdaQuoteRn more inner)))
      | let combined = bs ++ more
            named_occs = [nameOccName nm | Just nm <- map hsLTyVarName combined, not (isSystemName nm)]
      , null (findDupsEq (==) named_occs)
      = coalesce_lambdas combined inner
    coalesce_lambdas bs body = (bs, body)

    extract_invis_specs 0 _ = []
    extract_invis_specs n k
      | Just (_, Named (Bndr _ vis), rest) <- tcSplitPiTy_maybe k
      = let sp = case vis of { Inferred -> InferredSpec; _ -> SpecifiedSpec }
        in sp : extract_invis_specs (n - 1) rest
      | otherwise = replicate n SpecifiedSpec

    peel_unmatchable_arrows _ [] k = pure ([], k)
    peel_unmatchable_arrows hs_src (_ : bs) k = do
      k' <- liftZonkM (zonkTcType k)
      case tcSplitFunTy_maybe k' of
        Just (ma, arg, res) | isManyTy (scaledMult arg) -> do
          let orig = TypeEqOrigin { uo_actual = unmatchableDataConTy
                                  , uo_expected = ma
                                  , uo_thing = Just (HsTypeRnThing hs_src)
                                  , uo_invisible = Nothing }
          _ <- unifyTypeAndEmit KindLevel orig unmatchableDataConTy ma
          (ds, rk) <- peel_unmatchable_arrows hs_src bs res
          pure (scaledThing arg : ds, rk)
        _ -> do
          co <- matchExpectedFunKind (HsTypeRnThing hs_src)
                  (Just (pure unmatchableDataConTy)) (1 + length bs) k'
          k_exp <- liftZonkM (zonkTcType (pprPanic_or_rhs co k'))
          case tcSplitFunTy_maybe k_exp of
            Just (_, arg, res) -> do
              (ds, rk) <- peel_unmatchable_arrows hs_src bs res
              pure (scaledThing arg : ds, rk)
            Nothing -> failWithTc TcRnTypeLambdaContext

    pprPanic_or_rhs co orig_k = pFst (coercionKind co) `seq` pSnd (coercionKind co)
      where _ = orig_k

    direct_lambda_case bs body
      | L _ (XHsType (HsTypeCaseRn scrut rows)) <- ignoreParens body
      , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ s_nm))) <- ignoreParens scrut
      , Just col <- elemIndex s_nm (map unLoc bs)
      = Just (col, s_nm, rows)
      | otherwise = Nothing

    alloc_lambda_names in_kc site loc = do
      cur_mod <- tcg_mod <$> getGblEnv
      let mk_g occ = newGlobalBinder cur_mod (mkTcOcc occ) Nothing loc
          next_ord occ = if in_kc then pure 0 else allocSynthOrdinal occ
      case mb_pre_syn of
        Just (pre_syn, _) -> do
          let s_str = occNameString (nameOccName pre_syn)
          fn <- mk_g ("$lambda:" ++ s_str ++ ":0")
          _  <- next_ord (nameOccName pre_syn)
          pure (Just pre_syn, fn, fn, pre_syn)
        Nothing -> case site of
          SynthInLambda root_fam owner_nm -> do
            let o_occ = nameOccName owner_nm
            idx <- next_ord o_occ
            fn  <- mk_g ("$lambda:" ++ occNameString o_occ ++ ":" ++ show idx)
            pure (Nothing, fn, root_fam, owner_nm)
          SynthInSigma _ owner_nm -> do
            let o_occ = nameOccName owner_nm
            idx <- next_ord o_occ
            fn  <- mk_g ("$lambda:" ++ occNameString o_occ ++ ":" ++ show idx)
            pure (Nothing, fn, fn, owner_nm)
          SynthTopSig val_nm -> do
            let v_occ = nameOccName val_nm
            idx <- next_ord (mkTcOcc ("$signature-lambda:" ++ occNameString v_occ))
            let s_str = "$signature-lambda:" ++ occNameString v_occ ++ ":" ++ show idx
            sn <- mk_g s_str
            fn <- mk_g ("$lambda:" ++ s_str ++ ":0")
            _  <- next_ord (nameOccName sn)
            pure (Just sn, fn, fn, sn)
          SynthLocalSig label -> do
            idx <- next_ord (mkTcOcc ("$signature-lambda:" ++ label))
            let s_str = "$signature-lambda:" ++ label ++ ":local-" ++ show idx
            sn <- mk_g s_str
            fn <- mk_g ("$lambda:" ++ s_str ++ ":0")
            _  <- next_ord (nameOccName sn)
            pure (Just sn, fn, fn, sn)
          SynthConField con_nm _ -> do
            let c_occ = nameOccName con_nm
            idx <- next_ord (mkTcOcc ("$field-lambda:" ++ occNameString c_occ))
            let s_str = "$field-lambda:" ++ occNameString c_occ ++ ":" ++ show idx
            sn <- mk_g s_str
            fn <- mk_g ("$lambda:" ++ s_str ++ ":0")
            _  <- next_ord (nameOccName sn)
            pure (Just sn, fn, fn, sn)
          SynthTySyn syn_nm -> do
            let s_occ = nameOccName syn_nm
            idx <- next_ord s_occ
            fn  <- mk_g ("$lambda:" ++ occNameString s_occ ++ ":" ++ show idx)
            pure (Nothing, fn, fn, syn_nm)
          SynthClosedFam cf_nm -> do
            let c_occ = nameOccName cf_nm
            idx <- next_ord c_occ
            fn  <- mk_g ("$lambda:" ++ occNameString c_occ ++ ":" ++ show idx)
            pure (Nothing, fn, fn, cf_nm)
          SynthOpenFamInst of_nm -> do
            u_occ <- chooseUniqueOccTc (mkInstTyTcOcc ("$callback:" ++ occNameString (nameOccName of_nm) ++ ":"))
            fn    <- newGlobalBinder cur_mod u_occ Nothing loc
            pure (Nothing, fn, fn, fn)
          SynthExprT2T -> do
            uniq <- getUniqueM
            fn   <- mk_g ("$lambda$" ++ show uniq)
            pure (Nothing, fn, fn, fn)

    prepare_lambda_captures site mid_invis z_skols dom_kinds res_kind extra_tys = do
      gbl <- getGblEnv
      lcl <- getLclTypeEnv
      let local_skols = mkVarSet (map fst mid_invis ++ z_skols)
          raw_fvs0    = filterOut (`elemVarSet` local_skols) $
                          tyCoVarsOfTypesList (dom_kinds ++ [res_kind] ++ map (tyVarKind . fst) mid_invis ++ extra_tys)
          cap_ids     = [v | v <- raw_fvs0, isId v]
          allows_ids  = case site of
            SynthTopSig{}    -> True
            SynthConField{}  -> True
            SynthInLambda{}  -> True
            SynthInSigma{}   -> True
            SynthExprT2T     -> True
            SynthLocalSig "$type-argument" -> True
            _                -> False
      when (any isCoVar raw_fvs0
            || (not (null cap_ids) && not allows_ids)
            || any (\v -> isTyVar v && isForAllTy (tyVarKind v)) raw_fvs0) $
        failWithTc TcRnTypeLambdaContext
      case mb_pre_syn of
        Just (_, pre_cap_nms) -> do
          let vis_tvs0 = sort_bind [ tv | nm <- pre_cap_nms, Just (ATyVar _ tv) <- [lookupNameEnv lcl nm] ]
              vis_set  = mkVarSet vis_tvs0
              all_fvs  = scopedSort (close_k (dedup_v (map (\v -> v :: Var) vis_tvs0 ++ raw_fvs0)))
              invis_vs = [v | v <- all_fvs, not (v `elemVarSet` vis_set)]
              ordered  = invis_vs ++ map (\v -> v :: Var) vis_tvs0
          (subst_c, cap_pairs, _) <- abstractSynthCaptures ordered (map (\v -> v :: Var) (map fst mid_invis ++ z_skols))
          let (p_invis_ps, p_vis_ps) = splitAt (length invis_vs) cap_pairs
              p_invis = [(tv, InferredSpec) | (tv, _) <- p_invis_ps]
              p_vis   = map fst p_vis_ps
              cap_args = [if isTyVar ov then mkTyVarTy ov else TermVarTy ov | (_, ov) <- cap_pairs]
              id_env   = mkVarEnv [(ov, mkTyVarTy pv) | (pv, ov) <- cap_pairs, isId ov]
          pure (tcg_type_env gbl, subst_c, id_env, p_invis, p_vis, cap_args)
        Nothing -> do
          mb_hdr <- lookupDeclHeaderTyVars site
          (_, invis_specs, vis_dummy, cap_args0, _) <- partitionSynthCaptures False (mode_under_forall mode) mb_hdr raw_fvs0
          let ordered_orig = [ v | arg <- cap_args0
                             , Just v <- [case arg of { TyVarTy tv -> Just (tv :: Var); TermVarTy i -> Just (i :: Var); _ -> Nothing }] ]
              _ = vis_dummy
          (subst_c, cap_pairs, _) <- abstractSynthCaptures ordered_orig (map (\v -> v :: Var) (map fst mid_invis ++ z_skols))
          let (p_invis_ps, p_vis_ps) = splitAt (length invis_specs) cap_pairs
              p_invis = zip (map fst p_invis_ps) (map snd invis_specs)
              p_vis   = map fst p_vis_ps
              id_env  = mkVarEnv [(ov, mkTyVarTy pv) | (pv, ov) <- cap_pairs, isId ov]
          pure (tcg_type_env gbl, subst_c, id_env, p_invis, p_vis, cap_args0)
      where
        sort_bind = maybe [] (NE.toList . NE.sortBy cmp_b) . NE.nonEmpty
        cmp_b v1 v2 = case (srcSpanStart (nameSrcSpan (getName v1)), srcSpanStart (nameSrcSpan (getName v2))) of
          (RealSrcLoc l1 _, RealSrcLoc l2 _) -> compare l1 l2
          _ -> EQ
        dedup_v = go emptyVarSet
          where
            go _ [] = []
            go seen (v : vs)
              | v `elemVarSet` seen = go seen vs
              | otherwise           = v : go (extendVarSet seen v) vs
        close_k vs =
          let k_fvs = tyCoVarsOfTypesList (map varType vs)
              more  = filterOut (`elemVarSet` mkVarSet vs) k_fvs
          in if null more then vs else close_k (more ++ vs)

    alloc_skol_binders subst0 id_env rec_tenv skols =
      mapAccumLM (\s tv -> do
        nm <- newSysName (mkTyVarOccFS (occNameFS (nameOccName (getName tv))))
        let ki  = resolveRecTcTypes rec_tenv (replaceTypeTermIds id_env (Subst.substTy s (tyVarKind tv)))
            tv' = mkTyVar nm ki
        pure (Subst.extendTvSubstAndInScope s tv (mkTyVarTy tv'), tv')) subst0 skols

    build_beta_lambda site wrap_syn_mb fam_name root_fam lam_loc mid_invis z_skols dom_kinds res_kind body_ty proof_deps = do
      (rec_tenv, subst_c, id_env, p_invis0, p_vis0, cap_args0) <-
        prepare_lambda_captures site mid_invis z_skols dom_kinds res_kind [body_ty]
      let res_rec = resolveRecTcTypes rec_tenv
          p_invis = [(setTyVarKind tv (res_rec (tyVarKind tv)), sp) | (tv, sp) <- p_invis0]
          p_vis   = [setTyVarKind tv (res_rec (tyVarKind tv)) | tv <- p_vis0]
      (subst_m, mid_tvs) <- alloc_skol_binders subst_c id_env rec_tenv (map fst mid_invis)
      (subst_z, z_tvs)   <- alloc_skol_binders subst_m id_env rec_tenv
                              [setTyVarKind sk dk | (sk, dk) <- zip z_skols dom_kinds]
      let mid_out  = zip mid_tvs (map snd mid_invis)
          sub_raw  = replaceTypeTermIds id_env . Subst.substTy subst_z
          sub_rec  = res_rec . sub_raw
          res_k'   = sub_rec res_kind
          rhs_raw  = sub_raw body_ty
          rhs_ty'  = res_rec rhs_raw
          qtvs     = map fst p_invis ++ p_vis ++ mid_tvs ++ z_tvs
          tcbs     = mkDependentTyConBinders p_invis p_vis mid_out z_tvs res_k'
          leaked   = filterOut (`elemVarSet` mkVarSet qtvs) $
                       tyCoVarsOfTypesList (res_k' : rhs_ty' : map tyVarKind qtvs)
      unless (null leaked
              && tcIsLiftedTypeKind (typeKind res_k')
              && all (tcIsLiftedTypeKind . typeKind . tyVarKind) qtvs) $
        failWithTc TcRnTypeLambdaContext
      ax_name <- newImplicitBinder fam_name mkInstTyCoOcc
      let branch = mkCoAxBranch qtvs [] [] (mkTyVarTys qtvs) rhs_ty' (map (const Nominal) qtvs) lam_loc
          proof_rules = [(0, proof_deps) | not (null proof_deps)]
          fam_kind = mkTyConKindWithMatchability unmatchableDataConTy tcbs res_k'
          fam_tc = mkFamilyTyCon fam_name fam_kind tcbs 0 res_k' Nothing
                     (ClosedTypeFamilyTyCon (CTF_Scoped root_fam False axiom)) Nothing NotInjective
          axiom  = (mkBranchedCoAxiom Nominal ax_name fam_tc [branch])
                     { co_ax_source_proof_rules = proof_rules }
      case site of
        SynthExprT2T ->
          setXOptM LangExt.UndecidableInstances $
            checkSynthFamValidity fam_tc qtvs (mkTyVarTys qtvs) rhs_ty' lam_loc Nothing
        SynthLocalSig{} ->
          setXOptM LangExt.UndecidableInstances $
            checkSynthFamValidity fam_tc qtvs (mkTyVarTys qtvs) rhs_ty' lam_loc Nothing
        _ -> do
          undec <- xoptM LangExt.UndecidableInstances
          runOrDeferSynthCheck $
            (if undec then setXOptM LangExt.UndecidableInstances else id) $
              checkSynthFamValidity fam_tc qtvs (mkTyVarTys qtvs) rhs_ty' lam_loc Nothing
      _ <- registerSynthTyCon fam_tc
      finish_lambda_app site wrap_syn_mb fam_tc p_invis p_vis (map res_rec cap_args0)

    check_lambda_case_rows solve_row body_mode fam_name mid_skols0 z_skols0 dom_kinds0 scrut_col scrut_nm res_kind0 rows = do
      raw_fields <- xoptM LangExt.DependentTypes
      pat_mode   <- mkHoleMode TypeLevel (HM_FamPat raw_fields)
      let pat_mode' = pat_mode { mode_indices = mode_indices body_mode }
          init_in_scope = mkInScopeSet (tyCoVarsOfTypes (res_kind0 : dom_kinds0 ++ map tyVarKind (mid_skols0 ++ z_skols0)))
      raw_rows <- forM rows $ \(pat_nms, pat_hs, rhs_hs) -> do
        let row_loc = combineSrcSpans (getLocA pat_hs) (getLocA rhs_hs)
        setSrcSpan row_loc $ solve_row $ do
          (mid_subst, row_mid_tvs) <- mapAccumLM
            (\s tv -> do
              mtv <- newPatTyVar (getName tv) (Subst.substTy s (tyVarKind tv))
              pure (Subst.extendTvSubstAndInScope s tv (mkTyVarTy mtv), mtv))
            (Subst.mkEmptySubst init_in_scope)
            mid_skols0
          let row_z_skols = [ setTyVarKind sk (Subst.substTy mid_subst dk0)
                            | (sk, dk0) <- zip z_skols0 dom_kinds0 ]
              scrut_k   = Subst.substTy mid_subst (dom_kinds0 !! scrut_col)
              row_res_k = Subst.substTy mid_subst res_kind0
              row_m_env = zip (map getName mid_skols0) row_mid_tvs
              row_z_env = [(getName sk, rsk) | (sk, rsk) <- zip z_skols0 row_z_skols, getName sk /= scrut_nm]
          pat_tvs <- mapM (\nm -> newMetaKindVar >>= newPatTyVar nm) pat_nms
          tcExtendTyVarEnv row_mid_tvs $
            tcExtendNameTyVarEnv (row_m_env ++ row_z_env ++ zip pat_nms pat_tvs) $ do
              pat_ty0 <- tc_check_lhs_type pat_mode' pat_hs scrut_k
              let bind_alias = if scrut_nm `notElem` pat_nms
                    then updLclCtxt (tcExtendLocalTypeEnv [(scrut_nm, ATcTypeLet scrut_nm pat_ty0 scrut_k)])
                    else id
              ax_probe <- newImplicitBinder fam_name mkInstTyCoOcc
              rhs_ty0 <- withSourceProofSignature (Just ax_probe) $
                bind_alias $ tc_check_lhs_type body_mode rhs_hs row_res_k
              deps <- takeSourceProofRulesFor ax_probe
              pure (row_loc, pat_hs, pat_tvs, row_mid_tvs, row_z_skols, pat_ty0, rhs_ty0, deps)
      forM raw_rows $ \(row_loc, pat_hs, pat_tvs, row_mid_tvs, row_z_skols, pat_ty0, rhs_ty0, deps) -> do
        row_mid_tys <- mapM (liftZonkM . zonkTcType . mkTyVarTy) row_mid_tvs
        row_z_tys   <- mapM (liftZonkM . zonkTyCoVarKind) row_z_skols
        pat_ty      <- liftZonkM (zonkTcType pat_ty0)
        rhs_ty      <- liftZonkM (zonkTcType rhs_ty0)
        pat_tvs'    <- mapM (liftZonkM . zonkTyCoVarKind) pat_tvs
        pure (row_loc, pat_hs, pat_tvs', row_mid_tys, row_z_tys, pat_ty, rhs_ty, deps)

    build_case_lambda _ site wrap_syn_mb fam_name root_fam _ mid_invis z_skols dom_kinds res_kind scrut_col checked_rows = do
      let sig_fvs = tyCoVarsOfTypes (res_kind : dom_kinds ++ map (tyVarKind . fst) mid_invis)
          lhs_pat_tvs = mkVarSet
            [ tv | (_, _, ptvs, mtys, _, p, _, _) <- checked_rows
            , tv <- ptvs ++ [ v | v <- tyCoVarsOfTypesList (mtys ++ [p])
                                , isTcTyVar v, isMetaTyVar v, not (v `elemVarSet` sig_fvs) ] ]
          row_fvs = filterOut (`elemVarSet` lhs_pat_tvs) $
                      tyCoVarsOfTypesList [t | (_, _, _, mtys, _, p, r, _) <- checked_rows, t <- mtys ++ [p, r]]
          row_fv_tys = [if isTyVar v then mkTyVarTy v else TermVarTy v | v <- row_fvs]
      (rec_tenv, subst_c, id_env, p_invis0, p_vis0, cap_args0) <-
        prepare_lambda_captures site mid_invis z_skols dom_kinds res_kind row_fv_tys
      let res_rec = resolveRecTcTypes rec_tenv
          p_invis = [(setTyVarKind tv (res_rec (tyVarKind tv)), sp) | (tv, sp) <- p_invis0]
          p_vis   = [setTyVarKind tv (res_rec (tyVarKind tv)) | tv <- p_vis0]
      (subst_m, mid_tvs) <- alloc_skol_binders subst_c id_env rec_tenv (map fst mid_invis)
      (subst_z, z_tvs)   <- alloc_skol_binders subst_m id_env rec_tenv
                              [setTyVarKind sk dk | (sk, dk) <- zip z_skols dom_kinds]
      let mid_out   = zip mid_tvs (map snd mid_invis)
          sub_raw   = replaceTypeTermIds id_env . Subst.substTy subst_z
          sub_rec   = res_rec . sub_raw
          res_k'    = sub_rec res_kind
          tcbs      = mkDependentTyConBinders p_invis p_vis mid_out z_tvs res_k'
          fam_kind  = mkTyConKindWithMatchability unmatchableDataConTy tcbs res_k'
          abs_col   = length p_invis + length p_vis + length mid_tvs + scrut_col
          cap_vs    = map fst p_invis ++ p_vis
          all_tcbs  = cap_vs ++ mid_tvs ++ z_tvs
          pre_z_vs  = take scrut_col z_tvs
          suf_z_vs  = drop (scrut_col + 1) z_tvs
          orig_bound_set = mkVarSet ([tv | TyVarTy tv <- cap_args0] ++ map fst mid_invis ++ z_skols)
      unless (tcIsLiftedTypeKind (typeKind res_k')
              && all (tcIsLiftedTypeKind . typeKind . tyVarKind) all_tcbs) $
        failWithTc TcRnTypeLambdaContext
      built_rows <- forM checked_rows $ \(rloc, pat_hs, _, row_mid_tys0, row_z_tys0, pat_ty0, rhs_ty0, deps) -> do
        let pat_fvs = scopedSort $
                        filterOut (\v -> isId v || v `elemVarSet` orig_bound_set) $
                          tyCoVarsOfTypesList (row_mid_tys0 ++ [pat_ty0, rhs_ty0])
        (subst_p, _) <- alloc_skol_binders subst_z id_env rec_tenv pat_fvs
        let sub_row  = res_rec . replaceTypeTermIds id_env . Subst.substTy subst_p
            mid_tys' = map sub_row row_mid_tys0
            pat_ty'  = sub_row pat_ty0
            rhs_ty'  = sub_row rhs_ty0
            pre_tys  = [ mkTyVarTy (setTyVarKind tv (sub_row (tyVarKind sk)))
                       | (tv, sk) <- zip pre_z_vs (take scrut_col row_z_tys0) ]
            suf_tys  = [ mkTyVarTy (setTyVarKind tv (sub_row (tyVarKind sk)))
                       | (tv, sk) <- zip suf_z_vs (drop (scrut_col + 1) row_z_tys0) ]
            lhs_tys  = mkTyVarTys cap_vs ++ mid_tys' ++ pre_tys ++ [pat_ty'] ++ suf_tys
            qtvs     = scopedSort (tyCoVarsOfTypesList (rhs_ty' : lhs_tys))
        unless (all (\v -> isTyVar v && tcIsLiftedTypeKind (typeKind (tyVarKind v))) qtvs) $
          failWithTc TcRnTypeCaseContext
        let vi       = VI { vi_loc = rloc
                          , vi_qtvs = qtvs
                          , vi_non_user_tvs = emptyVarSet
                          , vi_pats = lhs_tys
                          , vi_case_pattern = Just (fam_name, abs_col, sourceCasePatternRn pat_hs, pat_ty')
                          , vi_rhs = rhs_ty' }
        pure (mkCoAxBranch qtvs [] [] lhs_tys rhs_ty' (map (const Nominal) qtvs) rloc, vi, deps)
      fam_tc <- finalizeCaseFamily fam_name fam_kind tcbs res_k' (Just (root_fam, False)) built_rows
      _ <- registerSynthTyCon fam_tc
      finish_lambda_app site wrap_syn_mb fam_tc p_invis p_vis (map res_rec cap_args0)

    finish_lambda_app site wrap_syn_mb fam_tc p_invis p_vis cap_args = do
      let app_ty = mkTyConApp fam_tc cap_args
          has_term_cap = any (\case TermVarTy{} -> True; _ -> False) cap_args
          eff_wrap_syn = case (site, wrap_syn_mb) of
            (SynthLocalSig "$type-argument", _) | has_term_cap -> Nothing
            _ -> wrap_syn_mb
      res_ty <- case eff_wrap_syn of
        Just syn_name -> do
          let syn_res_k = typeKind (mkTyConApp fam_tc (mkTyVarTys (map fst p_invis ++ p_vis)))
              syn_tcbs  = mkDependentTyConBinders p_invis p_vis [] [] syn_res_k
              syn_kind  = mkTyConKindWithMatchability unmatchableDataConTy syn_tcbs syn_res_k
              syn_rhs   = mkTyConApp fam_tc (mkTyVarTys (map fst p_invis ++ p_vis))
              syn_tc    = buildSynTyCon syn_name syn_kind syn_tcbs syn_res_k
                            (map (const Nominal) syn_tcbs) syn_rhs
          _ <- registerSynthTyCon syn_tc
          pure (mkTyConApp syn_tc cap_args)
        Nothing -> pure app_ty
      checkExpKind mode src res_ty (typeKind res_ty) exp_kind

-- | Lower a post-renaming type case ('HsTypeCaseRn') outside closed type
-- family equations into a synthesized @$case:@ closed type family.
-- See Note [Post-renaming lowering of lambdas, sums, and cases].
tcHsTypeCase
  :: TcTyMode -> HsType GhcRn -> LHsType GhcRn
  -> [([Name], LHsType GhcRn, LHsType GhcRn)]
  -> ExpKind -> TcM TcType
tcHsTypeCase mode src scrut_hs rows exp_kind = do
  enabled <- xoptM LangExt.DependentTypes
  unless enabled $ failWithTc TcRnTypeCaseDisabled
  level <- getThLevel
  case level of
    Comp -> pure ()
    _    -> failWithTc TcRnTypeCaseContext
  unless (isTypeLevel (mode_tyki mode)) $ failWithTc TcRnTypeCaseContext
  (site, in_kc) <- classifySynthSite mode
  unless (allowed_case_site site) $ failWithTc TcRnTypeCaseContext
  when (is_retained_con_field site) $
    unless (valid_retained_field_scrut scrut_hs) $ failWithTc TcRnTypeCaseContext
  (let_fam_pairs, extend_let_fams) <- if in_kc then pure ([], id) else abstract_let_fams
  let let_fam_env = mkVarEnv let_fam_pairs
  extend_let_fams $ do
    (scrut_ty0, scrut_k0) <- tc_infer_lhs_type mode scrut_hs
    scrut_ty <- liftZonkM (zonkTcType scrut_ty0)
    scrut_k  <- liftZonkM (zonkTcType scrut_k0)
    -- Intra-signature sharing for top-level retained signatures:
    mb_shared <- if isEmptyVarEnv let_fam_env then lookup_shared_sig_case site scrut_ty rows else pure Nothing
    case mb_shared of
      Just (shared_syn, shared_args) ->
        let app = mkTyConApp shared_syn shared_args
        in checkExpKind mode src app (typeKind app) exp_kind
      Nothing -> do
        case_loc <- getSrcSpanM
        (wrap_syn_mb, owner_for_nodes, fam_name, is_scoped_sig) <- alloc_case_names in_kc site case_loc
        res_k0 <- expTypeToType exp_kind >>= liftZonkM . zonkTcType
        norm_loc <- getCtLocM AnnOrigin Nothing
        Reduction _ scrut_k_norm <- tcRewriteFamilyAxioms norm_loc scrut_k
        (pat_scrut_k0, fam_abs_pairs0) <- abstract_scrut_fam_apps fam_name scrut_k_norm
        raw_fields <- xoptM LangExt.DependentTypes
        pat_mode   <- mkHoleMode TypeLevel (HM_FamPat raw_fields)
        let pat_mode' = pat_mode { mode_indices = mode_indices mode }
            solve_if_tc = if in_kc then id else solveEqualities "tcHsTypeCase"
            bare_scrut_nm = case unLoc (ignoreParens scrut_hs) of
              HsTyVar _ NotPromoted (L _ (WithUserRdr _ n)) -> Just n
              _ -> Nothing
        raw_rows <- solve_if_tc $ addErrCtxt (TyConDeclCtxt fam_name ClosedTypeFamilyFlavour) $
          forM rows $ \(pat_nms, pat_hs, rhs_hs) -> do
            let rloc
                  | isGoodSrcSpan (getLocA pat_hs), isGoodSrcSpan (getLocA rhs_hs)
                  = combineSrcSpans (getLocA pat_hs) (getLocA rhs_hs)
                  | isGoodSrcSpan (getLocA pat_hs) = getLocA pat_hs
                  | otherwise                      = case_loc
            setSrcSpan rloc $ do
              pat_tvs <- mapM (\nm -> newMetaKindVar >>= newPatTyVar nm) pat_nms
              tcExtendNameTyVarEnv (zip pat_nms pat_tvs) $ do
                pat_ty0 <- tc_check_lhs_type pat_mode' pat_hs pat_scrut_k0
                let bind_alias = case bare_scrut_nm of
                      Just sn | sn `notElem` pat_nms ->
                        updLclCtxt (tcExtendLocalTypeEnv [(sn, ATcTypeLet sn pat_ty0 pat_scrut_k0)])
                      _ -> id
                ax_probe <- newImplicitBinder fam_name mkInstTyCoOcc
                rhs_ty0  <- withSourceProofSignature (Just ax_probe) $
                              bind_alias $ tc_check_lhs_type mode rhs_hs res_k0
                deps     <- takeSourceProofRulesFor ax_probe
                pure (rloc, pat_hs, pat_tvs, pat_ty0, rhs_ty0, deps)
        checked_rows <- forM raw_rows $ \(rloc, pat_hs, pat_tvs, pat_ty0, rhs_ty0, deps) -> do
          pat_ty   <- liftZonkM (zonkTcType pat_ty0)
          rhs_ty   <- liftZonkM (zonkTcType rhs_ty0)
          pat_tvs' <- mapM (liftZonkM . zonkTyCoVarKind) pat_tvs
          pure (rloc, pat_hs, pat_tvs', pat_ty, rhs_ty, deps)
        res_k       <- liftZonkM (zonkTcType res_k0)
        scrut_ty'   <- liftZonkM (zonkTcType scrut_ty)
        pat_scrut_k <- liftZonkM (zonkTcType pat_scrut_k0)
        fam_abs_pairs <- forM fam_abs_pairs0 $ \(atv, fapp) -> do
          atv'  <- liftZonkM (zonkTyCoVarKind atv)
          fapp' <- liftZonkM (zonkTcType fapp)
          pure (atv', fapp')
        if in_kc
          then checkExpKind mode src (mkTyConApp anyTyCon [res_k]) res_k exp_kind
          else do
            gbl <- getGblEnv
            let rec_tenv   = tcg_type_env gbl
                res_rec    = resolveRecTcTypes rec_tenv
                row_tys    = concat [[p, r] | (_, _, _, p, r, _) <- checked_rows]
                abs_tv_set = mkVarSet (map fst fam_abs_pairs)
                sig_fvs    = tyCoVarsOfTypes (scrut_ty' : pat_scrut_k : res_k : map snd fam_abs_pairs)
                -- Free variables of the case (excluding branch-local pat_tvs and abstracted kind family skolems):
                pat_tv_set = mkVarSet [ tv | (_, _, ptvs, p, _, _) <- checked_rows
                                      , tv <- ptvs ++ [ v | v <- tyCoVarsOfTypesList [p]
                                                          , isTcTyVar v, isMetaTyVar v, not (v `elemVarSet` sig_fvs) ] ]
                raw_fvs = filterOut (\v -> v `elemVarSet` pat_tv_set || v `elemVarSet` abs_tv_set) $
                            tyCoVarsOfTypesList (pat_scrut_k : res_k : map snd fam_abs_pairs ++ row_tys)
                disallows_ids = case site of { SynthTySyn{} -> True; _ -> False }
            when (any isCoVar raw_fvs || (disallows_ids && any isId raw_fvs)) $
              failWithTc TcRnTypeCaseContext
            mb_hdr <- lookupDeclHeaderTyVars site
            let mb_hdr' = case mb_hdr of
                  Just (invis_h, vis_h) | not (isEmptyVarEnv let_fam_env) ->
                    let let_tvs = [tv | tv <- raw_fvs, isTyVar tv, tv `elemVarEnv` let_fam_env]
                    in Just (invis_h, vis_h ++ let_tvs)
                  _ -> mb_hdr
            (_, invis_specs, _, cap_args0, _) <- partitionSynthCaptures True (mode_under_forall mode) mb_hdr' raw_fvs
            let ordered_orig = [ v | arg <- cap_args0
                               , Just v <- [case arg of { TyVarTy tv -> Just (tv :: Var); TermVarTy i -> Just (i :: Var); _ -> Nothing }] ]
            when (mode_under_forall mode) $
              check_shadowed_field_kinds site wrap_syn_mb ordered_orig
            (subst_c0, cap_pairs, _) <- abstractSynthCaptures ordered_orig (map fst fam_abs_pairs)
            let id_env  = mkVarEnv [(ov, mkTyVarTy pv) | (pv, ov) <- cap_pairs, isId ov]
            (subst_c, abs_tvs) <- mapAccumLM (\s (atv, _) -> do
              nm <- newSysName (mkTyVarOcc "$fam_idx")
              let ki  = res_rec (replaceTypeTermIds id_env (Subst.substTy s (tyVarKind atv)))
                  tv' = mkTyVar nm ki
              pure (Subst.extendTvSubstAndInScope s atv (mkTyVarTy tv'), tv')) subst_c0 fam_abs_pairs
            let (p_invis_ps, p_vis_ps) = splitAt (length invis_specs) cap_pairs
                p_invis   = [(setTyVarKind tv (res_rec (tyVarKind tv)), sp) | ((tv, _), (_, sp)) <- zip p_invis_ps invis_specs]
                fam_invis = p_invis ++ [(tv, InferredSpec) | tv <- abs_tvs]
                p_vis     = [setTyVarKind tv (res_rec (tyVarKind tv)) | (tv, _) <- p_vis_ps]
                sub_raw   = replaceTypeTermIds id_env . Subst.substTy subst_c
                sub_rec   = res_rec . sub_raw
                scrut_k_sub = sub_rec pat_scrut_k
                res_k_sub   = sub_rec res_k
            scrut_nm <- newSysName (mkTyVarOcc "$scrutinee")
            let scrut_tv = mkTyVar scrut_nm scrut_k_sub
                tcbs     = mkDependentTyConBinders fam_invis (p_vis ++ [scrut_tv]) [] [] res_k_sub
                fam_kind = mkTyConKindWithMatchability unmatchableDataConTy tcbs res_k_sub
                abs_col  = length fam_invis + length p_vis
                cap_tvs        = map fst fam_invis ++ p_vis
                orig_bound_set = mkVarSet ([tv | TyVarTy tv <- cap_args0] ++ map fst fam_abs_pairs)
            unless (tcIsLiftedTypeKind (typeKind res_k_sub)
                    && all (tcIsLiftedTypeKind . typeKind . tyVarKind) (cap_tvs ++ [scrut_tv])) $
              failWithTc TcRnTypeCaseContext
            built_rows <- forM checked_rows $ \(rloc, pat_hs, pat_tvs', pat_ty0, rhs_ty0, deps) -> do
              let user_pat_set = mkVarSet pat_tvs'
                  pat_vs = scopedSort $
                             filterOut (\v -> isId v || v `elemVarSet` orig_bound_set) $
                               tyCoVarsOfTypesList [pat_ty0, rhs_ty0]
              (subst_p, pat_pairs) <- mapAccumLM (\s tv -> do
                let occ = mkTyVarOccFS (occNameFS (nameOccName (getName tv)))
                nm <- (\u -> mkInternalName u occ rloc) <$> newUnique
                let ki  = res_rec (replaceTypeTermIds id_env (Subst.substTy s (tyVarKind tv)))
                    tv' = mkTyVar nm ki
                pure (Subst.extendTvSubstAndInScope s tv (mkTyVarTy tv'), (tv, tv'))) subst_c pat_vs
              let sub_row = res_rec . replaceTypeTermIds id_env . Subst.substTy subst_p
                  pat_ty' = sub_row pat_ty0
                  rhs_ty' = sub_row rhs_ty0
                  lhs_tys = mkTyVarTys cap_tvs ++ [pat_ty']
                  qtvs    = scopedSort (tyCoVarsOfTypesList (rhs_ty' : lhs_tys))
                  non_usr = mkVarSet [tv' | (tv, tv') <- pat_pairs, not (tv `elemVarSet` user_pat_set)]
              unless (all (\v -> isTyVar v && tcIsLiftedTypeKind (typeKind (tyVarKind v))) qtvs) $
                failWithTc TcRnTypeCaseContext
              let vi      = VI { vi_loc = rloc
                               , vi_qtvs = qtvs
                               , vi_non_user_tvs = non_usr
                               , vi_pats = lhs_tys
                               , vi_case_pattern = Just (owner_for_nodes, abs_col, sourceCasePatternRn pat_hs, pat_ty')
                               , vi_rhs = rhs_ty' }
              pure (mkCoAxBranch qtvs [] [] lhs_tys rhs_ty' (map (const Nominal) qtvs) rloc, vi, deps)
            let mb_scope
                  | null built_rows = Nothing
                  | is_scoped_sig   = Just (owner_for_nodes, False)
                  | SynthInSigma root_fam _ <- site  = Just (root_fam, False)
                  | SynthInLambda root_fam _ <- site = Just (root_fam, False)
                  | otherwise = Nothing
            fam_tc <- finalizeCaseFamily fam_name fam_kind tcbs res_k_sub mb_scope built_rows
            _ <- registerSynthTyCon fam_tc
            let exp_let_ty
                  | null let_fam_pairs = id
                  | otherwise = Subst.substTyWith (map fst let_fam_pairs) (map snd let_fam_pairs)
                cap_args_resolved = [res_rec (exp_let_ty a) | a <- cap_args0]
                (invis_cap_args, vis_cap_args) = splitAt (length p_invis) cap_args_resolved
                abs_call_args = [ res_rec (Subst.substTyWith (map fst let_fam_pairs) (map snd let_fam_pairs) fapp)
                                | (_, fapp) <- fam_abs_pairs ]
                scrut_arg = res_rec (exp_let_ty scrut_ty')
                fam_app   = mkTyConApp fam_tc (invis_cap_args ++ abs_call_args ++ vis_cap_args ++ [scrut_arg])
            res_ty <- case wrap_syn_mb of
              Just syn_name -> do
                let syn_abs_args = [sub_rec fapp | (_, fapp) <- fam_abs_pairs]
                    syn_tcbs = mkDependentTyConBinders p_invis (p_vis ++ [scrut_tv]) [] [] res_k_sub
                    syn_kind = mkTyConKindWithMatchability unmatchableDataConTy syn_tcbs res_k_sub
                    syn_rhs  = mkTyConApp fam_tc (mkTyVarTys (map fst p_invis) ++ syn_abs_args ++ mkTyVarTys (p_vis ++ [scrut_tv]))
                    syn_tc   = buildSynTyCon syn_name syn_kind syn_tcbs res_k_sub
                                 (map (const Nominal) syn_tcbs) syn_rhs
                _ <- registerSynthTyCon syn_tc
                when (null let_fam_pairs) $
                  record_shared_sig_case site scrut_ty rows syn_tc (cap_args_resolved ++ [scrut_arg])
                pure (mkTyConApp syn_tc (cap_args_resolved ++ [scrut_arg]))
              Nothing -> pure fam_app
            checkExpKind mode src res_ty res_k exp_kind
  where
    abstract_let_fams = do
      lcl <- getLclTypeEnv
      let let_entries =
            [ (nm, let_ty, let_ki)
            | ATcTypeLet nm let_ty let_ki <- nonDetNameEnvElts lcl
            , not (isTyFamFree (expandTypeSynonyms let_ty)) ]
      if null let_entries
        then pure ([], id)
        else do
          skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn ([] :: [HsTyVarBndr () GhcRn])))
          triples <- forM let_entries $ \(nm, let_ty, let_ki) -> do
            ki' <- liftZonkM (zonkTcType let_ki)
            ty' <- liftZonkM (zonkTcType let_ty)
            tv  <- newSkolemTyVar skol_info nm ki'
            pure (nm, tv, ty')
          let pairs     = [(tv, ty') | (_, tv, ty') <- triples]
              ext_binds = [(nm, ATyVar nm tv) | (nm, tv, _) <- triples]
          pure (pairs, updLclCtxt (tcExtendLocalTypeEnv ext_binds))
    abstract_scrut_fam_apps owner_nm ki0
      | isTyFamFree ki0 = pure (ki0, [])
      | otherwise = do
          skol_info <- mkSkolemInfo (TyConSkol ClosedTypeFamilyFlavour owner_nm)
          let go ty
                | Just ty' <- coreView ty = go ty'
              go fam_ty@(TyConApp tc args)
                | isTypeFamilyTyCon tc = do
                    nm <- newSysName (mkTyVarOcc "$fam_idx")
                    tv <- newSkolemTyVar skol_info nm (typeKind fam_ty)
                    pure (mkTyVarTy tv, [(tv, fam_ty)])
                | otherwise = do
                    (args', pairs) <- mapAndUnzipM go args
                    pure (TyConApp tc args', concat pairs)
              go (AppTy f a) = do
                (f', p1) <- go f
                (a', p2) <- go a
                pure (AppTy f' a', p1 ++ p2)
              go ty = pure (ty, [])
          go ki0
    allowed_case_site SynthTopSig{}   = True
    allowed_case_site SynthConField{} = True
    allowed_case_site SynthTySyn{}    = True
    allowed_case_site SynthInLambda{} = True
    allowed_case_site SynthInSigma{}  = True
    allowed_case_site _               = False

    is_retained_con_field SynthConField{} = not (isEmptyNameEnv (mode_indices mode))
    is_retained_con_field _               = False

    valid_retained_field_scrut hs =
      let fvs = free_names hs
      in any (`elemNameEnv` mode_indices mode) fvs
         || all isExternalName fvs
      where
        free_names :: Data a => a -> [Name]
        free_names thing
          | isAstLeafAnnotation thing = case cast thing :: Maybe Name of
              Just n  -> [n]
              Nothing -> []
          | Just (L _ (HsSpliceTy (HsUntypedSpliceTop _ inner) _)) <- (cast thing :: Maybe (LHsType GhcRn))
          = free_names inner
          | Just (L _ HsSpliceTy{}) <- (cast thing :: Maybe (LHsType GhcRn)) = []
          | otherwise = concat (gmapQ free_names thing)

    check_shadowed_field_kinds (SynthConField con_nm _) (Just syn_name) ordered_orig = do
      lcl <- getLclTypeEnv
      let lcl_tvs = [(nm, tv) | ATyVar nm tv <- nonDetNameEnvElts lcl]
          is_shadowed v = isTyVar v && any (\(nm, tv') -> tv' /= v && nameOccName nm == nameOccName (getName v)) lcl_tvs
          shadowed = filter is_shadowed ordered_orig
      unless (null shadowed) $ addErrCtxt (TyConDeclCtxt syn_name TypeSynonymFlavour) $ do
        cur_mod <- tcg_mod <$> getGblEnv
        let mk_fk tag i v = do
              let occ = mkTyVarOcc ("$field-kind:" ++ occNameString (nameOccName con_nm) ++ ":" ++ tag ++ show (i :: Int))
              fnm <- newGlobalBinder cur_mod occ Nothing (getSrcSpan con_nm)
              det <- newMetaDetails TauTv
              pure (v, mkTyVarTy (mkTcTyVar fnm (varType v) det))
        prs1 <- zipWithM (mk_fk "a") [0..] shadowed
        prs2 <- zipWithM (mk_fk "b") [0..] shadowed
        let s1 = zipTvSubst [tv | (tv, _) <- prs1] (map snd prs1)
            s2 = zipTvSubst [tv | (tv, _) <- prs2] (map snd prs2)
        forM_ [v | v <- ordered_orig, not (v `elem` shadowed)] $ \v -> do
          let k0 = varType v
              k1 = Subst.substTy s1 k0
              k2 = Subst.substTy s2 k0
          when (not (k1 `tcEqType` k0)) $ do
            let orig = TypeEqOrigin
                  { uo_actual    = k1
                  , uo_expected  = k2
                  , uo_thing     = Just (HsTypeRnThing (HsTyVar noAnn NotPromoted (noLocA (noUserRdr (getName v)))))
                  , uo_invisible = Nothing }
            _ <- unifyTypeAndEmit KindLevel orig k1 k2
            _ <- unifyType Nothing k1 k0
            pure ()
    check_shadowed_field_kinds _ _ _ = pure ()

    lookup_shared_sig_case (SynthTopSig val_nm) scrut_ty rws = do
      st <- getSynthTypeState
      lcl <- getLclTypeEnv
      let has_type_let = any (\case { ATcTypeLet{} -> True; _ -> False }) (nonDetNameEnvElts lcl)
          active_tvs = scopedSort [tv | ATyVar _ tv <- nonDetNameEnvElts lcl]
          entries    = fromMaybe [] (lookupNameEnv (sts_sig_cases st) val_nm)
          same_rows r1 r2 = length r1 == length r2 && and (zipWith same_row r1 r2)
          same_row (ns1, p1, b1) (ns2, p2, b2) =
            length ns1 == length ns2 && showSDocUnsafe (ppr p1) == showSDocUnsafe (ppr p2)
            && showSDocUnsafe (ppr b1) == showSDocUnsafe (ppr b2)
      case [ (syn_tc, scrut_ty)
           | not has_type_let
           , (uf, tvs, s_ty, old_rws, syn_tc) <- entries
           , uf == mode_under_forall mode
           , tvs == active_tvs
           , s_ty `tcEqType` scrut_ty
           , same_rows old_rws rws
           , tyConArity syn_tc == 1 ] of
        (syn_tc, s_arg) : _ -> pure (Just (syn_tc, [s_arg]))
        [] -> pure Nothing
    lookup_shared_sig_case _ _ _ = pure Nothing

    record_shared_sig_case (SynthTopSig val_nm) scrut_ty rws syn_tc [_] = do
      cur_mod <- tcg_mod <$> getGblEnv
      lcl <- getLclTypeEnv
      let has_type_let = any (\case { ATcTypeLet{} -> True; _ -> False }) (nonDetNameEnvElts lcl)
          active_tvs = scopedSort [tv | ATyVar _ tv <- nonDetNameEnvElts lcl]
          entry = (mode_under_forall mode, active_tvs, scrut_ty, rws, syn_tc)
      unless has_type_let $
        modifySynthTypeState cur_mod
          (\st ->
             let prev = fromMaybe [] (lookupNameEnv (sts_sig_cases st) val_nm)
             in (st { sts_sig_cases = extendNameEnv (sts_sig_cases st) val_nm (entry : prev) }, ()))
          ()
    record_shared_sig_case _ _ _ _ _ = pure ()

    alloc_case_names in_kc site loc = do
      cur_mod <- tcg_mod <$> getGblEnv
      let mk_g occ = newGlobalBinder cur_mod (mkTcOcc occ) Nothing loc
          next_ord occ = if in_kc then pure 0 else allocSynthOrdinal occ
      case site of
        SynthTopSig val_nm -> do
          let v_occ = nameOccName val_nm
          idx <- next_ord (mkTcOcc ("$signature-case:" ++ occNameString v_occ))
          let s_str = "$signature-case:" ++ occNameString v_occ ++ ":" ++ show idx
          sn <- mk_g s_str
          fn <- mk_g ("$case:" ++ s_str ++ ":0")
          pure (Just sn, sn, fn, True)
        SynthConField con_nm _ -> do
          let c_occ = nameOccName con_nm
          idx <- next_ord (mkTcOcc ("$field-case:" ++ occNameString c_occ))
          let s_str = "$field-case:" ++ occNameString c_occ ++ ":" ++ show idx
          sn <- mk_g s_str
          fn <- mk_g ("$case:" ++ s_str ++ ":0")
          pure (Just sn, sn, fn, False)
        SynthTySyn syn_nm -> do
          let s_occ = nameOccName syn_nm
              sfx   = if mode_under_forall mode then "-forall" else ""
          idx <- next_ord (mkTcOcc ("$case:" ++ occNameString s_occ ++ sfx))
          fn  <- mk_g ("$case:" ++ occNameString s_occ ++ ":" ++ show idx ++ sfx)
          pure (Nothing, syn_nm, fn, False)
        SynthInLambda _ owner_nm -> do
          let o_occ = nameOccName owner_nm
          idx <- next_ord (mkTcOcc ("$case:" ++ occNameString o_occ))
          fn  <- mk_g ("$case:" ++ occNameString o_occ ++ ":" ++ show idx)
          pure (Nothing, owner_nm, fn, False)
        SynthInSigma _ owner_nm -> do
          let o_occ = nameOccName owner_nm
          idx <- next_ord (mkTcOcc ("$case:" ++ occNameString o_occ))
          fn  <- mk_g ("$case:" ++ occNameString o_occ ++ ":" ++ show idx)
          pure (Nothing, owner_nm, fn, False)
        _ -> failWithTc TcRnTypeCaseContext

sourceCasePatternRn :: LHsType GhcRn -> TypeCasePattern
sourceCasePatternRn original = go [] original
  where
    sp = getLocA original
    go args (L _ (HsParTy _ pat))      = go args pat
    go args (L _ (HsAppTy _ con fld))  = go (fld : args) con
    go args (L _ (HsAppKindTy _ con _)) = go args con
    go []   (L _ (HsOpTy _ lhs tyop rhs)) = go [lhs, rhs] tyop
    go []   (L _ (HsExplicitListTy _ _ tys)) =
      foldr (\hd acc -> CasePatternConstructor sp (dataConName consDataCon) [sourceCasePatternRn hd, acc])
            (CasePatternConstructor sp (dataConName nilDataCon) [])
            tys
    go []   (L _ (HsExplicitTupleTy _ _ tys))
      | length tys /= 1 =
          CasePatternConstructor sp (dataConName (tupleDataCon Boxed (length tys))) (map sourceCasePatternRn tys)
    go args (L _ (HsTyVar _ _ (L _ (WithUserRdr _ nm))))
      | not (isTvOcc (nameOccName nm)) =
          CasePatternConstructor sp nm (map sourceCasePatternRn args)
    go _ _ = CasePatternField

data CheckedSynthPattern
  = SynthCaseField
  | SynthCaseConstructor [Int] SrcSpan TyCon [CheckedSynthPattern]

finalizeCaseFamily
  :: Name -> Kind -> [TyConBinder] -> Kind -> Maybe (Name, Bool)
  -> [(CoAxBranch, TyFamEqnValidityInfo, [SourceProofRule])]
  -> TcM TyCon
finalizeCaseFamily fam_name fam_kind tcbs res_kind mb_scope built_rows = do
  ax_name <- newImplicitBinder fam_name mkInstTyCoOcc
  let branches   = [b | (b, _, _) <- built_rows]
      infos      = [v | (_, v, _) <- built_rows]
      proof_rules = [(i, ds) | (i, (_, _, ds)) <- zip [0..] built_rows, not (null ds)]
      mk_ctf Nothing            Nothing   = CTF Nothing
      mk_ctf Nothing            (Just ax) = CTF (Just ax)
      mk_ctf (Just (own, is_s)) (Just ax) = CTF_Scoped own is_s ax
      mk_ctf (Just _)           Nothing   = CTF Nothing
      raw_ax     = (mkBranchedCoAxiom Nominal ax_name fam_tc branches)
                     { co_ax_source_proof_rules = proof_rules }
      raw_mb_ax  = if null branches then Nothing else Just raw_ax
      fam_tc     = mkFamilyTyCon fam_name fam_kind tcbs 0 res_kind Nothing
                     (ClosedTypeFamilyTyCon (mk_ctf mb_scope raw_mb_ax)) Nothing NotInjective
  ordered <- synthNeedsOrderedCase fam_tc infos
  let flav = TyConInstFlavour ClosedTypeFamilyFlavour False
  if not ordered
    then do
      runOrDeferSynthCheck $
        setErrCtxt [] $
          addErrCtxt (TyConInstCtxt fam_name flav) $
            addErrCtxt (ClosedFamEqnCtxt fam_tc) $ do
              mapM_ (checkTyFamEqnValidityInfo fam_tc) infos
              forM_ raw_mb_ax (checkValidCoAxiom emptyNameEnv)
      pure fam_tc
    else do
      in_decl <- isInDeclKnotM
      (ord_ax0, node_tcs) <- compileOrderedSynthCase fam_tc infos
      let ord_ax = ord_ax0 { co_ax_source_proof_rules = proof_rules }
          ord_tc = mkFamilyTyCon fam_name fam_kind tcbs 0 res_kind Nothing
                     (ClosedTypeFamilyTyCon (mk_ctf mb_scope (Just ord_ax))) Nothing NotInjective
          run_ord_val =
            setErrCtxt [] $
              addErrCtxt (TyConInstCtxt fam_name flav) $
                addErrCtxt (ClosedFamEqnCtxt ord_tc) $ do
                  mapM_ (checkTyFamEqnValidityInfo fam_tc) infos
                  setXOptM LangExt.UndecidableInstances $ do
                    checkValidCoAxiom emptyNameEnv ord_ax
                    forM_ node_tcs $ \ntc ->
                      forM_ (closedFamilyTyConCoAxiom_maybe ntc) (checkValidCoAxiom emptyNameEnv)
      if in_decl then deferSynthCheck run_ord_val else checkNoErrs run_ord_val
      mapM_ registerSynthTyCon node_tcs
      pure ord_tc

synthNeedsOrderedCase :: TyCon -> [TyFamEqnValidityInfo] -> TcM Bool
synthNeedsOrderedCase root infos
  | any has_ord infos = pure True
  | _ : _ : _ <- infos = or <$> mapM has_expanded infos
  | otherwise = pure False
  where
    has_ord VI { vi_case_pattern = Just (_, _, CasePatternField, _) } = True
    has_ord VI { vi_case_pattern = Just (_, _, pat, _) } = nestedTypeCasePattern pat
    has_ord _ = False

    has_expanded vi@VI { vi_case_pattern = Just (_, _, pat, ty) }
      | may_expand ty = do
          in_decl <- isInDeclKnotM
          unless in_decl $ checkNoErrs (checkTyFamEqnValidityInfo root vi)
          checked <- synthCheckedPattern [] pat ty
          pure $ case checked of
            SynthCaseConstructor _ _ _ fields -> any is_ctor fields
            SynthCaseField -> False
    has_expanded _ = pure False

    may_expand (CastTy ty _) = may_expand ty
    may_expand ty = maybe False (isTypeSynonymTyCon . fst) (splitTyConAppNoView_maybe ty)

    is_ctor SynthCaseConstructor{} = True
    is_ctor SynthCaseField         = False

compileOrderedSynthCase :: TyCon -> [TyFamEqnValidityInfo] -> TcM (CoAxiom Branched, [TyCon])
compileOrderedSynthCase root infos = do
  let vars = binderVars (tyConBinders root)
      args = mkTyVarTys vars
      res  = tyConResKind root
      loc  = case infos of { VI { vi_loc = sp } : _ -> sp; _ -> getSrcSpan root }
      unsupported :: TcM a
      unsupported = setSrcSpan loc $
        failWithTc (mkTcRnUnknownMessage (mkPlainError []
          (text "This nested type case needs an unsupported constructor telescope.")))
  checked_rows <- forM infos $ \case
    vi@VI { vi_case_pattern = Just (owner, i, pat, ty) } -> do
      checked <- synthCheckedPattern [] pat ty
      pure ((owner, i), (vi, [checked]))
    _ -> unsupported
  ((owner, column), rows) <- case unzip checked_rows of
    (first_oc : rest_oc, rws) | all (== first_oc) rest_oc -> pure (first_oc, rws)
    _ -> unsupported
  scrutinee <- case drop column args of { s : _ -> pure s; [] -> unsupported }
  next <- newTcRef (0 :: Int)
  let fresh kind = (`mkTyVar` kind) <$> newSysName (mkTyVarOcc "field")
      bindersFor = mkNamedTyConBinders (Invisible InferredSpec)
      true_ty    = mkTyConTy promotedTrueDataCon
      false_ty   = mkTyConTy promotedFalseDataCon
      branch lhs rhs =
        let ts = scopedSort (tyCoVarsOfTypesList lhs)
        in mkCoAxBranch ts [] [] lhs rhs (replicate (length ts) Nominal) loc

      makeFamily bs result branches = do
        i <- readTcRef next
        writeTcRef next (i + 1)
        let suffix = zEncodeString (occNameString (nameOccName (tyConName root))) ++ "." ++ show i
        name <- newImplicitBinder owner $ \occ ->
          mkTcOcc ("$case-node:" ++ occNameString occ ++ ":" ++ suffix)
        let kind = mkTyConKindWithMatchability unmatchableDataConTy bs result
        ax_name <- newImplicitBinder name mkInstTyCoOcc
        let fam = mkFamilyTyCon name kind bs 0 result Nothing
                    (ClosedTypeFamilyTyCon closed) Nothing NotInjective
            closed
              | Just (sc_owner, _) <- closedFamilyScope_maybe root = CTF_Scoped sc_owner False ax
              | otherwise = CTF (Just ax)
            ax = mkBranchedCoAxiom Nominal ax_name fam branches
        pure fam

      makeChild sourceArgs slots result liveRows = do
        let ts = scopedSort (tyCoVarsOfTypesList (result : sourceArgs ++ slots))
        unless (all isTyVar ts) unsupported
        (branches, helpers) <- build sourceArgs ts slots result liveRows
        fam <- makeFamily (bindersFor ts) result branches
        pure (mkTyConApp fam (mkTyVarTys ts), fam : helpers)

      build sourceArgs variables slots result
        liveRows@((VI { vi_loc = row_loc, vi_pats = row_pats, vi_rhs = row_rhs }, first_pats) : _) =
        case [(path, (fld, sp, con)) | (fld, SynthCaseConstructor path sp con _) <- zip [0..] first_pats] of
          [] -> case tcMatchTyKis row_pats sourceArgs of
            Just subst
              | let rhs = Subst.substTyUnchecked subst row_rhs
              , typeKind rhs `eqType` result ->
                  pure ([branch (mkTyVarTys variables) rhs], [])
            _ -> unsupported
          d0 : d_rest -> do
            let (_, (fld, sp, con)) = minimumBy (\(p, _) (q, _) -> compare p q) (d0 :| d_rest)
            case drop fld slots of
              slot : _ | Just scrut <- getTyVar_maybe slot ->
                if varType scrut `eqType` liftedTypeKind
                  then classifyColumn sourceArgs variables slots result liveRows row_loc sp fld scrut con
                  else closedColumn sourceArgs variables slots result liveRows row_loc sp fld scrut con
              _ -> unsupported
        where
          specialize fld con (row, pats) = case splitAt fld pats of
            (pfx, SynthCaseField : sfx) ->
              Just (row, pfx ++ replicate arity SynthCaseField ++ sfx)
            (pfx, SynthCaseConstructor _ _ expected fields : sfx)
              | expected == con, length fields == arity ->
                  Just (row, pfx ++ fields ++ sfx)
            _ -> Nothing
            where arity = count isVisibleTyConBinder (tyConBinders con)

          closedColumn sourceArgs' variables' slots' result' liveRows' row_loc' sp fld scrut con = do
            (parent, _) <- maybe unsupported pure (tcSplitTyConApp_maybe (varType scrut))
            cs <- maybe unsupported pure (tyConDataCons_maybe parent)
            case isPromotedDataCon_maybe con of
              Just dc
                | isVanillaDataCon dc, null (dataConRepArgBinders dc)
                , any (not . isFamFreeTy . dataConOrigResTy) cs ->
                    classifyColumn sourceArgs' variables' slots' result' liveRows' row_loc' sp fld scrut con
              _ -> do
                let promotable dc = let (_, _, _, th, _, _) = dataConFullSig dc in null th
                results <- forM (filter promotable cs) $ \dc -> do
                  let promoted = promoteDataCon dc
                      selected = mapMaybe (specialize fld promoted) liveRows'
                  if null selected then pure Nothing else do
                    unless (null (dataConRepArgBinders dc)) unsupported
                    onConstructor sourceArgs' variables' slots' result' row_loc' fld scrut promoted selected
                let branches = catMaybes results
                if null branches then unsupported else
                  pure ( [branch lhs rhs | (_, lhs, rhs, _) <- branches]
                       , concat [hs | (_, _, _, hs) <- branches] )

          classifyColumn sourceArgs' variables' slots' result' liveRows' row_loc' sp fld scrut con = do
            unless (isGenerativeTyCon con Nominal && not (isTypeFamilyTyCon con)) $
              setSrcSpan sp $ failWithTc (TcRnNestedTypeCasePattern (mkTyVarTy scrut))
            let matchRows = mapMaybe (specialize fld con) liveRows'
                missRow (_, pats) = case drop fld pats of
                  SynthCaseConstructor _ _ expected _ : _ -> expected /= con
                  _ -> True
                missRows = filter missRow liveRows'
                v_args   = mkTyVarTys variables'
                kind     = varType scrut
                kindVars = scopedSort (tyCoVarsOfTypesList [kind])
                testArgs a = mkTyVarTys kindVars ++ [a]
            unless (all isTyVar kindVars) unsupported
            matched <- onConstructor sourceArgs' variables' slots' result' row_loc' fld scrut con matchRows
            (pat_ty, lhs, matchedRhs, matchedHelpers) <- maybe unsupported pure matched
            unless (typeKind pat_ty `eqType` kind) unsupported
            testArg  <- fresh kind
            classify <- makeFamily (bindersFor kindVars ++ [mkAnonTyConBinder testArg]) boolTy
              [branch (testArgs pat_ty) true_ty, branch (testArgs (mkTyVarTy testArg)) false_ty]
            (missBranches, missHelpers) <- if null missRows then pure ([], []) else do
              (missRhs, hs) <- makeChild sourceArgs' slots' result' missRows
              pure ([branch (false_ty : v_args) missRhs], hs)
            selector <- fresh boolTy
            decision <- makeFamily (mkAnonTyConBinder selector : bindersFor variables') result'
              (branch (true_ty : lhs) matchedRhs : missBranches)
            pure ( [branch v_args (mkTyConApp decision (mkTyConApp classify (testArgs (mkTyVarTy scrut)) : v_args))]
                 , classify : decision : matchedHelpers ++ missHelpers )
      build _ _ _ _ _ = unsupported

      onConstructor sourceArgs' variables' slots' result' row_loc' fld scrut con selected = do
        let bndrs = tyConBinders con
        binders <- map snd <$> foldM (freshBinder fresh) [] bndrs
        let instantiated = Subst.substTyWith (binderVars bndrs) (mkTyVarTys binders)
            con_kind     = instantiated (tyConResKind con)
            scrut_kind   = varType scrut
        refinement <- case tcUnifyTy con_kind scrut_kind of
          Just refine -> pure (Just (refine, Nothing))
          Nothing
            | typeListsAreApart [con_kind] [scrut_kind] -> pure Nothing
            | otherwise -> do
                (red_ref, apart) <- synthCaseKindRefinement con_kind scrut_kind
                case red_ref of
                  Just (refine, co) -> pure (Just (refine, Just co))
                  Nothing
                    | apart -> pure Nothing
                    | otherwise -> setSrcSpan row_loc' $
                        failWithTc (TcRnTypeCaseIndexApartness scrut_kind)
        case refinement of
          Nothing -> pure Nothing
          Just (refine, kind_co) -> do
            (before, after) <- case splitAt fld slots' of
              (bf, _ : af) -> pure (bf, af)
              _            -> unsupported
            let refineTy = Subst.substTyUnchecked refine
                extras   = result' : sourceArgs' ++ slots' ++ mkTyVarTys binders
                liveVars = scopedSort (tyCoVarsOfTypesList (map refineTy extras))
                renameVar s v
                  | v == scrut = pure s
                  | otherwise =
                      let nk = Subst.substTyUnchecked s (refineTy (varType v))
                      in if nk `eqType` varType v then pure s
                         else do { v' <- fresh nk; pure (Subst.extendTvSubstAndInScope s v (mkTyVarTy v')) }
                initial = Subst.mkEmptySubst (mkInScopeSet (mkVarSet liveVars))
            renaming <- foldM renameVar initial liveVars
            let retag   = Subst.substTyUnchecked renaming . refineTy
                con_pat = mkTyConApp con (map (retag . mkTyVarTy) binders)
                pat     = case kind_co of
                  Nothing -> con_pat
                  Just co -> mkCastTy con_pat (substCo renaming (substCo refine co))
                replace = Subst.substTyWith [scrut] [pat] . retag
                fields  = [replace (mkTyVarTy v) | (b, v) <- zip bndrs binders, isVisibleTyConBinder b]
                src_a   = map replace sourceArgs'
                c_slots = map replace before ++ fields ++ map replace after
                lhs     = map (replace . mkTyVarTy) variables'
            (rhs, helpers) <- makeChild src_a c_slots (replace result') selected
            pure (Just (pat, lhs, rhs, helpers))

  (branches, helpers) <- build args vars [scrutinee] res rows
  ax_name <- newImplicitBinder (tyConName root) mkInstTyCoOcc
  pure (mkBranchedCoAxiom Nominal ax_name root branches, helpers)
  where
    freshBinder fresh previous (Bndr var _) = do
      let s = Subst.substTyWith (map fst previous) (mkTyVarTys (map snd previous))
      fld <- fresh (s (varType var))
      pure (previous ++ [(var, fld)])

synthCaseKindRefinement :: Kind -> Kind -> TcM (Maybe (Subst.Subst, CoercionN), Bool)
synthCaseKindRefinement actual expected
  | Just (tc, as)  <- tcSplitTyConApp_maybe actual
  , Just (tc', bs) <- tcSplitTyConApp_maybe expected
  , tc == tc', isGenerativeTyCon tc Nominal, as `equalLength` bs
  = do env  <- getGblEnv
       st   <- getSynthTypeState
       fams <- tcGetFamInstEnvs
       let index_step a b
             | not (a `eqType` b)
             , Just (family, args) <- tcSplitTyConApp_maybe a
             , let name = tyConName family
             , not (nameIsLocalOrFrom (tcg_mod env) name)
               || name `elemNameEnv` tcg_type_env env
               || name `elemNameEnv` sts_tycons st
             , Just _ <- closedFamilyTyConCoAxiom_maybe family
             , Just redn <- reduceTyFamApp_maybe fams Nominal family args
             = redn
             | otherwise = Reduction (mkNomReflCo a) a
           binders = binderVars (tyConBinders tc)
           check_indices prior actual_args expected_args =
             let steps = zipWith index_step actual_args expected_args
                 redns
                   | binders `equalLength` steps, all isTyVar binders
                   = snd (mapAccumL transport (emptyLiftingContext emptyInScopeSet) (zip binders steps))
                   | otherwise = steps
                 reduced = map reductionReducedType redns
                 refinement = do
                   guard (kinded_args (tyConKind tc) reduced)
                   tcUnifyTy (mkTyConApp tc reduced) (mkTyConApp tc expected_args)
             in case refinement of
               Just refine ->
                 let co       = mkTyConAppCo Nominal tc (map reductionCoercion redns)
                     complete = maybe refine (Subst.composeTCvSubst refine) prior
                     scoped   = Subst.extendSubstInScopeSetFromSource complete (tyCoVarsOfCo co)
                 in (Just (scoped, co), False)
               Nothing -> (Nothing, or (zipWith index_apart reduced expected_args))

           transport lc (binder, Reduction co arg) =
             let kind_co = liftCoSubst Nominal lc (varType binder)
                 arg_co  = co `mkTransCo` mkGReflRightMCo Nominal arg (MCo kind_co)
                 lc'     = extendLiftingContextAndInScope lc binder arg_co
             in (lc', Reduction arg_co (mkCastTy arg kind_co))
           index_apart a b = typeKind a `eqType` typeKind b && typesAreApart a b
           kinded_args _ [] = True
           kinded_args k (arg : rest)
             | Just (_, bndr, _) <- splitPiTy_maybe k
             , typeKind arg `eqType` piTyBinderType bndr
             = kinded_args (piResultTy k arg) rest
             | otherwise = False
           structural_pair (a, b) = isFamFreeTy a && isFamFreeTy b && typeKind a `eqType` typeKind b
       pure $ case check_indices Nothing as bs of
         (Nothing, False)
           | let (lhs, rhs) = unzip (filter structural_pair (zip as bs))
           , Just prior <- tcUnifyTys alwaysBindTv lhs rhs
           , not (Subst.isEmptySubst prior)
           -> check_indices (Just prior) (Subst.substTys prior as) (Subst.substTys prior bs)
         result -> result
  | otherwise = pure (Nothing, False)

synthCheckedPattern :: [Int] -> TypeCasePattern -> Type -> TcM CheckedSynthPattern
synthCheckedPattern _ CasePatternField _ = pure SynthCaseField
synthCheckedPattern path pat (CastTy ty _) = synthCheckedPattern path pat ty
synthCheckedPattern path (CasePatternConstructor sp _ fields) ty =
  setSrcSpan sp $ case (splitTyConAppNoView_maybe ty, split_pat ty) of
    (Just (written, original), Just (resolved, canonical))
      | let old = vis_args written original
            new = vis_args resolved canonical
      , length fields == length old
      , isGenerativeTyCon resolved Nominal
      , not (isTypeFamilyTyCon resolved) -> do
          children <- if written == resolved
            then sequence [synthCheckedPattern (path ++ [i]) fld arg
                          | (i, fld, arg) <- zip3 [0..] fields old]
            else synonym_fields written original resolved new
          pure (SynthCaseConstructor path sp resolved children)
    (_, Nothing) -> failWithTc (TcRnTypeCasePattern (Just ty))
    _            -> failWithTc (TcRnNestedTypeCasePattern ty)
  where
    split_pat = tcSplitTyConApp_maybe . typeCasePatternView
    vis_args tc as = [a | (b, a) <- zip (tyConBinders tc) as, isVisibleTyConBinder b]
    unsupported :: TcM a
    unsupported = failWithTc (TcRnNestedTypeCasePattern ty)
    is_test CasePatternField         = False
    is_test CasePatternConstructor{} = True
    find_marker marked sh = getTyVar_maybe (typeCasePatternView sh) >>= (`lookup` marked)

    synonym_fields written original resolved new = do
      markers <- forM (vis_args written original) $ \arg ->
        (`mkTyVar` typeKind arg) <$> newSysName (mkTyVarOcc "case_argument")
      let marker_args [] as = map snd as
          marker_args _  [] = []
          marker_args pending ((bndr, a) : as)
            | isVisibleTyConBinder bndr, m : more <- pending =
                mkTyVarTy m : marker_args more as
            | otherwise = a : marker_args pending as
          expanded = mkTyConApp written (marker_args markers (zip (tyConBinders written) original))
          marked   = zip markers (zip [0..] fields)
      case split_pat expanded of
        Just (head_tc, as)
          | head_tc == resolved
          , let shape = vis_args head_tc as
          , length shape == length new ->
              case traverse (find_marker marked) shape of
                Just aligned
                  | let used = [i | (i, fld) <- aligned, is_test fld]
                  , length used == length (nub used) ->
                      sequence [synthCheckedPattern (path ++ [i]) fld actual
                               | ((i, fld), actual) <- zip aligned new]
                _ -> do
                  aligned <- sequence [expand_field marked (path ++ [i]) sh actual
                                      | (i, sh, actual) <- zip3 [0..] shape new]
                  let used = concatMap snd aligned
                  unless (used == sort (nub used)) unsupported
                  pure (map fst aligned)
        _ -> unsupported

    expand_field marked pos sh actual
      | Just (i, fld) <- find_marker marked sh = do
          checked <- synthCheckedPattern pos fld actual
          pure (checked, [i | is_test fld])
      | Just (con, sh_args) <- split_pat sh
      , Just (act_con, act_args) <- split_pat actual
      , act_con == con, isGenerativeTyCon con Nominal, not (isTypeFamilyTyCon con)
      , let sh_flds  = vis_args con sh_args
            act_flds = vis_args con act_args
      , length sh_flds == length act_flds = do
          children <- sequence [expand_field marked (pos ++ [i]) fld a
                               | (i, fld, a) <- zip3 [0..] sh_flds act_flds]
          pure (SynthCaseConstructor pos sp con (map fst children), concatMap snd children)
      | otherwise = unsupported

-- | Adapt a visible type argument from matchable arrows to unmatchable arrows
-- via a closed 1-equation eta-expansion family.
-- See Note [Matchability forgetfulness in visible type applications].
tcForgetMatchabilityArrows :: CtOrigin -> TcType -> TcKind -> TcKind -> TcM (Maybe TcType)
tcForgetMatchabilityArrows origin res_ty0 act_kind0 exp_kind0 = do
  dep <- xoptM LangExt.DependentTypes
  if not dep
    then pure Nothing
    else do
      res_ty <- liftZonkM (zonkTcType res_ty0)
      act_k  <- liftZonkM (zonkTcType act_kind0)
      exp_k  <- liftZonkM (zonkTcType exp_kind0)
      case plan_forget act_k exp_k of
        Just (flags, _doms, _final_res)
          | or flags -> case tcSplitTyConApp_maybe res_ty of
              Just (tc, tc_args) -> do
                mb_eta_tc <- getOrCreateEtaForgetTyCon tc (length tc_args) flags
                case mb_eta_tc of
                  Just eta_tc -> do
                    let eta_app = mkTyConApp eta_tc tc_args
                    co_k <- unifyTypeAndEmit KindLevel origin (typeKind eta_app) exp_kind0
                    pure (Just (eta_app `mkCastTy` co_k))
                  Nothing -> getOrCreateEtaForgetGeneral origin res_ty act_k exp_kind0 flags
              Nothing -> getOrCreateEtaForgetGeneral origin res_ty act_k exp_kind0 flags
        _ -> pure Nothing
  where
    forget_step ma_a ma_e
      | ma_a `tcEqType` matchableDataConTy && ma_e `tcEqType` unmatchableDataConTy = Just True
      | ma_a `tcEqType` unmatchableDataConTy && ma_e `tcEqType` unmatchableDataConTy = Just False
      | otherwise = Nothing

    has_unsound_u_to_m ak ek
      | FunTy FTF_T_T ma_a _ dom_a res_a <- coreFullView ak
      , FunTy FTF_T_T ma_e _ dom_e res_e <- coreFullView ek
      = (ma_a `tcEqType` unmatchableDataConTy && ma_e `tcEqType` matchableDataConTy)
        || has_unsound_u_to_m dom_a dom_e
        || has_unsound_u_to_m res_a res_e
      | otherwise = False

    plan_forget ak ek
      | FunTy FTF_T_T ma_a mult_a _dom_a res_a <- coreFullView ak
      , FunTy FTF_T_T ma_e mult_e dom_e  res_e <- coreFullView ek
      , isManyTy mult_a, isManyTy mult_e
      , Just step_forgets <- forget_step ma_a ma_e
      = case plan_forget res_a res_e of
          Just (more_flags, more_doms, final_res)
            | or more_flags
            -> Just (step_forgets : more_flags, dom_e : more_doms, final_res)
          _ | step_forgets
            , not (has_unsound_u_to_m res_a res_e)
            -> Just ([True], [dom_e], res_e)
            | otherwise
            -> Nothing
      | otherwise = Nothing

getOrCreateEtaForgetTyCon :: TyCon -> Int -> [Bool] -> TcM (Maybe TyCon)
getOrCreateEtaForgetTyCon tc n_prefix flags = do
  cur_mod <- tcg_mod <$> getGblEnv
  let pat_str = map (\b -> if b then 'M' else 'U') flags
      tc_str  = occNameString (nameOccName (tyConName tc)) ++ "_" ++ show (getUnique tc)
      occ     = mkTcOcc ("$eta$" ++ tc_str ++ "$" ++ show n_prefix ++ "$" ++ pat_str)
  fam_name <- newGlobalBinder cur_mod occ Nothing noSrcSpan
  tcInternSynthClosedFamily fam_name (const True) $ runMaybeT $ do
    let init_subst = Subst.mkEmptySubst (mkInScopeSet (tyCoVarsOfType (tyConKind tc)))
    (p_tvs, rem_kind)       <- MaybeT (peel_prefix n_prefix init_subst (tyConKind tc))
    (z_tvs, final_res_kind) <- MaybeT (peel_eta (length flags) rem_kind)
    let all_tvs = p_tvs ++ z_tvs
    guard (tcIsLiftedTypeKind (typeKind final_res_kind)
           && all (tcIsLiftedTypeKind . typeKind . tyVarKind) all_tvs
           && not (any (has_matchable_arrow . tyVarKind) all_tvs))
    let lhs_pats = mkTyVarTys all_tvs
        rhs_ty   = mkTyConApp tc lhs_pats
    pure (p_tvs, z_tvs, final_res_kind,
          SynthClosedEqns Nothing [(all_tvs, lhs_pats, rhs_ty, noSrcSpan)])
  where
    peel_prefix 0 subst k = pure (Just ([], Subst.substTy subst k))
    peel_prefix n subst k = case splitPiTy_maybe k of
      Just (_, Named (Bndr tv0 _), rest_k) | isTyVar tv0 -> do
        let ki' = Subst.substTy subst (tyVarKind tv0)
        nm <- newSysName (mkTyVarOccFS (occNameFS (nameOccName (getName tv0))))
        let tv'    = mkTyVar nm ki'
            subst' = Subst.extendTvSubstAndInScope subst tv0 (mkTyVarTy tv')
        fmap (\(tvs, rk) -> (tv' : tvs, rk)) <$> peel_prefix (n - 1) subst' rest_k
      Just (_, Anon (Scaled mult dom0) af, rest_k) | af == FTF_T_T, isManyTy mult -> do
        nm <- newSysName (mkTyVarOcc "p")
        let tv' = mkTyVar nm (Subst.substTy subst dom0)
        fmap (\(tvs, rk) -> (tv' : tvs, rk)) <$> peel_prefix (n - 1) subst rest_k
      _ -> pure Nothing

    peel_eta 0 k = pure (Just ([], k))
    peel_eta m k = case coreFullView k of
      FunTy FTF_T_T _ mult dom rest_k | isManyTy mult -> do
        nm <- newSysName (mkTyVarOcc "z")
        fmap (\(tvs, rk) -> (mkTyVar nm dom : tvs, rk)) <$> peel_eta (m - 1) rest_k
      _ -> pure Nothing

    has_matchable_arrow ty = case coreFullView ty of
      FunTy _ ma _ a r -> ma `eqType` matchableDataConTy || has_matchable_arrow a || has_matchable_arrow r
      ForAllTy ma b r  -> ma `eqType` matchableDataConTy || has_matchable_arrow (varType (binderVar b)) || has_matchable_arrow r
      AppTy f a        -> has_matchable_arrow f || has_matchable_arrow a
      TyConApp _ args  -> any has_matchable_arrow args
      CastTy t _       -> has_matchable_arrow t
      _                -> False

getOrCreateEtaForgetGeneral
  :: CtOrigin -> TcType -> TcKind -> TcKind -> [Bool] -> TcM (Maybe TcType)
getOrCreateEtaForgetGeneral origin res_ty act_k exp_kind0 flags
  | let raw_fvs = tyCoVarsOfTypeList res_ty
  , all (\v -> isTyVar v && not (isMetaTyVar v)) raw_fvs
  , not (hasNativeDependentExpr (Core.Type res_ty))
  = do
      cur_mod <- tcg_mod <$> getGblEnv
      ord <- allocSynthOrdinal (mkTcOcc "$eta$expr")
      let pat_str = map (\b -> if b then 'M' else 'U') flags
          occ     = mkTcOcc ("$eta$expr$" ++ show ord ++ "$" ++ pat_str)
      fam_name <- newGlobalBinder cur_mod occ Nothing noSrcSpan
      let cap_tvs = topo_sort_eta_tvs raw_fvs
      mb_tc <- tcInternSynthClosedFamily fam_name (const True) $ runMaybeT $ do
        let init_s = Subst.mkEmptySubst (mkInScopeSet (mkVarSet cap_tvs))
        (p_tvs, clone_s) <- lift (clone_cap_tvs init_s cap_tvs)
        let act_k_sub  = Subst.substTy clone_s act_k
            res_ty_sub = Subst.substTy clone_s res_ty
        (z_tvs, final_res_kind) <- MaybeT (peel_eta (length flags) act_k_sub)
        guard (tcIsLiftedTypeKind (typeKind final_res_kind)
               && all (tcIsLiftedTypeKind . typeKind . tyVarKind) (p_tvs ++ z_tvs))
        let all_tvs  = p_tvs ++ z_tvs
            lhs_pats = mkTyVarTys all_tvs
            rhs_ty   = mkAppTys res_ty_sub (mkTyVarTys z_tvs)
        pure (p_tvs, z_tvs, final_res_kind,
              SynthClosedEqns Nothing [(all_tvs, lhs_pats, rhs_ty, noSrcSpan)])
      case mb_tc of
        Nothing -> pure Nothing
        Just eta_tc -> do
          let eta_app = mkTyConApp eta_tc (mkTyVarTys cap_tvs)
          co_k <- unifyTypeAndEmit KindLevel origin (typeKind eta_app) exp_kind0
          pure (Just (eta_app `mkCastTy` co_k))
  | otherwise = pure Nothing
  where
    peel_eta 0 k = pure (Just ([], k))
    peel_eta m k = case coreFullView k of
      FunTy FTF_T_T _ mult dom rest_k | isManyTy mult -> do
        nm <- newSysName (mkTyVarOcc "z")
        fmap (\(tvs, rk) -> (mkTyVar nm dom : tvs, rk)) <$> peel_eta (m - 1) rest_k
      _ -> pure Nothing

    clone_cap_tvs subst [] = pure ([], subst)
    clone_cap_tvs subst (tv : tvs) = do
      let ki' = Subst.substTy subst (tyVarKind tv)
      nm <- newSysName (mkTyVarOccFS (occNameFS (nameOccName (getName tv))))
      let tv'    = mkTyVar nm ki'
          subst' = Subst.extendTvSubstAndInScope subst tv (mkTyVarTy tv')
      (rest, final_s) <- clone_cap_tvs subst' tvs
      pure (tv' : rest, final_s)

    topo_sort_eta_tvs :: [TyVar] -> [TyVar]
    topo_sort_eta_tvs vs0 =
      let uniq_vs  = reverse (snd (foldl' (\(s, acc) v ->
                       if v `elemVarSet` s then (s, acc) else (extendVarSet s v, v : acc))
                       (emptyVarSet, []) vs0))
          cand_set = mkVarSet uniq_vs
          visit (seen, acc) tv
            | tv `elemVarSet` seen = (seen, acc)
            | otherwise =
                let deps = [ d | d <- tyCoVarsOfTypeList (tyVarKind tv)
                               , isTyVar d, d `elemVarSet` cand_set ]
                    (s1, a1) = foldl' visit (extendVarSet seen tv, acc) deps
                in (s1, tv : a1)
      in reverse (snd (foldl' visit (emptyVarSet, []) uniq_vs))

{- Note [Uninterpreted term applications for TcPlugins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When no 'tcPlugin' is active, open term applications without a certified
'SourceTable' or 'OpenTypeEval' reduction report 'TcRnNoSourceTable' or
'TcRnClosedTypeEval'. When a 'tcPlugin' is active and an open call has no
certified reduction, 'getOrCreateUninterpretedTyCon' interns an equationless
closed type family @U:<occ>@ (or @U:@<unit>:<mod>:<occ>@ for imported terms)
via 'tcInternSynthClosedFamily' so solver plugins can inspect or discharge
nominal equalities over that application.
-}

hasActiveTcPlugins :: TcM Bool
hasActiveTcPlugins = do
  hsc_env <- getTopEnv
  pure (not (null (catMaybes (mapPlugins (hsc_plugins hsc_env) tcPlugin))))

canUseUninterpretedTerm :: Id -> TcM Bool
canUseUninterpretedTerm fun = do
  active <- hasActiveTcPlugins
  if active
    then pure True
    else hasImportedUninterpretedFamily fun

hasImportedUninterpretedFamily :: Id -> TcM Bool
hasImportedUninterpretedFamily fun = do
  gbl <- getGblEnv
  let orig_name = idName fun
      sem_mod   = tcg_semantic_mod gbl
      local_occ = mkTcOcc ("U:" ++ occNameString (nameOccName orig_name))
  case nameModule_maybe orig_name of
    Just def_mod
      | def_mod /= sem_mod, not (isInteractiveModule def_mod) -> do
          hsc_env <- getTopEnv
          ext_name <- lookupOrig def_mod local_occ
          mb_loaded <- liftIO (lookupType hsc_env ext_name)
          case mb_loaded of
            Just (ATyCon _) -> pure True
            _ -> do
              iface <- loadInterfaceForName (text "uninterpreted term family") orig_name
              pure (isJust (mi_hash_fn iface local_occ))
    _ -> pure False

tcUninterpretedTermCall :: TcTyMode -> HsType GhcRn -> ExpKind -> Int -> Id
                        -> TcM (Maybe TcType)
tcUninterpretedTermCall mode rn_ty exp_kind n_val_args fun
  | not (isTypeLevel (mode_tyki mode)) || n_val_args < 0
  = pure Nothing
  | isNothing (mode_holes mode) || isJust (mode_complete_type_arg mode)
  , mode_complete_type_arg mode /= Just False
  = do
      level <- getThLevel
      bootOrSig <- tcIsHsBootOrSig
      explicitLevels <- xoptM LangExt.ExplicitLevelImports
      case level of
        Comp | not bootOrSig && not explicitLevels -> do
          mb_tc <- getOrCreateUninterpretedTyCon fun n_val_args
          case mb_tc of
            Nothing     -> pure Nothing
            Just fam_tc -> Just <$> tc_app_ty_with_head mode rn_ty exp_kind (mkTyConTy fam_tc)
        _ -> pure Nothing
  | otherwise = pure Nothing

getOrCreateUninterpretedTyCon :: Id -> Int -> TcM (Maybe TyCon)
getOrCreateUninterpretedTyCon fun n_val_args = do
  gbl <- getGblEnv
  let orig_name = idName fun
      cur_mod   = tcg_mod gbl
      sem_mod   = tcg_semantic_mod gbl
      occ_str   = occNameString (nameOccName orig_name)
      local_occ = mkTcOcc ("U:" ++ occNameString (nameOccName orig_name))
  let _ = occ_str
  case nameModule_maybe orig_name of
    Nothing -> pure Nothing
    Just def_mod
      | def_mod == sem_mod || isInteractiveModule def_mod ->
          intern_in_cur_mod cur_mod local_occ (nameSrcSpan orig_name)
      | otherwise -> do
          ext_name <- lookupOrig def_mod local_occ
          mb_thing <- tcLookupImported_maybe ext_name
          pure $ case mb_thing of
            Maybe.Succeeded (ATyCon existing) | check_arity existing -> Just existing
            _                                                        -> Nothing
  where
    check_arity tc =
      let vis_count = length (filterOut isInvisibleTyConBinder (tyConBinders tc))
      in n_val_args >= vis_count && (vis_count > 0 || n_val_args == 0)

    intern_in_cur_mod cur_mod occ span_loc = do
      fam_name <- newGlobalBinder cur_mod occ Nothing span_loc
      tcInternSynthClosedFamily fam_name check_arity $ runMaybeT $ do
        let init_subst = Subst.mkEmptySubst (mkInScopeSet (tyCoVarsOfType (varType fun)))
        (p_tvs, rho) <- MaybeT (peel_invis init_subst (varType fun))
        let (domains, res_kind) = peel_arrows rho
        guard (n_val_args >= length domains
               && not (null domains && (n_val_args > 0 || isTyVarTy res_kind))
               && not (tcIsLiftedTypeKind res_kind)
               && tcIsLiftedTypeKind (typeKind res_kind)
               && all (tcIsLiftedTypeKind . typeKind) domains)
        z_tvs <- lift (freshAnonArgTyVars domains)
        pure (p_tvs, z_tvs, res_kind, SynthUninterpFam)

    peel_invis subst ty = case splitPiTy_maybe ty of
      Just (_, Named (Bndr tv0 vis), rest)
        | isInvisibleForAllTyFlag vis, isTyVar tv0 -> do
            let ki0 = Subst.substTy subst (tyVarKind tv0)
            if ki0 `tcEqType` runtimeRepTy
              then peel_invis (Subst.extendTvSubstAndInScope subst tv0 liftedRepTy) rest
              else if ki0 `tcEqType` levityTy
                then peel_invis (Subst.extendTvSubstAndInScope subst tv0 liftedDataConTy) rest
                else if tcIsLiftedTypeKind ki0 || tcIsLiftedTypeKind (typeKind ki0)
                  then do
                    nm <- newSysName (mkTyVarOccFS (occNameFS (nameOccName (getName tv0))))
                    let tv' = mkTyVar nm ki0
                    fmap (\(tvs, r) -> (tv' : tvs, r)) <$>
                      peel_invis (Subst.extendTvSubstAndInScope subst tv0 (mkTyVarTy tv')) rest
                  else pure Nothing
      Just (_, b, _) | isInvisiblePiTyBinder b -> pure Nothing
      _ -> pure (Just ([], Subst.substTy subst ty))

    peel_arrows ty = case coreFullView ty of
      FunTy FTF_T_T _ mult dom rest | isManyTy mult ->
        let (doms, res) = peel_arrows rest in (dom : doms, res)
      _ -> ([], ty)

---------------------------

tcHsContext :: Foldable t => t (LHsContext GhcRn) -> TcM [PredType]
tcHsContext = foldMapM (tc_hs_context typeLevelMode)

tcLHsPredType :: LHsType GhcRn -> TcM PredType
tcLHsPredType pred = tc_lhs_pred typeLevelMode pred

tc_hs_context :: TcTyMode -> LHsContext GhcRn -> TcM [PredType]
tc_hs_context mode ctxt = mapM (tc_lhs_pred mode) (hsc_ctxt $ unLoc ctxt)

tc_lhs_pred :: TcTyMode -> LHsType GhcRn -> TcM PredType
tc_lhs_pred mode pred@(L loc _)
  = setSrcSpanA loc $
    do { pred_ty <- tc_check_lhs_type mode pred constraintKind
       ; unless (isEmptyNameEnv (mode_indices mode)) $
         do { pred_ty <- liftZonkM $ zonkTcType pred_ty
            ; unless (supported_predicate pred_ty) $
                failWithTc $ TcRnRetainedQuantificationError
                             (RetainedConstraintType pred_ty) }
       ; pure pred_ty }
  where
    -- Check native components hidden by constraint-tuple synonyms too.
    supported_predicate :: PredType -> Bool
    supported_predicate ty
      | not (hasNativeDependentExpr (Core.Type ty)) = True
      | Just (cls, args) <- getClassPredTys_maybe ty
      = not (isCTupleClass cls) || all supported_predicate args
      | otherwise = False

{- Note [Retained source constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check each predicate in the registered runtime-index environment. Ordinary
class and equality predicates introduce Given dictionaries below the runtime
binders, while native quantified or irreducible predicates remain gated at
their source span (GHC Proposal #378; see Note [Retained application evidence]
in "GHC.Tc.Gen.App").
-}

tcTyVar :: TcTyMode -> Name -> TcM (TcType, TcKind)
-- See Note [Type checking recursive type and class declarations]
-- in GHC.Tc.TyCl
-- This does not instantiate. See Note [Do not always instantiate eagerly in types]
tcTyVar mode name
  | Just index <- lookupNameEnv (mode_indices mode) name
  = if isNativeOpaqueValueId index
    then pure (TermVarTy index, varType index)
    else failWithTc $ TcRnRetainedQuantificationError RetainedCallIndex
tcTyVar mode name      -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; pack <- isExistsPackName name
       ; when pack $ failWithTc (TcRnExistsPack ExistsPackInvalidContext)
       ; mb_pending <- lookup_pending_local_tycon name
       ; case mb_pending of
           Just tc ->
             do { when (isJust (sourceTableTermOcc (nameOccName (tyConName tc)))) $
                    sourceTableTypeUses (mkTyConTy tc)
                ; return (mkTyConTy tc, tyConKind tc) }
           Nothing -> do { thing <- tcLookup name
                         ; case thing of
           ATyVar _ tv -> return (mkTyVarTy tv, tyVarKind tv)
           ATcTypeLet _ ty kind -> return (ty, kind)

           -- An ordinary module can import a synonym for a checked computation.
           -- See Note [Source table dependency closure] in GHC.Tc.Module.
           AGlobal (ATyCon tc)
             | isInternalName (tyConName tc)
             -> return (mkTyConTy tc, tyConKind tc)
             | Just rhs <- synTyConRhs_maybe tc
             -> do { sourceTableTypeUses rhs
                   ; recordSourceProofAliasUse (mode_source_proof_owner mode) tc
                   ; return (mkTyConTy tc, tyConKind tc) }
             | isJust (sourceTableTermOcc (nameOccName (tyConName tc)))
             -> do { sourceTableTypeUses (mkTyConTy tc)
                   ; return (mkTyConTy tc, tyConKind tc) }

           -- See Note [Recursion through the kinds]
           (tcTyThingTyCon_maybe -> Just tc) -- TyCon or TcTyCon
             -> return (mkTyConTy tc, tyConKind tc)

           AGlobal (AConLike (RealDataCon dc))
             -> promote_real_data_con dc

           AGlobal AnId{}    -> promotionErr name TermVariablePE
           ATcId { tct_id = v } ->
             do { dep <- xoptM LangExt.DependentTypes
                ; mb_rep <- if dep
                            then tcTypeRepTarget_maybe (varType v)
                            else pure Nothing
                ; case mb_rep of
                    Just (kind, ty) -> return (ty, kind)
                    Nothing         -> promotionErr name TermVariablePE }
           APromotionErr FamDataConPE ->
             do { gbl_env <- getGblEnv
                ; case lookupTypeEnv (tcg_type_env gbl_env) name of
                    Just (AConLike (RealDataCon dc)) -> promote_real_data_con dc
                    _ -> promotionErr name FamDataConPE }
           APromotionErr err -> promotionErr name err

           _  -> wrongThingErr WrongThingType thing name } }
  where
    lookup_pending_local_tycon :: Name -> TcM (Maybe TyCon)
    lookup_pending_local_tycon nm = do
      gbl <- getGblEnv
      if isJust (lookupTypeEnv (tcg_type_env gbl) nm)
        then pure Nothing
        else lookupPendingSynthTyCon nm
    promote_real_data_con dc
      = do { unless (null (dataConRepArgBinders dc) || dataConHasPromotedRetained dc) $
               failWithTc $ TcRnRetainedQuantificationError RetainedPackagePromotion
           ; when (isDataFamInstTyCon (dataConTyCon dc)) $
               do { enabled <- xoptM LangExt.DependentTypes
                  ; unless (enabled && canPromoteDataFamilyCon dc) $
                      promotionErr name FamDataConPE }
           ; let (_, _, _, theta, _, _) = dataConFullSig dc
           ; traceTc "tcTyVar" (ppr dc <+> ppr theta)
             -- promotionErr: Note [No constraints in kinds] in GHC.Tc.Validity
           ; unless (null theta) $
             promotionErr name (ConstrainedDataConPE theta)
           ; let tc = promoteDataCon dc
           ; case tyConTuple_maybe (dataConTyCon dc) of
               Just UnboxedTuple -> do
                 ubx <- xoptM LangExt.UnboxedTuples
                 unless ubx $
                   failWithTc $
                     TcRnUnboxedTupleOrSumTypeFuncArg UnboxedTupleType (mkTyConTy tc)
                 edt <- xoptM LangExt.DependentTypes
                 if edt
                   then do
                     let arity = dataConSourceArity dc
                         reps = replicate arity liftedRepTy
                         (rr_bndrs, rest_bndrs) = splitAt arity (tyConBinders tc)
                         subst = zipTvSubst (binderVars rr_bndrs) reps
                         inst_kind = Subst.substTy subst (mkTyConKind rest_bndrs (tyConResKind tc))
                     return (mkTyConApp tc reps, inst_kind)
                   else return (mkTyConApp tc [], tyConKind tc)
               _ -> return (mkTyConApp tc [], tyConKind tc) }

-- | Admit finished lifted data or newtype family instance constructors with
-- no written class context (erased existential type binders are permitted).
-- See Note [Promoting a simple data family instance constructor].
canPromoteDataFamilyCon :: DataCon -> Bool
canPromoteDataFamilyCon dc =
  all isTyVar exs && null theta
  && null (dataConStupidTheta dc)
  && null (dataConRepArgBinders dc)
  && isLiftedTypeKind (typeKind (dataConOrigResTy dc))
  && all (isLiftedTypeKind . typeKind . scaledThing) fields
  where
    (_, exs, _, theta, fields, _) = dataConFullSig dc

{- Note [Promoting a simple data family instance constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A data family constructor has a runtime worker that returns the
representation type. Its wrapper uses the representational instance
axiom to return the public family application.

The promoted constructor already uses dataConOrigResTy as its result
kind. For a data family constructor this is the public family
application. An equality refinement in EqSpec already appears in this
result kind, as it does for an ordinary promoted GADT constructor.
See Note [Promoted data constructors] in GHC.Core.TyCon.
Use the existing promoted DataCon and its Name. Its kind contains
neither the representation TyCon nor the representation coercion.

Discussion #456 distinguishes this case from constructors whose
promotion needs to bind evidence. Keep that case and constructors of
unchecked instances out of scope. See Note [AFamDataCon: not
promoting data family constructors] in GHC.Tc.Utils.Env.
-}

{-
Note [Recursion through the kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these examples

Ticket #11554:
  data P (x :: k) = Q
  data A :: Type where
    MkA :: forall (a :: A). P a -> A

Ticket #12174
  data V a
  data T = forall (a :: T). MkT (V a)

The type is recursive (which is fine) but it is recursive /through the
kinds/.  In earlier versions of GHC this caused a loop in the compiler
(to do with knot-tying) but there is nothing fundamentally wrong with
the code (kinds are types, and the recursive declarations are OK). But
it's hard to distinguish "recursion through the kinds" from "recursion
through the types". Consider this (also #11554):

  data PB k (x :: k) = Q
  data B :: Type where
    MkB :: P B a -> B

Here the occurrence of B is not obviously in a kind position.

So now GHC allows all these programs.  #12081 and #15942 are other
examples.

Note [Body kind of a HsForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The body of a forall is usually a type.
Because of representation polymorphism, it can be a TYPE r, for any r.
(In fact, GHC can itself construct a function with an
unboxed tuple inside a for-all via CPR analysis; see
typecheck/should_compile/tc170).

A forall can also be used in an instance head, then the body should
be a constraint.

Right now, we do not have any easy way to enforce that a type is
either a TYPE something or CONSTRAINT something, so we accept any kind.
This is unsound (#22063). We could fix this by implementing a TypeLike
predicate, see #20000.

For a forall with a required argument, we do not allow constraints;
e.g. forall a -> Eq a is invalid. Therefore, we can enforce that the body
is a TYPE something in this case (#24176).

Note [Body kind of a HsQualTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If ctxt is non-empty, the HsQualTy really is a /function/, so the
kind of the result really is '*', and in that case the kind of the
body-type can be lifted or unlifted.

However, consider
    instance Eq a => Eq [a] where ...
or
    f :: (Eq a => Eq [a]) => blah
Here both body-kind and result kind of the HsQualTy is Constraint rather than *.
Rather crudely we tell the difference by looking at exp_kind. It's
very convenient to typecheck instance types like any other HsSigType.

(BK1) How do we figure out the right body kind?

Well, it's a bit of a kludge: I just look at the expected kind, `exp_kind`.
If we are in checking mode (`exp_kind` = `Check k`), and the pushed-in kind
`k` is `CONSTRAINT rep`, then we check that the body type has kind `Constraint` too.

This is a kludge because it wouldn't work if any unification was
involved to compute that result kind -- but it isn't.

Note that in the kludgy "figure out whether we are in a type or constraint"
check, we only check if `k` is a `CONSTRAINT rep`, not `Constraint`.
That turns out to give a better error message in T25243.

(BK2)

Note that, once we are in the constraint case, we check that the body has
kind Constraint; see the call to tc_check_lhs_type. (In contrast, for
types we check that the body has kind TYPE kappa for some fresh unification
variable kappa.)
Reason: we don't yet have support for constraints that are not lifted: it's
not possible to declare a class returning a different type than CONSTRAINT LiftedRep.
Evidence is always lifted, the fat arrow c => t requires c to be
a lifted constraint. In a far future, if we add support for non-lifted
constraints, we could allow c1 => c2 where
c1 :: CONSTRAINT rep1, c2 :: CONSTRAINT rep2
have arbitrary representations rep1 and rep2.

Note [Inferring tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Give a tuple type (a,b,c), which the parser labels as HsBoxedOrConstraintTuple,
we try to figure out whether it's a tuple of kind * or Constraint.
  Step 1: look at the expected kind
  Step 2: infer argument kinds

If after Step 2 it's not clear from the arguments that it's
Constraint, then it must be *.  Once having decided that we re-check
the arguments to give good error messages in
  e.g.  (Maybe, Maybe)

Note that we will still fail to infer the correct kind in this case:

  type T a = ((a,a), D a)
  type family D :: Constraint -> Constraint

While kind checking T, we do not yet know the kind of D, so we will default the
kind of T to * -> *. It works if we annotate `a` with kind `Constraint`.

Note [Desugaring types]
~~~~~~~~~~~~~~~~~~~~~~~
The type desugarer is phase 2 of dealing with HsTypes.  Specifically:

  * It transforms from HsType to Type

  * It zonks any kinds.  The returned type should have no mutable kind
    or type variables (hence returning Type not TcType):
      - any unconstrained kind variables are defaulted to (Any @Type) just
        as in GHC.Tc.Zonk.Type.
      - there are no mutable type variables because we are
        kind-checking a type
    Reason: the returned type may be put in a TyCon or DataCon where
    it will never subsequently be zonked.

You might worry about nested scopes:
        ..a:kappa in scope..
            let f :: forall b. T '[a,b] -> Int
In this case, f's type could have a mutable kind variable kappa in it;
and we might then default it to (Any @Type) when dealing with f's type
signature.  But we don't expect this to happen because we can't get a
lexically scoped type variable with a mutable kind variable in it.  A
delicate point, this.  If it becomes an issue we might need to
distinguish top-level from nested uses.

Moreover
  * it cannot fail,
  * it does no unifications
  * it does no validity checking, except for structural matters, such as
        (a) spurious ! annotations.
        (b) a class used as a type

Note [Kind of a type splice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these terms, each with TH type splice inside:
     [| e1 :: Maybe $(..blah..) |]
     [| e2 :: $(..blah..) |]
When kind-checking the type signature, we'll kind-check the splice
$(..blah..); we want to give it a kind that can fit in any context,
as if $(..blah..) :: forall k. k.

In the e1 example, the context of the splice fixes kappa to *.  But
in the e2 example, we'll desugar the type, zonking the kind unification
variables as we go.  When we encounter the unconstrained kappa, we
want to default it to 'Type', not to (Any @Type).

-}

addTypeCtxt :: LHsType GhcRn -> TcM a -> TcM a
addTypeCtxt = addTypeCtxtWithSource Nothing

-- | Attach a type context, with the source argument when the renamer lifted it.
addTypeCtxtWithSource :: Maybe HsAppTypeSource -> LHsType GhcRn -> TcM a -> TcM a
        -- Wrap a context around only if we want to show that contexts.
        -- Omit invisible ones and ones user's won't grok
addTypeCtxtWithSource _ (L _ (HsWildCardTy _)) thing = thing -- "In the type '_'" just isn't helpful.
addTypeCtxtWithSource source ty thing = addErrCtxt (TypeCtxt source ty) thing



{- *********************************************************************
*                                                                      *
                Type-variable binders
*                                                                      *
********************************************************************* -}

bindNamedWildCardBinders :: [Name]
                         -> ([(Name, TcTyVar)] -> TcM a)
                         -> TcM a
-- Bring into scope the /named/ wildcard binders.  Remember that
-- plain wildcards _ are anonymous and dealt with by HsWildCardTy
-- Soe Note [The wildcard story for types] in GHC.Hs.Type
bindNamedWildCardBinders wc_names thing_inside
  = do { wcs <- mapM newNamedWildTyVar wc_names
       ; let wc_prs = wc_names `zip` wcs
       ; tcExtendNameTyVarEnv wc_prs $
         thing_inside wc_prs }

newNamedWildTyVar :: Name -> TcM TcTyVar
-- ^ New unification variable '_' for a wildcard
newNamedWildTyVar _name   -- Currently ignoring the "_x" wildcard name used in the type
  = do { kind <- newMetaKindVar
       ; details <- newMetaDetails TauTv
       ; wc_name <- newMetaTyVarName (fsLit "w")   -- See Note [Wildcard names]
       ; let tyvar = mkTcTyVar wc_name kind details
       ; traceTc "newWildTyVar" (ppr tyvar)
       ; return tyvar }

---------------------------
tcAnonWildCardOcc :: IsExtraConstraint
                  -> TcTyMode -> HsType GhcRn -> Kind -> TcM TcType
tcAnonWildCardOcc is_extra mode@(TcTyMode { mode_holes = Just (hole_lvl, hole_mode) })
                  ty exp_kind
    -- hole_lvl: see Note [Checking partial type signatures]
    --           esp the bullet on nested forall types
  = do { kv_details <- newTauTvDetailsAtLevel hole_lvl
       ; kv_name    <- newMetaTyVarName (fsLit "k")
       ; wc_details <- newTauTvDetailsAtLevel hole_lvl
       ; wc_name    <- newMetaTyVarName wc_nm
       ; let kv      = mkTcTyVar kv_name liftedTypeKind kv_details
             wc_kind = mkTyVarTy kv
             wc_tv   = mkTcTyVar wc_name wc_kind wc_details

       ; traceTc "tcAnonWildCardOcc" (ppr hole_lvl <+> ppr emit_holes)
       ; when emit_holes $
         emitAnonTypeHole is_extra wc_tv
         -- Why the 'when' guard?
         -- See Note [Wildcards in visible kind application]

       -- You might think that this would always just unify
       -- wc_kind with exp_kind, so we could avoid even creating kv
       -- But the level numbers might not allow that unification,
       -- so we have to do it properly (T14140a)
       ; checkExpectedKind mode ty (mkTyVarTy wc_tv) wc_kind exp_kind }
  where
     -- See Note [Wildcard names]
     wc_nm = case hole_mode of
               HM_Sig      -> fsLit "w"
               HM_FamPat{} -> fsLit "_"
               HM_VTA      -> fsLit "w"
               HM_TyAppPat -> fsLit "_"

     emit_holes = case hole_mode of
                     HM_Sig     -> True
                     HM_FamPat{} -> False
                     HM_VTA     -> False
                     HM_TyAppPat -> False

tcAnonWildCardOcc is_extra _ _ _
-- mode_holes is Nothing. This means we have an anonymous wildcard
-- in an unexpected place. The renamer rejects these wildcards in 'checkAnonWildcard',
-- but it is possible for a wildcard to be introduced by a Template Haskell splice,
-- as per #15433. To account for this, we throw a generic catch-all error message.
  = failWith $ TcRnIllegalWildcardInType Nothing reason
    where
      reason =
        case is_extra of
          YesExtraConstraint ->
            ExtraConstraintWildcardNotAllowed
              SoleExtraConstraintWildcardNotAllowed
          NoExtraConstraint  ->
            WildcardsNotAllowedAtAll

{- Note [Wildcard names]
~~~~~~~~~~~~~~~~~~~~~~~~
So we hackily use the mode_holes flag to control the name used
for wildcards:

* For proper holes (whether in a visible type application (VTA) or no),
  we rename the '_' to 'w'. This is so that we see variables like 'w0'
  or 'w1' in error messages, a vast improvement upon '_0' and '_1'. For
  example, we prefer
       Found type wildcard ‘_’ standing for ‘w0’
  over
       Found type wildcard ‘_’ standing for ‘_1’

  Even in the VTA case, where we do not emit an error to be printed, we
  want to do the renaming, as the variables may appear in other,
  non-wildcard error messages.

* However, holes in the left-hand sides of type families ("type
  patterns") stand for type variables which we do not care to name --
  much like the use of an underscore in an ordinary term-level
  pattern. When we spot these, we neither wish to generate an error
  message nor to rename the variable.  We don't rename the variable so
  that we can pretty-print a type family LHS as, e.g.,
    F _ Int _ = ...
  and not
     F w1 Int w2 = ...

  See also Note [Wildcards in family instances] in
  GHC.Rename.Module. The choice of HM_FamPat is made in
  tcFamTyPats. There is also some unsavory magic, relying on that
  underscore, in GHC.Core.Coercion.tidyCoAxBndrsForUser.

Note [Wildcards in visible kind application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are cases where users might want to pass in a wildcard as a visible kind
argument, for instance:

data T :: forall k1 k2. k1 → k2 → Type where
  MkT :: T a b
x :: T @_ @Nat False n
x = MkT

So we should allow '@_' without emitting any hole constraints, and
regardless of whether PartialTypeSignatures is enabled or not. But how
would the typechecker know which '_' is being used in VKA and which is
not when it calls emitNamedTypeHole in
tcHsPartialSigType on all HsWildCardBndrs?  The solution is to neither
rename nor include unnamed wildcards in HsWildCardBndrs, but instead
give every anonymous wildcard a fresh wild tyvar in tcAnonWildCardOcc.

And whenever we see a '@', we set mode_holes to HM_VKA, so that
we do not call emitAnonTypeHole in tcAnonWildCardOcc.
See related Note [Wildcards in visible type application] here and
Note [The wildcard story for types] in GHC.Hs.Type
-}

{- *********************************************************************
*                                                                      *
             Kind inference for type declarations
*                                                                      *
********************************************************************* -}

-- See Note [kcCheckDeclHeader vs kcInferDeclHeader]
data InitialKindStrategy
  = InitialKindCheck SAKS_or_CUSK
  | InitialKindInfer

-- Does the declaration have a standalone kind signature (SAKS) or a complete
-- user-supplied kind (CUSK)?
data SAKS_or_CUSK
  = SAKS Kind  -- Standalone kind signature, fully zonked! (zonkTcTypeToType)
  | CUSK       -- Complete user-supplied kind (CUSK)

instance Outputable SAKS_or_CUSK where
  ppr (SAKS k) = text "SAKS" <+> ppr k
  ppr CUSK = text "CUSK"

-- See Note [kcCheckDeclHeader vs kcInferDeclHeader]
kcDeclHeader
  :: InitialKindStrategy
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind
  -> TcM TcTyCon        -- ^ A suitably-kinded TcTyCon
kcDeclHeader (InitialKindCheck msig) = kcCheckDeclHeader msig
kcDeclHeader InitialKindInfer = kcInferDeclHeader False

-- | Infer the header of a generated sum payload family.
-- See Note [Provisional kinds of sum payloads] in GHC.Tc.TyCl.
kcInferSumPayloadHeader :: Name -> LHsQTyVars GhcRn -> TcM ContextKind -> TcM MonoTcTyCon
kcInferSumPayloadHeader name = kcInferDeclHeader True name ClosedTypeFamilyFlavour

{- Note [kcCheckDeclHeader vs kcInferDeclHeader]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kcCheckDeclHeader and kcInferDeclHeader are responsible for getting the initial kind
of a type constructor.

* kcCheckDeclHeader: the TyCon has a standalone kind signature or a CUSK. In that
  case, find the full, final, poly-kinded kind of the TyCon.  It's very like a
  term-level binding where we have a complete type signature for the function.

* kcInferDeclHeader: the TyCon has neither a standalone kind signature nor a
  CUSK. Find a monomorphic kind, with unification variables in it; they will be
  generalised later.  It's very like a term-level binding where we do not have a
  type signature (or, more accurately, where we have a partial type signature),
  so we infer the type and generalise.
-}

------------------------------
kcCheckDeclHeader
  :: SAKS_or_CUSK
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind. AnyKind == no result signature
  -> TcM PolyTcTyCon    -- ^ A suitably-kinded generalized TcTyCon
kcCheckDeclHeader (SAKS sig) = kcCheckDeclHeader_sig sig
kcCheckDeclHeader CUSK       = kcCheckDeclHeader_cusk

kcCheckDeclHeader_cusk
  :: Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind
  -> TcM PolyTcTyCon    -- ^ A suitably-kinded generalized TcTyCon
kcCheckDeclHeader_cusk name flav
              (HsQTvs { hsq_ext = kv_ns
                      , hsq_explicit = hs_tvs }) kc_res_ki
  -- CUSK case
  -- See Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
  = addErrCtxt (TyConDeclCtxt name flav) $
    do { skol_info <- mkSkolemInfo skol_info_anon
       ; (tclvl, wanted, (scoped_kvs, (tc_bndrs, res_kind)))
           <- pushLevelAndSolveEqualitiesX "kcCheckDeclHeader_cusk" $
              bindImplicitTKBndrs_Q_Skol skol_info kv_ns                      $
              bindExplicitTKBndrs_Q_Skol skol_info ctxt_kind hs_tvs           $
              newExpectedKind =<< kc_res_ki

           -- Now, because we're in a CUSK,
           -- we quantify over the mentioned kind vars
       ; let spec_req_tkvs = scoped_kvs ++ binderVars tc_bndrs
             all_kinds     = res_kind : map tyVarKind spec_req_tkvs

       ; candidates <- candidateQTyVarsOfKinds all_kinds
             -- 'candidates' are all the variables that we are going to
             -- skolemise and then quantify over.  We do not include spec_req_tvs
             -- because they are /already/ skolems

       ; inferred <- quantifyTyVars skol_info DefaultNonStandardTyVars $
                     candidates `delCandidates` spec_req_tkvs
                     -- NB: 'inferred' comes back sorted in dependency order

       ; (scoped_kvs, tc_bndrs, res_kind) <- liftZonkM $
          do { scoped_kvs <- mapM zonkTyCoVarKind scoped_kvs      -- scoped_kvs and tc_bndrs are skolems,
             ; tc_bndrs   <- mapM zonkTyCoVarBndrKind tc_bndrs    -- so zonkTyCoVarBndrKind suffices
             ; res_kind   <- zonkTcType res_kind
             ; return (scoped_kvs, tc_bndrs, res_kind) }

       ; let mentioned_kv_set = candidateKindVars candidates
             specified        = scopedSort scoped_kvs
                                -- NB: maintain the L-R order of scoped_kvs

             all_tcbs =  mkNamedTyConBinders Inferred  inferred
                      ++ mkNamedTyConBinders Specified specified
                      ++ map (mkExplicitTyConBinder mentioned_kv_set) tc_bndrs

             -- The user-written kind, before eta-expansion
             user_kind :: TcKind
             user_kind = mkTyConKindWithMatchability
                           (tyConFlavourMatchability flav) all_tcbs res_kind

       -- Eta expand if necessary; we are building a PolyTyCon
       ; (eta_tcbs, res_kind) <- maybeEtaExpandAlgTyCon flav skol_info all_tcbs res_kind

       ; let all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ binderVars tc_bndrs)
             final_tcbs = all_tcbs `chkAppend` eta_tcbs
             tycon = mkTcTyCon name user_kind final_tcbs (length eta_tcbs) res_kind all_tv_prs
                               True -- Make a PolyTcTyCon, fully generalised
                               flav

       ; reportUnsolvedEqualities skol_info (binderVars final_tcbs)
                                  tclvl wanted

         -- If the ordering from
         -- Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
         -- doesn't work, we catch it here, before an error cascade
       ; checkTyConTelescope tycon

       ; traceTc "kcCheckDeclHeader_cusk" $
         vcat [ text "name" <+> ppr name
              , text "user_kind" <+> ppr user_kind
              , text "candidates" <+> ppr candidates
              , text "mentioned_kv_set" <+> ppr mentioned_kv_set
              , text "kv_ns" <+> ppr kv_ns
              , text "hs_tvs" <+> ppr hs_tvs
              , text "scoped_kvs" <+> ppr scoped_kvs
              , text "spec_req_tvs" <+> pprTyVars spec_req_tkvs
              , text "all_kinds" <+> ppr all_kinds
              , text "tc_tvs" <+> pprTyVars (binderVars tc_bndrs)
              , text "res_kind" <+> ppr res_kind
              , text "inferred" <+> ppr inferred
              , text "specified" <+> ppr specified
              , text "final_tcbs" <+> ppr final_tcbs
              , text "mkTyConKind final_tc_bndrs res_kind"
                <+> ppr (mkTyConKind final_tcbs res_kind)
              , text "all_tv_prs" <+> ppr all_tv_prs ]

       ; return tycon }
  where
    skol_info_anon = TyConSkol flav name
    ctxt_kind | tcFlavourIsOpen flav = TheKind liftedTypeKind
              | otherwise            = AnyKind

-- | Create a TyConBinder for a user-written type variable binder.
mkExplicitTyConBinder :: TyCoVarSet -- variables that are used dependently
                      -> VarBndr TyVar (HsBndrVis GhcRn)
                      -> TyConBinder
mkExplicitTyConBinder dep_set (Bndr tv flag) =
  case flag of
    HsBndrRequired{}  -> mkRequiredTyConBinder dep_set tv
    HsBndrInvisible{} -> mkNamedTyConBinder Specified tv

-- | Kind-check a 'LHsQTyVars'. Used in 'inferInitialKind' (for tycon kinds and
-- other kinds).
--
-- This function does not do telescope checking.
kcInferDeclHeader
  :: Bool               -- ^ Generated sum payload family
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn
  -> TcM ContextKind    -- ^ The result kind
  -> TcM MonoTcTyCon    -- ^ A suitably-kinded non-generalized TcTyCon
kcInferDeclHeader is_sum name flav
              (HsQTvs { hsq_ext = kv_ns
                      , hsq_explicit = hs_bndrs }) kc_res_ki
  -- No standalone kind signature and no CUSK.
  -- See Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
  = addErrCtxt (TyConDeclCtxt name flav) $
    do { rejectInvisibleBinders name hs_bndrs
       ; (scoped_kvs, (tc_bndrs, res_kind))
           -- Why bindImplicitTKBndrs_Q_Tv which uses newTyVarTyVar?
           -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
           <- bindImplicitTKBndrs_Q_Tv kv_ns              $
              bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                              , sm_tvtv = SMDTyVarTv, sm_kind = ctxt_kind
                                              , sm_family_metadata = tcFlavourIsOpen flav }) hs_bndrs $
              newExpectedKind =<< kc_res_ki
              -- Why "_Tv" not "_Skol"? See third wrinkle in
              -- Note [Inferring kinds for type declarations] in GHC.Tc.TyCl,

       ; let   -- NB: Don't add scoped_kvs to tyConTyVars, because they
               -- might unify with kind vars in other types in a mutually
               -- recursive group.
               -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl

             tc_tvs = binderVars tc_bndrs
               -- Discard visibility flags. We made sure all of them are HsBndrRequired
               -- by the call to rejectInvisibleBinders above.

             tc_binders
               | is_sum
               = map (mkRequiredTyConBinder (tyCoVarsOfTypes (map tyVarKind tc_tvs))) tc_tvs
               | otherwise
               = mkAnonTyConBinders tc_tvs
               -- The sum payload path keeps dependencies already present in
               -- the checked header. See Note [Provisional kinds of sum
               -- payloads] in GHC.Tc.TyCl.
               --
               -- Ordinary declarations must use mkAnonTyConBinders.
               -- See Note [No polymorphic recursion in type decls] in GHC.Tc.TyCl
               --
               -- Also, note that tc_binders has the tyvars from only the
               -- user-written type variable binders.
               -- See S1 Note [TcTyCon, MonoTcTyCon, and PolyTcTyCon] in GHC.Tc.TyCl

             all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
               -- NB: bindExplicitTKBndrs_Q_Tv does not clone;
               --     ditto Implicit
               -- See Note [Cloning for type variable binders]

             kind = mkTyConKindWithMatchability
                      (tyConFlavourMatchability flav) tc_binders res_kind
             tycon = mkTcTyCon name kind tc_binders 0 res_kind all_tv_prs
                               False -- Make a MonoTcTyCon
                               flav

       ; traceTc "kcInferDeclHeader: not-cusk" $
         vcat [ text "name:" <+> ppr name
              , text "kind:" <+> ppr kind
              , text "kv_ns:" <+>  ppr kv_ns
              , text "hs_bndrs:" <+> ppr hs_bndrs
              , text "scoped_kvs:" <+> ppr scoped_kvs
              , text "tc_tvs:" <+> ppr tc_tvs
              , text "tc_binders:" <+> ppr tc_binders
              , text "res_kind:" <+> ppr res_kind
              ]
       ; return tycon }
  where
    ctxt_kind | tcFlavourIsOpen flav = TheKind liftedTypeKind
              | otherwise            = AnyKind

-- rejectInvisibleBinders is called on on the inference code path; there is
-- no standalone kind signature, nor CUSK.
-- See Note [No inference for invisible binders in type decls] in GHC.Tc.TyCl
rejectInvisibleBinders :: Name -> [LHsTyVarBndr (HsBndrVis GhcRn) GhcRn] -> TcM ()
rejectInvisibleBinders name = mapM_ check_bndr_vis
  where
    check_bndr_vis :: LHsTyVarBndr (HsBndrVis GhcRn) GhcRn -> TcM ()
    check_bndr_vis bndr =
      when (isHsBndrInvisible (hsTyVarBndrFlag (unLoc bndr))) $
        addErr (TcRnInvisBndrWithoutSig name bndr)

-- | Kind-check a declaration header against a standalone kind signature.
-- See Note [kcCheckDeclHeader_sig]
kcCheckDeclHeader_sig
  :: Kind               -- ^ Standalone kind signature, fully zonked! (zonkTcTypeToType)
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind. AnyKind == no result signature
  -> TcM PolyTcTyCon    -- ^ A suitably-kinded, fully generalised TcTyCon
-- Postcondition to (kcCheckDeclHeader_sig sig_kind n f hs_tvs kc_res_ki):
--   kind(returned PolyTcTyCon) = sig_kind
--
kcCheckDeclHeader_sig sig_kind name flav
          (HsQTvs { hsq_ext      = implicit_nms
                  , hsq_explicit = hs_tv_bndrs }) kc_res_ki
  = addErrCtxt (TyConDeclCtxt name flav) $
    do { skol_info <- mkSkolemInfo (TyConSkol flav name)
       ; let avoid_occs = map nameOccName (hsLTyVarNames hs_tv_bndrs)
       ; (sig_ma_tcbs :: [(Type, TcTyConBinder)], sig_res_kind :: Kind)
             <- splitTyConKind skol_info emptyInScopeSet
                               avoid_occs sig_kind
       ; let sig_tcbs = map snd sig_ma_tcbs

       ; traceTc "kcCheckDeclHeader_sig {" $
           vcat [ text "sig_kind:" <+> ppr sig_kind
                , text "sig_tcbs:" <+> ppr sig_tcbs
                , text "sig_res_kind:" <+> ppr sig_res_kind
                , text "implict_nms:" <+> ppr implicit_nms
                , text "hs_tv_bndrs:" <+> ppr hs_tv_bndrs ]

       ; (tclvl, wanted, (implicit_tvs, (skol_tcbs, skol_scoped_tvs, (extra_tcbs, tycon_res_kind))))
           <- pushLevelAndSolveEqualitiesX "kcCheckDeclHeader_sig" $  -- #16687
              bindImplicitTKBndrs_Q_Tv implicit_nms                $  -- Q means don't clone
              matchUpSigWithDecl name sig_ma_tcbs sig_res_kind hs_tv_bndrs $ \ excess_ma_tcbs sig_res_kind ->
              do { -- Kind-check the result kind annotation, if present:
                   --    data T a b :: res_ki where ...
                   --               ^^^^^^^^^
                   -- We do it here because at this point the environment has been
                   -- extended with both 'implicit_tcv_prs' and 'explicit_tv_prs'.
                   --
                   -- Also see Note [Arity of type families and type synonyms]
                 ; res_kind :: ContextKind <- kc_res_ki

                 ; let (excess_mas, excess_sig_tcbs) = unzip excess_ma_tcbs
                       sig_res_kind' = mkPiTys
                         (zip excess_mas (tyConBindersPiTyBinders excess_sig_tcbs)) sig_res_kind

                 ; traceTc "kcCheckDeclHeader_sig 2" $
                    vcat [ text "excess_sig_tcbs" <+> ppr excess_sig_tcbs
                         , text "res_kind" <+> ppr res_kind
                         , text "sig_res_kind'" <+> ppr sig_res_kind'
                         ]

                 -- Unify res_ki (from the type declaration) with
                 -- sig_res_kind', the residual kind from the kind signature.
                 ; checkExpectedResKind sig_res_kind' res_kind

                 -- Add more binders for data/newtype, so the result kind has no arrows
                 -- See Note [Datatype return kinds]
                 ; if null excess_sig_tcbs || not (needsEtaExpansion flav)
                   then return ([],              sig_res_kind')
                   else return (excess_sig_tcbs, sig_res_kind)
          }


        -- Check that there are no unsolved equalities
        ; let all_tcbs = skol_tcbs ++ extra_tcbs
        ; reportUnsolvedEqualities skol_info (binderVars all_tcbs) tclvl wanted

        -- Check that distinct binders map to distinct tyvars (see #20916). For example
        --    type T :: k -> k -> Type
        --    data T (a::p) (b::q) = ...
        -- Here p and q both map to the same kind variable k.  We don't allow this
        -- so we must check that they are distinct.  A similar thing happens
        -- in GHC.Tc.TyCl.swizzleTcTyConBinders during inference.
        --
        -- With visible dependent quantification, one of the binders involved
        -- may be explicit.  Consider #24604
        --    type UF :: forall zk -> zk -> Constraint
        --    class UF kk (xb :: k)
        -- Here `k` and `kk` both denote the same variable; but only `k` is implicit
        -- Hence we need to add skol_scoped_tvs
        ; implicit_tvs <- liftZonkM $ zonkTcTyVarsToTcTyVars implicit_tvs
        ; let implicit_prs = implicit_nms `zip` implicit_tvs
              dup_chk_prs  = implicit_prs ++ mkTyVarNamePairs skol_scoped_tvs
        ; unless (null implicit_nms) $  -- No need if no implicit tyvars
          checkForDuplicateScopedTyVars dup_chk_prs
        ; checkForDisconnectedScopedTyVars name flav all_tcbs implicit_prs

        -- Swizzle the Names so that the TyCon uses the user-declared implicit names
        -- E.g  type T :: k -> Type
        --      data T (a :: j) = ....
        -- We want the TyConBinders of T to be [j, a::j], not [k, a::k]
        -- Why? So that the TyConBinders of the TyCon will lexically scope over the
        -- associated types and methods of a class.
        ; let swizzle_env = mkVarEnv (map swap implicit_prs)
              (subst, swizzled_tcbs) = mapAccumL (swizzleTcb swizzle_env) emptySubst all_tcbs
              swizzled_res_kind      = substTy subst tycon_res_kind
              all_tv_prs             = mkTyVarNamePairs (binderVars swizzled_tcbs)

        ; traceTc "kcCheckDeclHeader swizzle" $ vcat
          [ text "sig_kind ="          <+> debugPprType sig_kind
          , text "sig_tcbs ="          <+> ppr sig_tcbs
          , text "implicit_prs ="      <+> ppr implicit_prs
          , text "hs_tv_bndrs ="       <+> ppr hs_tv_bndrs
          , text "all_tcbs ="          <+> pprTyVars (binderVars all_tcbs)
          , text "swizzled_tcbs ="     <+> pprTyVars (binderVars swizzled_tcbs)
          , text "tycon_res_kind ="    <+> ppr tycon_res_kind
          , text "subst ="             <+> ppr subst
          , text "swizzled_res_kind =" <+> ppr swizzled_res_kind ]

        -- Build the final, generalized PolyTcTyCon
        -- NB: all_tcbs must bind the tyvars in the range of all_tv_prs
        --     because the tv_prs is used when (say) typechecking the RHS of
        --     a type synonym.
        ; let
            tc = mkTcTyCon name sig_kind swizzled_tcbs (length extra_tcbs) swizzled_res_kind all_tv_prs
                 True -- Make a PolyTcTyCon, fully generalised
                 flav

        ; traceTc "kcCheckDeclHeader_sig }" $ vcat
          [ text "tyConName =" <+> ppr (tyConName tc)
          , text "tyConKind =" <+> debugPprType (tyConKind tc)
          , text "tyConBinders = " <+> ppr (tyConBinders tc)
          , text "tyConResKind" <+> debugPprType (tyConResKind tc)
          ]
        ; return tc }

-- | Check the result kind annotation on a type constructor against
-- the corresponding section of the standalone kind signature.
-- Drops invisible binders that interfere with unification.
checkExpectedResKind :: TcKind       -- ^ the result kind from the separate kind signature
                     -> ContextKind  -- ^ the result kind from the declaration header
                     -> TcM ()
checkExpectedResKind _ AnyKind
  = return ()  -- No signature in the declaration header
checkExpectedResKind sig_kind res_ki
  = do { actual_res_ki <- newExpectedKind res_ki

       ; let -- Drop invisible binders from sig_kind until they match up
             -- with res_ki.  By analogy with checkExpectedKind.
             n_res_invis_bndrs = invisibleBndrCount actual_res_ki
             n_sig_invis_bndrs = invisibleBndrCount sig_kind
             n_to_inst         = n_sig_invis_bndrs - n_res_invis_bndrs

             (_, _, sig_kind') = splitInvisPiTysN n_to_inst sig_kind

       ; discardResult $ unifyKind Nothing sig_kind' actual_res_ki }

matchUpSigWithDecl
  :: Name                        -- Name of the type constructor for error messages
  -> [(Type, TcTyConBinder)]     -- Metadata and skolem binders from the separate kind signature
  -> TcKind                      -- The tail end of the kind signature
  -> [LHsTyVarBndr (HsBndrVis GhcRn) GhcRn]     -- User-written binders in decl
  -> ([(Type, TcTyConBinder)] -> TcKind -> TcM a)  -- All user-written binders are in scope
                                           --   Argument is excess TyConBinders and tail kind
  -> TcM ( [TcTyConBinder]       -- Skolemised binders, with TcTyVars
         , [TcTyVar]             -- Skolem tyvars brought into lexical scope by this matching-up
         , a )
-- See Note [Matching a kind signature with a declaration]
-- Invariant: Length of returned TyConBinders + length of excess TyConBinders
--            = length of incoming TyConBinders
matchUpSigWithDecl name sig_ma_tcbs sig_res_kind hs_bndrs thing_inside
  = go emptySubst sig_ma_tcbs hs_bndrs
  where
    go subst ma_tcbs []
      = do { let (mas, tcbs) = unzip ma_tcbs
                 (subst', tcbs') = substTyConBindersX subst tcbs
           ; res <- thing_inside (zip (map (substTy subst') mas) tcbs')
                                  (substTy subst' sig_res_kind)
           ; return ([], [], res) }

    go _ [] hs_bndrs
      = failWithTc (TcRnTooManyBinders sig_res_kind hs_bndrs)

    go subst ((_ma, tcb) : ma_tcbs') hs_bndrs@(hs_bndr : hs_bndrs')
      | zippableTyConBinder (binderFlag tcb) (hsTyVarBndrFlag (unLoc hs_bndr))
      = -- Visible TyConBinder, so match up with the hs_bndrs
        do { let Bndr tv vis = tcb
                 tv' = updateTyVarKind (substTy subst) $
                       maybe tv (setTyVarName tv) (hsLTyVarName hs_bndr)
                   -- Give the skolem the Name of the HsTyVarBndr, so that if it
                   -- appears in an error message it has a name and binding site
                   -- that come from the type declaration, not the kind signature
                 subst' = extendTCvSubstWithClone subst tv tv'
           ; tc_hs_bndr (unLoc hs_bndr) (tyVarKind tv')
           ; traceTc "musd1" (ppr tcb $$ ppr hs_bndr $$ ppr tv')
           ; (tcbs', tvs, res) <- tcExtendTyVarEnv [tv'] $
                                  go subst' ma_tcbs' hs_bndrs'
           ; return (Bndr tv' vis : tcbs', tv':tvs, res) }
             -- We do a tcExtendTyVarEnv [tv'], so we return tv' in
             -- the list of lexically-scoped skolem type variables

      | skippableTyConBinder (binderFlag tcb)
      = -- Invisible TyConBinder, so do not consume one of the hs_bndrs
        do { let (subst', tcb') = substTyConBinderX subst tcb
           ; traceTc "musd2" (ppr tcb $$ ppr hs_bndr $$ ppr tcb')
           ; (tcbs', tvs, res) <- go subst' ma_tcbs' hs_bndrs
                   -- NB: pass on hs_bndrs unchanged; we do not consume a
                   --     HsTyVarBndr for an invisible TyConBinder
           ; return (tcb' : tcbs', tvs, res) }
                   -- Return `tvs`; no new lexically-scoped TyVars brought into scope

      | otherwise =
          -- At this point we conclude that:
          --   * the quantifier (tcb) is visible: (ty -> ...), (forall a -> ...)
          --   * the binder (hs_bndr) is invisible: @t, @(t :: k)
          -- Any other combination should have been handled by the zippable/skippable clauses.
          failWithTc (TcRnInvalidInvisTyVarBndr name hs_bndr)

    tc_hs_bndr :: HsTyVarBndr (HsBndrVis GhcRn) GhcRn -> TcKind -> TcM ()
    tc_hs_bndr (HsTvb { tvb_kind = HsBndrNoKind _ }) _ = return ()
    tc_hs_bndr (HsTvb { tvb_kind = HsBndrKind _ kind, tvb_var = bvar })
               expected_kind
      = do { traceTc "musd3:unifying" (ppr kind $$ ppr expected_kind)
           ; tcHsTvbKind bvar kind expected_kind }

-- | Match a declaration binder to a signature binder, as in proposal 425.
zippableTyConBinder :: TyConBndrVis -> HsBndrVis GhcRn -> Bool
zippableTyConBinder vis (HsBndrInvisible _) = isInvisSpecTcbVis vis
zippableTyConBinder vis (HsBndrRequired _)  = isVisibleTcbVis vis

-- | Skip an invisible signature binder while a declaration binder remains.
skippableTyConBinder :: TyConBndrVis -> Bool
skippableTyConBinder vis = not (isVisibleTcbVis vis)

-- Check the kind of a type variable binder
tcHsTvbKind :: HsBndrVar GhcRn -> LHsKind GhcRn -> TcKind -> TcM ()
tcHsTvbKind bvar kind expected_kind =
  do { sig_kind <- tcLHsKindSig ctxt kind
     ; traceTc "tcHsTvbKind:unifying" (ppr sig_kind $$ ppr expected_kind)
     ; discardResult $ -- See Note [discardResult in tcHsTvbKind]
       unifyKind mb_thing sig_kind expected_kind }
  where
    (ctxt, mb_thing) = case bvar of
      HsBndrVar _ (L _ hs_nm) -> (TyVarBndrKindCtxt hs_nm, Just (NameThing hs_nm))
      HsBndrWildCard _ -> (KindSigCtxt, Nothing)

substTyConBinderX :: Subst -> TyConBinder -> (Subst, TyConBinder)
substTyConBinderX subst (Bndr tv vis)
  = (subst', Bndr tv' vis)
  where
    (subst', tv') = substTyVarBndr subst tv

substTyConBindersX :: Subst -> [TyConBinder] -> (Subst, [TyConBinder])
substTyConBindersX = mapAccumL substTyConBinderX

swizzleTcb :: VarEnv Name -> Subst -> TyConBinder -> (Subst, TyConBinder)
swizzleTcb swizzle_env subst (Bndr tv vis)
  = (subst', Bndr tv2 vis)
  where
    subst' = extendTCvSubstWithClone subst tv tv2
    tv1 = updateTyVarKind (substTy subst) tv
    tv2 = case lookupVarEnv swizzle_env tv of
             Just user_name -> setTyVarName tv1 user_name
             Nothing        -> tv1
    -- NB: the SrcSpan on an implicitly-bound name deliberately spans
    -- the whole declaration. e.g.
    --    data T (a :: k) (b :: Type -> k) = ....
    -- There is no single binding site for 'k'.
    -- See Note [Source locations for implicitly bound type variables]
    -- in GHC.Tc.Rename.HsType

{- Note [kcCheckDeclHeader_sig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a kind signature 'sig_kind' and a declaration header,
kcCheckDeclHeader_sig verifies that the declaration conforms to the
signature. The end result is a PolyTcTyCon 'tc' such that:
  tyConKind tc == sig_kind

Basic plan is this:
  * splitTyConKind: Take the Kind from the separate kind signature, and
    decompose it all the way to a [TyConBinder] and a Kind in the corner.

    NB: these TyConBinders contain TyVars, not TcTyVars.

  * matchUpSigWithDecl: match the [TyConBinder] from the signature with
    the [LHsTyVarBndr (HsBndrVis GhcRn) GhcRn] from the declaration.  The latter are the
    explicit, user-written binders.  e.g.
        data T (a :: k) b = ....
    There may be more of the former than the latter, because the former
    include invisible binders.  matchUpSigWithDecl uses isVisibleTcbVis
    to decide which TyConBinders are visible.

  * matchUpSigWithDecl also skolemises the [TyConBinder] to produce
    a [TyConBinder], corresponding 1-1 with the consumed [TyConBinder].
    Each new TyConBinder
      - Uses the Name from the LHsTyVarBndr, if available, both because that's
        what the user expects, and because the binding site accurately comes
        from the data/type declaration.
      - Uses a skolem TcTyVar.  We need these to allow unification.

  * machUpSigWithDecl also unifies the user-supplied kind signature for each
    LHsTyVarBndr with the kind that comes from the TyConBinder (itself coming
    from the separate kind signature).

  * Finally, kcCheckDeclHeader_sig unifies the return kind of the separate
    signature with the kind signature (if any) in the data/type declaration.
    E.g.
           type S :: forall k. k -> k -> Type
           type family S (a :: j) :: j -> Type
    Here we match up the 'k ->' with (a :: j); and then must unify the leftover
    part of the signature (k -> Type) with the kind signature of the decl,
    (j -> Type).  This unification, done in kcCheckDeclHeader, needs TcTyVars.

Note [Arity of type families and type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  type F0 :: forall k. k -> k -> Type
  type family F0

  type F1 :: forall k. k -> k -> Type
  type family F1 @k

  type F2a :: forall k. k -> k -> Type
  type family F2a @k a

  type F2b :: forall k. k -> k -> Type
  type family F2b a

  type F3 :: forall k. k -> k -> Type
  type family F3 a b

All five have the same /kind/, but what /arity/ do they have?
For a type family, the arity is critical:
* A type family must always appear saturated (up to its arity)
* A type family can match only on `arity` arguments, not further ones
* The arity is recorded by `tyConArity`, and is equal to the number of
  `TyConBinders` in the `TyCon`.
* In this context "arity" includes both kind and type arguments.

The arity is not determined by the kind signature (all five have the same signature).
Rather, it is determined by the declaration of the family:
* `F0` has arity 0.
* `F1` has arity 1.
* `F2a` has arity 2.
* `F2b` also has arity 2: the kind argument is invisible.
* `F3` has arity 3; again the kind argument is invisible.

The matching-up of kind signature with the declaration itself is done by
`matchUpWithSigDecl`.

Note [discardResult in tcHsTvbKind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use 'unifyKind' to check inline kind annotations in declaration headers
against the signature.

  type T :: [i] -> Maybe j -> Type
  data T (a :: [k1]) (b :: Maybe k2) :: Type where ...

Here, we will unify:

       [k1] ~ [i]
  Maybe k2  ~ Maybe j
      Type  ~ Type

The end result is that we fill in unification variables k1, k2:

    k1  :=  i
    k2  :=  j

We also validate that the user isn't confused:

  type T :: Type -> Type
  data T (a :: Bool) = ...

This will report that (Type ~ Bool) failed to unify.

Now, consider the following example:

  type family Id a where Id x = x
  type T :: Bool -> Type
  type T (a :: Id Bool) = ...

We will unify (Bool ~ Id Bool), and this will produce a non-reflexive coercion.
However, we are free to discard it, as the kind of 'T' is determined by the
signature, not by the inline kind annotation:

      we have   T ::    Bool -> Type
  rather than   T :: Id Bool -> Type

This (Id Bool) will not show up anywhere after we're done validating it, so we
have no use for the produced coercion.

Note [Kind variable ordering for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should be the kind of `T` in the following example? (#15591)

  class C (a :: Type) where
    type T (x :: f a)

As per Note [Ordering of implicit variables] in GHC.Rename.HsType, we want to quantify
the kind variables in left-to-right order of first occurrence in order to
support visible kind application. But we cannot perform this analysis on just
T alone, since its variable `a` actually occurs /before/ `f` if you consider
the fact that `a` was previously bound by the parent class `C`. That is to say,
the kind of `T` should end up being:

  T :: forall a f. f a -> Type

(It wouldn't necessarily be /wrong/ if the kind ended up being, say,
forall f a. f a -> Type, but that would not be as predictable for users of
visible kind application.)

In contrast, if `T` were redefined to be a top-level type family, like `T2`
below:

  type family T2 (x :: f (a :: Type))

Then `a` first appears /after/ `f`, so the kind of `T2` should be:

  T2 :: forall f a. f a -> Type

In order to make this distinction, we need to know (in kcCheckDeclHeader) which
type variables have been bound by the parent class (if there is one). With
the class-bound variables in hand, we can ensure that we always quantify
these first.
-}


{- *********************************************************************
*                                                                      *
             Expected kinds
*                                                                      *
********************************************************************* -}

-- | Describes the kind expected in a certain context.
data ContextKind = TheKind TcKind   -- ^ a specific kind
                 | AnyKind        -- ^ any kind will do
                 | OpenKind       -- ^ something of the form @TYPE _@

-- debug only
instance Outputable ContextKind where
  ppr AnyKind = text "AnyKind"
  ppr OpenKind = text "OpenKind"
  ppr (TheKind k) = text "TheKind" <+> ppr k

-----------------------
newExpectedKind :: ContextKind -> TcM TcKind
newExpectedKind (TheKind k)   = return k
newExpectedKind AnyKind       = newMetaKindVar
newExpectedKind OpenKind      = newOpenTypeKind

-----------------------
expectedKindInCtxt :: UserTypeCtxt -> ContextKind
-- Depending on the context, we might accept any kind (for instance, in a TH
-- splice), or only certain kinds (like in type signatures).
expectedKindInCtxt (TySynCtxt _)   = AnyKind
expectedKindInCtxt (GhciCtxt {})   = AnyKind
-- The types in a 'default' decl can have varying kinds
-- See Note [Extended defaults]" in GHC.Tc.Utils.Env
expectedKindInCtxt DefaultDeclCtxt     = AnyKind
expectedKindInCtxt DerivClauseCtxt     = AnyKind
expectedKindInCtxt TypeAppCtxt         = AnyKind
expectedKindInCtxt (ForSigCtxt _)      = TheKind liftedTypeKind
expectedKindInCtxt (InstDeclCtxt {})   = TheKind constraintKind
expectedKindInCtxt SpecInstCtxt        = TheKind constraintKind
expectedKindInCtxt _                   = OpenKind


{- *********************************************************************
*                                                                      *
          Scoped tyvars that map to the same thing
*                                                                      *
********************************************************************* -}

checkForDisconnectedScopedTyVars :: Name -> TyConFlavour TyCon -> [TcTyConBinder]
                                 -> [(Name,TcTyVar)] -> TcM ()
-- See Note [Disconnected type variables]
-- For the type synonym case see Note [Out of arity type variables]
-- `scoped_prs` is the mapping gotten by unifying
--    - the standalone kind signature for T, with
--    - the header of the type/class declaration for T
checkForDisconnectedScopedTyVars name flav all_tcbs scoped_prs
         -- needsEtaExpansion: see wrinkle (DTV1) in Note [Disconnected type variables]
  | needsEtaExpansion flav     = mapM_ report_disconnected (filterOut ok scoped_prs)
  | flav == TypeSynonymFlavour = mapM_ report_out_of_arity (filterOut ok scoped_prs)
  | otherwise = pure ()
  where
    all_tvs = mkVarSet (binderVars all_tcbs)
    ok (_, tc_tv) = tc_tv `elemVarSet` all_tvs

    report_disconnected :: (Name,TcTyVar) -> TcM ()
    report_disconnected (nm, _)
      = setSrcSpan (getSrcSpan nm) $
        addErrTc $ TcRnDisconnectedTyVar nm

    report_out_of_arity :: (Name,TcTyVar) -> TcM ()
    report_out_of_arity (tv_nm, _)
      = setSrcSpan (getSrcSpan tv_nm) $
        addErrTc $ TcRnOutOfArityTyVar name tv_nm

checkForDuplicateScopedTyVars :: [(Name,TcTyVar)] -> TcM ()
-- Check for duplicates
-- See Note [Aliasing in type and class declarations]
checkForDuplicateScopedTyVars scoped_prs
  = unless (null err_prs) $
    do { mapM_ report_dup err_prs; failM }
  where
    -------------- Error reporting ------------
    err_prs :: [(Name,Name)]
    err_prs = [ (n1,n2)
              | prs :: NonEmpty (Name,TyVar) <- findDupsEq ((==) `on` snd) scoped_prs
              , (n1,_) :| ((n2,_) : _) <- [NE.nubBy ((==) `on` fst) prs] ]
              -- This nubBy avoids bogus error reports when we have
              --    [("f", f), ..., ("f",f)....] in swizzle_prs
              -- which happens with  class C f where { type T f }

    report_dup :: (Name,Name) -> TcM ()
    report_dup (n1,n2)
      = setSrcSpan (getSrcSpan n2) $
        addErrTc $ TcRnDifferentNamesForTyVar n1 n2


{- Note [Aliasing in type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data SameKind (a::k) (b::k)
  data T1 (a::k1) (b::k2) c = MkT (SameKind a b) c
We do not allow this, because `k1` and `k2` would both stand for the same type
variable -- they are both aliases for `k`.

Other examples
  data T2 (a::k1)                 = MkT2 (SameKind a Int) -- k1 stands for Type
  data T3 @k1 @k2 (a::k1) (b::k2) = MkT (SameKind a b)    -- k1 and k2 are aliases

  type UF :: forall zk. zk -> Constraint
  class UF @kk (xb :: k) where   -- kk and k are aliases
    op :: (xs::kk) -> Bool

See #24604 for an example that crashed GHC.

There is a design choice here. It would be possible to allow implicit type variables
like `k1` and `k2` in T1's declartion to stand for /abitrary kinds/.  This is in fact
the rule we use in /terms/ pattern signatures:
    f :: [Int] -> Int
    f ((x::a) : xs) = ...
Here `a` stands for `Int`.  But in type /signatures/ we make a different choice:
    f1 :: forall (a::k1) (b::k2). SameKind a b -> blah
    f2 :: forall (a::k). SameKind a Int -> blah

Here f1's signature is rejected because `k1` and `k2` are aliased; and f2's is
rejected because `k` stands for `Int`.

Our current choice is that type and class declarations behave more like signatures;
we do not allow aliasing.  That is what `checkForDuplicateScopedTyVars` checks.
See !12328 for some design discussion.


Note [Disconnected type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This note applies when kind-checking the header of a type/class decl that has
a separate, standalone kind signature.  See #24083.

Consider:
   type S a = Type

   type C :: forall k. S k -> Constraint
   class C (a :: S kk) where
     op :: ...kk...

Note that the class has a separate kind signature, so the elaborated decl should
look like
   class C @kk (a :: S kk) where ...

But how can we "connect up" the scoped variable `kk` with the skolem kind from the
standalone kind signature for `C`?  In general we do this by unifying the two.
For example
   type T k = (k,Type)
   type W :: forall k. T k -> Type
   data W (a :: (x,Type)) = ..blah blah..

When we encounter (a :: (x,Type)) we unify the kind (x,Type) with the kind (T k)
from the standalone kind signature.  Of course, unification looks through synonyms
so we end up with the mapping [x :-> k] that connects the scoped type variable `x`
with the kind from the signature.

But in our earlier example this unification is ineffective -- because `S` is a
phantom synonym that just discards its argument.  So our plan is this:

  if matchUpSigWithDecl fails to connect `kk with `k`, by unification,
  we give up and complain about a "disconnected" type variable.

See #24083 for dicussion of alternatives, none satisfactory.  Also the fix is
easy: just add an explicit `@kk` parameter to the declaration, to bind `kk`
explicitly, rather than binding it implicitly via unification.

(DTV1) We only want to make this check when there /are/ scoped type variables; and
  that is determined by needsEtaExpansion.  Examples:

     type C :: x -> y -> Constraint
     class C a :: b -> Constraint where { ... }
     -- The a,b scope over the "..."

     type D :: forall k. k -> Type
     data family D :: kk -> Type
     -- Nothing for `kk` to scope over!

  In the latter data-family case, the match-up stuff in kcCheckDeclHeader_sig will
  return [] for `extra_tcbs`, and in fact `all_tcbs` will be empty.  So if we do
  the check-for-disconnected-tyvars check we'll complain that `kk` is not bound
  to one of `all_tcbs` (see #24083, comments about the `singletons` package).

  The scoped-tyvar stuff is needed precisely for data/class/newtype declarations,
  where needsEtaExpansion is True.

Note [Out of arity type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(Relevant ticket: #24470)
Type synonyms have a special scoping rule that allows implicit quantification in
the outermost kind signature:

  type P_e :: k -> Type
  type P_e @k = Proxy :: k -> Type     -- explicit binding

  type P_i    = Proxy :: k -> Type     -- implicit binding (relies on the special rule)

This is a deprecated feature (warning flag: -Wimplicit-rhs-quantification) but
we have to support it for a couple more releases. It is explained in more detail
in Note [Implicit quantification in type synonyms] in GHC.Rename.HsType.

Type synonyms `P_e` and `P_i` are equivalent.  Both of them have kind
`forall k. k -> Type` and arity 1. (Recall that the arity of a type synonym is
the number of arguments it requires at use sites; the arity matter because
unsaturated application of type families and type synonyms is not allowed).

We start to see problems when implicit RHS quantification (as in `P_i`) is
combined with a standalone king signature (like the one that `P_e` has).
That is:

  type P_i_sig :: k -> Type
  type P_i_sig = Proxy :: k -> Type

Per GHC Proposal #425, the arity of `P_i_sig` is determined /by the LHS only/,
which has no binders. So the arity of `P_i_sig` is 0.
At the same time, the legacy implicit quantification rule dictates that `k` is
brought into scope, as if there was a binder `@k` on the LHS.

We end up with a `k` that is in scope on the RHS but cannot be bound implicitly
on the LHS without affecting the arity. This led to #24470 (a compiler crash)

  GHC internal error: ‘k’ is not in scope during type checking,
                      but it passed the renamer

This problem occurs only if the arity of the type synonym is insufficiently
high to accommodate an implicit binding. It can be worked around by adding an
unused binder on the LHS:

  type P_w :: k -> Type
  type P_w @_w = Proxy :: k -> Type

The variable `_w` is unused. The only effect of the `@_w` binder is that the
arity of `P_w` is changed from 0 to 1. However, bumping the arity is exactly
what's needed to make the implicit binding of `k` possible.

All this is a rather unfortunate bit of accidental complexity that will go away
when GHC drops support for implicit RHS quantification. In the meantime, we
ought to produce a proper error message instead of a compiler panic, and we do
that with a check in checkForDisconnectedScopedTyVars:

  | flav == TypeSynonymFlavour = mapM_ report_out_of_arity (filterOut ok scoped_prs)

-}

{- *********************************************************************
*                                                                      *
             Bringing type variables into scope
*                                                                      *
********************************************************************* -}

--------------------------------------
--    HsForAllTelescope
--------------------------------------

tcTKTelescope :: TcTyMode
              -> HsForAllTelescope GhcRn
              -> TcM a
              -> TcM ([(Matchability, TcTyVarBinder)], a)
-- A HsForAllTelescope comes only from a HsForAllTy,
-- an explicit, user-written forall type
tcTKTelescope mode tele thing_inside =
 do { explicit_ma <- tcForAllMatchability (hsf_matchability tele)
    ; case tele of
      HsForAllVis { hsf_vis_bndrs = bndrs }
        -> do { skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn (unLoc <$> bndrs)))
              ; let skol_mode = smVanilla { sm_clone = False, sm_holes = mode_holes mode
                                          , sm_indices = mode_indices mode
                                          , sm_tvtv = SMDSkolemTv skol_info }
              ; (req_tv_bndrs, thing) <- tcExplicitTKBndrsX skol_mode bndrs thing_inside
                -- req_tv_bndrs :: [VarBndr TyVar ()],
                -- but we want [VarBndr TyVar ForAllTyFlag]
              ; ma_bndrs <- forAllMatchabilities mode explicit_ma (tyVarReqToBinders req_tv_bndrs)
              ; return (ma_bndrs, thing) }

      HsForAllInvis { hsf_invis_bndrs = bndrs }
        -> do { skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn (unLoc <$> bndrs)))
              ; let skol_mode = smVanilla { sm_clone = False, sm_holes = mode_holes mode
                                          , sm_indices = mode_indices mode
                                          , sm_tvtv = SMDSkolemTv skol_info }
              ; (inv_tv_bndrs, thing) <- tcExplicitTKBndrsX skol_mode bndrs thing_inside
                -- inv_tv_bndrs :: [VarBndr TyVar Specificity],
                -- but we want [VarBndr TyVar ForAllTyFlag]
              ; ma_bndrs <- forAllMatchabilities mode explicit_ma (tyVarSpecToBinders inv_tv_bndrs)
              ; return (ma_bndrs, thing) }
    }

-- | Check runtime invisible forall groups without flattening their scopes.
tcRuntimeInvisForAllTelescopes :: SkolemInfo -> [HsForAllTelescope GhcRn]
                              -> TcM a -> TcM ([InvisTVBinder], a)
tcRuntimeInvisForAllTelescopes _ [] inside = ([],) <$> inside
tcRuntimeInvisForAllTelescopes skol_info (tele@HsForAllInvis{hsf_invis_bndrs = bndrs} : rest) inside =
  do { tcCheckRuntimeForAllMatchability (hsf_matchability tele)
     ; (tvs, (rest_tvs, a)) <- tcExplicitTKBndrs skol_info bndrs $
         tcRuntimeInvisForAllTelescopes skol_info rest inside
     ; return (tvs ++ rest_tvs, a) }
tcRuntimeInvisForAllTelescopes _ (HsForAllVis{} : _) _ =
  pprPanic "tcRuntimeInvisForAllTelescopes: visible group" empty

--------------------------------------
--    HsOuterTyVarBndrs
--------------------------------------

bindOuterTKBndrsX :: OutputableBndrFlag flag 'Renamed  -- Only to support traceTc
                  => SkolemMode
                  -> HsOuterTyVarBndrs flag GhcRn
                  -> TcM a
                  -> TcM (HsOuterTyVarBndrs flag GhcTc, a)
bindOuterTKBndrsX skol_mode outer_bndrs thing_inside
  = case outer_bndrs of
      HsOuterImplicit{hso_ximplicit = imp_tvs} ->
        do { (imp_tvs', thing) <- bindImplicitTKBndrsX skol_mode imp_tvs thing_inside
           ; return ( HsOuterImplicit{hso_ximplicit = imp_tvs'}
                    , thing) }
      HsOuterExplicit{hso_matchability = ma, hso_bndrs = exp_bndrs} ->
        do { tcCheckRuntimeForAllMatchability ma
           ; (exp_tvs', thing) <- bindExplicitTKBndrsX skol_mode exp_bndrs thing_inside
           ; return ( HsOuterExplicit { hso_xexplicit = exp_tvs'
                                      , hso_matchability = ma
                                      , hso_bndrs     = exp_bndrs }
                    , thing) }

---------------
outerTyVars :: HsOuterTyVarBndrs flag GhcTc -> [TcTyVar]
-- The returned [TcTyVar] is not necessarily in dependency order
-- at least for the HsOuterImplicit case
outerTyVars (HsOuterImplicit { hso_ximplicit = tvs })  = tvs
outerTyVars (HsOuterExplicit { hso_xexplicit = tvbs }) = binderVars tvbs

---------------
outerTyVarBndrs :: HsOuterTyVarBndrs Specificity GhcTc -> [InvisTVBinder]
outerTyVarBndrs (HsOuterImplicit{hso_ximplicit = imp_tvs}) = [Bndr tv SpecifiedSpec | tv <- imp_tvs]
outerTyVarBndrs (HsOuterExplicit{hso_xexplicit = exp_tvs}) = exp_tvs

---------------
scopedSortOuter :: HsOuterTyVarBndrs flag GhcTc -> TcM (HsOuterTyVarBndrs flag GhcTc)
-- Sort any /implicit/ binders into dependency order
--     (zonking first so we can see the dependencies)
-- /Explicit/ ones are already in the right order
scopedSortOuter (HsOuterImplicit{hso_ximplicit = imp_tvs})
  = do { imp_tvs <- zonkAndScopedSort imp_tvs
       ; return (HsOuterImplicit { hso_ximplicit = imp_tvs }) }
scopedSortOuter bndrs@(HsOuterExplicit{})
  = -- No need to dependency-sort (or zonk) explicit quantifiers
    return bndrs

---------------
bindOuterSigTKBndrs_Tv :: HsOuterSigTyVarBndrs GhcRn
                       -> TcM a -> TcM (HsOuterSigTyVarBndrs GhcTc, a)
bindOuterSigTKBndrs_Tv
  = bindOuterTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv })

bindOuterSigTKBndrs_Tv_M :: TcTyMode
                         -> HsOuterSigTyVarBndrs GhcRn
                         -> TcM a -> TcM (HsOuterSigTyVarBndrs GhcTc, a)
-- Do not push level; do not make implication constraint; use Tvs
-- Two major clients of this "bind-only" path are:
--    Note [Using TyVarTvs for kind-checking GADTs] in GHC.Tc.TyCl
--    Note [Checking partial type signatures]
bindOuterSigTKBndrs_Tv_M mode
  = bindOuterTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv
                                 , sm_holes = mode_holes mode })

-- | Give an implicit field binder the kind of the wired-in constructor field.
-- See Note [Primitive family pattern binders] in GHC.Core.Type.
primFamilyPatKinds :: TyCon -> HsFamEqnPats GhcRn -> TcM (NameEnv Kind)
primFamilyPatKinds fam pats
  = do { enabled <- primFamilyPatEnabled fam
       ; return $ if enabled
           then mkNameEnv [binder | HsValArg _ pat <- pats, Just binder <- [prim_field pat]]
           else emptyNameEnv }
  where
    prim_field pat
      | (L _ (HsTyVar _ IsPromoted (L _ (WithUserRdr _ con))), [HsValArg _ arg])
          <- splitHsAppTys (unLoc (ignoreParens pat))
      , Just field_kind <- primFamilyPatFieldKind con
      , Just name <- field_name field_kind arg
      = Just (name, field_kind)
      | otherwise = Nothing

    field_name field_kind arg = case unLoc (ignoreParens arg) of
      HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))
        | isTyVarName name -> Just name
      HsKindSig _ field sig
        | isPrimKindAnnotation field_kind sig -> field_name field_kind field
      _ -> Nothing

-- | Enable the raw binder judgment only in an opted-in closed family equation.
primFamilyPatEnabled :: TyCon -> TcM Bool
primFamilyPatEnabled fam
  | ClosedTypeFamilyFlavour <- tyConFlavour fam = xoptM LangExt.DependentTypes
  | otherwise = return False

-- | Recognize a written reference to the constructor's primitive field kind.
isPrimKindAnnotation :: Kind -> LHsKind GhcRn -> Bool
isPrimKindAnnotation kind sig = case unLoc (ignoreParens sig) of
  HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))
    -> Just name == (tyConName <$> tyConAppTyCon_maybe kind)
  _ -> False

bindOuterFamEqnTKBndrs_Q_Tv :: NameEnv Kind -> HsOuterFamEqnTyVarBndrs GhcRn
                            -> TcM a
                            -> TcM (HsOuterFamEqnTyVarBndrs GhcTc, a)
bindOuterFamEqnTKBndrs_Q_Tv kinds hs_bndrs thing_inside
  = bindOuterTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                 , sm_implicit_kinds = kinds
                                 , sm_tvtv = SMDTyVarTv })
                      hs_bndrs thing_inside
    -- sm_clone=False: see Note [Cloning for type variable binders]

bindOuterFamEqnTKBndrs :: SkolemInfo -> NameEnv Kind
                       -> HsOuterFamEqnTyVarBndrs GhcRn
                       -> TcM a
                       -> TcM (HsOuterFamEqnTyVarBndrs GhcTc, a)
bindOuterFamEqnTKBndrs skol_info kinds
  = bindOuterTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                 , sm_implicit_kinds = kinds
                                 , sm_tvtv = SMDSkolemTv skol_info })
    -- sm_clone=False: see Note [Cloning for type variable binders]

---------------
tcOuterTKBndrs :: OutputableBndrFlag flag 'Renamed   -- Only to support traceTc
               => SkolemInfo
               -> HsOuterTyVarBndrs flag GhcRn
               -> TcM a -> TcM (HsOuterTyVarBndrs flag GhcTc, a)
tcOuterTKBndrs skol_info
  = tcOuterTKBndrsX (smVanilla { sm_clone = False
                               , sm_tvtv = SMDSkolemTv skol_info })
                    skol_info
  -- Do not clone the outer binders
  -- See Note [Cloning for type variable binders] under "must not"

tcOuterTKBndrsX :: OutputableBndrFlag flag 'Renamed   -- Only to support traceTc
                => SkolemMode -> SkolemInfo
                -> HsOuterTyVarBndrs flag GhcRn
                -> TcM a -> TcM (HsOuterTyVarBndrs flag GhcTc, a)
-- Push level, capture constraints, make implication
tcOuterTKBndrsX skol_mode skol_info outer_bndrs thing_inside
  = case outer_bndrs of
      HsOuterImplicit{hso_ximplicit = imp_tvs} ->
        do { (imp_tvs', thing) <- tcImplicitTKBndrsX skol_mode skol_info imp_tvs thing_inside
           ; return ( HsOuterImplicit{hso_ximplicit = imp_tvs'}
                    , thing) }
      HsOuterExplicit{hso_matchability = ma, hso_bndrs = exp_bndrs} ->
        do { (exp_tvs', thing) <- tcExplicitTKBndrsX skol_mode exp_bndrs thing_inside
           ; return ( HsOuterExplicit { hso_xexplicit = exp_tvs'
                                      , hso_matchability = ma
                                      , hso_bndrs     = exp_bndrs }
                    , thing) }

-- | Check constructor contexts before later groups enter scope.
-- See Note [Constructor telescope contexts] in GHC.Hs.Decls.
tcConDeclTelescope :: SkolemInfo -> [LHsGadtTelescope GhcRn]
                   -> TcM a -> TcM ([TcTyVarBinder], TcThetaType, a)
tcConDeclTelescope _ [] inside = do
  a <- inside
  return ([], [], a)
tcConDeclTelescope skol_info (L _ HsGadtPar{} : rest) inside =
  tcConDeclTelescope skol_info rest inside
tcConDeclTelescope skol_info (L _ (HsGadtContext ctxt) : rest) inside =
  do { theta <- tcHsContext (Just ctxt)
       -- Do not install constructor-local theta as Givens. See the owning Note.
     ; (tvbs, rest_theta, a) <- tcConDeclTelescope skol_info rest inside
     ; return (tvbs, theta ++ rest_theta, a) }
tcConDeclTelescope skol_info (L _ (HsGadtForAll _ tele) : rest) inside =
  do { tcCheckRuntimeForAllMatchability (hsf_matchability tele)
     ; (tvbs, (rest_tvbs, theta, a)) <- case tele of
         HsForAllInvis{hsf_invis_bndrs = bndrs} -> do
           (tvs, a) <- tcExplicitTKBndrs skol_info bndrs (tcConDeclTelescope skol_info rest inside)
           return (tyVarSpecToBinders tvs, a)
         HsForAllVis{hsf_vis_bndrs = bndrs} -> do
           (tvs, a) <- tcExplicitTKBndrs skol_info bndrs (tcConDeclTelescope skol_info rest inside)
           return (tyVarReqToBinders tvs, a)
     ; return (tvbs ++ rest_tvbs, theta, a) }

--------------------------------------
--    Explicit tyvar binders
--------------------------------------

tcExplicitTKBndrs :: OutputableBndrFlag flag 'Renamed    -- Only to suppor traceTc
                  => SkolemInfo
                  -> [LHsTyVarBndr flag GhcRn]
                  -> TcM a
                  -> TcM ([VarBndr TyVar flag], a)
tcExplicitTKBndrs skol_info
  = tcExplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDSkolemTv skol_info })

tcExplicitTKBndrsX :: OutputableBndrFlag flag 'Renamed    -- Only to suppor traceTc
                   => SkolemMode
                   -> [LHsTyVarBndr flag GhcRn]
                   -> TcM a
                   -> TcM ([VarBndr TyVar flag], a)
-- Push level, capture constraints, and emit an implication constraint.
-- The implication constraint has a ForAllSkol ic_info,
--   so that it is subject to a telescope test.
tcExplicitTKBndrsX skol_mode bndrs thing_inside = case nonEmpty bndrs of
    Nothing -> do
       { res <- thing_inside
       ; return ([], res) }

    Just bndrs1 -> do
       { (tclvl, wanted, (skol_tvs, res))
             <- pushLevelAndCaptureConstraints $
                bindExplicitTKBndrsX skol_mode bndrs $
                thing_inside

       -- Set up SkolemInfo for telescope test
       ; let bndr_1 = NE.head bndrs1; bndr_n = NE.last bndrs1
       ; skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn  (unLoc <$> bndrs)))
         -- Notice that we use ForAllSkol here, ignoring the enclosing
         -- skol_info unlike tcImplicitTKBndrs, because the bad-telescope
         -- test applies only to ForAllSkol

       ; setSrcSpan (combineSrcSpans (getLocA bndr_1) (getLocA bndr_n))
       $ emitResidualTvConstraint skol_info (binderVars skol_tvs) tclvl wanted

       ; return (skol_tvs, res) }

----------------
-- | Skolemise the 'HsTyVarBndr's in an 'HsForAllTelescope' with the supplied
-- 'TcTyMode'.
bindExplicitTKBndrs_Skol
    :: (OutputableBndrFlag flag 'Renamed)      -- Only to suppor traceTc
    => SkolemInfo
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)

bindExplicitTKBndrs_Tv
    :: (OutputableBndrFlag flag 'Renamed)    -- Only to suppor traceTc
    => [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)

bindExplicitTKBndrs_Skol skol_info = bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_tvtv = SMDSkolemTv skol_info })
bindExplicitTKBndrs_Tv   = bindExplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv })
   -- sm_clone: see Note [Cloning for type variable binders]

bindExplicitTKBndrs_Q_Skol
    :: (OutputableBndrFlag flag 'Renamed)   -- Only to support traceTc
    => SkolemInfo
    -> ContextKind
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)

bindExplicitTKBndrs_Q_Tv
    :: (OutputableBndrFlag flag 'Renamed)   -- Only to support traceTc
    => ContextKind
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)
-- These do not clone: see Note [Cloning for type variable binders]
bindExplicitTKBndrs_Q_Skol skol_info ctxt_kind hs_bndrs thing_inside
  = bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                    , sm_kind = ctxt_kind, sm_tvtv = SMDSkolemTv skol_info })
                         hs_bndrs thing_inside
    -- sm_clone=False: see Note [Cloning for type variable binders]

bindExplicitTKBndrs_Q_Tv  ctxt_kind hs_bndrs thing_inside
  = bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                    , sm_tvtv = SMDTyVarTv, sm_kind = ctxt_kind })
                         hs_bndrs thing_inside
    -- sm_clone=False: see Note [Cloning for type variable binders]

bindExplicitTKBndrsX
    :: (OutputableBndrFlag flag 'Renamed)   -- Only to support traceTc
    => SkolemMode
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)  -- Returned [TcTyVar] are in 1-1 correspondence
                                      -- with the passed-in [LHsTyVarBndr]
bindExplicitTKBndrsX skol_mode@(SM { sm_parent = check_parent, sm_kind = ctxt_kind
                                   , sm_indices = indices
                                   , sm_holes = hole_info, sm_family_metadata = family_metadata
                                   , sm_sig_kind_arrows = sig_kind_arrows })
                     hs_tvs thing_inside
  = do { traceTc "bindExplicitTKBndrs" (ppr hs_tvs)
       ; go hs_tvs }
  where
    tc_ki_mode = TcTyMode { mode_tyki = KindLevel, mode_holes = hole_info
                         , mode_declaration = False
                         , mode_callback_arrow = OrdinaryArrowChecking
                         , mode_source_givens = []
                         , mode_indices = indices
                         , mode_dictionaries = []
                         , mode_pending_calls = Nothing
                         , mode_synonym_metadata = if family_metadata then familyKindMetadata else Nothing
                         , mode_source_proof_owner = Nothing
                         , mode_complete_type_arg = Nothing, mode_under_forall = False, mode_lambda_root = Nothing, mode_sigma_root = Nothing }
                 -- Inherit the HoleInfo from the context

    go [] = do { res <- thing_inside
               ; return ([], res) }
    go (L _ hs_tv : hs_tvs)
       = do { lcl_env <- getLclTypeEnv
            ; tv <- tc_hs_bndr lcl_env hs_tv
            -- Extend the environment as we go, in case a binder
            -- is mentioned in the kind of a later binder
            --   e.g. forall k (a::k). blah
            -- NB: tv's Name may differ from hs_tv's
            -- See Note [Cloning for type variable binders]
            ; (tvs,res) <- tcExtendNameTyVarEnv (mk_tvb_pairs hs_tv tv) $
                           go hs_tvs
            ; return (Bndr tv (hsTyVarBndrFlag hs_tv):tvs, res) }

    tc_hs_bndr :: TcTypeEnv -> HsTyVarBndr flag GhcRn -> TcM TcTyVar
    tc_hs_bndr lcl_env (HsTvb { tvb_var = bvar, tvb_kind = kind })
      | check_parent
      , HsBndrVar _ (L _ name) <- bvar
      , Just (ATyVar _ tv) <- lookupNameEnv lcl_env name
      = do { check_hs_bndr_kind name (tyVarKind tv) kind
           ; return tv }
      | otherwise
      = do { name <- tcHsBndrVarName bvar
           ; kind' <- tc_hs_bndr_kind name kind
           ; newTyVarBndr skol_mode name kind' }

    -- Let a complete signature constrain omitted arrows in a binder kind.
    -- Check its shape first. Give each written bare arrow a separate variable
    -- in the signature's defaulting pass. scopedSortOuter checks the dependency
    -- order of the resulting binder kinds.
    tc_hs_bndr_kind :: Name -> HsBndrKind GhcRn -> TcM Kind
    tc_hs_bndr_kind _    (HsBndrNoKind _)    = newExpectedKind ctxt_kind
    tc_hs_bndr_kind name (HsBndrKind _ kind)
      = do { checked <- tc_lhs_kind_sig tc_ki_mode (TyVarBndrKindCtxt name) kind
           ; if sig_kind_arrows && (isFunTy checked || isForAllTy checked)
             then do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
                     ; case owner of
                         Just (defaults, _) -> defer_kind_arrows defaults kind checked
                         Nothing            -> pure checked }
             else pure checked }

    defer_kind_arrows defaults hs_kind checked
      | L _ (HsFunTy _ arrow domain result) <- ignoreParens hs_kind
      , FunTy flag ma mult domain_ty result_ty <- checked
      = do { ma' <- case arrow of
                       HsModifiedFunArr _ [] HsStandardArr{} ->
                         do { v <- newFlexiTyVar matchabilityTy
                            ; updTcRef defaults ((v, KindLevel) :)
                            ; pure (mkTyVarTy v) }
                       _ -> pure ma
           ; domain_ty' <- defer_kind_arrows defaults domain domain_ty
           ; result_ty' <- defer_kind_arrows defaults result result_ty
           ; pure (FunTy flag ma' mult domain_ty' result_ty') }
      | L _ (HsForAllTy _ tele body) <- ignoreParens hs_kind
      , let (mb_ma, tele_len) = case tele of
              HsForAllInvis _ m bs -> (m, length bs)
              HsForAllVis   _ m bs -> (m, length bs)
            total_hs_foralls = tele_len + count_hs_forall_bndrs (ignoreParens body)
            (all_core_foralls, after_foralls) = splitForAllForAllTyBinders checked
            extra_prefix = max 0 (length all_core_foralls - total_hs_foralls)
      , length all_core_foralls >= extra_prefix + tele_len
      = do { let (prefix_bndrs, rest0) = splitAt extra_prefix all_core_foralls
                 (tele_bndrs,   rest1) = splitAt tele_len rest0
                 inner_ty              = foldr (\(m, b) r -> ForAllTy m b r) after_foralls rest1
           ; ma_override <- case mb_ma of
               Nothing -> do { v <- newFlexiTyVar matchabilityTy
                             ; updTcRef defaults ((v, KindLevel) :)
                             ; pure (Just (mkTyVarTy v)) }
               Just _  -> pure Nothing
           ; inner_ty' <- defer_kind_arrows defaults body inner_ty
           ; let rebuild (orig_ma, tvb) r = ForAllTy (fromMaybe orig_ma ma_override) tvb r
           ; pure (foldr (\(m, b) r -> ForAllTy m b r)
                         (foldr rebuild inner_ty' tele_bndrs)
                         prefix_bndrs) }
      | otherwise = pure checked

    count_hs_forall_bndrs (L _ (HsForAllTy _ tele body)) =
      (case tele of
         HsForAllInvis _ _ bs -> length bs
         HsForAllVis   _ _ bs -> length bs)
      + count_hs_forall_bndrs (ignoreParens body)
    count_hs_forall_bndrs _ = 0

    -- Check the HsBndrKind against the kind of the parent type variable,
    -- e.g. the following is rejected:
    --   class C (m :: * -> *) where
    --     type F (m :: *) = ...
    check_hs_bndr_kind :: Name -> Kind -> HsBndrKind GhcRn -> TcM ()
    check_hs_bndr_kind _    _           (HsBndrNoKind _)    = return ()
    check_hs_bndr_kind name parent_kind (HsBndrKind _ kind) =
      do { kind' <- tc_lhs_kind_sig tc_ki_mode (TyVarBndrKindCtxt name) kind
         ; discardResult $
           unifyKind (Just $ NameThing name) kind' parent_kind }

tcHsBndrVarName :: HsBndrVar GhcRn -> TcM Name
tcHsBndrVarName (HsBndrVar _ (L _ name)) = return name
tcHsBndrVarName (HsBndrWildCard _) = newSysName (mkTyVarOcc "_")

mk_tvb_pairs :: HsTyVarBndr flag GhcRn -> TcTyVar -> [(Name, TcTyVar)]
mk_tvb_pairs tvb tv =
  case hsTyVarName tvb of
    Nothing -> []
    Just nm -> [(nm, tv)]

newTyVarBndr :: SkolemMode -> Name -> Kind -> TcM TcTyVar
newTyVarBndr (SM { sm_clone = clone, sm_tvtv = tvtv }) name kind
  = do { name <- case clone of
              True -> do { uniq <- newUnique
                         ; return (setNameUnique name uniq) }
              False -> return name
       ; details <- case tvtv of
                 SMDTyVarTv  -> newMetaDetails TyVarTv
                 SMDSkolemTv skol_info ->
                  do { lvl <- getTcLevel
                     ; return (SkolemTv skol_info lvl False) }
       ; return (mkTcTyVar name kind details) }

--------------------------------------
--    Implicit tyvar binders
--------------------------------------

tcImplicitTKBndrsX :: SkolemMode -> SkolemInfo
                   -> [Name]
                   -> TcM a
                   -> TcM ([TcTyVar], a)
-- The workhorse:
--    push level, capture constraints, and emit an implication constraint
tcImplicitTKBndrsX skol_mode skol_info bndrs thing_inside
  | null bndrs  -- Short-cut the common case with no quantifiers
                -- E.g. f :: Int -> Int
                --      makes a HsOuterImplicit with empty bndrs,
                --      and tcOuterTKBndrsX goes via here
  = do { res <- thing_inside; return ([], res) }
  | otherwise
  = do { (tclvl, wanted, (skol_tvs, res))
             <- pushLevelAndCaptureConstraints       $
                bindImplicitTKBndrsX skol_mode bndrs $
                thing_inside

       ; emitResidualTvConstraint skol_info skol_tvs tclvl wanted

       ; return (skol_tvs, res) }

------------------
bindImplicitTKBndrs_Skol,
  bindImplicitTKBndrs_Q_Skol :: SkolemInfo -> [Name] -> TcM a -> TcM ([TcTyVar], a)

bindImplicitTKBndrs_Tv, bindImplicitTKBndrs_Q_Tv :: [Name] -> TcM a -> TcM ([TcTyVar], a)
bindImplicitTKBndrs_Skol skol_info = bindImplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDSkolemTv skol_info })
bindImplicitTKBndrs_Tv   = bindImplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv })
bindImplicitTKBndrs_Q_Skol skol_info = bindImplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True, sm_tvtv = SMDSkolemTv skol_info })
bindImplicitTKBndrs_Q_Tv = bindImplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True, sm_tvtv = SMDTyVarTv })

bindImplicitTKBndrsX
   :: SkolemMode
   -> [Name]               -- Generated by renamer; not in dependency order
   -> TcM a
   -> TcM ([TcTyVar], a)   -- Returned [TcTyVar] are in 1-1 correspondence
                           -- with the passed in [Name]
bindImplicitTKBndrsX skol_mode@(SM { sm_parent = check_parent, sm_kind = ctxt_kind })
                     tv_names thing_inside
  = do { lcl_env <- getLclTypeEnv
       ; tkvs <- mapM (new_tv lcl_env) tv_names
       ; traceTc "bindImplicitTKBndrsX" (ppr tv_names $$ ppr tkvs)
       ; res <- tcExtendNameTyVarEnv (tv_names `zip` tkvs)
                thing_inside
       ; return (tkvs, res) }
  where
    new_tv lcl_env name
      | check_parent
      , Just (ATyVar _ tv) <- lookupNameEnv lcl_env name
      = return tv
      | otherwise
      = do { kind <- case lookupNameEnv (sm_implicit_kinds skol_mode) name of
                        Just known_kind -> return known_kind
                        Nothing -> newExpectedKind ctxt_kind
           ; newTyVarBndr skol_mode name kind }

--------------------------------------
--           SkolemMode
--------------------------------------

-- | 'SkolemMode' describes how to typecheck an explicit ('HsTyVarBndr') or
-- implicit ('Name') binder in a type. It is just a record of flags
-- that describe what sort of 'TcTyVar' to create.
data SkolemMode
  = SM { sm_parent :: Bool    -- True <=> check the in-scope parent type variable
                              -- Used only for asssociated types

       , sm_clone  :: Bool    -- True <=> fresh unique
                              -- See Note [Cloning for type variable binders]

       , sm_tvtv   :: SkolemModeDetails    -- True <=> use a TyVarTv, rather than SkolemTv
                              -- Why?  See Note [Inferring kinds for type declarations]
                              -- in GHC.Tc.TyCl, and (in this module)
                              -- Note [Checking partial type signatures]

       , sm_kind   :: ContextKind  -- Use this for the kind of any new binders
       , sm_implicit_kinds :: NameEnv Kind -- Known kinds of implicit family pattern binders

       , sm_family_metadata :: Bool
       , sm_sig_kind_arrows :: Bool -- Defer bare binder arrows until the signature is checked.
       , sm_indices :: NameEnv Id  -- Runtime indices in binder kinds
       , sm_holes  :: HoleInfo     -- What to do for wildcards in the kind
       }

data SkolemModeDetails
  = SMDTyVarTv
  | SMDSkolemTv SkolemInfo


smVanilla :: HasDebugCallStack => SkolemMode
smVanilla = SM { sm_clone  = panic "sm_clone"  -- We always override this
               , sm_parent = False
               , sm_tvtv   = pprPanic "sm_tvtv" callStackDoc -- We always override this
               , sm_kind   = AnyKind
               , sm_implicit_kinds = emptyNameEnv
               , sm_family_metadata = False
               , sm_sig_kind_arrows = False
               , sm_indices = emptyNameEnv
               , sm_holes  = Nothing }

{- Note [Cloning for type variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes we must clone the Name of a type variable binder (written in
the source program); and sometimes we must not. This is controlled by
the sm_clone field of SkolemMode.

In some cases it doesn't matter whether or not we clone. Perhaps
it'd be better to use MustClone/MayClone/MustNotClone.

When we /must not/ clone
* In the binders of a type signature (tcOuterTKBndrs)
      f :: forall a{27}. blah
      f = rhs
  Then 'a' scopes over 'rhs'. When we kind-check the signature (tcHsSigType),
  we must get the type (forall a{27}. blah) for the Id f, because
  we bring that type variable into scope when we typecheck 'rhs'.

* In the binders of a data family instance (bindOuterFamEqnTKBndrs)
     data instance
       forall p q. D (p,q) = D1 p | D2 q
  We kind-check the LHS in tcDataFamInstHeader, and then separately
  (in tcDataFamInstDecl) bring p,q into scope before looking at the
  the constructor decls.

* bindExplicitTKBndrs_Q_Tv/bindImplicitTKBndrs_Q_Tv do not clone
  We take advantage of this in kcInferDeclHeader:
     all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
  If we cloned, we'd need to take a bit more care here; not hard.

* bindExplicitTKBndrs_Q_Skol, bindExplicitTKBndrs_Skol, do not clone.
  There is no need, I think.

  The payoff here is that avoiding gratuitous cloning means that we can
  almost always take the fast path in swizzleTcTyConBndrs.

When we /must/ clone.
* bindOuterSigTKBndrs_Tv, bindExplicitTKBndrs_Tv do cloning

  This for a narrow and tricky reason which, alas, I couldn't find a
  simpler way round.  #16221 is the poster child:

     data SameKind :: k -> k -> *
     data T a = forall k2 (b :: k2). MkT (SameKind a b) !Int

  When kind-checking T, we give (a :: kappa1). Then:

  - In kcConDecl we make a TyVarTv unification variable kappa2 for k2
    (as described in Note [Using TyVarTvs for kind-checking GADTs],
    even though this example is an existential)
  - So we get (b :: kappa2) via bindExplicitTKBndrs_Tv
  - We end up unifying kappa1 := kappa2, because of the (SameKind a b)

  Now we generalise over kappa2. But if kappa2's Name is precisely k2
  (i.e. we did not clone) we'll end up giving T the utterly final kind
    T :: forall k2. k2 -> *
  Nothing directly wrong with that but when we typecheck the data constructor
  we have k2 in scope; but then it's brought into scope /again/ when we find
  the forall k2.  This is chaotic, and we end up giving it the type
    MkT :: forall k2 (a :: k2) k2 (b :: k2).
           SameKind @k2 a b -> Int -> T @{k2} a
  which is bogus -- because of the shadowing of k2, we can't
  apply T to the kind or a!

  And there no reason /not/ to clone the Name when making a unification
  variable.  So that's what we do.
-}

--------------------------------------
-- Binding type/class variables in the
-- kind-checking and typechecking phases
--------------------------------------

bindTyClTyVars :: Name -> (TcKind -> [TcTyConBinder] -> Int -> TcKind -> TcM a) -> TcM a
-- ^ Bring into scope the binders of a PolyTcTyCon
-- Used for the type variables of a type or class decl
-- in the "kind checking" and "type checking" pass,
-- but not in the initial-kind run.
bindTyClTyVars tycon_name thing_inside
  = do { tycon <- tcLookupTcTyCon tycon_name     -- The tycon is a PolyTcTyCon
       ; let kind       = tyConKind tycon
             res_kind   = tyConResKind tycon
             binders    = tyConBinders tycon
             nb_eta     = tyConEtaBinders tycon
       ; traceTc "bindTyClTyVars" (ppr tycon_name $$ ppr binders)
       ; tcExtendTyVarEnv (binderVars binders) $
         thing_inside kind binders nb_eta res_kind }

bindTyClTyVarsAndZonk :: Name -> (Kind -> [TyConBinder] -> Int -> Kind -> TcM a) -> TcM a
-- Like bindTyClTyVars, but in addition
-- zonk the skolem TcTyVars of a PolyTcTyCon to TyVars
-- We always do this same zonking after a call to bindTyClTyVars, but
-- here we do it right away because there are no more unifications to come
bindTyClTyVarsAndZonk tycon_name thing_inside
  = bindTyClTyVars tycon_name $ \ tc_kind tc_bndrs nb_eta tc_res_kind ->
    do { (kind, bndrs, res_kind) <- initZonkEnv NoFlexi $ do
          kind <- zonkTcTypeToTypeX tc_kind
          runZonkBndrT (zonkTyVarBindersX tc_bndrs) $ \ bndrs ->
            do { res_kind <- zonkTcTypeToTypeX tc_res_kind
               ; return (kind, bndrs, res_kind) }
       ; thing_inside kind bndrs nb_eta res_kind }


{- *********************************************************************
*                                                                      *
             Kind generalisation
*                                                                      *
********************************************************************* -}

zonkAndScopedSort :: [TcTyVar] -> TcM [TcTyVar]
zonkAndScopedSort spec_tkvs
  = do { spec_tkvs <- liftZonkM $ zonkTcTyVarsToTcTyVars spec_tkvs
         -- Zonk the kinds, so that we can do the dependency analysis

       -- Do a stable topological sort, following
       -- Note [Ordering of implicit variables] in GHC.Rename.HsType
       ; return (scopedSort spec_tkvs) }

-- | Generalize some of the free variables in the given type.
-- All such variables should be *kind* variables; any type variables
-- should be explicitly quantified (with a `forall`) before now.
--
-- The WantedConstraints are un-solved kind constraints. Generally
-- they'll be reported as errors later, but meanwhile we refrain
-- from quantifying over any variable free in these unsolved
-- constraints. See Note [Failure in local type signatures].
--
-- But in all cases, generalize only those variables whose TcLevel is
-- strictly greater than the ambient level. This "strictly greater
-- than" means that you likely need to push the level before creating
-- whatever type gets passed here.
--
-- Any variable whose level is greater than the ambient level but is
-- not selected to be generalized will be promoted. (See [Promoting
-- unification variables] in "GHC.Tc.Solver" and Note [Recipe for
-- checking a signature].)
--
-- The resulting KindVar are the variables to quantify over, in the
-- correct, well-scoped order. They should generally be Inferred, not
-- Specified, but that's really up to the caller of this function.
kindGeneralizeSome :: SkolemInfo
                   -> WantedConstraints
                   -> TcType    -- ^ needn't be zonked
                   -> TcM [KindVar]
kindGeneralizeSome skol_info wanted kind_or_type
  = do { -- Use the "Kind" variant here, as any types we see
         -- here will already have all type variables quantified;
         -- thus, every free variable is really a kv, never a tv.
       ; dvs <- candidateQTyVarsOfKind kind_or_type
       ; filtered_dvs <- filterConstrainedCandidates wanted dvs
       ; traceTc "kindGeneralizeSome" $
         vcat [ text "type:" <+> ppr kind_or_type
              , text "dvs:" <+> ppr dvs
              , text "filtered_dvs:" <+> ppr filtered_dvs ]
       ; quantifyTyVars skol_info DefaultNonStandardTyVars filtered_dvs }

filterConstrainedCandidates
  :: WantedConstraints    -- Don't quantify over variables free in these
                          --   Not necessarily fully zonked
  -> CandidatesQTvs       -- Candidates for quantification
  -> TcM CandidatesQTvs
-- filterConstrainedCandidates removes any candidates that are free in
-- 'wanted'; instead, it promotes them.  This bit is very much like
-- decidePromotedTyVars in GHC.Tc.Solver, but constraints are so much
-- simpler in kinds, it is much easier here. (In particular, we never
-- quantify over a constraint in a type.)
filterConstrainedCandidates wanted dvs
  | isEmptyWC wanted   -- Fast path for a common case
  = return dvs
  | otherwise
  = do { wc_tvs <- liftZonkM $ zonkTyCoVarsAndFV (tyCoVarsOfWC wanted)
       ; let (to_promote, dvs') = partitionCandidates dvs (`elemVarSet` wc_tvs)
       ; _ <- promoteTyVarSet to_promote
       ; return dvs' }

-- |- Specialised version of 'kindGeneralizeSome', but with empty
-- WantedConstraints, so no filtering is needed
-- i.e.   kindGeneraliseAll = kindGeneralizeSome emptyWC
kindGeneralizeAll :: SkolemInfo -> TcType -> TcM [KindVar]
kindGeneralizeAll skol_info kind_or_type
  = do { traceTc "kindGeneralizeAll" (ppr kind_or_type)
       ; dvs <- candidateQTyVarsOfKind kind_or_type
       ; quantifyTyVars skol_info DefaultNonStandardTyVars dvs }

-- | Specialized version of 'kindGeneralizeSome', but where no variables
-- can be generalized, but perhaps some may need to be promoted.
-- Use this variant when it is unknowable whether metavariables might
-- later be constrained.
--
-- To see why this promotion is needed, see
-- Note [Recipe for checking a signature], and especially
-- Note [Promotion in signatures].
kindGeneralizeNone :: TcType  -- needn't be zonked
                   -> TcM ()
kindGeneralizeNone kind_or_type
  = do { traceTc "kindGeneralizeNone" (ppr kind_or_type)
       ; dvs <- candidateQTyVarsOfKind kind_or_type
       ; _ <- promoteTyVarSet (candidateKindVars dvs)
       ; return () }

{- Note [Levels and generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f x = e
with no type signature. We are currently at level i.
We must
  * Push the level to level (i+1)
  * Allocate a fresh alpha[i+1] for the result type
  * Check that e :: alpha[i+1], gathering constraint WC
  * Solve WC as far as possible
  * Zonking the result type alpha[i+1], say to beta[i-1] -> gamma[i]
  * Find the free variables with level > i, in this case gamma[i]
  * Skolemise those free variables and quantify over them, giving
       f :: forall g. beta[i-1] -> g
  * Emit the residual constraint wrapped in an implication for g,
    thus   forall g. WC

All of this happens for types too.  Consider
  f :: Int -> (forall a. Proxy a -> Int)

Note [Kind generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We do kind generalisation only at the outer level of a type signature.
For example, consider
  T :: forall k. k -> *
  f :: (forall a. T a -> Int) -> Int
When kind-checking f's type signature we generalise the kind at
the outermost level, thus:
  f1 :: forall k. (forall (a:k). T k a -> Int) -> Int  -- YES!
and *not* at the inner forall:
  f2 :: (forall k. forall (a:k). T k a -> Int) -> Int  -- NO!
Reason: same as for HM inference on value level declarations,
we want to infer the most general type.  The f2 type signature
would be *less applicable* than f1, because it requires a more
polymorphic argument.

NB: There are no explicit kind variables written in f's signature.
When there are, the renamer adds these kind variables to the list of
variables bound by the forall, so you can indeed have a type that's
higher-rank in its kind. But only by explicit request.

Note [Kinds of quantified type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcTyVarBndrsGen quantifies over a specified list of type variables,
*and* over the kind variables mentioned in the kinds of those tyvars.

Note that we must zonk those kinds (obviously) but less obviously, we
must return type variables whose kinds are zonked too. Example
    (a :: k7)  where  k7 := k9 -> k9
We must return
    [k9, a:k9->k9]
and NOT
    [k9, a:k7]
Reason: we're going to turn this into a for-all type,
   forall k9. forall (a:k7). blah
which the type checker will then instantiate, and instantiate does not
look through unification variables!

Hence using zonked_kinds when forming tvs'.

-}

-----------------------------------
maybeEtaExpandAlgTyCon :: TyConFlavour tc  -> SkolemInfo
                  -> [TcTyConBinder] -> Kind
                  -> TcM ([TcTyConBinder], Kind)
maybeEtaExpandAlgTyCon flav skol_info tcbs res_kind
  | needsEtaExpansion flav
  = etaExpandAlgTyCon skol_info tcbs res_kind
  | otherwise
  = return ([], res_kind)

etaExpandAlgTyCon :: SkolemInfo
                  -> [TcTyConBinder] -> Kind
                  -> TcM ([TcTyConBinder], Kind)
etaExpandAlgTyCon skol_info tcbs declared_kind
  = do { (ma_tcbs, res_kind) <- splitTyConKind skol_info in_scope avoid_occs declared_kind
       ; solveEqualities "etaExpandAlgTyCon matchability" $
           forM_ (zip [0..] ma_tcbs) $ \ (i, (ma, _)) ->
             checkDeclArrowMatchability declared_kind i ma matchableDataConTy
       ; return (map snd ma_tcbs, res_kind) }
  where
    tyvars     = binderVars tcbs
    in_scope   = mkInScopeSetList tyvars
    avoid_occs = map getOccName tyvars

needsEtaExpansion :: TyConFlavour tc -> Bool
needsEtaExpansion NewtypeFlavour  = True
needsEtaExpansion DataTypeFlavour = True
needsEtaExpansion ClassFlavour    = True
needsEtaExpansion _               = False

splitTyConKind :: SkolemInfo
               -> InScopeSet
               -> [OccName]  -- Avoid these OccNames
               -> Kind       -- Must be zonked
               -> TcM ([(Type, TcTyConBinder)], TcKind)
-- ^ GADT decls can have a (perhaps partial) kind signature
--      e.g.  data T a :: * -> * -> * where ...
-- This function makes up suitable (kinded) TyConBinders for the
-- argument kinds.  E.g. in this case it might return
--   ([b::*, c::*], *)
-- Skolemises the type as it goes, returning skolem TcTyVars
-- Never emits constraints.
-- It's a little trickier than you might think: see Note [splitTyConKind]
-- See also Note [Datatype return kinds] in GHC.Tc.TyCl
splitTyConKind skol_info in_scope avoid_occs kind
  = do  { loc     <- getSrcSpanM
        ; new_uniqs <- getUniquesM
        ; rdr_env <- getLocalRdrEnv
        ; lvl     <- getTcLevel
        ; let new_occs = Inf.filter (\ occ ->
                  isNothing (lookupLocalRdrOcc rdr_env occ) &&
                  -- Note [Avoid name clashes for associated data types]
                  not (occ `elem` avoid_occs)) $ mkOccName tvName <$> allNameStrings
              subst = mkEmptySubst in_scope
              details = SkolemTv skol_info (pushTcLevel lvl) False
                        -- As always, allocate skolems one level in

              go occs uniqs subst acc kind
                = case splitPiTy_maybe kind of
                    Nothing -> (reverse acc, substTy subst kind)

                    Just (ma, Anon arg af, kind')
                      -> assert (af == FTF_T_T) $
                         go occs' uniqs' subst' ((substTy subst ma, tcb) : acc) kind'
                      where
                        tcb    = Bndr tv AnonTCB
                        arg'   = substTy subst (scaledThing arg)
                        name   = mkInternalName uniq occ loc
                        tv     = mkTcTyVar name arg' details
                        subst' = extendSubstInScope subst tv
                        (uniq,uniqs') = case uniqs of
                            uniq:uniqs' -> (uniq,uniqs')
                            _           -> panic "impossible"
                        Inf occ occs' = occs

                    Just (ma, Named (Bndr tv vis), kind')
                      -> go occs uniqs subst' ((substTy subst ma, tcb) : acc) kind'
                      where
                        tcb           = Bndr tv' (NamedTCB vis)
                        tc_tyvar      = mkTcTyVar (tyVarName tv) (tyVarKind tv) details
                        (subst', tv') = substTyVarBndr subst tc_tyvar

        ; return (go new_occs new_uniqs subst [] kind) }

isAllowedDataResKind :: AllowedDataResKind -> Kind -> Bool
isAllowedDataResKind AnyTYPEKind  kind = isTypeLikeKind     kind
isAllowedDataResKind AnyBoxedKind kind = tcIsBoxedTypeKind  kind
isAllowedDataResKind LiftedKind   kind = tcIsLiftedTypeKind kind

-- | Checks that the return kind in a data declaration's kind signature is
-- permissible. There are three cases:
--
-- If dealing with a @data@, @newtype@, @data instance@, or @newtype instance@
-- declaration, check that the return kind is @Type@.
--
-- If the declaration is a @newtype@ or @newtype instance@ and the
-- @UnliftedNewtypes@ extension is enabled, this check is slightly relaxed so
-- that a return kind of the form @TYPE r@ (for some @r@) is permitted.
-- See @Note [Implementation of UnliftedNewtypes]@ in "GHC.Tc.TyCl".
--
-- If dealing with a @data family@ declaration, check that the return kind is
-- either of the form:
--
-- 1. @TYPE r@ (for some @r@), or
--
-- 2. @k@ (where @k@ is a bare kind variable; see #12369)
--
-- See also Note [Datatype return kinds] in "GHC.Tc.TyCl"
checkDataKindSig :: DataSort -> Kind  -- any arguments in the kind are stripped off
                 -> TcM ()
checkDataKindSig data_sort kind
  = do { dflags <- getDynFlags
       ; traceTc "checkDataKindSig" (ppr kind)
       ; checkTc (tYPE_ok dflags || is_kind_var)
                 (err_msg dflags) }
  where
    (_, _, res_kind) = tcSplitPiTys kind
       -- Look for the result kind after
       -- peeling off any foralls and arrows

    is_newtype :: Bool
    is_newtype =
      case data_sort of
        DataDeclSort     new_or_data -> new_or_data == NewType
        DataInstanceSort new_or_data -> new_or_data == NewType
        DataFamilySort               -> False

    is_datatype :: Bool
    is_datatype =
      case data_sort of
        DataDeclSort     DataType -> True
        DataInstanceSort DataType -> True
        _                         -> False

    is_data_family :: Bool
    is_data_family =
      case data_sort of
        DataDeclSort{}     -> False
        DataInstanceSort{} -> False
        DataFamilySort     -> True

    allowed_kind :: DynFlags -> AllowedDataResKind
    allowed_kind dflags
      | is_newtype && xopt LangExt.UnliftedNewtypes dflags
        -- With UnliftedNewtypes, we allow kinds other than Type, but they
        -- must still be of the form `TYPE r` since we don't want to accept
        -- Constraint or Nat.
        -- See Note [Implementation of UnliftedNewtypes] in GHC.Tc.TyCl.
      = AnyTYPEKind
      | is_data_family
        -- If this is a `data family` declaration, we don't need to check if
        -- UnliftedNewtypes is enabled, since data family declarations can
        -- have return kind `TYPE r` unconditionally (#16827).
      = AnyTYPEKind
      | is_datatype && xopt LangExt.UnliftedDatatypes dflags
        -- With UnliftedDatatypes, we allow kinds other than Type, but they
        -- must still be of the form `TYPE (BoxedRep l)`, so that we don't
        -- accept result kinds like `TYPE IntRep`.
        -- See Note [Implementation of UnliftedDatatypes] in GHC.Tc.TyCl.
      = AnyBoxedKind
      | otherwise
      = LiftedKind

    tYPE_ok :: DynFlags -> Bool
    tYPE_ok dflags = isAllowedDataResKind (allowed_kind dflags) res_kind

    -- In the particular case of a data family, permit a return kind of the
    -- form `:: k` (where `k` is a bare kind variable).
    is_kind_var :: Bool
    is_kind_var | is_data_family = isJust (getCastedTyVar_maybe res_kind)
                | otherwise      = False

    err_msg :: DynFlags -> TcRnMessage
    err_msg dflags =
      TcRnInvalidReturnKind data_sort (allowed_kind dflags) kind (ext_hint dflags)

    ext_hint dflags
      | isTypeLikeKind kind
      , is_newtype
      , not (xopt LangExt.UnliftedNewtypes dflags)
      = Just SuggestUnliftedNewtypes
      | tcIsBoxedTypeKind kind
      , is_datatype
      , not (xopt LangExt.UnliftedDatatypes dflags)
      = Just SuggestUnliftedDatatypes
      | otherwise
      = Nothing

-- | Checks that the result kind of a class is exactly `Constraint`, rejecting
-- type synonyms and type families that reduce to `Constraint`. See #16826.
checkClassKindSig :: Kind -> TcM ()
checkClassKindSig kind = checkTc (isConstraintKind kind) err_msg
  where
    err_msg :: TcRnMessage
    err_msg = TcRnClassKindNotConstraint kind

tcbVisibilities :: TyCon -> [Type] -> [TyConBndrVis]
-- Result is in 1-1 correspondence with orig_args
tcbVisibilities tc orig_args
  = go (tyConKind tc) init_subst orig_args
  where
    init_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfTypes orig_args))
    go _ _ []
      = []

    go fun_kind subst all_args@(arg : args)
      | Just (_, tcb, inner_kind) <- splitPiTy_maybe fun_kind
      = case tcb of
          Anon _ af           -> assert (af == FTF_T_T) $
                                 AnonTCB      : go inner_kind subst  args
          Named (Bndr tv vis) -> NamedTCB vis : go inner_kind subst' args
                 where
                    subst' = extendTCvSubst subst tv arg

      | not (isEmptyTCvSubst subst)
      = go (substTy subst fun_kind) init_subst all_args

      | otherwise
      = pprPanic "addTcbVisibilities" (ppr tc <+> ppr orig_args)


{- Note [splitTyConKind]
~~~~~~~~~~~~~~~~~~~~~~~~
Given
  data T (a::*) :: * -> forall k. k -> *
we want to generate the extra TyConBinders for T, so we finally get
  (a::*) (b::*) (k::*) (c::k)
The function splitTyConKind generates these extra TyConBinders from
the result kind signature.  The same function is also used by
kcCheckDeclHeader_sig to get the [TyConBinder] from the Kind of
the TyCon given in a standalone kind signature.  E.g.
  type T :: forall (a::*). * -> forall k. k -> *

We need to take care to give the TyConBinders
  (a) Uniques that are fresh: the TyConBinders of a TyCon
      must have distinct uniques.

  (b) Preferably, OccNames that are fresh. If we happen to re-use
      OccNames that are other TyConBinders, we'll get a TyCon with
      TyConBinders like [a_72, a_53]; same OccName, different Uniques.
      Then when pretty-printing (e.g. in GHCi :info) we'll see
          data T a a0
      whereas we'd prefer
          data T a b
      (NB: the tidying happens in the conversion to Iface syntax,
      which happens as part of pretty-printing a TyThing.)

      Using fresh OccNames is not essential; it's cosmetic.
      And also see Note [Avoid name clashes for associated data types].

For (a) perhaps surprisingly, duplicated uniques can happen, even if
we use fresh uniques for Anon arrows.  Consider
   data T :: forall k. k -> forall k. k -> *
where the two k's are identical even up to their uniques.  Surprisingly,
this can happen: see #14515, #19092,3,4.  Then if we use those k's in
as TyConBinders we'll get duplicated uniques.

For (b) we'd like to avoid OccName clashes with the tyvars declared by
the user before the "::"; in the above example that is 'a'.

It's reasonably easy to solve all this; just run down the list with a
substitution; hence the recursive 'go' function.  But for the Uniques
it has to be done.

Note [Avoid name clashes for associated data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider    class C a b where
               data D b :: * -> *
When typechecking the decl for D, we'll invent an extra type variable
for D, to fill out its kind.  Ideally we don't want this type variable
to be 'a', because when pretty printing we'll get
            class C a b where
               data D b a0
(NB: the tidying happens in the conversion to Iface syntax, which happens
as part of pretty-printing a TyThing.)

That's why we look in the LocalRdrEnv to see what's in scope. This is
important only to get nice-looking output when doing ":info C" in GHCi.
It isn't essential for correctness.


************************************************************************
*                                                                      *
             Partial signatures
*                                                                      *
************************************************************************

-}

-- | Whether the caller checks pending computations in its inference group.
data PendingSigCalls = RetrySigCalls | RejectSigCalls

tcHsPartialSigType
  :: PendingSigCalls
  -> UserTypeCtxt
  -> LHsSigWcType GhcRn       -- The type signature
  -> TcM ( [(Name, TcTyVar)]  -- Wildcards
         , Maybe TcType       -- Extra-constraints wildcard
         , [(Name,(Matchability, InvisTVBinder))] -- Original tyvar names, in correspondence with
                              --   the implicitly and explicitly bound type variables
         , TcThetaType        -- Theta part
         , TcType             -- Tau part
         , [TcPendingTypeCall] )
-- See Note [Checking partial type signatures]
tcHsPartialSigType call_owner ctxt sig_ty
  | HsWC { hswc_ext  = sig_wcs, hswc_body = sig_ty } <- sig_ty
  , L _ (HsSig{sig_bndrs = hs_outer_bndrs, sig_body = body_ty}) <- sig_ty
  , (hs_ctxt, hs_tau) <- splitLHsQualTy body_ty
  = addSigCtxt ctxt (UserLHsSigType sig_ty) $
    do { mode0 <- mkHoleMode TypeLevel HM_Sig
       ; experimental <- xoptM LangExt.DependentTypes
       ; ambient <- if experimental
                    then tcRetainedAmbientIndices RetainedLocalSignatureIndex body_ty
                    else pure emptyNameEnv
       ; let mode = mode0 { mode_indices = ambient }
       ; pending <- if experimental && not (null sig_wcs)
                    then Just <$> newTcRef []
                    else pure Nothing
       ; (outer_ma, (outer_bndrs, (wcs, wcx, theta, tau)))
            <- solveEqualities "tcHsPartialSigType" $
               -- See Note [Failure in local type signatures]
               do { outer_ma <- tcForAllMatchability (hsOuterMatchability hs_outer_bndrs)
                  ; stuff <- bindNamedWildCardBinders sig_wcs $ \ wcs ->
                             bindOuterSigTKBndrs_Tv_M mode hs_outer_bndrs $
               do {   -- Instantiate the type-class context; but if there
                      -- is an extra-constraints wildcard, just discard it here
                    (theta, wcx) <- tcPartialContext mode hs_ctxt

                  ; ek <- newOpenTypeKind
                  ; let tau_mode
                          | null theta, isNothing wcx, Just calls <- pending
                          = mode { mode_pending_calls = Just (mkNameEnv wcs, call_owner, calls) }
                          | otherwise = mode
                  ; tau <- -- Don't do (addTypeCtxt hs_tau) here else we get
                           --   In the type <blah>
                           --   In the type signature: foo :: <blah>
                           tc_check_lhs_type tau_mode hs_tau ek

                  ; return (wcs, wcx, theta, tau) }
                  ; return (outer_ma, stuff) }

       ; traceTc "tcHsPartialSigType 2" empty
       ; outer_bndrs <- scopedSortOuter outer_bndrs
       ; let outer_tv_bndrs = map (fromMaybe unmatchableDataConTy outer_ma,) (outerTyVarBndrs outer_bndrs)
       ; traceTc "tcHsPartialSigType 3" empty

         -- No kind-generalization here:
       ; let written_ty = mkInvisForAllTys outer_tv_bndrs (tcMkPhiTy theta tau)
       ; kindGeneralizeNone written_ty
       ; checkConstrainedSignature ctxt written_ty
         -- See Note [Constrained associated family formation] in GHC.Tc.Validity.

       -- Spit out the wildcards (including the extra-constraints one)
       -- as "hole" constraints, so that they'll be reported if necessary
       -- See Note [Extra-constraint holes in partial type signatures]
       ; mapM_ emitNamedTypeHole wcs

         -- The "tau" from tcHsPartialSigType might very well have some foralls
         -- at the top, hidden behind a type synonym. Instantiate them! E.g.
         --    type T x = forall b. x -> b -> b
         --    f :: forall a. T (a,_)
         -- We must instantiate the `forall b` just as we do the `forall a`!
         -- Missing this led to #21667.
       ; (tv_prs', theta', tau) <- tcInstTypeBndrs tau

         -- We return a proper (Name,InvisTVBinder) environment, to be sure that
         -- we bring the right name into scope in the function body.
         -- Test case: partial-sigs/should_compile/LocalDefinitionBug
       ; let outer_bndr_names :: [Name]
             outer_bndr_names = hsOuterTyVarNames hs_outer_bndrs
             tv_prs :: [(Name,(Matchability, InvisTVBinder))]
             tv_prs = outer_bndr_names `zip` outer_tv_bndrs

       -- Zonk, so that any nested foralls can "see" their occurrences
       -- See Note [Checking partial type signatures], and in particular
       -- Note [Levels for wildcards]
       ; (tv_prs, theta, tau) <- liftZonkM $
         do { tv_prs <- mapSndM (\ (ma,bndr) -> (,) <$> zonkTcType ma <*> zonkInvisTVBinder bndr)
                               (tv_prs ++ tv_prs')
            ; theta  <- mapM    zonkTcType        (theta ++ theta')
            ; tau    <- zonkTcType                tau
            ; return (tv_prs, theta, tau) }

      -- NB: checkValidType on the final inferred type will be
      --     done later by checkInferredPolyId.  We can't do it
      --     here because we don't have a complete type to check

       ; traceTc "tcHsPartialSigType" (ppr tv_prs)
       ; calls <- maybe (pure []) (fmap reverse . readTcRef) pending
       ; return (wcs, wcx, tv_prs, theta, tau, calls) }

tcPartialContext :: TcTyMode -> Maybe (LHsContext GhcRn) -> TcM (TcThetaType, Maybe TcType)
tcPartialContext _ Nothing = return ([], Nothing)
tcPartialContext mode (Just (L _ (HsContext _ hs_theta)))
  | Just (hs_theta1, hs_ctxt_last) <- snocView hs_theta
  , L wc_loc ty@(HsWildCardTy _) <- ignoreParens hs_ctxt_last
  = do { wc_tv_ty <- setSrcSpanA wc_loc $
                     tcAnonWildCardOcc YesExtraConstraint mode ty constraintKind
       ; theta <- mapM (tc_lhs_pred mode) hs_theta1
       ; return (theta, Just wc_tv_ty) }
  | otherwise
  = do { theta <- mapM (tc_lhs_pred mode) hs_theta
       ; return (theta, Nothing) }

{- Note [Checking partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note is about tcHsPartialSigType.  See also
Note [Recipe for checking a signature]

When we have a partial signature like
   f :: forall a. a -> _
we do the following

* tcHsPartialSigType does not make quantified type (forall a. blah)
  and then instantiate it -- it makes no sense to instantiate a type
  with wildcards in it.  Rather, tcHsPartialSigType just returns the
  'a' and the 'blah' separately.

  Nor, for the same reason, do we push a level in tcHsPartialSigType.

* We instantiate 'a' to a unification variable, a TyVarTv, and /not/
  a skolem; hence the "_Tv" in bindExplicitTKBndrs_Tv.  Consider
    f :: forall a. a -> _
    g :: forall b. _ -> b
    f = g
    g = f
  They are typechecked as a recursive group, with monomorphic types,
  so 'a' and 'b' will get unified together.  Very like kind inference
  for mutually recursive data types (sans CUSKs or SAKS); see
  Note [Cloning for type variable binders]

* In GHC.Tc.Gen.Sig.tcUserSigType we return a PartialSig, which (unlike
  the companion CompleteSig) contains the original, as-yet-unchecked
  source-code LHsSigWcType

* Then, for f and g /separately/, we call tcInstSig, which in turn
  call tcHsPartialSig (defined near this Note).  It kind-checks the
  LHsSigWcType, creating fresh unification variables for each "_"
  wildcard.  It's important that the wildcards for f and g are distinct
  because they might get instantiated completely differently.  E.g.
     f,g :: forall a. a -> _
     f x = a
     g x = True
  It's really as if we'd written two distinct signatures.

* Nested foralls. See Note [Levels for wildcards]

* Just as for ordinary signatures, we must solve local equalities and
  zonk the type after kind-checking it, to ensure that all the nested
  forall binders can "see" their occurrences

  Just as for ordinary signatures, this zonk also gets any Refl casts
  out of the way of instantiation.  Example: #18008 had
       foo :: (forall a. (Show a => blah) |> Refl) -> _
  and that Refl cast messed things up.  See #18062.

Note [Levels for wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
     f :: forall b. (forall a. a -> _) -> b
We do /not/ allow the "_" to be instantiated to 'a'; although we do
(as before) allow it to be instantiated to the (top level) 'b'.
Why not?  Suppose
   f x = (x True, x 'c')

During typecking the RHS we must instantiate that (forall a. a -> _),
so we must know /precisely/ where all the a's are; they must not be
hidden under (possibly-not-yet-filled-in) unification variables!

We achieve this as follows:

- For /named/ wildcards such sas
     g :: forall b. (forall la. a -> _x) -> b
  there is no problem: we create them at the outer level (ie the
  ambient level of the signature itself), and push the level when we
  go inside a forall.  So now the unification variable for the "_x"
  can't unify with skolem 'a'.

- For /anonymous/ wildcards, such as 'f' above, we carry the ambient
  level of the signature to the hole in the TcLevel part of the
  mode_holes field of TcTyMode.  Then, in tcAnonWildCardOcc we us that
  level (and /not/ the level ambient at the occurrence of "_") to
  create the unification variable for the wildcard.  That is the sole
  purpose of the TcLevel in the mode_holes field: to transport the
  ambient level of the signature down to the anonymous wildcard
  occurrences.

Note [Extra-constraint holes in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: (_) => a -> a
  f x = ...

* The renamer leaves '_' untouched.

* Then, in tcHsPartialSigType, we make a new hole TcTyVar, in
  tcWildCardBinders.

* GHC.Tc.Gen.Bind.chooseInferredQuantifiers fills in that hole TcTyVar
  with the inferred constraints, e.g. (Eq a, Show a)

* GHC.Tc.Errors.mkHoleError finally reports the error.

An annoying difficulty happens if there are more than 64 inferred
constraints. Then we need to fill in the TcTyVar with (say) a 70-tuple.
Where do we find the TyCon?  For good reasons we only have constraint
tuples up to 62 (see Note [How tuples work] in GHC.Builtin.WiredIn.Types).  So how
can we make a 70-tuple?  This was the root cause of #14217.

It's incredibly tiresome, because we only need this type to fill
in the hole, to communicate to the error reporting machinery.  Nothing
more.  So I use a HACK:

* I make an /ordinary/ tuple of the constraints, in
  GHC.Tc.Gen.Bind.chooseInferredQuantifiers. This is ill-kinded because
  ordinary tuples can't contain constraints, but it works fine. And for
  ordinary tuples we don't have the same limit as for constraint
  tuples (which need selectors and an associated class).

* Because it is ill-kinded (unifying something of kind Constraint with
  something of kind Type), it should trip an assert in writeMetaTyVarRef.

Result works fine, but it may eventually bite us.

See also Note [Do not simplify ConstraintHoles] in GHC.Tc.Solver for
information about how these are printed.

************************************************************************
*                                                                      *
      Pattern signatures (i.e signatures that occur in patterns)
*                                                                      *
********************************************************************* -}

tcHsPatSigType :: UserTypeCtxt
               -> HoleMode -- HM_Sig when in a SigPat, HM_TyAppPat when in a ConPat checking type applications.
               -> HsPatSigType GhcRn          -- The type signature
               -> ContextKind                -- What kind is expected
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)       -- The type
-- Used for type-checking type signatures in
-- (a) patterns           e.g  f (x::Int) = e
-- (b) RULE forall bndrs  e.g. forall (x::Int). f x = x
-- See Note [Pattern signature binders and scoping] in GHC.Hs.Type
--
-- This may emit constraints
-- See Note [Recipe for checking a signature]
tcHsPatSigType ctxt hole_mode
  (HsPS { hsps_ext  = HsPSRn { hsps_nwcs = sig_wcs, hsps_imp_tvs = sig_ns }
        , hsps_body = hs_ty })
  ctxt_kind
  = tc_type_in_pat ctxt Nothing hole_mode hs_ty sig_wcs sig_ns ctxt_kind

tcRuleBndrSig :: Name
              -> SkolemInfo
              -> HsPatSigType GhcRn          -- The type signature
              -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                     , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                             -- the scoped type variables
                     , TcType)       -- The type
-- Used for type-checking type signatures in
--     RULE forall bndrs  e.g. forall (x::Int). f x = x
-- See Note [Pattern signature binders and scoping] in GHC.Hs.Type
--
-- This may emit constraints
-- See Note [Recipe for checking a signature]
tcRuleBndrSig name skol_info
    (HsPS { hsps_ext  = HsPSRn { hsps_nwcs = sig_wcs, hsps_imp_tvs = sig_ns }
          , hsps_body = hs_ty })
  = tc_type_in_pat (RuleBndrTypeCtxt name) (Just skol_info)
                   HM_Sig hs_ty sig_wcs sig_ns OpenKind

-- Typecheck type patterns, in data constructor patterns, e.g
--    f (MkT @a @(Maybe b) ...) = ...
--
-- We have two completely separate typing rules,
--   one for binder type patterns  (handled by `tc_bndr_in_pat`)
--   one for unifier type patterns (handled by `tc_type_in_pat`)
-- The two cases are distinguished by `tyPatToBndr`.
-- See Note [Type patterns: binders and unifiers]
tcHsTyPat :: HsTyPat GhcRn               -- The type pattern
          -> Kind                        -- What kind is expected
          -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                 , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                         -- the scoped type variables
                 , TcType)               -- The type
tcHsTyPat hs_pat@(HsTP{hstp_ext = hstp_rn, hstp_body = hs_ty}) expected_kind
  = case tyPatToBndr hs_pat of
    Nothing   -> tc_unif_in_pat hs_ty wcs all_ns (TheKind expected_kind)
    Just bndr -> tc_bndr_in_pat bndr  wcs imp_ns  expected_kind
  where
    all_ns = imp_ns ++ exp_ns
    HsTPRn{hstp_nwcs = wcs, hstp_imp_tvs = imp_ns, hstp_exp_tvs = exp_ns} = hstp_rn
    tc_unif_in_pat = tc_type_in_pat TypeAppCtxt Nothing HM_TyAppPat

-- `tc_bndr_in_pat` is used in type patterns to handle the binders case.
-- See Note [Type patterns: binders and unifiers]
tc_bndr_in_pat :: HsTyVarBndr flag GhcRn
               -> [Name]  -- All named wildcards in type
               -> [Name]  -- Implicit (but not explicit) binders in type
               -> Kind    -- Expected kind
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)               -- The type
tc_bndr_in_pat bndr wcs imp_ns expected_kind = do
  let HsTvb { tvb_var = bvar, tvb_kind = bkind } = bndr
  traceTc "tc_bndr_in_pat 1" (ppr expected_kind)
  name <- tcHsBndrVarName bvar
  tv <- newPatTyVar name expected_kind
  case bkind of
    HsBndrNoKind _ ->
      pure ([], mk_tvb_pairs bndr tv, mkTyVarTy tv)
    HsBndrKind _ ki -> do
      tkv_prs <- mapM new_implicit_tv imp_ns
      wcs <- addTypeCtxt ki              $
             solveEqualities "tc_bndr_in_pat" $
               -- See Note [Failure in local type signatures]
               -- and c.f #16033
             bindNamedWildCardBinders wcs $ \ wcs ->
             tcExtendNameTyVarEnv tkv_prs $
             do { tcHsTvbKind bvar ki expected_kind
                ; pure wcs }

      mapM_ emitNamedTypeHole wcs

      traceTc "tc_bndr_in_pat 2" $ vcat
        [ text "expected_kind" <+> ppr expected_kind
        , text "wcs" <+> ppr wcs
        , text "(name,tv)" <+>  ppr (name,tv)
        , text "tkv_prs" <+> ppr tkv_prs]

      let tvb_prs = mk_tvb_pairs bndr tv
      pure (wcs, tvb_prs ++ tkv_prs, mkTyVarTy tv)
  where
    new_implicit_tv name
      = do { kind <- newMetaKindVar
           ; tv   <- newPatTyVar name kind
             -- NB: tv's Name is fresh
           ; return (name, tv) }

-- * In type patterns `tc_type_in_pat` is used to handle the unifiers case.
--   See Note [Type patterns: binders and unifiers]
--
-- * In patterns `tc_type_in_pat` is used to check pattern signatures.
tc_type_in_pat :: UserTypeCtxt
               -> Maybe SkolemInfo    -- Just sk for RULE and SPECIALISE pragmas only
               -> HoleMode -- HM_Sig when in a SigPat, HM_TyAppPat when in a ConPat checking type applications.
               -> LHsType GhcRn          -- The type in pattern
               -> [Name]                 -- All named wildcards in type
               -> [Name]                 -- All binders in type
               -> ContextKind                -- What kind is expected
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)       -- The type
tc_type_in_pat ctxt mb_skol hole_mode hs_ty wcs ns ctxt_kind
  = addSigCtxt ctxt (UserLHsType hs_ty) $
    do { tkvs <- mapM new_implicit_tv ns
       ; let tkv_prs = ns `zip` tkvs
       ; mode <- mkHoleMode TypeLevel hole_mode
       ; (wcs, ty)
            <- addTypeCtxt hs_ty                $
               solveEqualities "tc_type_in_pat" $
                 -- See Note [Failure in local type signatures]
                 -- and c.f #16033
               bindNamedWildCardBinders wcs $ \ wcs ->
               tcExtendNameTyVarEnv tkv_prs $
               do { ek <- newExpectedKind ctxt_kind
                  ; experimental <- xoptM LangExt.DependentTypes
                  ; mode <- case ctxt of
                      RuleBndrTypeCtxt{} -> do
                        indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex hs_ty
                        return (mode { mode_indices = indices })
                      PatSigCtxt | experimental -> do
                        indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex hs_ty
                        return (mode { mode_indices = indices })
                      _ -> return mode
                  ; ty <- tc_check_lhs_type mode hs_ty ek
                  ; return (wcs, ty) }

        ; mapM_ emitNamedTypeHole wcs

          -- ty might have tyvars that are at a higher TcLevel (if hs_ty
          -- contains a forall). Promote these.
          -- Ex: f (x :: forall a. Proxy a -> ()) = ... x ...
          -- When we instantiate x, we have to compare the kind of the argument
          -- to a's kind, which will be a metavariable.
          -- kindGeneralizeNone does this:
        ; kindGeneralizeNone ty
        ; ty <- liftZonkM $ zonkTcType ty
        ; checkValidType ctxt ty

        ; traceTc "tc_type_in_pat" (ppr tkv_prs)
        ; return (wcs, tkv_prs, ty) }
  where
    new_implicit_tv name
      = do { kind <- newMetaKindVar
           ; case mb_skol of
                Just skol_info -> newSkolemTyVar skol_info name kind
                Nothing        -> newPatTyVar name kind }
                -- See Note [Typechecking pattern signature binders]
                -- NB: tv's Name may be fresh (in the case of newPatTyVar)

-- See Note [Type patterns: binders and unifiers]
tyPatToBndr :: HsTyPat GhcRn -> Maybe (HsTyVarBndr () GhcRn)
tyPatToBndr HsTP{hstp_body = (L _ hs_ty)} = go hs_ty where
  go :: HsType GhcRn -> Maybe (HsTyVarBndr () GhcRn)
  go (HsParTy _ (L _ ty)) = go ty
  go (HsKindSig _ (L _ ty) ki) = do
    bvar <- go_bvar ty
    let bkind = HsBndrKind noExtField ki
    Just (HsTvb noAnn () bvar bkind)
  go ty = do
    bvar <- go_bvar ty
    let bkind = HsBndrNoKind noExtField
    Just (HsTvb noAnn () bvar bkind)

  go_bvar :: HsType GhcRn -> Maybe (HsBndrVar GhcRn)
  go_bvar (HsTyVar _ _ tv)
    | isTyVarName (getName tv)
    = Just (HsBndrVar noExtField (fmap getName tv))
  go_bvar (HsWildCardTy h)
    = Just (HsBndrWildCard h)
  go_bvar _ = Nothing

{- Note [Type patterns: binders and unifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type pattern, of type `HsTyPat`, represents a type argument in a data
constructor pattern.  For example
    f (MkT @a @(Maybe b) p q) = ...
Here the `@a` and `@(Maybe b)` are type patterns.  In general, then a
`HsTyPat` is represented by a `HsType`.

However, for /typechecking/ purposes (only) we distinguish two categories of
type pattern:
* Binder type patterns
* Unifier type patterns

Binder type patterns are a subset of type patterns described by the following grammar:

  bvar ::= tv  | '_'        -- type variable or wildcard
  tp_bndr ::=
      bvar                  -- plain binder
    | bvar '::' kind        -- binder with kind annotation
    | '(' tp_bndr ')'       -- parentheses

This subset of HsTyPat can be represented by HsTyVarBndr, which is also used
in foralls and type declaration headers.

Unifier type patterns include all other forms of type patterns, such as `Maybe x`.
This distinction allows the typechecker to accept more programs.
Consider this example from #18986:

  data T where
    MkT :: forall (f :: forall k. k -> Type).
      f Int -> f Maybe -> T

  k :: T -> ()
  k (MkT @f (x :: f Int) (y :: f Maybe)) = ()

In general case (if we treat `f` as a unifier) we would create a metavariable for its kind:
  f :: kappa
Checking `x :: f Int` would unify
  kappa := Type -> Type
and then checking `y :: f Maybe` would unify
  kappa := (Type -> Type) -> Type
leading to a type error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’

However, `@f` is a simple type variable binder, we don't need a metavariable for its kind, we
can add it directly to the context with its polymorphic kind:
  f :: forall k . k -> Type
This way both `f Int` and `f Maybe` can be accepted because `k` can be instantiated differently at
each call site.

Note [Typechecking pattern signature binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Type variables in the type environment] in GHC.Tc.Utils.
Consider

  data T where
    MkT :: forall a. a -> (a -> Int) -> T

  f :: T -> ...
  f (MkT x (f :: b -> c)) = <blah>

Here
 * The pattern (MkT p1 p2) creates a *skolem* type variable 'a_sk',
   It must be a skolem so that it retains its identity, and
   GHC.Tc.Errors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: b -> c) makes fresh meta-tyvars
   beta and gamma (TauTvs), and binds "b" :-> beta, "c" :-> gamma in the
   environment

 * Then unification makes beta := a_sk, gamma := Int
   That's why we must make beta and gamma a MetaTv,
   not a SkolemTv, so that it can unify to a_sk (or Int, respectively).

 * Finally, in '<blah>' we have the envt "b" :-> beta, "c" :-> gamma,
   so we return the pairs ("b" :-> beta, "c" :-> gamma) from tcHsPatSigType,

Another example (#13881):
   fl :: forall (l :: [a]). Sing l -> Sing l
   fl (SNil :: Sing (l :: [y])) = SNil
When we reach the pattern signature, 'l' is in scope from the
outer 'forall':
   "a" :-> a_sk :: *
   "l" :-> l_sk :: [a_sk]
We make up a fresh meta-TauTv, y_sig, for 'y', and kind-check
the pattern signature
   Sing (l :: [y])
That unifies y_sig := a_sk.  We return from tcHsPatSigType with
the pair ("y" :-> y_sig).

For RULE binders, though, things are a bit different (yuk).
  RULE "foo" forall (x::a) (y::[a]).  f x y = ...
Here this really is the binding site of the type variable so we'd like
to use a skolem, so that we get a complaint if we unify two of them
together.  Hence the new_implicit_tv function in tcHsPatSigType.


************************************************************************
*                                                                      *
        Checking kinds
*                                                                      *
************************************************************************

-}

unifyKinds :: TcTyMode -> [LHsType GhcRn] -> [(TcType, TcKind)] -> TcM ([TcType], TcKind)
unifyKinds mode rn_tys act_kinds
  = do { kind <- newMetaKindVar
       ; let check rn_ty (ty, act_kind)
               = checkExpectedKind mode (unLoc rn_ty) ty act_kind kind
       ; tys' <- zipWithM check rn_tys act_kinds
       ; return (tys', kind) }

{-
************************************************************************
*                                                                      *
        Sort checking kinds
*                                                                      *
************************************************************************

tcLHsKindSig converts a user-written kind to an internal, sort-checked kind.
It does sort checking and desugaring at the same time, in one single pass.
-}

tcLHsKindSig :: UserTypeCtxt -> LHsKind GhcRn -> TcM Kind
tcLHsKindSig ctxt hs_kind
  = tc_lhs_kind_sig kindLevelMode ctxt hs_kind

-- | A type-family result can depend on metadata chosen by each application.
tcLHsFamilyResultKindSig :: UserTypeCtxt -> LHsKind GhcRn -> TcM Kind
tcLHsFamilyResultKindSig = tc_lhs_kind_sig
  (kindLevelMode { mode_synonym_metadata = familyKindMetadata })

-- | Give each checked kind application its own metadata variables.
familyKindMetadata :: Maybe (Name -> [Bool] -> TcM [Type], Maybe (TypeOrKind -> TcM Matchability))
familyKindMetadata = Just (indices, Nothing)
  where
    indices _ callbacks = do
      vars <- mapM (const (newFlexiTyVar matchabilityTy)) callbacks
      owner <- tcl_kind_arrow_defaults <$> getLclEnv
      forM_ owner $ \(_, ref) -> updTcRef ref (vars ++)
      pure (mkTyVarTys vars)

tc_lhs_kind_sig :: TcTyMode -> UserTypeCtxt -> LHsKind GhcRn -> TcM Kind
tc_lhs_kind_sig mode ctxt hs_kind
-- See  Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
-- Result is zonked
  = do { kind <- addErrCtxt (KindCtxt hs_kind) $
                 solveEqualities "tcLHsKindSig" $
                 tc_check_lhs_type mode hs_kind liftedTypeKind
       ; traceTc "tcLHsKindSig" (ppr hs_kind $$ ppr kind)
       -- No generalization:
       ; kindGeneralizeNone kind
       ; kind <- liftZonkM $ zonkTcType kind
         -- This zonk is very important in the case of higher rank kinds
         -- E.g. #13879    f :: forall (p :: forall z (y::z). <blah>).
         --                          <more blah>
         --      When instantiating p's kind at occurrences of p in <more blah>
         --      it's crucial that the kind we instantiate is fully zonked,
         --      else we may fail to substitute properly

       ; checkValidType ctxt kind
       ; traceTc "tcLHsKindSig2" (ppr kind)
       ; return kind }

promotionErr :: Name -> PromotionErr -> TcM a
promotionErr name err
  = do { traceTc "promotionError" (ppr name)
       ; failWithTc $ TcRnUnpromotableThing name err }

{-
************************************************************************
*                                                                      *
          Utils for constructing TyLit
*                                                                      *
************************************************************************
-}

tyLitFromOverloadedLit :: OverLitVal GhcRn -> HsLit GhcRn
tyLitFromOverloadedLit (HsIntegral   n) = HsNatural noExtField n
tyLitFromOverloadedLit (HsFractional f) = HsDouble  noExtField f
tyLitFromOverloadedLit (HsIsString   s) = HsString  (sl_src s) (sl_fs s)

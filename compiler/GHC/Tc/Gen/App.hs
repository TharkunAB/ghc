{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.Gen.App
       ( tcApp, tcRuleAppPrefix, RuleAppPrefix(..)
       , tcExprPrag
       , tcQuickLookLambdaMatches
       , tcLexicalPackageOperand
       , tcRetainedInferableArg, tcRetainedArg
       , RetainedCoreMode(..), tcExprToRetainedCore
       , tcOptionalLetOperand, tcOptionalLetFunctionOperand
       , expr_to_type, expr_to_type_at_kind, isUnadornedTypeExpr ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcCheckMonoExpr, tcCheckPolyExpr, tcPolyExpr, tcInferRho )

import GHC.Hs

import GHC.Rename.Env ( addUsedGRE, DeprecationWarnings(AllDeprecationWarnings, NoDeprecationWarnings) )
import GHC.Rename.Utils ( mkExpandedLExpr )
import GHC.Tc.Gen.Head
import GHC.Tc.Gen.Match ( implicitPayloadType, tcCloseRuntimeScope )
import GHC.Tc.Gen.SourceTable ( tcLookupSourceTableId, sourceTableOccurrence, recordSourceMethodUse, recordSourceLiteralUse, lookupBootModuleIdMaybe )
import GHC.Tc.Gen.Sig ( isCompleteHsSig )
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr
import GHC.Tc.Utils.Monad
import GHC.Tc.Solver ( simplifyTopWanteds, tcRewriteFamilyAxioms )
import GHC.Tc.Solver.Monad ( bumpReductionDepthTcM, matchFamTcM, runTcSEqualities )
import GHC.Tc.Validity ( checkConstrainedSignature )
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Env ( isExistsPackName, tcLookupIdMaybe, tcLookupConLike, tcLookupKnownOccId
                        , tcLookup, tcLookupGlobal, tcLookupLcl_maybe
                        , tcExtendIdEnv, tcExtendIdEnv1, tcExtendGeneratedIdEnv )
import GHC.Iface.Env ( lookupOrig )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst, tcLookupDataFamInst_maybe )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.Concrete  ( unifyConcrete, idConcreteTvs )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint (WantedConstraints(wc_simple, wc_impl), emptyWC, andWC, isEmptyWC, CtEvidence(CtWanted), TcEvDest(EvVarDest),
                                ctPred, ctEvLoc, ctEvEvId, mkWantedCt,
                                mkNonCanonical, DelayedError(DE_SourceMethod), Implication(..))
import GHC.Tc.Types.CtLoc ( CtLoc )
import GHC.Tc.Types.ErrCtxt ( FunAppCtxtFunArg(..), ReportRedundantConstraints(NoRRC) )
import GHC.Tc.Types.LclEnv ( getLclEnvBinderStack, getLclEnvTypeEnv )
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Zonk.TcType

import GHC.Core.Class ( Class, classTyVars )
import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.DataCon ( DataCon, dataConConcreteTyVars, dataConFullSig, dataConName, isNewDataCon, dataConOrigArgTys, dataConOrigResTy, dataConSourceArity, dataConHasRetainedResult, dataConRepArgBinders, dataConTyCon, dataConUnivTyVars, dataConUserTyVars, dataConWorkId, dataConWrapId )
import qualified GHC.Core as Core
import GHC.Core.Dependent ( hasNativeDependentExpr, hasScopedDictionaryType
                          , isScopedSelectorReceiver, supportsQuickLookType )
import GHC.Core.FamInstEnv ( reduceTyFamApp_maybe )
import GHC.Core.Make ( mkCharExpr, mkCoreTup, mkNaturalExpr )
import GHC.Core.Multiplicity ( mkMultMul )
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Utils ( applyTypeToArgs, exprType, isVacuousRetainedBinder )
import GHC.Core.FVs ( exprFreeVars, exprFreeVarsList )
import GHC.Core.InstEnv ( lookupUniqueInstEnv, instanceDFunId )
import GHC.Core.Predicate ( classMethodInstTy, getEqPredTys_maybe, mkNomEqPred )
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Unify ( tcMatchTyX_BM, tcMatchRetainedTyX_BM, tcUnifyTys, BindFlag(..), BindTvFun )
import GHC.Core.Type
import GHC.Core.UsageEnv ( UsageEnv, popUE, singleUsageUE, zeroUE )
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom (coAxiomBranches, coAxBranchLHS, coAxBranchRHS, coAxBranchCoVars, fromBranches)

import GHC.Builtin.WiredIn.Types ( charTy, integerTy, matchabilityTy, multiplicityTy, runtimeRepTy, nilDataCon, consDataCon, mkListTy, tupleDataCon, unitDataCon, unmatchableDataConTy )
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs ( composeIdOcc, fromIntegerClassOpOcc )
import GHC.Builtin.Modules ( gHC_INTERNAL_BASE )

import GHC.Driver.DynFlags ( GeneralFlag(Opt_DeferTypeErrors, Opt_DeferTypedHoles, Opt_DeferOutOfScopeVariables), DynFlags(reductionDepth), HasDynFlags(getDynFlags) )
import GHC.Types.TyThing ( TyThing(AnId, AConLike, ATyCon) )
import GHC.Types.Basic ( RecFlag(NonRecursive), TypeOrKind(TypeLevel), intGtLimit
                       , Origin(Generated), GenReason(OtherExpansion), DoPmc(SkipPmc) )
import GHC.Types.TypeEnv ( lookupTypeEnv )
import GHC.Types.Var
import GHC.Types.Id ( idName, idType, isRecordSelector, mkLocalId, isClassOpId_maybe, isDFunId, realIdUnfolding, recordSelectorTyCon_maybe, setInlinePragma, sourceTableId_maybe, sourceMethodTarget_maybe, sourceLiteralCons_maybe )
import GHC.Types.InlinePragma ( neverInlinePragma )
import GHC.Types.Id.Info ( RecSelParent(RecSelData) )
import GHC.Types.FieldLabel ( FieldLabel(flSelector) )
import GHC.Types.Var.Set ( VarSet, IdSet, elemVarSet, emptyVarSet, extendVarSet, isEmptyVarSet, mkVarSet, sizeVarSet, unionVarSet, unitVarSet )
import GHC.Types.Var.FV
import GHC.Types.Name
import GHC.Unit.Types ( isInteractiveModule )
import GHC.Types.Name.Env
import GHC.Types.Name.Set ( elemNameSet, mkNameSet )
import GHC.Types.Unique ( getKey )
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.SourceText ( SourceText(NoSourceText) )
import GHC.Types.Var.Env  ( emptyTidyEnv, mkInScopeSet )

import GHC.Data.Maybe
import GHC.Data.Bag ( bagToList, unitBag )
import GHC.Data.FastString
import Language.Haskell.Syntax.Text ( unpackHText )

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import qualified GHC.LanguageExtensions as LangExt

import Control.Applicative ( (<|>) )
import Control.Exception ( throwIO )
import Control.Monad
import Data.Data ( Data, cast, gmapQ )
import Data.Dynamic ( fromDynamic, toDyn )
import Data.Function
import Data.List ( find, sortOn )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy ( Proxy(..) )
import Data.Semigroup
import Data.Typeable ( typeRep )

import GHC.Prelude

{- *********************************************************************
*                                                                      *
                 Quick Look overview
*                                                                      *
********************************************************************* -}

{- Note [Quick Look overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The implementation of Quick Look closely follows the QL paper
   A quick look at impredicativity, Serrano et al, ICFP 2020
   https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/

All the moving parts are in this module, GHC.Tc.Gen.App, so named
because it deal with n-ary application.  The main workhorse is tcApp.

Some notes relative to the paper

(QL1) The "instantiation variables" of the paper are ordinary unification
  variables.  We keep track of which variables are instantiation variables
  by giving them a TcLevel of QLInstVar, which is like "infinity".

  See Note [QuickLook instantiation variables] in GHC.Tc.Types.TcType.

(QL2) When we learn what an instantiation variable must be, we simply unify
  it with that type; this is done in qlUnify, which is the function mgu_ql(t1,t2)
  of the paper.  This may fill in a (mutable) instantiation variable with
  a polytype.

(QL3) When QL is done, we turn the instantiation variables into ordinary unification
  variables, using qlZonkTcType.  This function fully zonks the type (thereby
  revealing all the polytypes), and updates any instantiation variables with
  ordinary unification variables. See Note [Instantiation variables are short lived].

(QL4) We cleverly avoid the quadratic cost of QL, alluded to in the paper.
  See Note [Quick Look at value arguments]

Note [Instantiation variables are short lived]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* An instantation variable is a mutable meta-type-variable, whose level number
  is QLInstVar.  See Note [QuickLook instantiation variables] in GHC.Tc.Utils.TcType.

* Ordinary unification variables always stand for monotypes; only instantiation
  variables can be unified with a polytype (by `qlUnify`).

* Before we start typechecking the arguments of the call, in tcValArgs, we will
  (a) Monomorphise any un-filled-in instantiation variables
      (see Note [Monomorphise instantiation variables] in GHC.Tc.Utils.Unify),
      This happens in Step 4, at the start of `finishApp`.
  (b) Zonk the argument type to reveal any polytypes before typechecking that
      argument (see calls to `zonkTcType` and "Crucial step" in tcValArg).
  See Section 4.3 "Applications and instantiation" of the paper.

  TL;DR: instantiation variables are short-lived. So it is fine for them
         to have an infinite level (=QLInstVar) because they are monomorphised
         before we do anything like skolem-escape checks.

* The constraint solver never sees an instantiation variable
  [not quite true; see below]

  However, the constraint solver can see a meta-type-variable filled
  in with a polytype (#18987). Suppose
    f :: forall a. Dict a => [a] -> [a]
    xs :: [forall b. b->b]
  and consider the call (f xs).  QL will
  * Instantiate f, with a := kappa, where kappa is an instantiation variable
  * Emit a constraint (Dict kappa), via instantiateSigma, called from tcInstFun
  * Do QL on the argument, to discover kappa := forall b. b->b

  But by the time the third step has happened, the constraint has been emitted
  into the monad.  The constraint solver will later find it, and rewrite it to
  (Dict (forall b. b->b)). That's fine -- the constraint solver does no implicit
  instantiation (which is what makes it so tricky to have foralls hiding inside
  unification variables), so there is no difficulty with allowing those
  filled-in kappa's to persist.  (We could find them and zonk them away, but
  that would cost code and execution time, for no purpose.)

  Since the constraint solver does not do implicit instantiation (as the
  constraint generator does), the fact that a unification variable might stand
  for a polytype does not matter.

* Actually, sadly the constraint solver /can/ see an instantiation variable.
  Consider this from test VisFlag1_ql:
     f :: forall {k} {a :: k} (hk :: forall j. j -> Type). hk a -> ()

     bad_wild :: ()
     bad_wild = f @_ MkV
  In tcInstFun instantiate f with [k:=k0, a:=a0], and then encounter the `@_`,
  expecting it to have kind (forall j. j->Type).  We make a fresh variable (it'll
  be an instantiation variable since we are in tcInstFun) for the `_`, thus
  (_ : k0) and do `checkExpectedKind` to match up `k0` with `forall j. j->Type`.
  The unifier doesn't solve it (it does not unify instantiation variables) so
  it leaves it for the constraint solver.  Yuk.   It's hard to see what to do
  about this, but it seems to do no harm for the constraint solver to see the
  occasional instantiation variable.
-}


{- *********************************************************************
*                                                                      *
              Typechecking n-ary applications
*                                                                      *
********************************************************************* -}

{- Note [Application chains and heads]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Quick Look treats application chains specially.  What is an
"application chain"?  See Fig 2, of the QL paper: "A quick look at
impredicativity" (ICFP'20). Here's the syntax:

app ::= head
     |  app expr            -- HsApp: ordinary application
     |  app @type           -- HsTypeApp: VTA
     |  expr `head` expr    -- OpApp: infix applications
     |  ( app )             -- HsPar: parens
     |  {-# PRAGMA #-} app  -- HsPragE: pragmas

head ::= f                -- HsVar:    variables
      |  fld              -- HsRecSel: record field selectors
      |  (expr :: ty)     -- ExprWithTySig: expr with user type sig
      |  lit              -- HsOverLit: overloaded literals

* When tcExpr sees something that starts an application chain (namely,
  any of the constructors in 'head' or in 'app'), it invokes tcApp to
  typecheck it: see Note [tcApp: typechecking applications].  However,
  for HsPar and HsPragE, there is no tcWrapResult (which would
  instantiate types, bypassing Quick Look), so nothing is gained by
  using the application chain route, and we can just recurse to tcExpr.

* If tcExpr sees a constructor belonging to app (except HsPar and HsPargE),
  it does not directly call tcApp, it first splits the expression
  into a maximal application chain using splitHsApps (see `GHC.Tc.Gen.Expr.tcCollectApp`), and obtain
  the head of the application chain and a list of arguments.

* A "head" has three special cases (for which we can infer a polytype
  using tcInferAppHead_maybe); otherwise is just any old expression (for
  which we can infer a rho-type (via runInferExpr).

* There is no special treatment for HsHole (HsVar ...), HsOverLit, etc, because
  we can't get a polytype from them.

* Left and right sections (e.g. (x +) and (+ x)) are not yet supported.
  Probably left sections (x +) would be easy to add, since x is the
  first arg of (+); but right sections are not so easy.  For symmetry
  reasons I've left both unchanged, in GHC.Tc.Gen.Expr.

* It may not be immediately obvious why ExprWithTySig (e::ty) should be
  dealt with by tcApp, even when it is not applied to anything. Consider

      f :: [forall a. a->a] -> Int
      ...(f (undefined :: forall b. b))...

  Clearly this should work!  But it will /only/ work because if we
  instantiate that (forall b. b) impredicatively!  And that only happens
  in tcApp.

Note [tcApp: typechecking applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcApp implements the APP-Downarrow/Uparrow rule of
Fig 3, plus the modification in Fig 5, of the QL paper:
"A quick look at impredicativity" (ICFP'20).

It treats application chains (f e1 @ty e2) specially:

* So we can report errors like "in the third argument of a call of f"

* So we can do Visible Type Application (VTA), for which we must not
  eagerly instantiate the function part of the application.

* So that we can do Quick Look impredicativity.

tcApp accepts 4 arguments:
  1. The original expression
  2. The expression at the head of the application
  3. the argument list (types and terms)
  4. The expected result type

PRECONDITION : the head (2) and the list of arguments (3) will
               are the de-constructred version of the expression (1)
POSTCONDITION: The return expression is the typechecked version of (1)

tcApp works on the application chain by:

1. Using `tcInferAppHead` to infer the type of the function,
     as an (uninstantiated) TcSigmaType
   There are special cases for
     HsVar, HsRecSel, and ExprWithTySig
   Otherwise, delegate back to tcExpr, which
     infers an (instantiated) TcRhoType

   This isn't perfect. Consider this (which uses visible type application):
    (let { f :: forall a. a -> a; f x = x } in f) @Int
   Since 'let' is not among the special cases for tcInferAppHead,
   we'll delegate back to tcExpr, which will instantiate f's type
   and the type application to @Int will fail.  Too bad!

2. Using tcInstFun to instantiate the function, Quick-Looking as we go.  This
   implements the |-inst judgement in Fig 4, plus the modification in Fig 5, of
   the QL paper: "A quick look at impredicativity" (ICFP'20).

   In tcInstFun we take a quick look at value arguments, using quickLookArg.
   See Note [Quick Look at value arguments].

   Crucially, `tcInstFun` ensures that all the unification variables
   it allocates, notably by instantiating the function at the head of the
   application, have level QLInstVar, and hence will be "instantiation
   variables", written using \kappa in the paper.
   See Note [Instantiating type variables in QuickLook]

   Note that we take a quick look at the arguments before quick-looking
   the result type; see Note [QuickLook: arguments before result]

3. In case DoQL, when we are checking, use quickLookResultType to take a
   quick look at the result type.

The next steps are in `finishApp`:

4. In case DoQL:
   4.1  Monomorphise any as-yet-ununified instantiation variables (`qlMonoInstArgs`)
        See Note [Monomorphise instantiation variables] in GHC.Tc.Utils.Unify
   4.2  Use `zonkTcType` to expose the polymorphism hidden under instantiation
        variables in `app_res_rho`, and the monomorphic versions of any
        un-unified instantiation variables.

5. Use `checkResultTy` to connect the result type.
   Do this /before/ checking the arguments; see
   Note [Unify with expected type before typechecking arguments]

6.  Check the arguments with `tcValArgs`. Importantly, we must remember to zonk in
    the EValArg case, to expose polymorphism

7. The function `finishApp` mainly calls `rebuildHsApps` to rebuild the
   application; but it also has a horrible special case for `tagToEnum`.

Note [Quick Look for particular Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We switch on Quick Look (regardless of -XImpredicativeTypes) for certain
particular Ids:

* ($): For a long time GHC has had a special typing rule for ($), that
  allows it to type (runST $ foo), which requires impredicative instantiation
  of ($), without language flags.  It's a bit ad-hoc, but it's been that
  way for ages.  Using quickLookKeys is the only special treatment ($) needs
  now, which is a lot better.

* leftSection, rightSection: these are introduced by the expansion step in
  the renamer (Note [Handling overloaded and rebindable constructs] in
  GHC.Rename.Expr), and we want them to be instantiated impredicatively
  so that (f `op`), say, will work OK even if `f` is higher rank.
  See Note [Left and right sections] in GHC.Rename.Expr.

Note [QuickLook: arguments before result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally we try to push the expected result type into the call;
see Note [Unify with expected type before typechecking arguments].
But surprisingly we can't do that for QuickLook.  (#26599 was a failed
attempt to suggest this.)  Consider
   h :: (Int->Int) -> Int    ids :: [forall a. a->a]
and the call
   h (head ids)
Let's say we instantiate `head :: [a] -> a` with `a:=kappa`.
We QuickLook at the argument with expected type (Int->Int). We must NOT
unify kappa:=(Int->Int), because kappa:=forall a. a->a is the only
solution, and one that we will find if we look at the argument first.

TL;DR: we must do `quickLookArg` on all arguments before doing `quickLookResultType`
on the result type.

Remember: `quickLookResultType` is really anticipating a /subsumption/ test, not
an equality test.  Doing `qlUnify` on the result type (always a rho-type) is
in general wrong if there is an un-guarded kappa in the result type.  It's wrong
UNLESS we have already gotten all the info about `kappa` (from the
arguments) that we are going to get.  That's why we QuickLook the arguments first.

This is a bit sad, as #26599 shows.  Consider

  woo :: (x, [forall a. a->a])
  bar1 = snd woo
  bar2 = snd (id woo)

For `bar1` we instantiate `snd` at [a:=k1,b:=k2].  Then we QL on the argument `woo`
(instantiating with `x:=k3`).  Since the result type is guarded we qlUnify
    (k3,forall a.a->a) ~ (k1,k2)
and thus succeed in discovering k2:=forall a.a->a, as desired.

But for `bar2` we fail. When quick-looking (id woo) we instantiate `id` with [a:=k],
and quick-look the argument `woo` with expected type `k`.  That's not guarded,
so we get no information.  Adding the `id` call makes it un-typeable. Boo.

This is a bit tricky.
* We QuickLook the arguments (tcInstFun)
     THEN QuickLook the result type (quickLookResultType).
  See this Note
* and then...
* We typecheck the result type (checkResultTy)
     THEN typecheck the arguments (tcValArgs)
  see Note [Unify with expected type before typechecking arguments]

Note [Unify with expected type before typechecking arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#19364)
  data Pair a b = Pair a b
  baz :: MkPair Int Bool
  baz = MkPair "yes" "no"

We instantiate MkPair with `alpha`, `beta`, and push its argument
types (`alpha` and `beta`) into the arguments ("yes" and "no").
But if we first unify the result type (Pair alpha beta) with the expected
type (Pair Int Bool) we will push the much more informative types
`Int` and `Bool` into the arguments.   This makes a difference:

Unify result type /after/ typechecking the args
    • Couldn't match type ‘[Char]’ with ‘Bool’
      Expected type: Pair Foo Bar
        Actual type: Pair [Char] [Char]
    • In the expression: Pair "yes" "no"

Unify result type /before/ typechecking the args
    • Couldn't match type ‘[Char]’ with ‘Bool’
      Expected: Foo
        Actual: String
    • In the first argument of ‘Pair’, namely ‘"yes"’

The latter is much better. That is why we call `checkResultTy` before `tcValArgs`.
-}

-- | Test whether an application spine element is a visible type argument.
isETypeArg :: HsExprArg p -> Bool
isETypeArg ETypeArg{} = True
isETypeArg _          = False

-- | Test whether an application spine element is a type, wrapper, or pragma prefix item.
isNonValExprArg :: HsExprArg p -> Bool
isNonValExprArg ETypeArg{} = True
isNonValExprArg EWrap{}    = True
isNonValExprArg EPrag{}    = True
isNonValExprArg _          = False

-- | Build a 'BindTvFun' that binds only the supplied outer template variables.
bindOuterTvs :: [TyVar] -> BindTvFun
bindOuterTvs [] _ _   = DontBindMe
bindOuterTvs [v] tv _ = if tv == v then BindMe else DontBindMe
bindOuterTvs tvs tv _ = if tv `elemVarSet` s then BindMe else DontBindMe
  where
    !s = mkVarSet tvs

-- | Match kinds and types under 'bindOuterTvs'.
matchOuterTyKi :: [TyVar] -> Subst.Subst -> Type -> Type -> Maybe Subst.Subst
matchOuterTyKi tvs subst formal actual = do
  let bind = bindOuterTvs tvs
  s1 <- tcMatchTyX_BM bind subst (typeKind formal) (typeKind actual)
  tcMatchTyX_BM bind s1 formal actual

-- | Check that a chosen type argument has the substituted kind of its binder.
substKindAgrees :: Subst.Subst -> TyVar -> Type -> Bool
substKindAgrees subst tv ty = eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty)

-- | Test whether 'zonkTcType' can change any node or metadata of a 'TcType'.
-- See Note [Skipping no-op within-TC zonks] in GHC.Tc.Gen.HsType.
canZonkChangeAppType :: TcType -> Bool
canZonkChangeAppType = go
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

-- | Zonk a 'TcType' only when 'canZonkChangeAppType' holds.
zonkAppTypeIfNeeded :: TcType -> TcM TcType
zonkAppTypeIfNeeded ty
  | canZonkChangeAppType ty = liftZonkM (zonkTcType ty)
  | otherwise               = pure ty

-- | Zonk an 'Id''s type and multiplicity only when 'canZonkChangeAppType' holds.
zonkAppIdIfNeeded :: TcId -> TcM TcId
zonkAppIdIfNeeded v
  | canZonkChangeAppType (varType v) || canZonkChangeAppType (idMult v) = liftZonkM (zonkId v)
  | otherwise = pure v

--------------------
tcApp :: HsExpr GhcRn        -- ^ The whole application (For error messages)
      -> HsExpr GhcRn        -- ^ The Function head
      -> [HsExprArg 'TcpRn]  -- ^ The list of arguments
      -> ExpRhoType          -- ^ When checking, -XDeepSubsumption <=> deeply skolemised
      -> TcM (HsExpr GhcTc)
-- See Note [tcApp: typechecking applications]
-- See Note [splitHsApps] in GHC.Tc.Head
tcApp rn_expr rn_fun rn_args exp_res_ty
  = do { pack <- case rn_fun of
                   HsVar _ (L _ (WithUserRdr _ name)) -> isExistsPackName name
                   _ -> return False
       ; if pack
         then tcExistsPack rn_expr rn_args exp_res_ty
         else do
           { mb_dot <- tcRewriteDependentDotApp rn_args
           ; case mb_dot of
               Just (sel_fun, sel_args) -> tcOrdinaryApp rn_expr sel_fun sel_args exp_res_ty
               Nothing                  -> tcOrdinaryApp rn_expr rn_fun rn_args exp_res_ty } }

-- | Lexical key for sharing an unrestricted existential opening across one
-- application spine: either a local package variable @(v, Nothing)@ or a
-- monomorphic unary producer call @(f, Just v)@.
-- See Note [Implicit existential arguments].
type OpeningKey = (Name, Maybe Name)

-- | One opened implicit existential binder recorded for an application spine.
data OpenedPkg = OpenedPkg
  { op_witness  :: TcTyVar
  , op_payload  :: Id
  , op_pkg_expr :: LHsExpr GhcTc
  }

-- | Mutable accumulator of packages opened within the active application scope.
data AppUnpackEnv = AppUnpackEnv
  { aue_rev_pkgs   :: [OpenedPkg]
  , aue_keys       :: Map.Map OpeningKey (TcType, Id)
  , aue_pkg_wanted :: WantedConstraints
  }

recordPkgWanted :: AppUnpackState -> WantedConstraints -> TcM ()
recordPkgWanted cur_st w
  | isEmptyWC w = pure ()
  | otherwise   = updTcRef (aus_env_ref cur_st) $ \env ->
      env { aue_pkg_wanted = aue_pkg_wanted env `andWC` w }

-- | Application-scoped state for @L+1@ implicit existential unpacking.
data AppUnpackState = AppUnpackState
  { aus_outer_lvl :: TcLevel
  , aus_app_lvl   :: TcLevel
  , aus_can_share :: Bool
  , aus_env_ref   :: TcRef AppUnpackEnv
  }

getAppUnpackState :: TcM (Maybe AppUnpackState)
getAppUnpackState = do
  gbl <- getGblEnv
  st_map <- readTcRef (tcg_th_state gbl)
  let rep = typeRep (Proxy :: Proxy (Maybe AppUnpackState))
  pure $ join (Map.lookup rep st_map >>= fromDynamic)

withAppUnpackState :: Maybe AppUnpackState -> TcM a -> TcM a
withAppUnpackState next thing_inside = do
  gbl <- getGblEnv
  let ref = tcg_th_state gbl
      rep = typeRep (Proxy :: Proxy (Maybe AppUnpackState))
  st_map0 <- readTcRef ref
  let prev :: Maybe AppUnpackState
      prev = join (Map.lookup rep st_map0 >>= fromDynamic)
  writeTcRef ref (Map.insert rep (toDyn next) st_map0)
  res <- tryM thing_inside
  st_map1 <- readTcRef ref
  writeTcRef ref (Map.insert rep (toDyn prev) st_map1)
  case res of
    Right val -> pure val
    Left ex   -> liftIO $ throwIO ex

-- | Demote unfilled metavariables born at @app_lvl@ back to @outer_lvl@ when an
-- application unpacked no existential packages.
demoteAppMetas :: TcLevel -> TcLevel -> WantedConstraints -> Maybe TcType -> ZonkM ()
demoteAppMetas outer_lvl app_lvl wanted mb_ty = do
  mapM_ (go_ty . ctPred) (bagToList (wc_simple wanted))
  mapM_ go_implic (bagToList (wc_impl wanted))
  maybe (pure ()) go_ty mb_ty
  where
    go_implic implic = do
      mapM_ (go_ty . varType) (ic_given implic)
      demoteAppMetas outer_lvl app_lvl (ic_wanted implic) Nothing

    go_ty :: TcType -> ZonkM ()
    go_ty ty = unZonkUnit (runFV (do_ty ty) ())

    (do_ty, _, _, _) = foldTyCo folder

    folder :: TyCoFolder (FV () ZonkUnit)
    folder = TyCoFolder
      { tcf_view       = noView
      , tcf_lit        = mempty
      , tcf_tyvar      = do_tv
      , tcf_termvar    = \v -> do_ty (varType v) `mappend` do_ty (idMult v)
      , tcf_termbinder = \v _ -> do_ty (varType v) `mappend` do_ty (idMult v)
      , tcf_covar      = mempty
      , tcf_hole       = \h -> do_ty (coVarKind (coHoleCoVar h))
      , tcf_tycobinder = const id
      }

    do_tv :: TcTyVar -> FV () ZonkUnit
    do_tv tv
      | isTcTyVar tv
      , MetaTv { mtv_ref = ref, mtv_tclvl = lvl, mtv_info = info
               , mtv_term_depth = depth } <- tcTyVarDetails tv
      = MkFV $ \_ -> ZU $ do
          flex <- readTcRef ref
          case flex of
            Indirect ty -> go_ty ty
            Flexi
              | lvl `sameDepthAs` app_lvl -> do
                  let kind = tyVarKind tv
                  go_ty kind
                  ref2 <- newTcRef Flexi
                  let details = MetaTv { mtv_info       = info
                                       , mtv_ref        = ref2
                                       , mtv_tclvl      = outer_lvl
                                       , mtv_term_depth = depth }
                      tv2 = mkTcTyVar (tyVarName tv) kind details
                  writeTcRef ref (Indirect (mkTyVarTy tv2))
              | otherwise -> pure ()
      | otherwise = mempty

newtype ZonkUnit = ZU { unZonkUnit :: ZonkM () }
instance Semigroup ZonkUnit where
  ZU l <> ZU r = ZU (l >> r)
instance Monoid ZonkUnit where
  mempty = ZU (pure ())

-- | Run an application body inside a provisional @L+1@ unpacking scope when
-- @may_unpack@ holds; otherwise run directly at the ambient 'TcLevel'.
-- See Note [Implicit existential arguments].
runAppUnpackScopeIf
  :: Bool
  -> ExpRhoType
  -> (Maybe AppUnpackState -> ExpRhoType -> TcM (HsExpr GhcTc))
  -> TcM (HsExpr GhcTc)
runAppUnpackScopeIf False exp_res_ty body_action =
  withAppUnpackState Nothing $ body_action Nothing exp_res_ty
runAppUnpackScopeIf True exp_res_ty body_action = do
  outer_lvl <- getTcLevel
  (app_lvl, ((res_expr, mb_inf_ty, pkgs, pkg_wanted), wanted)) <-
    pushTcLevelM $ captureConstraints $ do
      app_lvl <- getTcLevel
      inner_exp <- case exp_res_ty of
        Check rho -> pure (Check rho)
        Infer ir  -> do
          ref <- newTcRef Nothing
          pure (Infer ir { ir_lvl = app_lvl, ir_ref = ref })
      env_ref <- newTcRef (AppUnpackEnv [] Map.empty emptyWC)
      let root_st = AppUnpackState
            { aus_outer_lvl = outer_lvl
            , aus_app_lvl   = app_lvl
            , aus_can_share = True
            , aus_env_ref   = env_ref
            }
      expr0 <- withAppUnpackState (Just root_st) $ body_action (Just root_st) inner_exp
      env <- readTcRef env_ref
      let pkgs = reverse (aue_rev_pkgs env)
          pkg_w = aue_pkg_wanted env
      case (pkgs, inner_exp, exp_res_ty) of
        ([], Infer ir_in, _) -> do
          mb_ty <- readTcRef (ir_ref ir_in)
          pure (expr0, mb_ty, [], pkg_w)
        (_ : _, Infer ir_in, Infer outer_ir) -> do
          mb_ty <- readTcRef (ir_ref ir_in)
          case mb_ty of
            Just inf_ty -> do
              inf_ty' <- liftZonkM (zonkTcType inf_ty)
              co <- fillInferResultNoInst inf_ty' outer_ir
              pure (mkHsWrapCo co expr0, Nothing, pkgs, pkg_w)
            Nothing -> pure (expr0, Nothing, pkgs, pkg_w)
        _ -> pure (expr0, Nothing, pkgs, pkg_w)
  case pkgs of
    [] -> do
      let all_wanted = pkg_wanted `andWC` wanted
      liftZonkM $ demoteAppMetas outer_lvl app_lvl all_wanted mb_inf_ty
      emitConstraints all_wanted
      case (mb_inf_ty, exp_res_ty) of
        (Just inf_ty, Infer outer_ir) -> do
          inf_ty' <- liftZonkM (zonkTcType inf_ty)
          co <- fillInferResultNoInst inf_ty' outer_ir
          pure (mkHsWrapCo co res_expr)
        _ -> pure res_expr
    _ : _ -> do
      unless (isEmptyWC pkg_wanted) $ do
        liftZonkM $ demoteAppMetas outer_lvl app_lvl pkg_wanted Nothing
        emitConstraints pkg_wanted
      result_ty <- expTypeToType exp_res_ty
      (implics, ev_binds) <- buildImplicationFor app_lvl ExistsUnpackSkol
                               (map op_witness pkgs) [] wanted
      emitImplications implics
      let inner_body = mkHsWrap (mkWpLet ev_binds) res_expr
      pure $ foldr
        (\op acc -> XExpr (NativeExistsUnpack (op_witness op) (op_payload op)
                             result_ty (op_pkg_expr op) (noLocA acc)))
        inner_body pkgs

-- | Test whether a type mentions an implicit existential quantifier within a
-- bounded synonym/closed-family expansion depth.
typeHasImplicitExists :: Int -> Type -> Bool
typeHasImplicitExists !depth ty
  | depth <= 0 = False
  | Just ty' <- coreView ty = typeHasImplicitExists (depth - 1) ty'
  | otherwise = case ty of
      ExistsTy ImplicitExists _ _ -> True
      ExistsTy ExplicitExists b r ->
        typeHasImplicitExists (depth - 1) (tyVarKind b) || typeHasImplicitExists (depth - 1) r
      ForAllTy _ b r ->
        typeHasImplicitExists (depth - 1) (varType (binderVar b)) || typeHasImplicitExists (depth - 1) r
      FunTy { ft_ma = ma, ft_mult = mult, ft_arg = a, ft_res = r } ->
        typeHasImplicitExists (depth - 1) ma
        || typeHasImplicitExists (depth - 1) mult
        || typeHasImplicitExists (depth - 1) a
        || typeHasImplicitExists (depth - 1) r
      AppTy f a ->
        typeHasImplicitExists (depth - 1) f || typeHasImplicitExists (depth - 1) a
      CastTy t _ -> typeHasImplicitExists (depth - 1) t
      TyConApp tc tys ->
        any (typeHasImplicitExists (depth - 1)) tys
        || maybe False (typeHasImplicitExists (depth - 1)) (synTyConRhs_maybe tc)
        || maybe False (any (typeHasImplicitExists (depth - 1) . coAxBranchRHS) . fromBranches . coAxiomBranches)
                       (closedFamilyTyConCoAxiom_maybe tc)
      _ -> False

lookupDeclaredIdMaybe :: Name -> TcM (Maybe Id)
lookupDeclaredIdMaybe name = do
  local <- tcLookupLcl_maybe name
  case local of
    Just (ATcId { tct_id = ident }) -> pure (Just ident)
    Just (AGlobal (AnId ident))     -> pure (Just ident)
    Just _                          -> pure Nothing
    Nothing
      | isExternalName name ->
          discardErrs $ tryTcDiscardingErrs (pure Nothing) (tcLookupIdMaybe name)
      | otherwise -> pure Nothing

lookupDeclaredTyConMaybe :: Name -> TcM (Maybe TyCon)
lookupDeclaredTyConMaybe name = do
  local <- tcLookupLcl_maybe name
  case local of
    Just (AGlobal (ATyCon tc)) -> pure (Just tc)
    Just (ATcTyCon tc)         -> pure (Just tc)
    Just _                     -> pure Nothing
    Nothing
      | isExternalName name ->
          discardErrs $ tryTcDiscardingErrs (pure Nothing) $ do
            thing <- tcLookupGlobal name
            pure $ case thing of
              ATyCon tc -> Just tc
              _         -> Nothing
      | otherwise -> pure Nothing

hsTypeMayHaveImplicitExists :: LHsType GhcRn -> TcM Bool
hsTypeMayHaveImplicitExists (L _ ty) = case ty of
  HsParTy _ t -> hsTypeMayHaveImplicitExists t
  XHsType (HsTypeExistsRn ImplicitExists _ _) -> pure True
  XHsType (HsTypeExistsRn ExplicitExists _ b) -> hsTypeMayHaveImplicitExists b
  HsAppTy _ f a -> (||) <$> hsTypeMayHaveImplicitExists f <*> hsTypeMayHaveImplicitExists a
  HsAppKindTy _ f k -> (||) <$> hsTypeMayHaveImplicitExists f <*> hsTypeMayHaveImplicitExists k
  HsFunTy _ _ a r -> (||) <$> hsTypeMayHaveImplicitExists a <*> hsTypeMayHaveImplicitExists r
  HsForAllTy { hst_body = b } -> hsTypeMayHaveImplicitExists b
  HsQualTy { hst_body = b } -> hsTypeMayHaveImplicitExists b
  HsTupleTy _ _ ts -> anyM hsTypeMayHaveImplicitExists ts
  HsListTy _ t -> hsTypeMayHaveImplicitExists t
  HsKindSig _ t k -> (||) <$> hsTypeMayHaveImplicitExists t <*> hsTypeMayHaveImplicitExists k
  HsOpTy _ l _ r -> (||) <$> hsTypeMayHaveImplicitExists l <*> hsTypeMayHaveImplicitExists r
  HsTyVar _ _ (L _ (WithUserRdr _ name))
    | isTcOcc (nameOccName name) -> do
        mb_tc <- lookupDeclaredTyConMaybe name
        let check_tc tc =
              maybe False (typeHasImplicitExists 16) (synTyConRhs_maybe tc)
              || maybe False (any (typeHasImplicitExists 16 . coAxBranchRHS) . fromBranches . coAxiomBranches)
                             (closedFamilyTyConCoAxiom_maybe tc)
        pure (maybe False check_tc mb_tc)
  _ -> pure False

rnExprMayHaveImplicitExists :: LHsExpr GhcRn -> TcM Bool
rnExprMayHaveImplicitExists (L _ expr) = case expr of
  HsPar _ e -> rnExprMayHaveImplicitExists e
  HsVar _ lname
    | let name = unLocWithUserRdr lname
    , not (isDataOcc (nameOccName name)) -> do
        mb_id <- lookupDeclaredIdMaybe name
        case mb_id of
          Nothing -> pure False
          Just ident -> typeHasImplicitExists 16 <$> liftZonkM (zonkTcType (varType ident))
  HsApp _ f a -> (||) <$> rnExprMayHaveImplicitExists f <*> rnExprMayHaveImplicitExists a
  HsAppType _ f hswc -> (||) <$> rnExprMayHaveImplicitExists f <*> hsTypeMayHaveImplicitExists (hswc_body hswc)
  OpApp _ l op r -> anyM rnExprMayHaveImplicitExists [l, op, r]
  SectionL _ e op -> (||) <$> rnExprMayHaveImplicitExists e <*> rnExprMayHaveImplicitExists op
  SectionR _ op e -> (||) <$> rnExprMayHaveImplicitExists op <*> rnExprMayHaveImplicitExists e
  ExprWithTySig _ e (HsWC { hswc_body = L _ (HsSig { sig_body = body }) }) ->
    (||) <$> rnExprMayHaveImplicitExists e <*> hsTypeMayHaveImplicitExists body
  ExplicitTuple _ args _ -> anyM id [rnExprMayHaveImplicitExists e | Present _ e <- args]
  HsIf _ _ t f -> (||) <$> rnExprMayHaveImplicitExists t <*> rnExprMayHaveImplicitExists f
  HsMultiIf _ grhss -> anyM (\(L _ (GRHS _ _ b)) -> rnExprMayHaveImplicitExists b) grhss
  HsCase _ scrut (MG { mg_alts = L _ alts }) ->
    (||) <$> rnExprMayHaveImplicitExists scrut <*> anyM match_may alts
  HsLet _ binds body -> (||) <$> binds_may binds <*> rnExprMayHaveImplicitExists body
  _ -> pure False
  where
    match_may (L _ Match { m_grhss = GRHSs _ grhss lbinds }) =
      (||) <$> binds_may lbinds <*> anyM (\(L _ (GRHS _ _ b)) -> rnExprMayHaveImplicitExists b) grhss

    binds_may (L _ (HsValBinds _ (XValBindsLR (HsVBG groups sigs)))) = do
      sig_hit <- anyM sig_may sigs
      if sig_hit then pure True else anyM bind_may [b | (_, bs) <- groups, L _ b <- bs]
    binds_may _ = pure False

    sig_may (L _ (TypeSig _ _ _ (HsWC { hswc_body = L _ (HsSig { sig_body = body }) }))) =
      hsTypeMayHaveImplicitExists body
    sig_may _ = pure False

    bind_may (FunBind { fun_matches = MG { mg_alts = L _ alts } }) = anyM match_may alts
    bind_may (PatBind { pat_rhs = GRHSs _ grhss lbinds }) =
      (||) <$> binds_may lbinds <*> anyM (\(L _ (GRHS _ _ b)) -> rnExprMayHaveImplicitExists b) grhss
    bind_may _ = pure False

spineMayUnpackImplicitExists :: TcSigmaType -> [HsExprArg 'TcpRn] -> TcM Bool
spineMayUnpackImplicitExists fun_sigma rn_args
  | ExistsTy ImplicitExists _ body <- implicitSpineHead fun_sigma
  , opensImplicitHead (count isHsValArg rn_args) body
  = pure True
  | otherwise = anyM arg_may rn_args
  where
    arg_may (EValArg { ea_arg = larg }) = rnExprMayHaveImplicitExists larg
    arg_may _                           = pure False

instArgsMayUnpackImplicitExists :: [HsExprArg 'TcpInst] -> TcM Bool
instArgsMayUnpackImplicitExists = anyM arg_may
  where
    arg_may (EValArg { ea_arg = larg }) = rnExprMayHaveImplicitExists larg
    arg_may (EValArgQL { eaql_larg = larg, eaql_res_rho = rho }) =
      if typeHasImplicitExists 16 rho then pure True else rnExprMayHaveImplicitExists larg
    arg_may _ = pure False

-- | Unpack consecutive implicit existential binders at @aus_app_lvl@, sharing
-- an existing payload when @mb_key@ already opened the same schema in this
-- application spine.
-- See Note [Implicit existential arguments].
unpackImplicitExistsAt
  :: AppUnpackState -> SrcSpan -> Mult -> Maybe OpeningKey
  -> TcType -> LHsExpr GhcTc -> TcM Id
unpackImplicitExistsAt cur_st loc mult mb_key schema checked_pkg = do
  env0 <- readTcRef (aus_env_ref cur_st)
  case do { guard (aus_can_share cur_st)
          ; key <- mb_key
          ; (prev_schema, prev_payload) <- Map.lookup key (aue_keys env0)
          ; guard (eqType prev_schema schema)
          ; pure prev_payload } of
    Just shared_payload -> pure shared_payload
    Nothing -> do
      final_payload <- peel schema checked_pkg
      when (aus_can_share cur_st) $
        case mb_key of
          Just key -> updTcRef (aus_env_ref cur_st) $ \env ->
            env { aue_keys = Map.insert key (schema, final_payload) (aue_keys env) }
          Nothing -> pure ()
      pure final_payload
  where
    peel cur_schema cur_expr
      | ExistsTy ImplicitExists binder payload_ty <- coreFullView cur_schema
      = do { skol <- setSrcSpan loc $ mkSkolemInfo (UnifyForAllSkol cur_schema)
           ; uniq <- newUnique
           ; let name = mkSystemNameAt uniq (getOccName binder) loc
           ; witness <- setTcLevel (aus_app_lvl cur_st) $
                          newSkolemTyVar skol name (tyVarKind binder)
           ; let body_ty = substTyWith [binder] [mkTyVarTy witness]
                                       (expandTypeSynonyms payload_ty)
           ; payload <- newSysLocalId (fsLit "unpacked") mult body_ty
           ; updTcRef (aus_env_ref cur_st) $ \env ->
               env { aue_rev_pkgs = OpenedPkg witness payload cur_expr : aue_rev_pkgs env }
           ; case coreFullView body_ty of
               ExistsTy ImplicitExists _ _ ->
                 peel body_ty (noLocA (mkHsVar (noLocA payload)))
               _ -> pure payload }
      | otherwise
      = failWithTc (TcRnExistsPack (ExistsPackInvalidSchema cur_schema))

-- | Compute the lexical sharing key for a checked implicit existential operand.
-- See Note [Implicit existential arguments].
openingKey :: LHsExpr GhcRn -> LHsExpr GhcTc -> TcM (Maybe OpeningKey)
openingKey original checked
  | HsVar _ (L _ ident) <- bare_expr checked
  = do { ident_ty <- liftZonkM (zonkTcType (varType ident))
       ; return $ case coreFullView ident_ty of
           ExistsTy ImplicitExists _ _ -> (\name -> (name, Nothing)) <$> lexical_name ident
           _                           -> Nothing }
  | HsApp _ fun operand <- bare_expr checked
  , HsVar _ (L _ producer) <- bare_expr fun
  , HsVar _ (L _ input) <- bare_expr operand
  , let producer_name = getName producer
  , isExternalName producer_name
  , Just input_name <- lexical_name input
  , FunTy{ft_af = af, ft_mult = mult, ft_arg = domain, ft_res = result}
      <- coreFullView (varType producer)
  , isVisibleFunArg af, isManyTy mult
  , isTauTy domain
  , ExistsTy ImplicitExists _ _ <- coreFullView result
  , let key = (producer_name, Just input_name)
  , renamed_key original == Just key
  = do { input_type <- liftZonkM (zonkTcType (varType input))
       ; return (if eqType domain input_type then Just key else Nothing) }
  | otherwise = return Nothing
  where
    lexical_name :: Id -> Maybe Name
    lexical_name ident
      | let name = getName ident
      , isLocalId ident, not (isExternalName name) = Just name
      | otherwise = Nothing

    renamed_key :: LHsExpr GhcRn -> Maybe OpeningKey
    renamed_key expr = case bare_expr expr of
      HsVar _ name -> Just (unLocWithUserRdr name, Nothing)
      HsApp _ fun arg
        | HsVar _ producer <- bare_expr fun
        , HsVar _ input <- bare_expr arg
        -> Just (unLocWithUserRdr producer, Just (unLocWithUserRdr input))
      _ -> Nothing

    bare_expr :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
    bare_expr = unLoc . stripParensLHsExpr

-- | Check whether a formal argument type demands an unpacked payload rather
-- than forwarding an implicit existential package.
-- See Note [Implicit existential arguments].
demandsPayload :: TcType -> TcM Bool
demandsPayload raw_ty = do
  ty <- unwrapMetaView raw_ty
  case ty of
    ExistsTy ImplicitExists _ _ -> pure False
    ExistsTy ExplicitExists _ _ -> pure True
    TyVarTy tv
      | isTcTyVar tv && isMetaTyVar tv -> pure False
      | otherwise                      -> pure True
    FunTy { ft_af = af } -> pure (isVisibleFunArg af)
    AppTy {}             -> pure True
    TyConApp tc args
      | isTypeFamilyTyCon tc -> do
          envs <- tcGetFamInstEnvs
          args' <- mapM zonkAppTypeIfNeeded args
          case reduceTyFamApp_maybe envs Nominal tc args' of
            Just (Reduction _ red) -> demandsPayload red
            Nothing                -> pure True
      | otherwise -> pure True
    _ -> pure False

tcOrdinaryApp :: HsExpr GhcRn -> HsExpr GhcRn -> [HsExprArg 'TcpRn]
              -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryApp rn_expr rn_fun rn_args exp_res_ty = do
  fun_lspan <- getFunSrcSpan rn_args
  let rn_head = (rn_fun, fun_lspan)
  mb_st <- getAppUnpackState
  case mb_st of
    _ | null rn_args -> do
      (tc_fun0, fun_sigma0) <- tcInferAppHead rn_head
      tcOrdinaryAppWithPrefix [] rn_expr rn_head tc_fun0 fun_sigma0 rn_args exp_res_ty
    Just cur_st -> do
      (tc_fun0, fun_sigma0) <- withAppUnpackState Nothing $ tcInferAppHead rn_head
      fun_sigma <- liftZonkM (zonkTcType fun_sigma0)
      tcOrdinaryAppHead (Just cur_st) [] rn_expr rn_head tc_fun0 fun_sigma rn_args exp_res_ty
    Nothing -> do
      (tc_fun0, fun_sigma0) <- tcInferAppHead rn_head
      fun_sigma <- liftZonkM (zonkTcType fun_sigma0)
      may_unpack <- spineMayUnpackImplicitExists fun_sigma rn_args
      runAppUnpackScopeIf may_unpack exp_res_ty $ \mb_root_st inner_exp ->
        tcOrdinaryAppHead mb_root_st [] rn_expr rn_head tc_fun0 fun_sigma rn_args inner_exp

tcOrdinaryAppHead
  :: Maybe AppUnpackState -> [HsExprArg 'TcpInst] -> HsExpr GhcRn -> (HsExpr GhcRn, SrcSpan)
  -> HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn] -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryAppHead mb_cur_st checked_prefix rn_expr rn_head tc_fun0 fun_sigma0 rn_args exp_res_ty
  | Just cur_st <- mb_cur_st
  , null checked_prefix
  , ExistsTy ImplicitExists _ body <- implicitSpineHead fun_sigma0
  , all ordinary_arg payload_args
  , opensImplicitHead (count isHsValArg payload_args) body
  = do { ((prefix', schema0), pkg_w) <- captureConstraints $
           withAppUnpackState Nothing $ do
             (pfx, s0) <- tcInstFun NoQL True rn_head tc_fun0 fun_sigma0 type_prefix
             pfx' <- tcValArgsWithCache [] NoQL rn_head pfx
             pure (pfx', s0)
       ; recordPkgWanted cur_st pkg_w
       ; schema <- liftZonkM (zonkTcType schema0)
       ; package_loc <- getFunSrcSpan payload_args
       ; let package = L (noAnnSrcSpan package_loc) (rebuildHsApps tc_fun0 prefix')
       ; payload <- unpackImplicitExistsAt cur_st package_loc ManyTy Nothing schema package
       ; tcOrdinaryAppWithPrefix [] rn_expr rn_head
           (mkHsVar (noLocA payload)) (varType payload) payload_args exp_res_ty }
  | otherwise
  = tcOrdinaryAppWithPrefix checked_prefix rn_expr rn_head tc_fun0 fun_sigma0 rn_args exp_res_ty
  where
    -- See Note [Type arguments before an implicit package head].
    (type_prefix, payload_args)
      | let (prefix, rest) = span prefix_arg rn_args
      , any isETypeArg prefix = (prefix, rest)
      | otherwise = ([], rn_args)

    prefix_arg EWrap{} = True
    prefix_arg arg     = isETypeArg arg

    ordinary_arg EValArg{} = True
    ordinary_arg EWrap{}   = True
    ordinary_arg _         = False

-- | Suspend the check at each written type argument until the preceding
-- rule variables are in scope. Reuse the checked arguments at the next step.
-- See Note [Dependent RULE binder domains] in GHC.Tc.Gen.Sig.
data RuleAppPrefix
  = CheckRulePrefix (TcM (TcSigmaType, RuleAppPrefix))
  | FinishRuleApp (TcM (LHsExpr GhcTc, TcRhoType))

-- | Infer the head of the retained RULE application.
tcRuleAppPrefix :: LHsExpr GhcRn -> TcM (TcSigmaType, RuleAppPrefix)
tcRuleAppPrefix (L loc expr) = context $
  do { (fun, args) <- splitHsApps expr
     ; fun_span <- getFunSrcSpan args
     ; let head_ctxt = (fun, fun_span)
     ; (checked_fun, sigma) <- tcInferAppHead head_ctxt
     ; let check prefix residual remaining = case break looks_like_type_arg remaining of
             (values, ty_arg : rest) -> CheckRulePrefix $ context $
               do { (checked, sigma') <- tcInstFunWithPrefix quickLookArg True prefix Nothing Nothing Nothing
                                          NoQL False head_ctxt checked_fun residual (values ++ [ty_arg])
                  ; return (sigma', check checked sigma' rest) }
             _ -> FinishRuleApp $ context $
               do { (lhs, rho) <- runInferRho $ \expected ->
                      if null remaining
                      then tcOrdinaryAppWithPrefix prefix expr head_ctxt checked_fun residual remaining expected
                      else do
                        residual' <- liftZonkM (zonkTcType residual)
                        may_unpack <- spineMayUnpackImplicitExists residual' remaining
                        runAppUnpackScopeIf may_unpack expected $ \mb_root_st inner_exp ->
                          tcOrdinaryAppHead mb_root_st prefix expr head_ctxt checked_fun residual' remaining inner_exp
                  ; return (L loc lhs, rho) }
     ; return (sigma, check [] sigma args) }
  where
    context :: TcM a -> TcM a
    context action = setSrcSpanA loc $ addExprCtxt expr action

tcOrdinaryAppWithPrefix :: [HsExprArg 'TcpInst] -> HsExpr GhcRn -> (HsExpr GhcRn, SrcSpan)
                        -> HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn]
                        -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryAppWithPrefix checked_prefix rn_expr rn_head tc_fun fun_sigma rn_args expected
  = do { (args, fields) <- if null checked_prefix
                          then tcRetainedSpineFields rn_expr fun_sigma rn_args
                          else return (rn_args, [])
       ; let check names = tcOrdinaryAppScoped names checked_prefix rn_expr rn_head
                                              tc_fun fun_sigma args expected
       ; if null fields then check [] else tcWithSpineFields fields check }

-- | Check each field once and share it with the retained application below.
tcWithSpineFields :: [(Id, LHsExpr GhcRn)] -> ([Name] -> TcM (HsExpr GhcTc))
                  -> TcM (HsExpr GhcTc)
tcWithSpineFields fields check
  = do { rhss <- forM fields $ \(field, rhs) ->
           tcScalingUsage ManyTy (tcCheckPolyExpr rhs (idType field))
       ; let ids = map fst fields
       ; (usage, result) <- tcCollectingUsage $
           foldr tcExtendGeneratedIdEnv (check (map getName ids)) ids
       ; tcEmitBindingUsage (foldl' (\ue field -> snd (popUE ue field)) usage ids)
       ; let binds = HsValBinds noAnn (XValBindsLR (HsVBG
                       [(NonRecursive, [mk_bind field rhs]) | (field, rhs) <- zip ids rhss] []))
       ; return (HsLet noExtField (noLocA binds) (noLocA result)) }
  where
    mk_bind :: Id -> LHsExpr GhcTc -> LHsBind GhcTc
    mk_bind field rhs = noLocA PatBind
      { pat_ext = XPatBindTc (idType field) [] [] ManyTy
      , pat_lhs = noLocA (LazyPat noExtField (noLocA (VarPat noExtField (noLocA field))))
      , pat_mods = []
      , pat_rhs = unguardedGRHSs (getLocA rhs) rhs noAnn }

-- | Select plain list fields when the remainder of the contract forgets all fields.
-- See Note [Opaque operands for a retained list spine].
tcRetainedSpineFields :: HsExpr GhcRn -> Type -> [HsExprArg 'TcpRn]
                     -> TcM ([HsExprArg 'TcpRn], [(Id, LHsExpr GhcRn)])
tcRetainedSpineFields expr sigma args
  | not (any needs_fields args) = return (args, [])
  | (_, _, tau) <- tcRetainedInstTelescope sigma
  , DepFunTy DepFunVisible _ _ <- coreFullView tau
  = do { let (prefix, operands) = span spine_prefix args
       ; plan <- tryTcDiscardingErrs (return Nothing) $
           runMaybeT (scan tau operands [])
       ; case plan of
           Nothing -> return (args, [])
           Just count -> do
             { changed <- traverse prepare (take count operands)
             ; return (prefix ++ map fst changed ++ drop count operands, concatMap snd changed) } }
  | otherwise = return (args, [])
  where
    spine_prefix ETypeArg{} = True
    spine_prefix (EWrap EPar{}) = True
    spine_prefix _ = False

    needs_fields = maybe False (any opaque . snd) . list_fields
    list_fields EValArg{ea_arg = arg}
      | L loc (ExplicitList _ elements) <- stripParensLHsExpr arg = Just (loc, elements)
    list_fields _ = Nothing

    opaque element = case unLoc (stripParensLHsExpr element) of
      HsVar{} -> False
      HsLit _ HsChar{} -> False
      HsLit _ HsString{} -> False
      _ -> True

    scan ty (EWrap EPar{} : rest) fields
      = (1 +) <$> scan ty rest fields
    scan ty (arg : rest) fields
      | DepFunTy DepFunVisible binder _ <- coreFullView ty
      , Just (_, elements) <- list_fields arg
      , Just [element_ty] <- nativeIndexDataConTyArgs (idType binder) nilDataCon
      = do { ids <- liftMaybeT $ mapM (const (newSysLocalId (fsLit "element") ManyTy element_ty)) elements
           ; let nil = Core.mkTyApps (Core.Var (dataConWorkId nilDataCon)) [element_ty]
                 cons = Core.mkTyApps (Core.Var (dataConWorkId consDataCon)) [element_ty]
                 operand = foldr (\field tail -> Core.mkApps cons [Core.Var field, tail]) nil ids
           ; guard (isJust (Core.retainedArgIndex operand))
           ; next <- scan (applyTypeToArgs ty [operand]) rest (ids ++ fields)
           ; return (1 + next) }
    scan ty _ fields
      = do { guard (not (null fields))
           ; _ <- liftMaybeT (tcCloseRuntimeScope (map getName fields) (exprCtOrigin expr) ty)
           ; return 0 }

    prepare (EValArg { ea_loc_span = span_loc, ea_arg_ty = arg_ty, ea_arg = expr_arg })
      | L loc (ExplicitList _ elements) <- stripParensLHsExpr expr_arg
      , any opaque elements
      = do { element_ty <- newFlexiTyVarTy liftedTypeKind
           ; (elements', fields) <- mapAndUnzipM (name_field element_ty) elements
           ; let arg' = EValArg { ea_loc_span = span_loc
                                , ea_arg_ty = arg_ty
                                , ea_arg = L loc (ExplicitList noExtField elements') }
           ; return (arg', concat fields) }
    prepare arg = return (arg, [])

    name_field ty rhs
      | opaque rhs
      = do { field <- newSysLocalId (fsLit "element") ManyTy ty
           ; let reference = mkExpandedLExpr (unLoc rhs) (nlHsVar (idName field))
           ; return (L (getLoc rhs) reference, [(field, rhs)]) }
      | otherwise = return (rhs, [])

{- Note [Opaque operands for a retained list spine]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A list's checked length can depend on its constructors without depending on
its values. Check the whole remaining telescope once with independent opaque
fields before changing an argument. Checked family reduction must remove
these fields from the entire telescope, including its constraints.
A result or constraint that compares contents keeps the original checking
rule. A successful projection does not establish any new field equality.

Ignore leading written type arguments and parentheses in this scan. A
conversion valid before substituting the quantified type variables is valid
after substitution. Keep the written arguments and parentheses in the
application. The usual type application rules still check the type arguments
and determine the actual field types. A checked RULE prefix can already
contain retained value operands. This rewrite applies to the source spine
before checking such operands.

Check each new RHS once as a monomorphic ordinary expression. A lazy binding
owns the fresh Id used by both the retained index and the runtime list. Bind
the application evidence underneath those Ids. Normalize its actual outward
type under the same scope and use the nominal coercion there. See
Note [Closing an inferred runtime index scope] in GHC.Tc.Gen.Match.
-}

tcOrdinaryAppScoped :: [Name] -> [HsExprArg 'TcpInst] -> HsExpr GhcRn -> (HsExpr GhcRn, SrcSpan)
                    -> HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn]
                    -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryAppScoped fields checked_prefix rn_expr rn_head@(rn_fun, fun_lspan) tc_fun0 fun_sigma0 rn_args written_res_ty
  -- See Note [Constructors in a nested dependent sum] in GHC.Tc.Utils.Unify.
  = do { wanted_ql <- wantQuickLook rn_fun
       ; exp_res_ty <- case (tc_fun0, written_res_ty) of
           (XExpr (ConLikeTc (RealDataCon dc)), Check ty)
             | not (null (dataConRepArgBinders dc))
             , let tc = dataConTyCon dc
             , Just (_, params) <- matchSumPayload tc ty
             -> return (mkCheckExpType (mkTyConApp tc params))
           (_, Check ty) | DoQL <- wanted_ql
             -> do { result <- setSrcSpan fun_lspan $
                              tcKnownRetainedConstructorResult rn_args tc_fun0 fun_sigma0 ty
                   ; return (maybe written_res_ty mkCheckExpType result) }
           _ -> return written_res_ty
       ; traceTc "tcApp {" $
           vcat [ text "rn_fun:" <+> ppr rn_fun
                , text "fun_lspan:" <+> ppr fun_lspan
                , text "rn_args:" <+> ppr rn_args ]


       -- See Note [Imported native application contracts].
       ; native_app <- tcRetainedAppCapability fun_sigma0 exp_res_ty rn_args
       ; dep_ev <- if native_app then return True else tcHasDepImports
       -- See Note [Known native variables with ImpredicativeTypes].
       ; head_ql <- if not (null fields) || any isExplicitRetainedArg rn_args
                        || (native_app && hasScopedDictionaryPrefix fun_sigma0)
                    then return NoQL
                    else tcRetainedKnownHeadQL native_app wanted_ql rn_args exp_res_ty tc_fun0 fun_sigma0
       ; tcRetainedAppEvidence dep_ev $ withLocalSynthFunIds $
         do { (prefix, do_ql, fun_sigma, remaining_args) <-
                tcPrepareNativeQLApp native_app head_ql exp_res_ty rn_head tc_fun0 fun_sigma0 rn_args
            ; let tc_fun = tc_fun0
                  expected_rho = case exp_res_ty of
                                   Check rho -> Just rho
                                   Infer {}  -> Nothing
                  -- Top-instantiate the result unless inferring a sigma type.
                  inst_final = case exp_res_ty of
                                 Check {} -> True
                                 Infer (IR {ir_inst=iif}) ->
                                   case iif of
                                     IIF_ShallowRho -> True
                                     IIF_DeepRho    -> True
                                     IIF_Sigma      -> False
            ; traceTc "tcApp:inferAppHead" $
                vcat [ text "tc_fun:" <+> ppr tc_fun
                     , text "fun_sigma:" <+> ppr fun_sigma
                     , text "do_ql:" <+> ppr do_ql ]
            ; -- Step 2: Instantiate the function type, then take a quick look
              -- at value arguments. Preserve the checked leading prefix.
              -- See Note [Quick Look for particular Ids].
              retained_state_ref <- newTcRef ([], [])
            ; let instantiate =
                    tcInstFunWithPrefix quickLookArg False (checked_prefix ++ prefix)
                      (Just retained_state_ref) expected_rho Nothing
                      do_ql inst_final rn_head tc_fun fun_sigma remaining_args
            ; ((inst_args, app_res_rho), emit_wanteds) <- case do_ql of
                NoQL -> (, return ()) <$> instantiate
                DoQL -> do { (instantiated, wanted) <- captureConstraints instantiate
                           ; return (instantiated, emitConstraints wanted) }
            ; (retained_holes, arg_cache) <- readTcRef retained_state_ref
            ; let prepare = do
                    emit_wanteds
                    -- See Note [QuickLook: arguments before result].
                    case (do_ql, exp_res_ty) of
                      (DoQL, Check exp_rho) -> quickLookResultType app_res_rho exp_rho
                      (NoQL, Check exp_rho)
                        | hasNativeDependentExpr (Core.Type exp_rho)
                        -> tcPreUnifyMetaSkeleton (Just (exprCtOrigin rn_expr))
                             emptyVarSet emptyVarSet app_res_rho exp_rho
                      _ -> return ()
            ; finishAppBody retained_holes fields arg_cache prepare do_ql rn_expr fun_lspan
                source_fun tc_fun inst_args app_res_rho written_res_ty } }
  where
    -- See Note [Type arguments before an implicit package head].
    source_fun
      | HsVar _ name <- rn_fun
      , isSystemName (unLocWithUserRdr name)
      , EWrap (EExpand _ (ExprCtxt original@HsAppType{})) : _ <- rn_args
      = original
      | otherwise = rn_fun

-- | Classify whether a non-Quick-Look value argument produces an implicit
-- existential package that should be unpacked into the enclosing application.
-- Returns @Just (Just schema)@ when the operand should be checked at @schema@
-- before unpacking, @Just Nothing@ when it should be inferred before unpacking,
-- and @Nothing@ when ordinary checking applies.
-- See Note [Implicit existential arguments].
tcImplicitPkgOperand :: TcType -> LHsExpr GhcRn -> TcM (Maybe (Maybe TcType))
tcImplicitPkgOperand formal_domain larg = case unLoc (stripParensLHsExpr larg) of
  HsVar _ lname
    | let name = unLocWithUserRdr lname
    , not (isDataOcc (nameOccName name)) -> do
        mb_id <- lookupDeclaredIdMaybe name
        id_ty <- traverse (liftZonkM . zonkTcType . varType) mb_id
        pure $ case implicitSpineHead <$> id_ty of
          Just (ExistsTy ImplicitExists _ _) -> Just Nothing
          _                                  -> Nothing
  ExprWithTySig _ _ sig
    | isCompleteHsSig sig
    , HsWC { hswc_body = L _ (HsSig { sig_body = body }) } <- sig -> do
        is_dot <- written_implicit_exists body
        pure (if is_dot then Just Nothing else Nothing)
  _ | Just branches <- resultBranches larg -> do
        declared <- tcPackageOperandResult False larg
        case declared of
          Just (schema, check_s)
            | ExistsTy ImplicitExists _ _ <- coreFullView schema ->
                pure (Just (if check_s then Just schema else Nothing))
          _ | isGuardedTy formal_domain -> do
                all_pkg <- allM is_declared_pkg_branch (NE.toList branches)
                pure (if all_pkg then Just Nothing else Nothing)
            | otherwise -> pure Nothing
  HsLet{} -> do
    declared <- tcPackageOperandResult False larg
    pure $ case declared of
      Just (schema, check_s)
        | ExistsTy ImplicitExists _ _ <- coreFullView schema ->
            Just (if check_s then Just schema else Nothing)
      _ -> Nothing
  _ -> do
    declared <- tcPackageOperandResult False larg
    case declared of
      Just (schema, check_s)
        | ExistsTy ImplicitExists _ _ <- coreFullView schema ->
            pure (Just (if check_s then Just schema else Nothing))
      _ -> do
        proj <- isProjectedImplicitOperand larg
        pure (if proj then Just Nothing else Nothing)
  where
    is_declared_pkg_branch expr
      | Just bs <- resultBranches expr = allM is_declared_pkg_branch (NE.toList bs)
      | otherwise = do
          res <- tcDeclaredOperandResult expr
          pure $ case res of
            Just (ExistsTy ImplicitExists _ _) -> True
            _                                  -> False

    written_implicit_exists (L _ (HsParTy _ nested)) = written_implicit_exists nested
    written_implicit_exists (L _ (XHsType (HsTypeExistsRn ImplicitExists _ _))) = pure True
    written_implicit_exists ty
      | Just (L _ (WithUserRdr _ name)) <- hsTyGetAppHead_maybe ty
      , isTcOcc (nameOccName name) = do
          mb_tc <- lookupDeclaredTyConMaybe name
          pure $ case mb_tc of
            Just tc
              | Just rhs <- synTyConRhs_maybe tc
              , ExistsTy ImplicitExists _ _ <- coreFullView rhs -> True
            _ -> False
      | otherwise = pure False

    isProjectedImplicitOperand :: LHsExpr GhcRn -> TcM Bool
    isProjectedImplicitOperand expr
      | L _ (HsApp _ fun operand) <- stripParensLHsExpr expr
      , (written, L _ (HsVar _ lname)) <- strip_ty_apps [] fun
      , let id_name = unLocWithUserRdr lname
      , not (isDataOcc (nameOccName id_name)) = do
          mb_id <- lookupDeclaredIdMaybe id_name
          case mb_id of
            Nothing -> pure False
            Just var
              | (_, binders, body) <- tcSplitForAllTyVarsReqTVBindersN 0 (varType var)
              , let tvs = map (binderVar . snd) binders
                    specified = [binderVar b | (_, b) <- binders, isSpecifiedForAllTyFlag (binderFlag b)]
              , length written <= length specified
              , isTauTy body, not (hasNativeDependentExpr (Core.Type body))
              , Just (domain, result) <- splitVisibleFunTy_maybe body
              , TyVarTy result_var <- coreFullView result, result_var `elem` tvs
              , Just (tc, domain_args) <- tcSplitTyConApp_maybe domain
              , isGenerativeTyCon tc Nominal
              , any (eqType (mkTyVarTy result_var)) domain_args -> do
                  declared <- tcDeclaredOperandResult operand
                  case declared >>= match_dot tvs tc domain result_var of
                    Just () -> case lookup result_var (zip specified written) of
                      Just (HsWC { hswc_body = ty }) -> written_implicit_exists ty
                      Nothing -> pure True
                    Nothing -> pure False
            _ -> pure False
      | otherwise = pure False

    strip_ty_apps acc (L _ (HsAppType _ f ty)) = strip_ty_apps (ty : acc) f
    strip_ty_apps acc (L _ (HsPar _ f))        = strip_ty_apps acc f
    strip_ty_apps acc e                        = (acc, e)

    match_dot tvs tc domain result_var actual = do
      (actual_tc, _) <- tcSplitTyConApp_maybe actual
      guard (actual_tc == tc && supportsQuickLookType actual && noQuickLookVars actual)
      guard (not (any (`elemVarSet` tyCoVarsOfType actual) tvs))
      let in_scope = mkInScopeSet (tyCoVarsOfTypes [domain, actual])
      subst <- tcMatchTyX_BM (bindOuterTvs tvs) (Subst.mkEmptySubst in_scope) domain actual
      guard (eqType (Subst.substTy subst domain) actual)
      selected <- Subst.lookupTyVar subst result_var
      case coreFullView selected of
        ExistsTy ImplicitExists _ _ -> Just ()
        _                           -> Nothing

-- | Read the result after the written ordinary arguments.
implicitSpineResult :: Int -> TcType -> Maybe TcType
implicitSpineResult 0 ty = Just (implicitSpineHead ty)
implicitSpineResult n ty
  | Just (_, result) <- splitVisibleFunTy_maybe (implicitSpineHead ty)
  = implicitSpineResult (n - 1) result
  | otherwise = Nothing

-- | Read the declared result of a named head and its ordinary value arguments.
tcDeclaredOperandResult :: LHsExpr GhcRn -> TcM (Maybe TcType)
tcDeclaredOperandResult operand = do
  mb_sp <- tcDeclaredOperandSpine False operand
  case mb_sp of
    Nothing -> pure Nothing
    Just (ident, args) -> do
      sigma <- liftZonkM (zonkTcType (varType ident))
      pure (implicitSpineResult (length args) sigma)

-- | Read the result expressions of a branching operand.
-- See Note [Implicit existential arguments].
resultBranches :: LHsExpr GhcRn -> Maybe (NE.NonEmpty (LHsExpr GhcRn))
resultBranches operand = case unLoc (stripParensLHsExpr operand) of
    HsIf _ _ left right -> Just (left NE.:| [right])
    HsMultiIf _ rhss -> Just (fmap rhs rhss)
    HsCase _ _ (MG { mg_alts = L _ alts }) -> do
      matches <- NE.nonEmpty alts
      results <- traverse match_results matches
      pure (join results)
    _ -> Nothing
  where
    match_results (L _ Match { m_grhss = GRHSs _ rhss (L _ EmptyLocalBinds{}) })
      = Just (fmap rhs rhss)
    match_results _ = Nothing

    rhs :: LGRHS GhcRn (LHsExpr GhcRn) -> LHsExpr GhcRn
    rhs (L _ (GRHS _ _ body)) = body

-- | Read a lexical package operand's declared type. A forwarded result
-- supplies an expected type for checking the original operand.
tcLexicalPackageOperand :: LHsExpr GhcRn -> TcM (Maybe (TcType, Bool))
tcLexicalPackageOperand = tcPackageOperandResult True

-- | Read a package result from a declaration, forwarding call or branching expression.
-- See Note [Scope of a forwarded package schema].
tcPackageOperandResult :: Bool -> LHsExpr GhcRn -> TcM (Maybe (TcType, Bool))
tcPackageOperandResult lexical = go False
  where
    go _ operand
      | HsLet _ (L _ (HsValBinds _ (XValBindsLR (HsVBG [(NonRecursive, [L _ binding])] [])))) body
          <- unLoc (stripParensLHsExpr operand)
      , FunBind { fun_id = L _ name, fun_matches = matches } <- binding
      , Just rhs <- lazyLetRhs matches
      , all (returnsLocal name) (fromMaybe (NE.singleton body) (resultBranches body))
      = do { declared <- go True rhs
           ; pure ((\(schema, _) -> (schema, True)) <$> declared) }
    go _ operand
      | Just (first NE.:| rest) <- resultBranches operand
      = runMaybeT $ do
          (schema, _) <- MaybeT (go True first)
          others <- traverse (MaybeT . go True) rest
          guard (all (eqType schema . fst) others)
          pure (schema, True)
    go forwarded operand = runMaybeT $ do
      (ident, args) <- MaybeT (tcDeclaredOperandSpine lexical operand)
      sigma <- liftMaybeT (liftZonkM (zonkTcType (varType ident)))
      let arity = length args
      result <- MaybeT (pure (implicitSpineResult arity sigma))
      case result of
        TyVarTy v
          | isEmptyNameEnv (idConcreteTvs ident)
          , (tvs, _, body) <- tcRetainedInstTelescope sigma
          , v `elem` tvs
          , (matches, domains, final) <- splitFunTys body
          , domains `equalLength` args
          , getTyVar_maybe (coreFullView final) == Just v
          , all isTauTy matches, all (ordinary_domain v) domains
          , first : rest <- [arg | (Scaled _ domain, arg) <- zip domains args
                                 , getTyVar_maybe (coreFullView domain) == Just v]
          -> do { selected@(schema, _) <- MaybeT (go (forwarded || lexical) first)
                ; case rest of
                    [] | lexical -> pure (schema, True)
                    [] -> pure selected
                    _ -> do { guard (not lexical && closed_dot_schema schema)
                            ; alternatives <- traverse (MaybeT . go False) rest
                            ; guard (all (agrees_with schema . fst) alternatives)
                            ; pure (schema, True) } }
        _ -> do { when forwarded $ guard (scoped_dot_schema sigma result)
                ; pure (result, False) }

    scoped_dot_schema sigma ty
      | schema@(ExistsTy ImplicitExists _ _) <- coreFullView ty
      = all rigid (tyCoVarsOfTypeList schema)
      | otherwise = False
      where
        scope = tyCoVarsOfType sigma
        rigid v = isTcTyVar v && isSkolemTyVar v && v `elemVarSet` scope

    closed_dot_schema ty
      | schema@(ExistsTy ImplicitExists _ _) <- coreFullView ty
      = noFreeVarsOfType schema
      | otherwise = False

    agrees_with schema other = closed_dot_schema other && eqType schema other

    ordinary_domain v (Scaled mult domain) =
      isTauTy mult && (isTauTy domain || independent_callback v domain)

    independent_callback v domain
      | (binders@(_:_), _, body) <- tcRetainedInstTelescope domain
      = all isTyVar binders && isTauTy body
        && not (v `elemVarSet` tyCoVarsOfType domain)
        && not (hasNativeDependentExpr (Core.Type domain))
      | otherwise = False

{- Note [Scope of a forwarded package schema]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A nonrecursive local binding can forward its declared package through a case.
For example, in let p = chooseMixed flag in case flag of { True -> p; False -> p },
each result is the same renamed variable. Read the schema from the defining
expression and check the original let at that schema once.

A lexical tuple binding checks a forwarding call at the selected package
schema before opening it. Use the complete declaration of the selected operand
as the scope check so only rigid outer variables remain free; see
Note [Computing deep free variables] in GHC.Core.TyCo.FVs.
-}

-- | Read a named head and its ordinary arguments without elaboration.
tcDeclaredOperandSpine :: Bool -> LHsExpr GhcRn -> TcM (Maybe (Id, [LHsExpr GhcRn]))
tcDeclaredOperandSpine lexical = go []
  where
    go args (L _ (HsApp _ fun arg)) = go (arg : args) fun
    go args (L _ (HsPar _ fun)) = go args fun
    go args expr
      | (written, L _ (HsVar _ lname)) <- leading_types 0 expr
      , written == 0 || lexical
      , let name = unLocWithUserRdr lname
      , not (isDataOcc (nameOccName name))
      = runMaybeT $ do
          ident <- MaybeT (lookup_declared name)
          let (_, binders, _) = tcSplitForAllTyVarsReqTVBindersN 0 (varType ident)
              specified = take written [binderVar b | (_, b) <- binders
                                                    , isSpecifiedForAllTyFlag (binderFlag b)]
          guard (length specified == written && all (isMultiplicityTy . tyVarKind) specified)
          pure (ident, args)
    go _ _ = return Nothing

    lookup_declared name
      | isExternalName name = tcLookupIdMaybe name
      | otherwise = do { local <- tcLookupLcl_maybe name
                       ; return $ case local of
                           Just ATcId{tct_id = ident} -> Just ident
                           _ -> Nothing }

    leading_types n (L _ (HsPar _ fun)) = leading_types n fun
    leading_types n (L _ (HsAppType _ fun _)) = leading_types (n + 1) fun
    leading_types n expr = (n, expr)

-- | Find the declared shape after ordinary invisible instantiation.
implicitSpineHead :: TcType -> TcType
implicitSpineHead ty = case coreFullView ty of
  ForAllTy _ (Bndr _ vis) body | isInvisibleForAllTyFlag vis -> implicitSpineHead body
  FunTy { ft_af = af, ft_res = body } | isInvisibleFunArg af -> implicitSpineHead body
  DepDictTy _ body -> implicitSpineHead body
  other -> other

-- | Check a value spine against the declared payload of an implicit head.
opensImplicitHead :: Int -> TcType -> Bool
opensImplicitHead arity body = arity > 0 && go arity (implicitPayloadType body)
  where
    go 0 _ = True
    go n ty = case splitVisibleFunTy_maybe ty of
      Just (_, result) -> go (n - 1) result
      Nothing -> False

{- Note [Type arguments before an implicit package head]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given builder :: forall a. C a => exists t. a -> Callback t, the prefix
builder @Bool produces a package and the value argument applies to the
function inside that package. Instantiate and check the invisible and visible
type prefix first, unpack the resulting package expression at the application
level, and continue checking the value arguments against the unpacked payload.
-}

{- Note [Implicit existential arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Application checking opens implicit existential packages at a provisional
inner level @L+1@ while anchoring the expected result type at the outer level
@L@. This implements rule App / Elab-App from Eisenberg, Duboc, Weirich & Lee,
"An Existential Crisis Resolved: Type Inference for First-Class Existential
Types", ICFP 2021 (PACMPL 5(ICFP):64), Fig. 6 and §9.2.2 Aside 2, together
with Quick Look (Serrano et al., ICFP 2020) and GHC Proposal #473.

1. Level discipline and escape prevention:
   Function instantiation, result checking ('checkResultTy'), and argument
   checking ('tcValArgs') run at @L+1@. Therefore head unification variables
   born at @L+1@ can unify freely with witness skolems unpacked at @L+1@,
   while 'checkResultTy' connects the application result to the outer @L@
   expected type first so product constructors such as '(filterVec p xs, f)'
   push their instantiated payload domains into arguments automatically. Any
   witness skolem at @L+1@ that reaches the outer @L@ result fails the
   implication's skolem-escape check. When no package is unpacked during the
   application, unfilled @L+1@ metavariables are demoted back to @L@ with no
   implication overhead.

2. Demand boundary ('demandsPayload'):
   An argument with type 'ExistsTy ImplicitExists' unpacks only when the
   formal parameter type demands a payload (algebraic constructor, 'AppTy',
   visible arrow, explicit 'ExistsTy ExplicitExists', or rigid skolem). When
   the formal parameter is itself 'ExistsTy ImplicitExists' (or reduces to one)
   or is an unfilled metavariable, the argument stays packed so polymorphic
   forwarding calls preserve laziness.

3. Lexical witness sharing ('OpeningKey'):
   Unrestricted operands within one pure application spine share a single
   skolem and lazy 'NativeExistsUnpack' binding when 'openingKey' identifies
   the same local package variable ('firstPair p p', 'snd p (fst p)') or the
   same monomorphic unary producer applied to the same local variable
   ('snd (projectionPair b) (fst (projectionPair b))'); see Eisenberg et al.,
   ICFP 2021, §9.2.2 Aside 2. Linear operands and polymorphic variables never
   share an opening.
-}

-- | Check a saturated explicit existential constructor application.
-- See Note [Checking explicit existential packages].
tcExistsPack :: HsExpr GhcRn -> [HsExprArg 'TcpRn] -> ExpRhoType -> TcM (HsExpr GhcTc)
tcExistsPack rn_expr args expected
  = do { enabled <- xoptM LangExt.DependentTypes
       ; unless enabled $ failWithTc (TcRnExistsPack ExistsPackExtension)
       ; let (type_args, rest) = span isETypeArg (filter_not_parens args)
       ; case rest of
           [EValArg { ea_arg = witness }, EValArg { ea_arg = payload }] ->
             do { schema <- check_schema False [ty | ETypeArg { ea_hs_ty = ty } <- type_args]
                ; case coreFullView schema of
                    ExistsTy ExplicitExists binder body ->
                      check_pack_payload schema binder body (Left witness) payload
                    _ -> failWithTc (TcRnExistsPack (ExistsPackInvalidSchema schema)) }
           [EValArg { ea_arg = payload }]
             | not (null type_args) ->
             do { let ty_apps = [ty | ETypeArg { ea_hs_ty = ty } <- type_args]
                ; schema <- check_schema True (init ty_apps)
                ; case coreFullView schema of
                    ExistsTy _ binder body ->
                      check_pack_payload schema binder body (Right (last ty_apps)) payload
                    _ -> failWithTc (TcRnExistsPack (ExistsPackInvalidSchema schema)) }
           _ -> failWithTc (TcRnExistsPack ExistsPackArguments) }
  where
    filter_not_parens = filter $ \case
      EWrap (EPar _) -> False
      _ -> True

    check_pack_payload schema binder body witness_src payload
      = do { arg <- either expr_to_type pure witness_src
           ; inferred <- case arg of
               HsWC [] ty | HsWildCardTy (HoleVar (L _ name)) <- unLoc (ignoreParens ty)
                          , name == unnamedHoleRdrName
                 -> infer_retained_witness binder body payload
               _ -> return Nothing
           ; witness_ty <- maybe (tcHsTypeApp arg (tyVarKind binder)) return inferred
           ; let payload_ty = substTyWith [binder] [witness_ty] body
           ; payload' <- tcScalingUsage ManyTy $ tcCheckPolyExpr payload payload_ty
           ; let packed = XExpr (NativeExistsPack schema witness_ty payload')
           ; tcWrapResult rn_expr packed schema expected }

    infer_retained_witness binder body payload = runMaybeT $ do
      actual <- MaybeT (tcRetainedDeclaredArgType return payload)
      matchRetainedExistsWitness (lexprCtOrigin payload) binder body actual

    expected_schema = traverse (liftZonkM . zonkTcType) =<< readExpType_maybe expected
    read_schema = expected_schema >>= maybe (failWithTc (TcRnExistsPack ExistsPackMissingSchema)) return

    check_schema _ [] = read_schema
    check_schema allow_implicit [arg] = do
      ty <- tcHsTypeApp arg liftedTypeKind
      case coreFullView ty of
        ExistsTy{} -> return ty
        _ -> do
          mb_schema <- expected_schema
          case mb_schema of
            Just schema | ExistsTy{} <- coreFullView schema -> check_kind allow_implicit ty schema
            _ | eqType ty liftedTypeKind -> read_schema
              | otherwise -> return ty
    check_schema allow_implicit [kind, schema] =
      do { k <- tcHsTypeApp kind liftedTypeKind
         ; ty <- tcHsTypeApp schema liftedTypeKind
         ; check_kind allow_implicit k ty }
    check_schema _ _ = failWithTc (TcRnExistsPack ExistsPackArguments)
    check_kind allow_implicit k schema = case coreFullView schema of
      ExistsTy ex_flag binder _
        | ex_flag == ExplicitExists || allow_implicit
        , not (eqType k (tyVarKind binder)) ->
        failWithTc (TcRnExistsPack (ExistsPackInvalidKind k (tyVarKind binder)))
      _ -> return schema

{- Note [Checking explicit existential packages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recognize Pack by its resolved name in base. A constructor named Pack in a
different unit or module uses ordinary application checking.

The existential schema determines the type of the payload. Read it from an
explicit type argument or the expected result type. A single visible argument
can be the schema or its witness kind. Check the kind against the expected
package. With two visible arguments the second gives the schema. Check
the witness with the required type argument translation used by tcVDQ. Check
the payload at the instantiated body type. Keep the schema and witness for
Core Lint. CoreToStg erases both and keeps the payload's demand.

For `Pack _ flag`, the declared type of flag may already refer to a
retained field of an enclosing pattern. A wildcard metavariable cannot be
filled with that field. See Note [Rigid retained indices] in
GHC.Tc.Utils.Unify. Instead, match the package body against the declared
payload type after family reduction. The existential binder is the only
template variable. Check the chosen witness in the current scope, then
typecheck the payload against the instantiated body as usual. Reading its
declaration neither evaluates the payload nor consumes a second use.

Skip ordinary inference of the head, including the Quick Look shortcut.
Pack has no ordinary function type with which the solver could discover the
existential schema from the payload.
-}

-- | Select native application checking from the flag or existing type contracts.
-- See Note [Imported native application contracts].
tcRetainedAppCapability :: TcSigmaType -> ExpRhoType -> [HsExprArg 'TcpRn]
                        -> TcM Bool
tcRetainedAppCapability sigma expected args
  = do { enabled <- xoptM LangExt.DependentTypes
       ; if enabled then return True else
    do { has_dep <- tcHasDepImports
       ; if not has_dep then return False else
    do { sigma' <- zonkAppTypeIfNeeded sigma
       ; expected_ty <- readExpType_maybe expected >>= mapM zonkAppTypeIfNeeded
       ; if any (hasNativeDependentExpr . Core.Type) (sigma' : maybeToList expected_ty)
         then return True else
    do { shapes <- mapM tcRetainedInferableArg [arg | EValArg { ea_arg = arg } <- args]
       ; return (any snd shapes) } } } }

{- Note [Imported native application contracts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An imported type can contain a retained index when the client does not enable
DependentTypes. For example, toList consumes Vec a n through an
erased n. An argument with the declared type Vec Int n supplies that choice.
The client flag must not remove the inference needed to use this contract.

Compute one capability for the application. Enable it when the source flag is
set, when the zonked head or an available expected type contains native syntax,
or when tcRetainedInferableArg finds a declared native value anchor. A record
selector with a retained receiver is such an anchor. The renamer gives it a
separate expression form. A generated selector can quantify the datatype
parameters before taking its retained receiver. This selection reads only
existing types. It does not infer arguments, instantiate a signature, record
occurrences or usage, or expand source syntax. Read an available expected type
without allocating an inference cell.

Pass the capability to the existing application helpers and evidence capture.
Keep their Quick Look modes, complete immutable choices, scope checks, typed
argument caches and final subsumption. A false capability keeps the original
ordinary path. It does not disable checks triggered by an actual DepFunTy.

A checked callback type can establish the same contract at tcValArg. The
guarded paths in tcRetainedCallbackArgs also capture evidence at each callback
instantiation or result check. This change does not enable source type syntax,
container inference or dependent case refinement. It does not assign a native
type to a mutable metavariable. See Note [Retained application evidence].
-}

-- | Select ordinary checking when Quick Look is enabled on a supported native
-- application, keeping `DoQL` on unsupported higher-rank or stuck-sum forms.
-- See Note [Ground retained applications].
tcPrepareNativeQLApp :: Bool -> QLFlag -> ExpRhoType -> (HsExpr GhcRn, SrcSpan)
                     -> HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn]
                     -> TcM ([HsExprArg 'TcpInst], QLFlag, TcSigmaType, [HsExprArg 'TcpRn])
tcPrepareNativeQLApp True DoQL exp_ty rn_head fun sigma args
  = do { impred <- xoptM LangExt.ImpredicativeTypes
       ; if not impred
         then return ([], NoQL, sigma, args)
         else if not (known_head fun)
         then return ([], DoQL, sigma, args)
         else do
           { let (leading, rest) = span isNonValExprArg args
           ; (prefix, residual0, remaining) <-
               if any isETypeArg leading
               then do { (checked, ty) <- tcInstFunWithPrefix quickLookArg True [] Nothing Nothing
                                            Nothing DoQL False rn_head fun sigma leading
                       ; return (checked, ty, rest) }
               else return ([], sigma, args)
           ; expected <- readExpType_maybe exp_ty >>= mapM zonkAppTypeIfNeeded
           ; residual <- zonkAppTypeIfNeeded residual0
           ; mb_ranked_con <- match_ranked_container_con prefix expected remaining residual
           ; case mb_ranked_con of
               Just (inst_tys, body) ->
                 return (prefix ++ [EWrap (EHsWrap (mkWpTyApps inst_tys))], NoQL, body, remaining)
               Nothing -> do
                 { ranked_arg <- has_unannotated_ranked_callback_arg residual remaining
                 ; non_ql_arg <- if not (supportsQuickLookType residual)
                                    || maybe False (not . supportsQuickLookType) expected
                                 then return False
                                 else anyM arg_has_non_ql_anchor
                                        [arg | EValArg { ea_arg = arg } <- remaining]
                 ; let (tvs, _theta, tau) = tcRetainedInstTelescope residual
                       stuck_con_family
                         | XExpr (ConLikeTc (RealDataCon _)) <- fun
                         , Just exp_rho <- expected
                         , Just (fam, _) <- tcSplitTyConApp_maybe exp_rho
                         = isTypeFamilyTyCon fam
                         | otherwise = False
                       use_no_ql
                         | ranked_arg = False
                         | any isETypeArg remaining
                         , not (supportsQuickLookType residual) = False
                         | stuck_con_family
                         , not (supportsQuickLookType residual) = False
                         | not (supportsQuickLookType residual) =
                             null tvs
                               ? (isRetainedValueType tau || isGroundRetainedCallType tau)
                               $ isRetainedValueType tau
                         | maybe False (not . supportsQuickLookType) expected =
                             maybe True (\t -> isRetainedValueType t || isRankedRetainedContainer t) expected
                         | otherwise =
                             non_ql_arg && not (null remaining) && tcAllInvisTvsInArgs residual remaining
                 ; return (prefix, if use_no_ql then NoQL else DoQL, residual, remaining) } } }
  where
    (?) True x _  = x
    (?) False _ y = y

    known_head HsVar{} = True
    known_head (XExpr (ConLikeTc (RealDataCon _))) = True
    known_head _ = False

    arg_has_non_ql_anchor :: LHsExpr GhcRn -> TcM Bool
    arg_has_non_ql_anchor (L _ expr) = case expr of
      HsVar _ lname
        | isDataOcc (nameOccName (unLocWithUserRdr lname)) -> do
            con <- tcLookupConLike (unLoc lname)
            return $ case con of
              RealDataCon dc -> dataConHasRetainedResult dc
              _ -> False
        | otherwise -> do
            mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
            case mb_id of
              Just v  -> not . supportsQuickLookType <$> zonkAppTypeIfNeeded (varType v)
              Nothing -> return False
      XExpr (HsRecSelRn (FieldOcc _ (L _ name))) -> do
        mb_id <- tcLookupIdMaybe name
        return $ case mb_id of
          Just sel
            | (_, body) <- tcSplitForAllInvisTyVars (varType sel)
            , DepFunTy DepFunVisible _ _ <- coreFullView body
            -> True
          _ -> False
      HsApp _ f a -> arg_has_non_ql_anchor f `orM` arg_has_non_ql_anchor a
      OpApp _ l op r -> do
        ql <- wantQuickLook (unLoc op)
        case ql of
          DoQL -> return False
          NoQL -> anyM arg_has_non_ql_anchor [l, op, r]
      HsAppType _ f _ -> arg_has_non_ql_anchor f
      ExplicitList _ xs -> anyM arg_has_non_ql_anchor xs
      ExplicitTuple _ tup_args Boxed ->
        anyM arg_has_non_ql_anchor [a | Present _ a <- tup_args]
      ExprWithTySig _ a _ -> arg_has_non_ql_anchor a
      HsPar _ a -> arg_has_non_ql_anchor a
      HsPragE _ _ a -> arg_has_non_ql_anchor a
      XExpr (ExpandedThingRn (HSE { hse_ctxt = ExprCtxt (HsGetField _ recv (L _ dfo))
                                  , hse_exp = a })) -> do
        mb_sel <- tcResolveDependentDot False recv dfo
        case mb_sel of
          Just (_, sel_id)
            | (_, body) <- tcSplitForAllInvisTyVars (varType sel_id)
            , DepFunTy DepFunVisible _ _ <- coreFullView body
            -> return True
          _ -> arg_has_non_ql_anchor recv `orM` arg_has_non_ql_anchor a
      XExpr (ExpandedThingRn (HSE { hse_exp = a })) -> arg_has_non_ql_anchor a
      _ -> return False

    match_ranked_container_con prefix (Just exp_rho) remaining residual
      | null prefix
      , XExpr (ConLikeTc (RealDataCon _)) <- fun
      , not (any isETypeArg remaining)
      , isRankedRetainedContainer exp_rho
      , (ma_tvs@(_:_), body) <- tcSplitForAllInvisTyVars residual
      , let tvs = map snd ma_tvs
      , isTauTy body
      , Just res <- tcRetainedOrdinaryResult (count isHsValArg remaining) body
      , Just subst <- matchOuterTyKi tvs Subst.emptySubst res exp_rho
      , Just inst_tys <- traverse (Subst.lookupTyVar subst) tvs
      , and (zipWith (substKindAgrees subst) tvs inst_tys)
      , all noQuickLookVars (exp_rho : inst_tys)
      = do { valid <- tcRetainedInstScope inst_tys
           ; return (if valid then Just (inst_tys, Subst.substTy subst body) else Nothing) }
    match_ranked_container_con _ _ _ _ = return Nothing

    has_unannotated_ranked_callback_arg residual remaining
      | (tvs@(_:_), _, tau) <- tcRetainedInstTelescope residual
      , let (_, domains, _) = splitFunTys tau
            val_args = [arg | EValArg { ea_arg = arg } <- remaining]
      = anyM (\(Scaled _ dom, arg) -> case (getTyVar_maybe dom, unLoc (stripParensLHsExpr arg)) of
                (Just tv, HsVar _ lname)
                  | tv `elem` tvs
                  , let nm = unLocWithUserRdr lname
                  , not (isDataOcc (nameOccName nm)) -> do
                      mb_id <- tcLookupIdMaybe nm
                      case mb_id of
                        Just v -> isRankedRetainedCallback <$> zonkAppTypeIfNeeded (varType v)
                        Nothing -> return False
                _ -> return False)
             (zip domains val_args)
      | otherwise = return False
tcPrepareNativeQLApp _ ql _ _ _ sigma args = return ([], ql, sigma, args)

tcAllInvisTvsInArgs :: Type -> [HsExprArg 'TcpRn] -> Bool
tcAllInvisTvsInArgs sigma0 args0 =
  let collect ty rem_args =
        let (tvs, theta, body) = tcRetainedInstTelescope ty
        in case rem_args of
          EWrap{} : rs -> collect ty rs
          EPrag{} : rs -> collect ty rs
          EValArg{} : rs
            | Just (_, tvb, inner) <- tcSplitForAllTyVarBinder_maybe body
            , isVisibleForAllTyFlag (binderFlag tvb) ->
                let (more_tvs, fvs) = collect inner rs
                in (tvs ++ more_tvs, tyCoVarsOfTypes (tyVarKind (binderVar tvb) : theta) `unionVarSet` fvs)
            | Just (af, _, mult, dom, res) <- splitFunTy_maybe body
            , isVisibleFunArg af ->
                let (more_tvs, fvs) = collect res rs
                in (tvs ++ more_tvs, tyCoVarsOfTypes (mult : dom : theta) `unionVarSet` fvs)
          _ -> (tvs, tyCoVarsOfTypes theta)
      (all_invis, arg_fvs) = collect sigma0 args0
  in all (`elemVarSet` arg_fvs) all_invis

-- | Select erased arguments from declared monomorphic values without usage.
-- See Note [Declared native argument choices].
tcRetainedKnownArgPlan :: (LHsExpr GhcRn -> TcM (Maybe Type))
                      -> HsExpr GhcTc -> TcSigmaType
                      -> [HsExprArg 'TcpRn] -> TcM RetainedInstPlan
tcRetainedKnownArgPlan declared fun sigma args
  | hasNoConcreteTyVars fun
  , not (any isETypeArg args)
  = tcRetainedKnownValuePlan declared sigma [arg | EValArg { ea_arg = arg } <- args]
  | otherwise = return Nothing

-- | Match a known declaration and a supplied sequence of value arguments.
tcRetainedKnownValuePlan :: (LHsExpr GhcRn -> TcM (Maybe Type))
                        -> TcSigmaType
                        -> [LHsExpr GhcRn] -> TcM RetainedInstPlan
tcRetainedKnownValuePlan declared sigma values
  | (tvs, theta, tau) <- tcRetainedInstTelescope sigma
  , not (null tvs)
  , let (scoped_preds, body) = scopedDictionaryContext tau
  , isRetainedValueType body
  , not (null values)
  = case coreFullView tau of
      DepFunTy DepFunInvisible binder body ->
        fmap (fmap snd) (tcInferInvisibleRetained tvs binder body values)
      _ -> choose_erased scoped_preds tvs theta tau (retainedDeclaredDomains (length values) tau)
  where
    choose_erased scoped_preds tvs theta tau (domains, _, masked)
      = do { pairs <- fmap catMaybes $ mapM (declared_arg tvs) (zip domains values)
           ; let actuals = map snd pairs
                 matched = foldM (\s (formal, actual) ->
                                    matchRetainedAppType tvs s formal actual)
                                 Subst.emptySubst pairs
           ; case matched of
               Just subst
                 | any (hasNativeDependentExpr . Core.Type) (sigma : actuals)
                 -> runMaybeT $ do
                      -- See Note [Applying a scoped dictionary].
                      choices_subst <- case scoped_preds of
                        [] -> pure subst
                        _  -> do
                          let missing = filter (isNothing . Subst.lookupTyVar subst) tvs
                              required = tyCoVarsOfTypes scoped_preds
                          guard (not (any (`elemVarSet` required) missing))
                          fst <$> liftMaybeT (newMetaTyVarsX subst missing)
                      let fixed = mapMaybe (Subst.lookupTyVar subst) tvs
                          -- See Note [Multiplicity of a supplied native argument].
                          fresh_mult tv = eqType (tyVarKind tv) multiplicityTy
                            && tv `elemVarSet` tyCoVarsOfType tau
                            && not (any (anyFreeVarsOfType (== tv))
                                        (masked : theta ++ map tyVarKind tvs))
                            && any (hasNativeDependentExpr . Core.Type) fixed
                          choice tv = case Subst.lookupTyVar choices_subst tv of
                            Just ty -> pure ty
                            Nothing -> guard (fresh_mult tv) >> liftMaybeT newMultiplicityVar
                      choices <- traverse choice tvs
                      guard (not (any (`elemVarSet` tyCoVarsOfTypes choices) tvs))
                      guard (and (zipWith (\tv ty ->
                                eqType (Subst.substTy choices_subst (tyVarKind tv)) (typeKind ty)) tvs choices))
                      guard (all (\(formal, actual) -> eqType (Subst.substTy choices_subst formal) actual) pairs)
                      let tys = sigma : actuals ++ choices
                      guard (all noQuickLookVars tys)
                      valid <- liftMaybeT (tcRetainedInstScope tys)
                      guard valid
                      return choices
               _ -> return Nothing }

    declared_arg tvs (Just (retained, formal), arg)
      | any (`elemVarSet` tyCoVarsOfType formal) tvs
      = do { actual <- if retained then tcRetainedConstructorArgType arg
                       else declared arg
           ; return ((formal,) . declared_domain_view retained formal <$> actual) }
      | otherwise = return Nothing
    declared_arg _ _ = return Nothing

    -- See Note [Declared retained-domain choices].
    declared_domain_view True formal actual
      | Just (tc, _) <- tcSplitTyConApp_maybe formal
      , isAlgTyCon tc
      , Just (_, params) <- matchSumPayload tc actual
      = mkTyConApp tc params
    declared_domain_view _ _ actual = actual

tcRetainedKnownValuePlan _ _ _ = return Nothing

-- | Does a value argument accept a retained value or a constructor indexed by its field?
-- See Note [Nested immutable retained applications].
takesDirectIndexArgument :: Type -> Bool
takesDirectIndexArgument ty = case coreFullView ty of
  DepFunTy DepFunVisible _ _ -> True
  _ | Just (domain, result) <- splitVisibleFunTy_maybe ty ->
    case tcSplitTyConApp_maybe domain of
      Just (tc, _) | any dataConHasRetainedResult (tyConDataCons tc) -> True
      _ -> takesDirectIndexArgument result
  _ -> False

-- | Peel visible ordinary and retained arguments to check for a tau result shape.
tcRetainedValueResultShape :: Int -> Type -> Maybe Type
tcRetainedValueResultShape 0 ty
  | isTauTy ty = Just ty
  | otherwise  = Nothing
tcRetainedValueResultShape n ty
  | n > 0 = case coreFullView ty of
      DepFunTy DepFunVisible _ res -> tcRetainedValueResultShape (n - 1) res
      _ | Just (_, res) <- splitVisibleFunTy_maybe ty -> tcRetainedValueResultShape (n - 1) res
      _ -> Nothing
  | otherwise = Nothing

{- Note [Nested immutable retained applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `toList (appendVec left right)`, the declared vector types can
fix all erased arguments of appendVec. Its result still contains a
scoped runtime index. Quick Look's nested application rule would
enter the inner application through tcInstFun, without the complete
immutable plan available when checking it directly. Instantiation
variables cannot contain that retained index.

Read the declared result of a simple, fully applied, unconstrained
inner call using the same immutable selector as a direct call. Inspect
only operands that determine an erased argument. Each recursive call
examines a proper subexpression. Recurse through a named call only if a
declared domain has a constructor with a retained result index. Repeated
ordinary calls such as choose (choose (Just id)) must not inspect every
suffix of the argument tree. This also handles an ordinary projection
of a rebuilt value, such as `readLayout (roundCell (Cell xs value))`.
Use the algebraic argument type only to complete the outer caller's plan.
Otherwise leave normal Quick Look in place. The normal checker still
checks the inner and outer applications, including their argument types,
lexical scopes and usage. The plan does not evaluate any operand or
assign a native type to a Quick Look variable.

A constructor can return its own field as an index. In
`rebuild (IndexedList xs)`, the outer function must instantiate its erased
index with the caller's xs. Its signature alone cannot supply this index.
The constructor's result equality enables the same argument inference as
a named value with a retained type. With Quick Look, read the constructor
wrapper's codomain at the scoped field Id before instantiating rebuild.
An ordinary payload arrow does not bind an index. Step past each such
arrow without checking its operand. For example, in
`adjust f (Reading calibrated value)`, only calibrated fixes the result
type. For an unsigned local variable, use the ordinary retained-variable
check to fix its type or multiplicity from the constructor domain. Only
touchable ordinary metavariables qualify. Check the operand's kind too.
The saturated constructor requires these assignments even if the outer
plan fails and ordinary checking resumes. The normal argument check
records usage. Report a linear operand at its source occurrence.
See Note [Retained constructor result equality] in GHC.Core.DataCon.
-}

-- | Read a monomorphic declaration for immutable retained argument matching.
-- See Note [Declared retained-domain choices].
tcRetainedDeclaredArgType :: (Type -> TcM Type) -> LHsExpr GhcRn -> TcM (Maybe Type)
tcRetainedDeclaredArgType view (L _ (HsPar _ arg)) = tcRetainedDeclaredArgType view arg
tcRetainedDeclaredArgType view (L _ (XExpr (ExpandedThingRn (HSE _ arg))))
  = tcRetainedDeclaredArgType view arg
tcRetainedDeclaredArgType view (L _ (ExplicitList _ fields))
  = fmap (fmap mkListTy) (tcRetainedListElementType (tcRetainedDeclaredArgType view) fields)
tcRetainedDeclaredArgType _ (L _ (HsLit _ HsChar{})) = return (Just charTy)
tcRetainedDeclaredArgType _ (L _ (HsLit _ HsString{})) = return (Just (mkListTy charTy))
tcRetainedDeclaredArgType view_type expr@(L loc HsAppType{})
  | Just (name, written@(_:_)) <- retainedSourceHead expr
  = setSrcSpan (locA loc) $
    do { mb_id <- tcLookupIdMaybe name
       ; case mb_id of
           Just var -> do
             { ty0 <- zonkAppTypeIfNeeded (varType var)
             ; (mb_ty, w) <- captureConstraints $
                 discardErrs $ tryTcDiscardingErrs (return Nothing) $
                   runMaybeT $ foldM (step_vta var) ty0 written
             ; case mb_ty of
                 Just ty
                   | isEmptyWC w -> do
                       { valid <- tcRetainedInstScope [ty]
                       ; let (tvs, theta, _) = tcRetainedInstTelescope ty
                       ; if valid && null tvs && null theta && isRetainedValueType ty
                                  && noQuickLookVars ty
                         then do { view <- view_type ty
                                 ; return (if isRetainedValueType view && noQuickLookVars view
                                           then Just view else Nothing) }
                         else return Nothing }
                 _ -> return Nothing }
           Nothing -> return Nothing }
  where
    step_vta var sigma hs_ty = do
      let (inf_tvs, body0)   = tcSplitSomeForAllTyVars isInferredForAllTyFlag sigma
          (inf_theta, body1) = tcSplitPhiTy body0
      sigma1 <- if null inf_tvs && null inf_theta
                then return sigma
                else snd <$> liftMaybeT (instantiateSigmaQLWithAcc [] NoQL
                               (OccurrenceOf (idName var)) (idConcreteTvs var) inf_tvs inf_theta body1)
      case tcSplitForAllTyVarBinder_maybe sigma1 of
        Just (_, binder, _)
          | isSpecifiedForAllTyFlag (binderFlag binder) -> do
              (_, body) <- liftMaybeT (tcVTA (idConcreteTvs var) sigma1 Nothing hs_ty)
              body' <- liftMaybeT (liftZonkM (zonkTcType body))
              guard (not (any (\v -> isTcTyVar v && isMetaTyVar v) (tyCoVarsOfTypeList body')))
              return body'
        _ -> mzero
tcRetainedDeclaredArgType view_type (L loc (HsVar _ lname))
  | not (isDataOcc (nameOccName (unLocWithUserRdr lname)))
  = setSrcSpan (locA loc) $
    do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
       ; case mb_id of
           Just var -> do
             { ty <- zonkAppTypeIfNeeded (varType var)
             ; valid <- tcRetainedInstScope [ty]
             ; let (tvs, theta, _) = tcRetainedInstTelescope ty
             ; if valid && null tvs && null theta && isRetainedValueType ty
                        && noQuickLookVars ty
               then do { view <- view_type ty
                       ; return (if isRetainedValueType view && noQuickLookVars view
                                 then Just view else Nothing) }
               else return Nothing }
           Nothing -> return Nothing }
  | otherwise
  = setSrcSpan (locA loc) $ do
      thing <- tcLookupGlobal (unLocWithUserRdr lname)
      case thing of
        AConLike (RealDataCon dc)
          | null (dataConRepArgBinders dc)
          , let (_, _, _, theta, _, _) = dataConFullSig dc
          , null (dataConUserTyVars dc) && null theta
          , [Scaled mult in_ty] <- dataConOrigArgTys dc
          , isOneTy mult || isManyTy mult
          , let res_ty = dataConOrigResTy dc
                fun_ty = mkVisFunTyManyU in_ty res_ty
          , isJust (nativeIndexDataConTyArgs res_ty dc)
          , isRetainedValueType fun_ty && noQuickLookVars fun_ty
          -> do { valid <- tcRetainedInstScope [fun_ty]
                ; return (if valid then Just fun_ty else Nothing) }
        _ -> return Nothing
tcRetainedDeclaredArgType _ _ = return Nothing

-- | Read a visible retained argument's type from its constructor fields or
-- saturated certified source calls.
-- See Note [Erased parameters of a retained constructor argument] and
-- Note [Pre-unifying polymorphic retained domains in inference mode].
tcRetainedConstructorArgType :: LHsExpr GhcRn -> TcM (Maybe Type)
tcRetainedConstructorArgType = tcRetainedConstructorArgTypeWith tcRetainedDeclaredTypeView Nothing

-- | Read a visible retained argument's type with a caller-selected variable view,
-- optionally seeding undetermined parameters of a polymorphic constructor
-- (such as @VNil :: Vec a Zero@) from a partially-known enclosing formal type.
-- See Note [Pre-unifying polymorphic retained domains in inference mode].
tcRetainedConstructorArgTypeWith :: (Type -> TcM Type) -> Maybe ([TyVar], Type)
                                 -> LHsExpr GhcRn -> TcM (Maybe Type)
tcRetainedConstructorArgTypeWith view mb_exp = collect []
  where
    collect args (L _ (HsPar _ fun)) = collect args fun
    collect args (L _ (XExpr (ExpandedThingRn (HSE _ fun)))) = collect args fun
    collect args (L _ (HsApp _ fun arg)) = collect (arg : args) fun
    collect [] (L _ (OpApp _ lhs op rhs)) = collect [lhs, rhs] op
    collect [] (L _ (ExplicitTuple _ [Present _ first, Present _ second] Boxed))
      = collect [first, second] (nlHsVar (dataConName (tupleDataCon Boxed 2)))
    collect [] (L _ (ExplicitList _ fields))
      = fmap (fmap mkListTy) (firstJustsM (map (tcRetainedConstructorArgTypeWith view Nothing) fields))
    collect [] (L loc (ExprWithTySig _ _ sig))
      | isCompleteHsSig sig
      , HsWC { hswc_ext = wcs, hswc_body = L _ (HsSig _ bndrs body) } <- sig
      , null wcs, null (hsOuterTyVarNames bndrs), hsTypeHasNoAnonWildcards body
      = setSrcSpanA loc $
        tryTcDiscardingErrs (return Nothing) $ do
          { ty <- tcHsSigWcType False (ExprSigCtxt NoRRC) sig
          ; if isTauTy ty && isRetainedValueType ty && noQuickLookVars ty
            then do { valid <- tcRetainedInstScope [ty]
                    ; return (if valid then Just ty else Nothing) }
            else return Nothing }
    collect args (L loc (HsVar _ lname))
      | let name = unLocWithUserRdr lname
      , isDataOcc (nameOccName name)
      = setSrcSpanA loc $ do
          { thing <- tcLookupGlobal name
          ; case thing of
              AConLike (RealDataCon dc)
                | null (dataConRepArgBinders dc)
                , let (_, _, _, theta, _, _) = dataConFullSig dc
                      raw_tvs = dataConUserTyVars dc
                      raw_fields = dataConOrigArgTys dc
                      raw_result = dataConOrigResTy dc
                , null theta
                , raw_fields `equalLength` args
                  || (length raw_fields == length args + 1
                      && let mult = scaledMult (last raw_fields)
                         in isOneTy mult || isManyTy mult)
                -> do { (clone_s, tvs) <- clone_tvs
                          (Subst.mkEmptySubst (mkInScopeSet (mkVarSet raw_tvs))) raw_tvs
                      ; let fields = map (Subst.substScaledTy clone_s) raw_fields
                            result = Subst.substTy clone_s raw_result
                            partial_1 = length raw_fields == length args + 1
                            prefix_fields = if partial_1 then init fields else fields
                            target = if partial_1
                                     then mkVisFunTyManyU (scaledThing (last fields)) result
                                     else result
                            seed_s = seed_from_expected tvs target
                      ; subst0 <- foldM (tcMatchDeclaredFieldWith view tvs) seed_s (zip prefix_fields args)
                      ; let subst = fill_from_expected tvs target subst0
                      ; case traverse (Subst.lookupTyVar subst) tvs of
                          Just choices
                            | let bad_tvs = tvs ++ maybe [] fst mb_exp
                            , not (any (`elemVarSet` tyCoVarsOfTypes choices) bad_tvs)
                            , all (\v -> not (isTyVar v && isMetaTyVar v)) (tyCoVarsOfTypesList choices)
                            , let res_ty = substTyWith tvs choices result
                                  ty     = substTyWith tvs choices target
                            , isJust (nativeIndexDataConTyArgs res_ty dc)
                              || isJust (Core.nativeIndexedConArgs dc choices)
                            , isRetainedValueType ty && noQuickLookVars ty
                            -> do { valid <- tcRetainedInstScope [ty]
                                  ; return (if valid then Just ty else Nothing) }
                          _ -> return Nothing }
              _ -> return Nothing }
    collect args head_expr
      | not (null args)
      , Just (name, written) <- retainedSourceHead head_expr
      = setSrcSpanA (getLoc head_expr) $
        if isExternalName name
          then collect_source_call name written args
          else if null written
                 then collect_local_fun name args
                 else return Nothing
    collect [] arg = tcRetainedDeclaredArgType view arg
    collect _ _ = return Nothing

    clone_tvs = mapAccumLM $ \s tv -> do
      nm <- newSysName (nameOccName (getName tv))
      let tv' = mkTyVar nm (Subst.substTy s (tyVarKind tv))
          s'  = Subst.extendTvSubstAndInScope s tv (mkTyVarTy tv')
      return (s', tv')

    seed_from_expected tvs result
      | Just (outer_tvs, exp_ty) <- mb_exp
      , let flexi_set = mkVarSet (tvs ++ outer_tvs)
            bad_set   = flexi_set
            bind_var v _ = if v `elemVarSet` flexi_set then BindMe else DontBindMe
      , Just u_subst <- tcUnifyTys bind_var [result] [exp_ty]
      = foldl' (\acc tv -> case Subst.lookupTyVar u_subst tv of
                  Just rhs | not (anyFreeVarsOfType (`elemVarSet` bad_set) rhs) ->
                    Subst.extendTvSubst acc tv rhs
                  _ -> acc)
               Subst.emptySubst tvs
      | otherwise = Subst.emptySubst

    fill_from_expected tvs result subst
      | Just (outer_tvs, exp_ty) <- mb_exp
      , let missing = [tv | tv <- tvs, isNothing (Subst.lookupTyVar subst tv)]
      , not (null missing)
      , let flexi_set = mkVarSet (missing ++ outer_tvs)
            bind_var v _ = if v `elemVarSet` flexi_set then BindMe else DontBindMe
            res_partial = Subst.substTy subst result
      , Just u_subst <- tcUnifyTys bind_var [res_partial] [exp_ty]
      = foldl' (\acc tv -> maybe acc (Subst.extendTvSubst acc tv) (Subst.lookupTyVar u_subst tv))
               subst missing
      | otherwise = subst

    collect_source_call name written args = do
      mb_actual <- tcLookupIdMaybe name
      env <- getGblEnv
      case mb_actual >>= sourceTableOccurrence env of
        Just owner
          | Just family <- sourceTableId_maybe owner
          , isEmptyNameEnv (idConcreteTvs owner) -> do
              owner_ty <- liftZonkM (zonkTcType (varType owner))
              mb_sigma <- apply_written owner written owner_ty
              case mb_sigma of
                Nothing -> return Nothing
                Just sigma -> do
                  let (ma_tvs, raw_body) = tcSplitForAllInvisTyVars sigma
                      raw_tvs = map snd ma_tvs
                  (clone_s, tvs) <- clone_tvs
                    (Subst.mkEmptySubst (mkInScopeSet (mkVarSet raw_tvs))) raw_tvs
                  let body = Subst.substTy clone_s raw_body
                      (theta, tau) = tcSplitPhiTy body
                  case peel_fun_args (length args) tau of
                    Just (fields, result)
                      | null theta
                      , length written + length tvs + length args == tyConArity family -> do
                          let seed_s = seed_from_expected tvs result
                          subst0 <- foldM (tcMatchDeclaredFieldWith view tvs)
                                          seed_s (zip fields args)
                          let subst = fill_from_expected tvs result subst0
                          case traverse (Subst.lookupTyVar subst) tvs of
                            Just choices
                              | let bad_tvs = tvs ++ maybe [] fst mb_exp
                              , not (any (`elemVarSet` tyCoVarsOfTypes choices) bad_tvs)
                              , all (\v -> not (isTyVar v && isMetaTyVar v)) (tyCoVarsOfTypesList choices)
                              , and (zipWith (\tv c ->
                                      eqType (Subst.substTy subst (tyVarKind tv)) (typeKind c))
                                      tvs choices)
                              , let ty = substTyWith tvs choices result
                              , isRetainedValueType ty && noQuickLookVars ty -> do
                                  valid <- tcRetainedInstScope (choices ++ [ty])
                                  return (if valid then Just ty else Nothing)
                            _ -> return Nothing
                    _ -> return Nothing
        _ -> return Nothing

    apply_written _ [] sigma = return (Just sigma)
    apply_written owner written sigma0 =
      tryTcDiscardingErrs (return Nothing) $ runMaybeT $
        foldM step sigma0 written
      where
        step sigma hs_ty
          | Just (_, binder, _) <- tcSplitForAllTyVarBinder_maybe sigma
          , isSpecifiedForAllTyFlag (binderFlag binder) = do
              (_, body) <- liftMaybeT (tcVTA (idConcreteTvs owner) sigma Nothing hs_ty)
              body' <- liftMaybeT (liftZonkM (zonkTcType body))
              valid <- liftMaybeT (tcRetainedInstScope [body'])
              guard (valid && noQuickLookVars body'
                     && not (any (\v -> isTcTyVar v && isMetaTyVar v) (tyCoVarsOfTypeList body')))
              return body'
          | otherwise = mzero

    collect_local_fun name args = do
      mb_lcl <- tcLookupLcl_maybe name
      case mb_lcl of
        Just (ATcId { tct_id = fun_id }) -> do
          fun_id' <- liftZonkM (zonkId fun_id)
          case peel_fun_args (length args) (varType fun_id') of
            Just (_, result)
              | isNativeIndexFunctionId fun_id'
              , not (isFunTy result)
              , isRetainedValueType result && noQuickLookVars result -> do
                  valid <- tcRetainedInstScope [result]
                  return (if valid then Just result else Nothing)
            _ -> return Nothing
        _ -> return Nothing

    peel_fun_args 0 res = Just ([], res)
    peel_fun_args n ty
      | Just (af, _, mult, arg_ty, res_ty) <- splitFunTy_maybe ty
      , isVisibleFunArg af
      = do { (rest, final_res) <- peel_fun_args (n - 1) res_ty
           ; Just (Scaled mult arg_ty : rest, final_res) }
      | otherwise = Nothing

-- | Use a field's declaration only while it can determine a missing binder.
tcMatchDeclaredField :: [TyVar] -> Subst.Subst -> (Scaled Type, LHsExpr GhcRn) -> TcM Subst.Subst
tcMatchDeclaredField = tcMatchDeclaredFieldWith tcRetainedDeclaredTypeView

-- | Match a field or operand declaration with a caller-selected variable view.
tcMatchDeclaredFieldWith :: (Type -> TcM Type) -> [TyVar] -> Subst.Subst
                         -> (Scaled Type, LHsExpr GhcRn) -> TcM Subst.Subst
tcMatchDeclaredFieldWith view tvs subst (Scaled _ formal, arg)
  | any (\tv -> isNothing (Subst.lookupTyVar subst tv)
              && tv `elemVarSet` tyCoVarsOfType formal) tvs
  = do { let formal_partial = Subst.substTy subst formal
       ; actual <- tcRetainedConstructorArgTypeWith view (Just (tvs, formal_partial)) arg
       ; return (fromMaybe subst (actual >>= matchRetainedAppType tvs subst formal)) }
  | otherwise = return subst

{- Note [Pre-unifying polymorphic retained domains in inference mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a polymorphic retained lemma called in a case scrutinee:

  appendNilR :: forall a. foreach (xs :: [a]) -> (xs ++ '[]) :~: xs
  withRevCase ys = case appendNilR (rev ys) of Refl -> Refl

In inference mode, the expected result type is absent and cannot fix the
erased parameter `a`. If `appendNilR` instantiates `a` with a fresh unification
variable `a0`, the retained binder domain `[a0]` still contains `a0` when
`tcRetainedArgWith` checks `rev ys`.

We resolve `a` before or during retained operand checking in three steps:

1. `tcRetainedConstructorArgTypeWith` reads the result type of saturated
   certified source calls (such as `rev ys` and `rev (rev ys)`) and local
   retained function calls without recording occurrences or usage. When every
   outer erased parameter of `appendNilR` is determined, `tcRetainedKnownArgPlan`
   selects the immutable type argument `@a` before `IALL` allocates `a0`. This
   path also handles element types that contain runtime indices, such as
   `ys :: [Vec Int n]`.

2. When `tcInstFun` has already allocated a touchable unification variable in
   the retained binder domain (for example, `appendNilR @_ (rev ys)` or a call
   with an unrelated undetermined type parameter), `constructor_domain` in
   `tcRetainedArgWith` unifies the domain against the non-dependent type read
   by `tcRetainedConstructorArgType` and zonks the domain before resolving the
   operand. `tcInstFun` also zonks the binder domain stored in `ERetainedArg`.

3. If `resolve` produces a valid index whose kind still faces an unfilled
   meta-variable in the zonked domain (for example, from a beta redex or
   partially determined product), `tcRetainedArgWith` unifies the non-dependent
   index kind with the domain and requires a reflexive coercion after zonking.

None of these steps invokes unbounded family reduction or assigns a native
dependent type to a mutable unification variable.
-}

{- Note [Erased parameters of a retained constructor argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In sourceVector (x : xs), the type of x supplies the list element type.
This can be Vec Bool n for a runtime binder n. Select the erased argument
before IALL creates a metavariable that cannot mention n.

Read the declared types of fields of saturated, eligible index constructors.
Resolve the constructor by its renamed Name. Match its original field types
in source order. A field contributes only while it can determine a missing
parameter. For Twin x y z with fields a, a, b, the declaration of x can fix
a. The ordinary argument checker then reports a mismatch at y. A nullary
constructor such as False determines Bool. A polymorphic nullary constructor
cannot supply its missing parameter. Numeric literals require a known domain.

Use the same constructor application to infer and check a written boxed pair
with both fields present. The constructor leaves both fields lazy. Tuple
sections and unboxed tuples keep their existing checking paths.

This pass reads declarations without checking occurrences. The enclosing
selector validates the scope and kinds of the complete choice. tcRetainedArg
checks the actual constructor tree at that domain. Value checking accounts
for field usage. See Note [Declared retained-domain choices].
-}

-- | Select one invisible value operand and any outer erased parameters.
-- See Note [Inferring an invisible retained argument].
tcInferInvisibleRetained :: [TyVar] -> Id -> Type -> [LHsExpr GhcRn]
                         -> TcM (Maybe (InvisibleOperand, [Type]))
tcInferInvisibleRetained tvs binder body args
  = do { name <- newSysName (mkTyVarOcc "retained_argument")
       ; let template = mkTyVar name (varType binder)
       ; (indices, suffix) <- collect [(TermVarTy binder, template)] body
       ; let (domains, _, _) = retainedDeclaredDomains (length args) suffix
             choose view = do
               { pairs <- catMaybes <$> mapM (declared_pair view indices) (zip domains args)
               ; let determines (formal, _) = template `elemVarSet` injectiveVarsOfType True formal
                     anchors | null tvs  = filter determines pairs
                             | otherwise = pairs  -- Erased argument prepass
               ; select template (map snd indices ++ tvs) anchors }
       ; givens <- getNativeGivens
       ; selected <- choose return
       ; case selected of
           Nothing | not (null givens) -> choose tcRetainedDeclaredTypeView
           _ -> return selected }
  where
    collect indices ty
      | DepFunTy DepFunInvisible next rest <- coreFullView ty
      , Just kind <- abstractNativeIndices indices (varType next)
      = do { name <- newSysName (mkTyVarOcc "retained_argument")
           ; collect ((TermVarTy next, mkTyVar name kind) : indices) rest }
      | otherwise = return (indices, ty)

    select template variables pairs
      = case foldM match Subst.emptySubst pairs of
          Just subst
            | Just index <- Subst.lookupTyVar subst template
            , Just choices <- traverse (Subst.lookupTyVar subst) tvs
            , all noQuickLookVars (index : choices)
            , not (any (`elemVarSet` tyCoVarsOfTypes (index : choices)) variables)
            , typeKind index `eqType` Subst.substTy subst (varType binder)
            , and (zipWith (\tv ty ->
                     eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty))
                   tvs choices)
            , all (\(formal, actual) -> eqType (Subst.substTy subst formal) actual) pairs
            -> do { valid <- tcRetainedInstScope (index : choices)
                  ; if valid then fmap (fmap (, choices)) (tcInvisibleOperand index)
                             else return Nothing }
          _ -> return Nothing
      where
        match subst (formal, actual) = matchRetainedAppType variables subst formal actual

    declared_pair view indices (Just (_, formal), arg)
      | any (`elemVarSet` tyCoVarsOfType formal) (binder : tvs)
      , Just abstract <- abstractNativeIndices indices formal
      = do { actual0 <- tcRetainedDeclaredArgType view arg
           ; actual <- case actual0 of
               Just ty -> return (Just ty)
               Nothing -> tcRetainedConstructorArgTypeWith view
                            (Just (map snd indices ++ tvs, abstract)) arg
           ; return ((abstract,) <$> actual) }
    declared_pair _ _ _ = return Nothing

-- | The inferred value and the usage to record when it is inserted.
data InvisibleOperand = InvisibleOperand Core.CoreExpr (TcM ())

-- | Recover a runtime operand and its term and constructor uses.
-- See Note [Inferring an invisible retained argument].
tcInvisibleOperand :: Type -> TcM (Maybe InvisibleOperand)
tcInvisibleOperand index
  = do { rdr_env <- getGlobalRdrEnv
       ; candidate <- recover rdr_env index
       ; return $ do { (operand, uses) <- candidate
                     ; recovered <- Core.exprToNativeIndex operand
                     ; guard (eqType recovered index)
                     ; return (InvisibleOperand operand uses) } }
  where
    recover _ (TermVarTy occurrence)
      = do { thing <- tcLookupLcl_maybe (getName occurrence)
           ; case thing of
               Just (ATcId { tct_id = actual }) ->
                 do { actual <- liftZonkM (zonkId actual)
                    ; return $ do { guard (occurrence == actual && isNativeOpaqueValueId actual
                                          && varType occurrence `eqType` varType actual)
                                  ; return (Core.Var actual, tcEmitBindingUsage (singleUsageUE actual)) } }
               _ -> return Nothing }
    recover env ty@(TyConApp tc args)
      | Just con <- isPromotedDataCon_maybe tc
      , not (isTypeDataTyCon (dataConTyCon con))
      , Just gre <- lookupGRE_Name env (dataConName con)
      , Just tys <- nativeIndexDataConTyArgs (typeKind ty) con
      = do { fields <- mapM (recover env) (drop (length tys) args)
           ; return $ do { (values, uses) <- unzip <$> sequence fields
                         ; let worker = Core.mkTyApps (Core.Var (dataConWorkId con)) tys
                         ; return (Core.mkApps worker values,
                                   addUsedGRE NoDeprecationWarnings gre >> sequence_ uses) } }
    recover _ _ = return Nothing

{- Note [Inferring an invisible retained argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a signature such as

  f :: forall a. foreach (n :: Nat). Vec a n -> Int

the declaration of xs in f xs can determine a := Int and n := the local
runtime Id used in Vec Int n. Replace the formal index with an immutable
template. Match the declared argument domains together. Require an exact
match and a complete choice for the erased parameters. Recheck the runtime
Id in the actual lexical environment. Treat an unavailable declaration as
no advice. The normal argument checker still checks the written argument.

Use this match before IALL can allocate native metavariable candidates.
When the invisible binder is reached, match its instantiated type and
insert the same runtime operand. Keep the written argument and its position
for the usual cache and diagnostics. Emit usage once at Many and let the
remaining application instantiate constraints with the chosen local Id.
See Note [Retained application evidence].

For appendHidden, the two consecutive invisible Nat binders precede
Vec a n and Vec a m. Abstract each binder with a fresh template. Abstract
earlier indices in the kind of each later template. Read the domains after
the group. Select only the current operand and outer erased parameters.
The next application step sees the substituted type and checks the next
operand in its own scope. A written operand also enters through that step.

When inserting a runtime operand, match only domains with an injective
occurrence of its template. For example, Vec Int (Forget n) cannot select n
when Forget maps every Nat to Z. A later Vec Int n can supply it. Check the
Forget domain at its written argument after substitution. The erased
argument prepass may use all domain matches to select its type arguments.
It discards the speculative runtime operand. This distinction also lets
an explicit operand select the runtime value.

Prefer the original declaration for runtime identity. A constructor Given
can refine the index n in Vec a n to Z. The caller can still pass its local
n. Retry with the existing refined declaration view when raw matching fails
in a context with Givens. A generated branch capture can already have the
refined declaration Vec a (S pred). Rebuild the constructor application
from its promoted form. Require term visibility for each constructor and
resolve each field variable in its lexical environment. The Core index
judgment must recover the selected index from the complete operand.
Only term fields contribute occurrences to runtime usage. Record the
constructor imports when inserting the selected operand, after the erased
argument prepass. See wrinkle (DTW2) of Note [DataToTag overview] in
GHC.Tc.Instance.Class.

Agda inserts hidden argument terms from the expected telescope (Norell, 2007).
Here the immutable match supplies a checked runtime occurrence and leaves stuck
families and opaque calls alone; see GHC Proposal #378 (§4.7) and #448.
-}

-- | Find a declared native element type without checking the literal.
-- See Note [Declared retained-domain choices] and Note [Native list anchors].
tcRetainedListElementType :: (LHsExpr GhcRn -> TcM (Maybe Type))
                          -> [LHsExpr GhcRn] -> TcM (Maybe Type)
tcRetainedListElementType _ [] = return Nothing
tcRetainedListElementType declared (L _ (HsPar _ field) : fields)
  = tcRetainedListElementType declared (field : fields)
tcRetainedListElementType declared (field@(L _ HsVar{}) : fields)
  = do { actual <- declared field
       ; case actual of
           Just ty | hasNativeDependentExpr (Core.Type ty)
                   , isTauTy ty
                   , typeKind ty `eqType` liftedTypeKind
             -> return (Just ty)
           _ -> tcRetainedListElementType declared fields }
tcRetainedListElementType declared (_ : fields)
  = tcRetainedListElementType declared fields

-- | Refine a declared argument type with actual lexical constructor Givens.
-- Keep the argument Id and its stored type unchanged.
-- See Note [Declared retained argument refinement].
tcRetainedDeclaredTypeView :: Type -> TcM Type
tcRetainedDeclaredTypeView ty = coercionRKind <$> tcRetainedTypeCoercion (const True) ty

-- | Transport a type using constructor proofs about the selected indices.
tcRetainedTypeCoercion :: (Id -> Bool) -> Type -> TcM CoercionN
tcRetainedTypeCoercion select ty
  = do { givens <- getNativeGivens
       ; givens <- mapM (liftZonkM . zonkId) givens
       ; foldM refine (mkNomReflCo ty) (reverse givens) }
  where
    refine result_co given
      | isCoVar given
      , Just (index, co) <- orient (mkCoVarCo given)
      , select index
      , isRetainedConstructorProof index co
      , let result = coercionRKind result_co
      , anyFreeVarsOfType (== index) result
      = do { name <- newSysName (mkTyVarOcc "retained_view")
           ; let template = mkTyVar name (varType index)
           ; case abstractNativeIndices [(TermVarTy index, template)] result of
               Nothing -> return result_co
               Just _ -> mkTransCo result_co <$> tcRetainedCoercion index co result }
      | otherwise = return result_co

    orient co
      | TermVarTy index <- coercionLKind co = Just (index, co)
      | TermVarTy index <- coercionRKind co = Just (index, mkSymCo co)
      | otherwise = Nothing

{- Note [Declared retained argument refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor branch can have retained ~# S k while a declared argument keeps
its original type [Vec Int retained]. Select the erased element parameter of
genericVector from the proof-backed view [Vec Int (S k)]. Otherwise Count at
the original and refined element types can block result checking before the
ordinary solver checks the argument.

Use only the actual lexical evidence parameters returned by getNativeGivens. This
list is newest first. Apply checked nominal constructor proofs from outer to
inner scope. Orient an actual nominal proof toward the runtime index before
checking its constructor endpoint. For example, matching Refl can supply
S Z ~# size rather than size ~# S Z. The symmetric coercion proves the same
view. The existing tcRetainedCoercion supplies the type view. Check the
original declaration's runtime scope first. Keep its Id and stored type.
First probe the existing abstractNativeIndices grammar with a fresh immutable
template. Unsupported input casts or coercion types keep the current type.
This optional selection must not turn an unavailable view into a diagnostic.
After a successful probe, tcRetainedCoercion still reports any real error.
The ordinary argument checker must still prove the domain conversion and cast
the actual occurrence. The view is a type-argument choice, not a replacement
argument or equality evidence. Final result subsumption still runs.

A generated retained binder can be absent from the source name environment
until desugaring installs its AsPat. Its exact prototype occurs free in the
predicate of an actual lexical Given. tcRetainedInstScope can use that
prototype only when no normal binding exists. It must not obtain scope from
the candidate type itself. A present normal binding remains authoritative,
even when its metadata or multiplicity fails the check. CoVars are evidence,
not runtime values; ordinary coercion scope checking remains responsible for
them. See Note [Native index bindings during typechecking].

Keep free-variable prototypes separately for each Given predicate. Require a
nonempty set of matching prototypes and check all their metadata. The standard
FV list deduplicates within one predicate. Therefore also use the existing
deep anyFreeVarsOfType traversal to reject every incompatible occurrence,
including occurrences in variable types. That traversal does not deduplicate
occurrences. This check assumes canonical finite typechecked predicates and
Core's NoTypeShadowing invariant. It does not validate arbitrary cyclic or
malformed type graphs. See Note [Computing deep free variables] in
GHC.Core.TyCo.FVs.
-}

-- | Check for a dependent dictionary before the first written value argument.
hasScopedDictionaryPrefix :: TcSigmaType -> Bool
hasScopedDictionaryPrefix sigma
  | (_, _, rho) <- tcRetainedInstTelescope sigma
  , DepDictTy{} <- coreFullView rho = True
  | otherwise = False

-- | Collect the predicates whose evidence occurs in the body of this prefix.
scopedDictionaryContext :: Type -> ([PredType], Type)
scopedDictionaryContext ty = case coreFullView ty of
  DepDictTy dict body ->
    let (preds, rho) = scopedDictionaryContext body
    in (varType dict : preds, rho)
  FunTy af _ _ _ body | isInvisibleFunArg af -> scopedDictionaryContext body
  _ -> ([], ty)

-- | Collect declared domains for the written value-argument prefix.
-- Mark visible retained domains with True. Skip dependent argument domains.
-- Keep the set of crossed retained binders with the result.
-- See Note [Declared retained-domain choices].
retainedDeclaredDomains :: Int -> Type -> ([Maybe (Bool, Type)], Maybe (IdSet, Type), Type)
retainedDeclaredDomains supplied = go supplied emptyVarSet
  where
    go 0 bound result = result_view bound result
    go remaining bound ty
      | Just ty' <- coreView ty = go remaining bound ty'
    go remaining bound (DepDictTy dict body)
      = let (domains, result, masked) = go remaining (extendVarSet bound dict) body
        in (domains, result, DepDictTy dict masked)
    go _ _ ty@(DepFunTy DepFunInvisible _ _) = ([], Nothing, ty)
    go remaining bound (DepFunTy DepFunVisible binder body)
      = let domain = independent bound True (varType binder)
            (rest, result, masked) = go (remaining - 1) (extendVarSet bound binder) body
        in (domain : rest, result, DepFunTy DepFunVisible binder masked)
    go remaining bound ty@(FunTy af _ mult arg res)
      | isVisibleFunArg af
      = let (rest, result, masked) = go (remaining - 1) bound res
            weight | Just _ <- getTyVar_maybe mult = ManyTy
                   | otherwise = mult
        in (independent bound False arg : rest, result, ty { ft_mult = weight, ft_res = masked })
      | otherwise
      = let (rest, result, masked) = go remaining bound res
        in (rest, result, ty { ft_res = masked })
    go _ bound result = result_view bound result

    result_view bound result = ([], Just (bound, result), result)

    independent bound retained domain
      | mentions bound domain = Nothing
      | otherwise = Just (retained, domain)
    mentions bound = anyFreeVarsOfType (`elemVarSet` bound)

{- Note [Multiplicity of a supplied native argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In indexGeneric [v] pos, v can have type Vec Int n for a runtime n.
The element type must be selected before introducing ordinary metavariables.
The position arrow also quantifies a multiplicity p. Reading the type of pos
does not determine how often the function uses it.

Keep the known native element type. Give p an ordinary fresh multiplicity
variable when it occurs only as the weight of supplied value arrows. The
third result of retainedDeclaredDomains masks these weights to check that
condition. Check the original function type and its argument usage with
the fresh variable. Neither the result nor a cached argument-type view
can expose a variable introduced by this rule.
-}

{- Note [Declared retained-domain choices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For genericVector :: forall a. foreach (xs :: [a]) -> Vec a (Count xs),
a declared argument xs0 :: [Vec Int n] determines a := Vec Int n. Select it
before ordinary instantiation creates a meta that cannot be filled with n
inside the retained application implication. Read the declaration without
checking the occurrence or consuming its usage. The complete immutable IALL
plan and its existing kind, exact-domain, Quick Look and lexical-scope checks
remain authoritative.

A built-in list literal such as [vector] can also supply [Vec Int n] from a
field's declared type. Read the first bare or parenthesized variable field
whose declared view is native, monomorphic and lifted. Form its list type
without inferring the literal or checking any occurrence. Skip empty lists,
fields without such a declaration, polymorphic fields and checking-only forms.
Applications, splices and nested literals cannot supply this advice. The
ordinary argument checker must still check the literal and every element at
the selected type and record their usage. This advice only selects complete
outer erased arguments; it adds no type equality or mutable meta assignment.

Include a retained binder's own domain, which is outside that binder's scope.
Skip later domains that mention a crossed runtime binder. An exact match
there would need the actual runtime argument. Keep a skipped domain's
position for source argument alignment. Original argument checking still resolves every actual index and
checks the substituted domain. No runtime Id or metadata-only index is invented.

Stop after the written value-argument prefix. A remaining retained telescope
can be the result value. For example, withScoped Refl f can return ScopedQ m
while f has type ScopedQ n. The expected result selects index := m, and the
declared f selects value := ScopedQ n. Keep that complete candidate. The
ordinary Refl and result checks must diagnose the different scoped indices.
Do not count the returned ScopedQ telescope as more supplied arguments.

Run the declaration selector at the NoQL prepass and after VTA too. Reading
types and zonking filled assignments allocates no native meta. Failed or
incomplete selection uses the existing cached inference path. The original
signature, evidence order, and final result subsumption remain unchanged.
-}

-- | Match kinds and types with the same outer erased binder substitution.
-- See Note [Declared native argument choices].
matchRetainedAppType :: [TyVar] -> Subst.Subst -> Type -> Type -> Maybe Subst.Subst
matchRetainedAppType tvs subst formal actual
  = do { let bind = bindOuterTvs tvs
       ; subst <- tcMatchRetainedTyX_BM bind subst (typeKind formal) (typeKind actual)
       ; tcMatchRetainedTyX_BM bind subst formal actual }

{- Note [Declared native argument choices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In withQ Refl f, the declared monomorphic type of f can determine value := QPi.
Reading this declaration does not check the occurrence or consume its usage.
Do not call tcInferId: it emits usage. Global lookup may load an interface.
Visible retained domains can also use constructor fields. See
Note [Erased parameters of a retained constructor argument]. Look through
parentheses without expanding source syntax. A built-in list literal can
use a declared native variable field as element advice. See Note [Declared retained-domain choices].

A retained operand bound by Sigma.Pair can have the generated family type
of its payload. For a known algebraic domain, reuse matchSumPayload to find
the nominal result of that generated family equation. The family arguments
supply no injectivity evidence. The result can determine erased parameters from
Vec a size while size is still in scope. Keep the original declared type
for argument checking and for its nominal cast.

Match known types against declared independent visible argument domains.
Include retained domains without choosing their runtime arguments. See
Note [Declared retained-domain choices]. Match kinds before types and bind
only the outer erased variables. Require every choice,
correct substituted kinds, exact matched domains and valid runtime scope.
Reject free Quick Look variables, including those in kinds and coercions.
Use an expected seed only after collecting all written values, with an
independent remaining result. Retry a failed seeded match
independently from the empty substitution. Conflicts retain the guarded path.

Return the choices as an IALL plan. Keep the original signature and checked
VTA prefix. IALL consumes each forall/context group in its original order.
Do not infer arguments or create a cache before installing this plan. Refl
then checks at its actual expected proof type. All arguments retain ordinary
subsumption and scaled usage; all constraints and final result checks remain.
A linear value may supply a type. Only runtime indices inside that type must
meet the unrestricted native-index scope rule. No native meta is assigned.

A checked runtime argument can expose another erased prefix. For example,
in keepAfter b m the remaining type first binds a, then m :: Maybe a.
At IALL, reuse the declaration check on m before instantiating that prefix.
Use only the next retained operand. A later written type argument belongs
to a different position. Written type arguments at this position still use
the usual visible type application rule.

Quick Look's h-var reads a declared type and its argument advice precedes full
checking (Serrano et al., ICFP 2020; GHC Proposal #274). Agda checks each domain
under the substituted telescope and retains the checked prefix (Norell, 2007).
This route uses complete immutable choices, not mutable instantiation rules.
-}

{- Note [Ground retained applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ImpredicativeTypes enables Quick Look even when a checked application does
not need impredicative inference. A known worker can have all its native
parameters fixed by explicit type arguments. A proof constructor can instead
have all its parameters fixed by a complete native expected result.

Check only the leading type, pragma and wrapper prefix with the ordinary
IALL and ITYARG rules. Keep their original Quick Look allocations. Stop before
IRESULT and before any value argument. Resume with the original head and the
checked prefix in telescope order. Do not check a type argument twice. Keep
this preparation inside the application's evidence scope.

Select ordinary checking only when no erased choices remain, the native
value types are supported, and deep zonking finds no free Quick Look variables
in the residual type, expected type or saved prefix metadata. Ground theta
remains an obligation for IALL and instCall. Outer rigid variables and runtime
indices are permitted. Ordinary metavariables keep their existing rules.

A written type application can supply a ranked retained callback to an
algebraic constructor. In Just @Callback reflect, ITYARG has already
checked Callback. The residual type Callback -> Maybe Callback declares
the field and the result. Keep this type argument in the checked prefix
and use ordinary checking for that field. The callback keeps its binders,
constraints and scope. Quick Look does not assign an instantiation
variable to Callback.

An expected result can determine the same choice for Just reflect. Match
the ordinary constructor result Maybe alpha against Maybe Callback.
With ImpredicativeTypes, this match can use a ranked retained type argument
from an algebraic result. Require the same exact result, complete choices,
kinds and scope as a written application. Check the field against Callback
afterwards. The expected result supplies the choice before argument checking.

Alternatively, use the existing immutable expected-result selection, including
its algebraic candidate fallback. Keep the actual substituted signature. Append
its type applications after the explicit prefix. This lets a nested Refl use
its own complete expected proof type. It does not assume that the proof holds;
argument checking, nominal entailment and final subsumption still run.

A saturated nominal equality continuation such as gcastWith or rewrite has an
implication domain ((a ~ b) => r) -> r rather than a ground value type. When
no VTA prefix is written, the expected result is a tau type and either the
expected result or an inferable proof argument supplies a native anchor, switch
from Quick Look to NoQL so tcRetainedArgInstCachedWith can infer the proof and
check the continuation under its Given. Do the same for a required-type-prefix
call such as cong Succ (plusSucc m n) whose post-VDQ body has tau domains and a
native anchor, so the post-VTA argument cache can instantiate the remaining
invisible binders. Likewise, quickLookArg1 skips Quick Look on a saturated
DepFunTy variable or constructor head when either no invisible binders remain
or the saturated result shape has no retained binders or indices.

A failed selection keeps the checked prefix and resumes Quick Look. It never
infers a value argument or creates an argument cache. Public tcInstFun callers
retain their native Quick Look guards. See Note [Retained types and Quick Look]
in GHC.Tc.Utils.Unify and Note [Retained application evidence]. Declared arguments can supply a
complete IALL plan; see Note [Declared native argument choices].
The IALL/ITYARG rules follow
Serrano et al., A quick look at impredicativity, ICFP 2020; see
Note [Quick Look overview]. This checked route does not extend those rules
with mutable native instantiation. Agda retains a checked prefix and substitutes
arguments in telescope order (Norell, 2007; Eisenberg, 2016, §6).
-}

-- | Use ordinary checking for a known native value or callback.
-- See Note [Known native variables with ImpredicativeTypes].
tcRetainedKnownHeadQL :: Bool -> QLFlag -> [HsExprArg 'TcpRn] -> ExpRhoType
                     -> HsExpr GhcTc -> TcSigmaType -> TcM QLFlag
tcRetainedKnownHeadQL True DoQL [] exp_res_ty HsVar{} actual
  = do { actual <- liftZonkM (zonkTcType actual)
       ; expected <- readExpType_maybe exp_res_ty
       ; expected <- mapM (liftZonkM . zonkTcType) expected
       ; let tys = actual : maybeToList expected
             known_value
               = not (isSigmaTy actual || isForAllTy actual)
                   && any (hasNativeDependentExpr . Core.Type) tys
             known_callback
               = isRankedRetainedCallback actual
                   && maybe False isRetainedCallbackRho expected
       ; if (known_value || known_callback) && all noQuickLookVars tys
         then if known_value
              then return NoQL
              else do { scoped <- tcRetainedInstScope tys
                      ; return (if scoped then NoQL else DoQL) }
         else return DoQL }
tcRetainedKnownHeadQL _ do_ql _ _ _ _ = return do_ql

-- | Accept a complete call telescope with declared higher-rank inputs.
-- See Note [Known native variables with ImpredicativeTypes].
isGroundRetainedCallType :: Type -> Bool
isGroundRetainedCallType ty
  | FunTy flag ma mult arg res <- coreFullView ty
  , isVisibleFunArg flag
  = isTauTy ma && isTauTy mult
    && (isRetainedValueType arg || isRankedRetainedArgument arg)
    && isGroundRetainedCallType res
  | otherwise
  = isRetainedValueType ty || isRankedRetainedContainer ty

-- | Recognize an algebraic result with a ranked retained type argument.
-- See Note [Ground retained applications].
isRankedRetainedContainer :: Type -> Bool
isRankedRetainedContainer ty
  | TyConApp tc args <- coreFullView ty
  , isAlgTyCon tc, not (isClassTyCon tc)
  , length args == tyConArity tc
  , typeKind ty `eqType` liftedTypeKind
  = any isRankedRetainedCallback args
    && all (\arg -> isRetainedValueType arg || isRankedRetainedCallback arg) args
  | otherwise
  = False

isRankedRetainedCallback :: Type -> Bool
isRankedRetainedCallback ty
  = case tcRetainedInstTelescope ty of
      (_ : _, _, rho) -> isRetainedCallbackRho rho
      _              -> False

isRetainedCallbackRho :: Type -> Bool
isRetainedCallbackRho ty
  | DepFunTy{} <- coreFullView ty = isRetainedValueType ty
  | otherwise                    = False

-- | Recognize a declared retained callback or a container for it.
isRankedRetainedArgument :: Type -> Bool
isRankedRetainedArgument ty = isRankedRetainedCallback ty || isRankedRetainedContainer ty

-- | Defer a nested call with a fixed ordinary result and a ranked retained input.
-- See Note [Quick Look and ordinary argument checking].
isGroundRankedRetainedCall :: Int -> Type -> Bool
isGroundRankedRetainedCall arity ty
  | has_ranked_input arity ty
  , Just result <- tcRetainedOrdinaryResult arity ty
  = supportsQuickLookType result && noQuickLookVars ty
      && isGroundRetainedCallType ty
  | otherwise = False
  where
    has_ranked_input n ty
      | n > 0, Just (arg, res) <- splitVisibleFunTy_maybe ty
      = isRankedRetainedArgument arg || has_ranked_input (n-1) res
      | otherwise = False

{- Note [Known native variables with ImpredicativeTypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ImpredicativeTypes permits a qualified retained Pi in a checked type argument.
It also enables Quick Look for every application, including a bare variable.
For fromGiven f = f under a Given value ~ (foreach (n :: Nat) ->
Eq Int => Vec Int n), the RHS has no argument or invisible binder to
instantiate. The checking context supplies its expected type. This check
needs conversion, not Quick Look instantiation.

Use NoQL for an actual variable with no arguments and no leading forall or
context. Require native syntax in the actual or an available expected type.
Zonk these types and reject this route if either has a free QL instantiation
variable. Use the native-aware deep free-variable traversal; foldQLInstVars
does not support retained binders or indices. Keep final result subsumption
and native evidence capture. Do not eta-expand the variable.

A case scrutinee uses inference mode even when its variable has a complete
type. Return that existing type through the ordinary InferResult path. Read
an available expected type without allocating an inference cell. Preserve
InferResult promotion, level, runtime-representation and branch checks.

A complete call telescope can also declare a polymorphic retained parameter.
If the head has no type variables to instantiate, use the ordinary argument
checker for that call. It pushes the parameter's type into its operand and
skolemises it. When that operand is a bare name with a retained telescope,
instantiate it predicatively against the resulting retained type. Check its
scope and keep the complete subsumption check and dictionary obligations.

The call-telescope check permits a retained polytype or its algebraic
container at a declared argument. For example, consume can supply
Maybe Callback to its operand Just reflect. The constructor then uses
the expected result to choose Callback. See Note [Ground retained applications]
and GHC Proposal #378 (§4.7).
-}

-- | Bind native dictionary evidence at the application that needs it.
tcRetainedAppEvidence :: Bool -> TcM (HsExpr GhcTc) -> TcM (HsExpr GhcTc)
tcRetainedAppEvidence enabled thing_inside
  | not enabled = thing_inside
  | otherwise
  = do { (expr, wanted) <- captureConstraints thing_inside
       ; needs_ev_implic <- do
           let simple_cts = bagToList (wc_simple wanted)
           if null simple_cts
             then return False
             else do
               simple_preds <- mapM (zonkAppTypeIfNeeded . ctPred) simple_cts
               if any (hasNativeDependentExpr . Core.Type) simple_preds
                 then return True
                 else do
                   let dict_preds = filter (not . isEqPred) simple_preds
                   if any (anyFreeVarsOfType isMetaTyVar) dict_preds
                     then wc_has_native wanted
                     else return False
       ; if needs_ev_implic
         then do { tclvl <- getTcLevel
                 ; (implic, ev_binds) <- buildImplicationFor tclvl RetainedAppSkol [] [] wanted
                 ; emitImplications implic
                 ; return (mkHsWrap (mkWpLet ev_binds) expr) }
         else do { emitConstraints wanted
                 ; return expr } }
  where
    ct_has_native ct = do
      pred <- zonkAppTypeIfNeeded (ctPred ct)
      return (hasNativeDependentExpr (Core.Type pred))

    wc_has_native wc = do
      s_ok <- anyM ct_has_native (bagToList (wc_simple wc))
      if s_ok
        then return True
        else anyM (wc_has_native . ic_wanted) (bagToList (wc_impl wc))

{- Note [Retained application evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Immutable instantiation can produce Show (Vec Int n), where n is a runtime Id.
The dictionary expression can contain n in its type arguments. Its binding must
therefore remain below the runtime lambda that binds n. Checking the type
arguments at the call site does not establish this evidence scope.

Capture constraint generation for the instantiated application. Inspect the
actual simple Wanteds after instantiation, not the initial theta. A visible type
argument or an earlier value argument can expose a later forall whose immutable
instantiation introduces a native predicate. Nested implications already own
their evidence and do not require another wrapper.

For a native Wanted, build an
implication with its own evidence bindings and put WpLet around the application.
Force this implication even with no type-variable skolems or Givens. The normal
checkConstraints fast path would omit it. The solver inherits enclosing Givens
and uses the ordinary instance and equality rules. Do not use StaticFormSkol,
which deliberately removes enclosing Givens. RetainedAppSkol records the actual
origin for diagnostics. Residual native predicates must also remain inside this
implication; see Note [Retained residual constraints] in GHC.Tc.Types.Constraint.
Keep the current TcLevel. There are no new type-variable binders. Raising the
level would force an inferred native result through promoteTcType and a mutable
meta-variable. Applications without native Wanteds emit their original constraints.

This is a context boundary for evidence, not a new equality rule or a way to fill
native meta-variables. Keep the original theta and the final subsumption checks.
Agda inserts instance arguments in the current context and postpones their search
during application checking (Norell, 2007). The GHC implementation uses delayed
FC evidence and an expression-local implication (Vytiniotis et al., JFP 2011).
-}

finishApp :: TcM ()                 -- Saved constraints and result Quick Look
          -> QLFlag
          -> HsExpr GhcRn           -- The whole application
          -> SrcSpan                -- Span for the /head/ of the application
          -> HsExpr GhcRn           -- The head of the application (renamed)
          -> HsExpr GhcTc           -- The head of the application (typechecked)
          -> [HsExprArg 'TcpInst]   -- Arguments
          -> TcRhoType -> ExpRhoType
          -> TcM (HsExpr GhcTc)
-- Instantiation and Quick Look are done.  Now do typechecking,
-- of both the result type and the arguments, before rebuilding.
finishApp = finishAppBody [] [] []

-- | Check the instantiated result and arguments, then rebuild the call.
finishAppBody :: [TcTyVar] -> [Name] -> RetainedArgCache -> TcM () -> QLFlag
              -> HsExpr GhcRn -> SrcSpan -> HsExpr GhcRn -> HsExpr GhcTc
              -> [HsExprArg 'TcpInst] -> TcRhoType -> ExpRhoType
              -> TcM (HsExpr GhcTc)
finishAppBody retained_holes fields cache prepare do_ql rn_expr fun_loc rn_fun tc_fun inst_args
              app_res_rho exp_res_ty
  = withLocalSynthFunIds $
    do { prepare
       ; case do_ql of
           NoQL -> return ()
           DoQL -> do { rejectNativeQuickLook app_res_rho
                      ; setSrcSpan fun_loc $ rejectNativeQLInstArgs inst_args }

       ; (app_res_rho, scope_wrap) <- case fields of
           [] -> return (app_res_rho, idHsWrapper)
           _ -> do { Reduction co reduced <-
                       tcCloseRuntimeScope fields (exprCtOrigin rn_expr) app_res_rho
                   ; return (reduced, mkWpCastN co) }

         -- Step 4.1: monomorphise un-unified QuickLook instantiation variables
         -- Step 4.2: zonk app_res_rho to expose the polymorphism hidden under
         --           instantiation variables in `app_res_rho`
       ; app_res_rho <- case do_ql of
           NoQL -> return app_res_rho
           DoQL -> liftZonkM $
                   do { qlMonoInstArgs inst_args
                      ; zonkTcType app_res_rho }

         -- Step 5: subsumption check against the expected type
         -- See Note [Unify with expected type before typechecking arguments]
         -- and Note [Typed holes in retained applications].
       ; (app_res_rho, inst_args, mb_hole_wrap) <-
           tcResolveRetainedHoleResult rn_expr do_ql retained_holes inst_args
                                       app_res_rho exp_res_ty
       ; (branch_co, res_wrap) <- case mb_hole_wrap of
           Just hole_wrap -> return (mkNomReflCo app_res_rho, hole_wrap)
           Nothing -> do
             { b_co <- case (do_ql, exp_res_ty) of
                 (NoQL, Check expected)
                   | isTauTy expected, not (anyFreeVarsOfType (const True) expected)
                   -> tcRetainedResultCoercion inst_args app_res_rho
                 _ -> return (mkNomReflCo app_res_rho)
             ; r_wrap <- checkResultTy rn_expr (tc_fun, fun_loc) inst_args
                                       (coercionRKind b_co) exp_res_ty
             ; return (b_co, r_wrap) }

         -- Step 6: Typecheck the arguments
       ; case do_ql of
           NoQL | null cache, classHasUnmatchableReceiver tc_fun
                , all valueOrWrapper inst_args ->
             void (seedClassReceiver NoQL (rn_fun, fun_loc) (valueDomains inst_args))
           _ -> return ()
       ; tc_args <- tcValArgsWithCache cache do_ql (rn_fun, fun_loc) inst_args

         -- Step 7: Horrible special case for tagToEnum#.
       ; res_expr <- if isTagToEnum tc_fun
                     then tcTagToEnum tc_fun tc_args app_res_rho
                     else return (rebuildHsApps tc_fun tc_args)

       ; traceTc "End tcApp }" (ppr tc_fun)
       ; return (mkHsWrap (res_wrap <.> mkWpCastN branch_co <.> scope_wrap) res_expr) }

-- | Refine the result of applying a retained function to the branch index.
-- See Note [Source retained case refinement] in GHC.Tc.Gen.Match.
tcRetainedResultCoercion :: [HsExprArg 'TcpInst] -> Type -> TcM CoercionN
tcRetainedResultCoercion args ty
  = do { indices <- catMaybes <$> mapM index_arg args
       ; if null indices then return (mkNomReflCo ty)
         else tcRetainedTypeCoercion (`elem` indices) ty }
  where
    index_arg (ERetainedArg { ea_retained_vis = DepFunVisible, ea_arg = arg })
      | HsVar _ name <- unLoc (stripParensLHsExpr arg)
      = tcLookupIdMaybe (unLocWithUserRdr name)
    index_arg _ = return Nothing

-- | Instantiate call constraints, transporting predicates that mention a
-- preceding retained motive index along the active constructor Given so class
-- constraints after @foreach@ solve against the branch's refined Givens.
instRetainedCall :: QLFlag -> [HsExprArg 'TcpInst] -> CtOrigin -> [TcType] -> TcThetaType -> TcM HsWrapper
instRetainedCall NoQL rev_acc orig inst_tys theta@(_:_)
  | any is_retained rev_acc
  = do { wraps <- forM theta $ \pred -> do
           { co <- tcRetainedResultCoercion rev_acc pred
           ; w  <- instCallConstraints orig [coercionRKind co]
           ; let sym_sub = mkSymCo (mkSubCo co)
           ; pure $ case (isReflCo co, w) of
               (False, WpEvApp (EvExpr (Core.Coercion co_ev))) | isEqPred pred ->
                 WpEvApp (evCoercion (mkCoCast co_ev sym_sub))
               (False, WpEvApp (EvExpr e)) | not (isEqPred pred) ->
                 WpEvApp (evCast e sym_sub)
               _ -> w }
       ; pure (foldl' (flip (<.>)) (mkWpTyApps inst_tys) wraps) }
  where
    is_retained ERetainedArg{} = True
    is_retained _              = False
instRetainedCall _ _ orig inst_tys theta = instCall orig inst_tys theta

-- | Identify class selectors with an unmatchable-capable class parameter.
classHasUnmatchableReceiver :: HsExpr GhcTc -> Bool
classHasUnmatchableReceiver (HsVar _ (L _ selector))
  | Just cls <- isClassOpId_maybe selector
  = any unmatchable_parameter (classTyVars cls)
  where
    unmatchable_parameter tv
      | Just (ma, _, _) <- tcSplitFunTy_maybe (tyVarKind tv)
      = isTyVarTy ma || ma `eqType` unmatchableDataConTy
      | otherwise = False
classHasUnmatchableReceiver _ = False

-- | Recognize ordinary arguments for a declaration-only receiver hint.
valueOrWrapper :: HsExprArg 'TcpInst -> Bool
valueOrWrapper EValArg{} = True
valueOrWrapper EWrap{} = True
valueOrWrapper ETypeArg{} = True
valueOrWrapper _ = False

-- | Preserve positions of checked arguments after type applications.
valueDomains :: [HsExprArg 'TcpInst] -> [(Int, LHsExpr GhcRn, Type)]
valueDomains = go 1
  where
    go _ [] = []
    go pos (EValArg{ea_arg = arg, ea_arg_ty = Scaled _ expected} : rest) =
      (pos, arg, expected) : go (pos + 1) rest
    go pos (ETypeArg{ea_loc_span = l} : rest)
      | not (isGeneratedSrcSpan (locA l)) = go (pos + 1) rest
    go pos (_ : rest) = go pos rest

-- | Learn a closed unmatchable class receiver before checking its callback.
-- See Note [Unmatchable class receivers].
seedClassReceiver :: QLFlag -> (HsExpr GhcRn, SrcSpan)
                  -> [(Int, LHsExpr GhcRn, Type)] -> TcM Bool
seedClassReceiver do_ql rn_head = go []
  where
    go _ [] = return False
    go callbacks ((pos, arg, expected) : rest) =
      do { selected <- if null callbacks
                       then return False
                       else addArgCtxt pos rn_head arg $
                              seed_receiver callbacks arg expected
         ; callbacks' <- if isJust (tcSplitFunTy_maybe expected)
                         then do { ty <- liftZonkM (zonkTcType expected)
                                 ; return (ty : callbacks) }
                         else return callbacks
         ; selected_rest <- go callbacks' rest
         ; return (selected || selected_rest) }

    seed_receiver callbacks arg expected = fmap isJust $ runMaybeT $ do
      (tc, indices) <- MaybeT (tcSplitTyConApp_maybe <$> liftZonkM (zonkTcType expected))
      guard (isAlgTyCon tc)
      let shared f = any (elemVarSet f . tyCoVarsOfType) callbacks
      [(f, kind)] <- return
        [ (f, kind) | TyVarTy f <- indices, eligible f
                    , Just kind@(ma, _, _) <- [tcSplitFunTy_maybe (tyVarKind f)]
                    , isTyVarTy ma || ma `eqType` unmatchableDataConTy
                    , shared f ]
      open <- liftMaybeT (isUnfilledMetaTyVar f)
      guard open
      actual <- declared_receiver 0 arg
      (outer, outer_args) <- liftMaybe (tcSplitTyConApp_maybe actual)
      guard (any family_prefix outer_args)
      (actual_tc, actual_indices) <- if outer == tc
        then return (outer, outer_args)
        else do
          guard (isTypeFamilyTyCon outer)
          envs <- liftMaybeT tcGetFamInstEnvs
          reduction <- liftMaybe (reduceTyFamApp_maybe envs Nominal outer outer_args)
          liftMaybe (tcSplitTyConApp_maybe (reductionReducedType reduction))
      guard (tc == actual_tc)
      [target] <- return
        [ t | (TyVarTy v, t) <- zip indices actual_indices, v == f, family_prefix t ]
      guard (not (hasNativeDependentExpr (Core.Type target)))
      (target_ma, Scaled target_mult target_domain, target_result) <-
        liftMaybe (tcSplitFunTy_maybe (typeKind target))
      let (ma, Scaled mult domain, result) = kind
      guard (target_ma `eqType` unmatchableDataConTy && mult `eqType` target_mult
             && domain `eqType` target_domain && result `eqType` target_result)
      compatible <- case getTyVar_maybe ma of
        Just tv | eligible tv -> liftMaybeT (isUnfilledMetaTyVar tv)
        _ -> return (ma `eqType` target_ma)
      guard compatible
      liftMaybeT $ case do_ql of
        NoQL -> void (unifyType Nothing target (mkTyVarTy f))
        DoQL -> qlUnify target (mkTyVarTy f)

    declared_receiver n (L _ (HsPar _ fun)) = declared_receiver n fun
    declared_receiver n (L _ (HsApp _ fun _)) = declared_receiver (n + 1) fun
    declared_receiver n arg = do
      (_, L _ (HsVar _ lname)) <- liftMaybe (retainedNamedCallback arg)
      ident <- MaybeT (tcLookupIdMaybe (unLocWithUserRdr lname))
      let ty = varType ident
      guard (isTauTy ty && noFreeVarsOfType ty)
      liftMaybe (tcRetainedOrdinaryResult n ty)

    eligible tv = isTcTyVar tv && isMetaTyVar tv &&
      case do_ql of
        NoQL -> not (isQLInstTyVar tv)
        DoQL -> isQLInstTyVar tv

    liftMaybe :: Maybe a -> MaybeT TcM a
    liftMaybe = MaybeT . pure

    family_prefix t
      | Just (tc, args) <- tcSplitTyConApp_maybe t = isUnsaturatedUserFamily tc args
      | otherwise = False

{- Note [Unmatchable class receivers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider mapIndexed (fmap not) familyInput. The class method checks its
callback against f Bool -> f Bool. If f is unknown, the ordinary check
can select a matchable f before it checks familyInput :: Batch (Pick True).

Check only classes with a parameter that can have an unmatchable arrow
kind. Use the receiver's closed declared type when it contains a family
prefix. For makeReceiver (), peel the visible arrows of the monomorphic
producer's declared type. This reads no producer operands and permits no
type instantiation or dependent arrows. Look through at most one nominal
family equation to expose the same
algebraic constructor. Avoid recursive reduction in this optional step.
An earlier callback must use the same receiver index. Unify that index
with the partial unmatchable family. The ordinary receiver check supplies
the family coercion and checks the other indices. Its context can contain
witnesses from opening earlier implicit operands. Keep checking terms and
recording usage in source order. For Quick Look, use the same hint on
the already instantiated visible telescope before looking at the callback.
Fill only its instantiation variables. Zonk the callback type after the
hint so an application of that index takes the family path in qlUnify.
The callback already had a visible arrow, so its outer shape and the
guardedness test stay the same. Continue with the original Quick Look
order and cache.
-}

quickLookResultType :: TcRhoType -> TcRhoType -> TcM ()
-- This function implements the shaded bit of rule APP-Downarrow in
-- Fig 5 of the QL paper: "A quick look at impredicativity" (ICFP'20),
-- extended to peel expected implicit existential binders before Step 4
-- monomorphises instantiation variables (Eisenberg et al., ICFP 2021, Fig. 10).
-- See Note [QuickLook: arguments before result]
quickLookResultType app_res_rho exp_res_rho = do
  exp_view <- unwrapMetaView exp_res_rho
  case exp_view of
    ExistsTy ImplicitExists _ _ -> do
      payload <- liftZonkM (zonkTcType app_res_rho)
      schema  <- liftZonkM (zonkTcType exp_view)
      ds_flag <- getDeepSubsumptionFlag
      case coreFullView payload of
        TyVarTy tv | isTcTyVar tv && isQLInstTyVar tv ->
          qlUnify app_res_rho exp_res_rho
        _ -> do
          let is_payload = case coreFullView payload of
                TyConApp tc _ -> not (isTypeFamilyTyCon tc)
                AppTy {}      -> True
                FunTy {}      -> case ds_flag of { Deep {} -> False; Shallow -> True }
                _             -> False
          when (is_payload && supportsQuickLookType schema
                && supportsQuickLookType payload && noQuickLookVars schema) $ do
            (ws, inst_body) <- peel_ql schema
            qlUnify payload inst_body
            forM_ ws $ \w -> do
              rejectNativeQuickLook w
              liftZonkM (qlMonoTcType w)
    _ -> qlUnify app_res_rho exp_res_rho
  where
    peel_ql cur_schema
      | ExistsTy ImplicitExists binder body <- coreFullView cur_schema = do
          w <- newMetaTyVarTyAtLevel QLInstVar (tyVarKind binder)
          (ws, final_body) <- peel_ql (substTyWith [binder] [w] body)
          pure (w : ws, final_body)
      | otherwise = pure ([], cur_schema)

-- | Connect up the inferred type of an application with the expected type.
-- This is usually just a unification, but with deep subsumption there is more to do.
checkResultTy :: HsExpr GhcRn
              -> (HsExpr GhcTc, SrcSpan)  -- Head
              -> [HsExprArg p]            -- Arguments, just for error messages
              -> TcRhoType  -- Inferred type of the application; zonked to
                            --   expose foralls, but maybe not /deeply/ instantiated
              -> ExpRhoType -- Expected type; this is deeply skolemised
              -> TcM HsWrapper
checkResultTy rn_expr (tc_fun, fun_loc) inst_args app_res_rho res_ty
 = perhaps_add_res_ty_ctxt $
   tcSubTypeApp rn_expr tc_fun app_res_rho res_ty
  where
    -- perhaps_add_res_ty_ctxt: Inside an expansion, the addFunResCtxt stuff is
    -- more confusing than helpful because the function at the head isn't in
    -- the source program; it was added by the renamer.  See
    -- Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr
    perhaps_add_res_ty_ctxt thing_inside
      | isGeneratedSrcSpan fun_loc
      = thing_inside
      | otherwise
      = addFunResCtxt tc_fun inst_args app_res_rho res_ty $
        thing_inside

----------------
-- | Check cached arguments by subsumption and emit each saved usage once.
tcValArgsWithCache :: RetainedArgCache -> QLFlag
                   -> (HsExpr GhcRn, SrcSpan) -> [HsExprArg 'TcpInst]
                   -> TcM [HsExprArg 'TcpTc]
tcValArgsWithCache cache do_ql rn_head args = go do_ql 0 0 args
  where
    go :: QLFlag -> Int -> Int -> [HsExprArg TcpTcInst] -> TcM [HsExprArg 'TcpTc]
    go _ _ _ [] = return []
    go do_ql pos value_pos (arg : args)
      = do { arg'  <- case (arg, lookup value_pos' cache) of
               (EValArg { ea_loc_span = lspan, ea_arg = larg
                        , ea_arg_ty = Scaled mult expected },
                Just (typed_arg, actual, usage))
                 -> addArgCtxt pos' rn_head larg $ tcScalingUsage mult $
                    do { tcEmitBindingUsage usage
                       ; mb_st <- getAppUnpackState
                       ; demands <- case mb_st of
                           Just _  -> demandsPayload expected
                           Nothing -> pure False
                       ; actual_schema <- if demands
                                          then liftZonkM (zonkTcType actual)
                                          else pure actual
                       ; case (mb_st, coreFullView actual_schema) of
                           (Just cur_st, ExistsTy ImplicitExists _ _)
                             | demands ->
                               do { mb_key <- if isManyTy mult
                                              then openingKey larg typed_arg
                                              else pure Nothing
                                  ; payload_id <- unpackImplicitExistsAt cur_st (getLocA larg) mult
                                                    mb_key actual_schema typed_arg
                                  ; wrap <- tcSubTypeSigma (exprCtOrigin (unLoc larg))
                                              GenSigCtxt (varType payload_id) expected
                                  ; return (EValArg { ea_loc_span = lspan
                                                    , ea_arg_ty   = noExtField
                                                    , ea_arg      = L (getLoc larg)
                                                        (mkHsWrap wrap (mkHsVar (noLocA payload_id))) }) }
                           _ ->
                             do { wrap <- tcSubTypeSigma (exprCtOrigin (unLoc larg))
                                            GenSigCtxt actual expected
                                ; return (EValArg { ea_loc_span = lspan
                                                  , ea_arg_ty = noExtField
                                                  , ea_arg = fmap (mkHsWrap wrap) typed_arg }) } }
               _ -> tcValArg do_ql pos' rn_head arg
           ; args' <- go do_ql pos' value_pos' args
           ; return (arg' : args') }
      where
        !value_pos' | EValArg{} <- arg = value_pos + 1
                    | EValArgQL{} <- arg = value_pos + 1
                    | otherwise = value_pos
        -- Increment position if the argument is user-written type or value argument
        !pos' | user_visible arg = pos + 1
              | otherwise        = pos

    user_visible (EValArg{})                   = True
    user_visible (EValArgQL{})                 = True
    user_visible (ERetainedArg{})              = True
    user_visible (ETypeArg{ ea_loc_span = l }) = not (isGeneratedSrcSpan (locA l))
    user_visible _                             = False

type TcpTcInst = 'TcpInst

tcValArg :: QLFlag                  -- ^ Are we typechecking with Quick Look turned on?
         -> Int                     -- ^ Argument position (used only for error message generation)
         -> (HsExpr GhcRn, SrcSpan) -- ^ Head of the application chain (used only for error message generation)
         -> HsExprArg 'TcpInst      -- ^ Actual argument
         -> TcM (HsExprArg 'TcpTc)  -- ^ Resulting argument
tcValArg _ _ _ (EPrag l p)         = return (EPrag l (tcExprPrag p))
tcValArg _ _ _ (ETypeArg l hty source ty) = return (ETypeArg l hty source ty)
tcValArg _ _ _ (EWrap (EHsWrap w)) = return (EWrap (EHsWrap w))
tcValArg _ _ _ (EWrap ew)          = return (EWrap ew)

tcValArg _ _ _ (ERetainedArg vis loc (Scaled mult _) _ res_ty co (Just (checked, arg_ue)))
  = do { tcScalingUsage mult (tcEmitBindingUsage arg_ue)
       ; return (ERetainedArg vis loc noExtField
                   (maybe checked (`mkLHsWrapCo` checked) co) res_ty noExtField noExtField) }
tcValArg do_ql pos rn_head (ERetainedArg DepFunVisible loc (Scaled mult ty) arg res_ty co Nothing)
  = withAppUnpackState Nothing $
    do { let checked_ty = maybe ty coercionLKind co
       ; checked <- tcValArgExpr do_ql pos rn_head loc (Scaled mult checked_ty) arg
       ; return (ERetainedArg DepFunVisible loc noExtField
                   (maybe checked (`mkLHsWrapCo` checked) co) res_ty noExtField noExtField) }
tcValArg _ pos rn_head (ERetainedArg DepFunInvisible loc (Scaled mult ty) arg res_ty co Nothing)
  = withAppUnpackState Nothing $
    addArgCtxt pos rn_head arg $
    do { let checked_ty = maybe ty coercionLKind co
       ; checked <- tcScalingUsage mult $
                      tcWithRetainedHoleScope arg $
                      setSrcSpanA (getLoc arg) $
                      L (getLoc arg) <$> tcPolyExpr (unLoc arg) (mkCheckExpType checked_ty)
       ; return (ERetainedArg DepFunInvisible loc noExtField
                   (maybe checked (`mkLHsWrapCo` checked) co) res_ty noExtField noExtField) }

tcValArg do_ql pos rn_head (EValArg loc ty arg)
  = EValArg loc noExtField <$> tcValArgExpr do_ql pos rn_head loc ty arg

tcValArg _ pos rn_fun_head
         (EValArgQL { eaql_wanted   = wanted
                    , eaql_loc_span = lspan
                    , eaql_arg_ty   = sc_arg_ty
                    , eaql_larg     = larg@(L arg_loc rn_expr)
                    , eaql_rn_fun   = rn_fun
                    , eaql_tc_fun   = tc_arg_head@(tc_fun,arg_head_loc)
                    , eaql_fun_ue   = head_ue
                    , eaql_args     = inst_args
                    , eaql_encl     = arg_influences_enclosing_call
                    , eaql_res_rho  = app_res_rho })
  = addArgCtxt pos rn_fun_head larg $
    do { -- Expose QL results to tcSkolemise, as in EValArg case
         Scaled mult exp_arg_ty <- liftZonkM $ zonkScaledTcType sc_arg_ty
       ; traceTc "tcEValArgQL {" (vcat [ text "app_res_rho:" <+> ppr app_res_rho
                                       , text "exp_arg_ty:" <+> ppr exp_arg_ty
                                       , text "args:" <+> ppr inst_args
                                       , text "mult:" <+> ppr mult
                                       , text "app_lspan" <+> ppr lspan
                                       , text "fun_head" <+> ppr rn_fun_head
                                       , text "tc_arg_head" <+> ppr tc_arg_head])
       ; mb_st <- getAppUnpackState
       ; demands <- case mb_st of
           Just _  -> demandsPayload exp_arg_ty
           Nothing -> pure False
       ; actual_pkg_ty <- if demands
                          then liftZonkM (zonkTcType app_res_rho)
                          else pure app_res_rho
       ; case (mb_st, coreFullView actual_pkg_ty) of
           (Just cur_st, ExistsTy ImplicitExists _ _)
             | demands ->
               do { (pkg_expr, pkg_w) <- captureConstraints $
                      withAppUnpackState Nothing $
                      tcScalingUsage mult $ do
                        tcEmitBindingUsage head_ue
                        let prepare = emitConstraints wanted
                        may_unpack <- instArgsMayUnpackImplicitExists inst_args
                        runAppUnpackScopeIf may_unpack (mkCheckExpType actual_pkg_ty) $ \_ inner_exp ->
                          finishApp prepare DoQL rn_expr arg_head_loc rn_fun tc_fun
                                    inst_args app_res_rho inner_exp
                  ; recordPkgWanted cur_st pkg_w
                  ; let checked_pkg = L arg_loc pkg_expr
                  ; mb_key <- if isManyTy mult
                              then openingKey larg checked_pkg
                              else pure Nothing
                  ; payload_id <- unpackImplicitExistsAt cur_st (locA arg_loc) mult
                                    mb_key actual_pkg_ty checked_pkg
                  ; wrap <- tcSubTypeSigma (exprCtOrigin rn_expr) GenSigCtxt
                              (varType payload_id) exp_arg_ty
                  ; traceTc "tcEValArgQL (unpacked) }" (ppr payload_id)
                  ; return (EValArg { ea_loc_span = lspan
                                    , ea_arg      = L arg_loc (mkHsWrap wrap (mkHsVar (noLocA payload_id)))
                                    , ea_arg_ty   = noExtField }) }
           _ -> do
             { ds_flag <- getDeepSubsumptionFlag
               -- NB: whether to do deep /skolemisation/ is independent of data constructors
             ; let is_poly_arg = isSigmaTy exp_arg_ty
                   child_st = case mb_st of
                     Just cur_st | demands, not is_poly_arg ->
                       Just cur_st { aus_can_share = aus_can_share cur_st && isManyTy mult }
                     _ -> Nothing
             ; (wrap, arg')
                  <- withAppUnpackState child_st $
                     tcScalingUsage mult $
                     tcSkolemise ds_flag GenSigCtxt exp_arg_ty $ \ exp_arg_rho ->
                     do { -- Emit saved-up usages /under/ the tcScalingUsage.
                          -- See (QLA5) in Note [Quick Look at value arguments]
                          tcEmitBindingUsage head_ue
                        ; let prepare = do
                                -- See (QLA4) in Note [Quick Look at value arguments].
                                emitConstraints wanted
                                unless arg_influences_enclosing_call $
                                  qlUnify app_res_rho exp_arg_rho
                              run_finish exp_t =
                                finishApp prepare DoQL rn_expr arg_head_loc rn_fun tc_fun
                                          inst_args app_res_rho exp_t
                        ; case child_st of
                            Just _  -> run_finish (mkCheckExpType exp_arg_rho)
                            Nothing -> do
                              may_unpack <- instArgsMayUnpackImplicitExists inst_args
                              runAppUnpackScopeIf may_unpack (mkCheckExpType exp_arg_rho) $ \_ inner_exp ->
                                run_finish inner_exp }

             ; traceTc "tcEValArgQL }" $
                 vcat [ text "app_res_rho:" <+> ppr app_res_rho ]

             ; return (EValArg { ea_loc_span = lspan
                               , ea_arg      = L arg_loc (mkHsWrap wrap arg')
                               , ea_arg_ty   = noExtField }) } }


--------------------
wantQuickLook :: HsExpr GhcRn -> TcM QLFlag
wantQuickLook (HsVar _ (L _ (WithUserRdr _ f)))
  | getUnique f `elem` quickLookKeys = return DoQL
wantQuickLook _                      = do { impred <- xoptM LangExt.ImpredicativeTypes
                                          ; if impred then return DoQL else return NoQL }

skipQuickLook :: HsExprLoc -> LHsExpr GhcRn -> Scaled TcRhoType
              -> TcM (HsExprArg 'TcpInst)
skipQuickLook loc larg arg_ty
  = return (EValArg { ea_loc_span = loc
                    , ea_arg      = larg
                    , ea_arg_ty   = arg_ty })

quickLookKeys :: [Unique]
-- See Note [Quick Look for particular Ids]
quickLookKeys = [dollarIdKey, leftSectionKey, rightSectionKey]

{- *********************************************************************
*                                                                      *
              Instantiating the call
*                                                                      *
********************************************************************* -}

tcInstFun :: QLFlag
          -> Bool   -- False <=> Instantiate only /top-level, inferred/ variables;
                    --           so may return a sigma-type
                    -- True  <=> Instantiate /top-level, invisible/ type variables;
                    --           always return a rho-type (but not a deep-rho type)
                    -- Generally speaking we pass in True; in Fig 5 of the paper
                    --    |-inst returns a rho-type
          -> (HsExpr GhcRn, SrcSpan)
          -> HsExpr GhcTc
          -> TcSigmaType -> [HsExprArg 'TcpRn]
          -> TcM ( [HsExprArg 'TcpInst]
                 , TcSigmaType )   -- Does not instantiate trailing invisible foralls
-- This crucial function implements the |-inst judgement in Fig 4,
-- plus the modification in Fig 5, of the QL paper:
-- "A quick look at impredicativity" (ICFP'20).
tcInstFun = tcInstFunWithExpected Nothing Nothing

-- | Check a written visible value at its argument type and multiplicity.
tcValArgExpr :: QLFlag -> Int -> (HsExpr GhcRn, SrcSpan) -> HsExprLoc
             -> Scaled TcSigmaTypeFRR -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc)
tcValArgExpr do_ql pos rn_fun_head lspan sc_arg_ty larg@(L arg_loc arg)
  = tcWithRetainedHoleScope larg $
    addArgCtxt pos rn_fun_head larg $
    do { -- Crucial step: expose QL results before checking exp_arg_ty
         -- See Note [Instantiation variables are short lived]
         Scaled mult exp_arg_ty <- case do_ql of
              DoQL -> liftZonkM $ zonkScaledTcType sc_arg_ty
              NoQL -> Scaled <$> zonkAppTypeIfNeeded (scaledMult sc_arg_ty)
                             <*> zonkAppTypeIfNeeded (scaledThing sc_arg_ty)
       ; traceTc "tcValArg {" $
         vcat [ text "arg lspan:" <+> ppr lspan
              , text "rn_head" <+> ppr rn_fun_head
              , text "sigma_type" <+> ppr (mkCheckExpType exp_arg_ty)
              , text "arg:" <+> ppr larg
              , text "arg_loc:" <+> ppr arg_loc
              ]
       ; mb_st <- getAppUnpackState
       ; demands <- case mb_st of
           Just _  -> demandsPayload exp_arg_ty
           Nothing -> pure False
       ; mb_pkg_plan <- if demands
                        then tcImplicitPkgOperand exp_arg_ty larg
                        else pure Nothing
       ; case (mb_st, mb_pkg_plan) of
           (Just cur_st, Just mb_known_schema) ->
             do { ((checked_pkg, actual_ty), pkg_w) <- captureConstraints $
                    withAppUnpackState Nothing $
                    tcScalingUsage mult $
                    case mb_known_schema of
                      Just schema -> (, schema) <$> tcCheckMonoExpr larg schema
                      Nothing     -> tcInferRho larg
                ; actual_schema <- liftZonkM (zonkTcType actual_ty)
                ; case coreFullView actual_schema of
                    ExistsTy ImplicitExists _ _ ->
                      do { recordPkgWanted cur_st pkg_w
                         ; mb_key <- if isManyTy mult
                                     then openingKey larg checked_pkg
                                     else pure Nothing
                         ; payload_id <- unpackImplicitExistsAt cur_st (locA arg_loc) mult
                                           mb_key actual_schema checked_pkg
                         ; wrap <- tcSubTypeSigma (exprCtOrigin arg) GenSigCtxt
                                     (varType payload_id) exp_arg_ty
                         ; return (L arg_loc (mkHsWrap wrap (mkHsVar (noLocA payload_id)))) }
                    _ -> do
                      { emitConstraints pkg_w
                      ; wrap <- tcSubTypeSigma (exprCtOrigin arg) GenSigCtxt actual_schema exp_arg_ty
                      ; return (fmap (mkHsWrap wrap) checked_pkg) } }
           _ -> do
             { let is_pure_app = case unLoc (stripParensLHsExpr larg) of
                     HsApp {}     -> True
                     HsAppType {} -> True
                     OpApp {}     -> True
                     _            -> False
                   child_st = case mb_st of
                     Just cur_st | demands, is_pure_app, not (isSigmaTy exp_arg_ty) ->
                       Just cur_st { aus_can_share = aus_can_share cur_st && isManyTy mult }
                     _ -> Nothing
             ; withAppUnpackState child_st $ do
                 arg' <- tcScalingUsage mult $
                   tcPolyExpr arg (mkCheckExpType exp_arg_ty)
                 traceTc "tcValArg" $ vcat [ ppr arg'
                                           , text "}" ]
                 return (L arg_loc arg') } }

-- | Instantiate arguments in telescope order, with optional immutable choices.
tcInstFunWithExpected :: Maybe TcRhoType -> RetainedInstPlan -> QLFlag -> Bool
                      -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
                      -> TcSigmaType -> [HsExprArg 'TcpRn]
                      -> TcM ([HsExprArg 'TcpInst], TcSigmaType)
tcInstFunWithExpected = tcInstFunWithPrefix quickLookArg False [] Nothing

-- | Resume instantiation after a checked prefix, or check that prefix only.
-- A prefix-only call stops before instantiating the result. Retained RULEs
-- may include value operands before a written type argument.
-- See Note [Ground retained applications].
tcInstFunWithPrefix :: QuickLookArg -> Bool -> [HsExprArg 'TcpInst]
                    -> Maybe (TcRef ([TcTyVar], RetainedArgCache))
                    -> Maybe TcRhoType -> RetainedInstPlan -> QLFlag -> Bool
                    -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
                    -> TcSigmaType -> [HsExprArg 'TcpRn]
                    -> TcM ([HsExprArg 'TcpInst], TcSigmaType)
tcInstFunWithPrefix look_arg prefix_only prefix retained_holes_ref expected_rho inst_plan do_ql inst_final
                    rn_head@(_, fun_lspan) tc_fun fun_sigma rn_args
  = do { lvl <- getTcLevel
       ; traceTc "tcInstFun" (vcat [ text "tc_fun" <+> ppr tc_fun
                                   , text "rn_fun" <+> ppr rn_head
                                   , text "fun_sigma" <+> ppr fun_sigma
                                   , text "args:" <+> ppr rn_args
                                   , text "do_ql" <+> ppr do_ql
                                   , text "lvl:" <+> ppr lvl ])
       ; case do_ql of
           NoQL -> return ()
           DoQL | not prefix_only -> setSrcSpan fun_lspan $ rejectNativeQuickLook fun_sigma
                | otherwise -> return ()
       ; fun_origin <- mk_origin rn_head
       ; res@(_, fun_ty) <- go inst_plan fun_origin (1 + prefix_values) (reverse prefix) fun_sigma rn_args
       ; traceTc "tcInstFun:ret" (ppr fun_ty)
       ; return res
       }
  where
    -- What should be the origin for this function call?
    -- If the head of the function is user written
    -- then it can be used in the error message
    -- If it is generated code location span, blame it on the
    -- origin that can be retrived from the top of the error ctxt stack.
    -- See Note [Error contexts in generated code]
    mk_origin :: (HsExpr GhcRn, SrcSpan)  -- The head of the application chain and its location
              -> TcM CtOrigin
    mk_origin (rn_fun, fun_lspan)
     | not (isGeneratedSrcSpan fun_lspan)
     = return $ exprCtOrigin rn_fun

     | otherwise -- If the location is generated, the best we can do is to
                 -- approximate by looking on top of the error message stack
     = do { err_ctxt_stack <- getErrCtxt
          ; let hs_ctxt = case err_ctxt_stack of
                             (c:_) -> c
                             [] -> pprPanic "mk_origin" (ppr rn_fun)
          ; traceTc "mk_origin" (pprHsCtxt hs_ctxt)
          ; return $ hsCtxtCtOrigin hs_ctxt
          }

    -- These are the type variables which must be instantiated to concrete
    -- types. See Note [Representation-polymorphic Ids with no binding]
    -- in GHC.Tc.Utils.Concrete
    fun_conc_tvs
      | HsVar _ (L _ fun_id) <- tc_fun
      = idConcreteTvs fun_id
      -- Recall that DataCons are represented using ConLikeTc at GhcTc stage,
      -- see Note [Typechecking data constructors] in GHC.Tc.Gen.Head.
      | XExpr (ConLikeTc (RealDataCon dc)) <- tc_fun
      = dataConConcreteTyVars dc
      | otherwise
      = noConcreteTyVars

    -- Count value args only when complaining about a function
    -- applied to too many value args
    -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify.
    n_val_args = prefix_values + count isHsValArg rn_args
    prefix_values = count consumed_value prefix
    consumed_value EValArg{} = True
    consumed_value ERetainedArg{ ea_retained_vis = DepFunVisible } = True
    consumed_value _ = False

    peel_ql_body cur_schema
      | ExistsTy ImplicitExists binder body <- coreFullView cur_schema = do
          w <- newMetaTyVarTyAtLevel QLInstVar (tyVarKind binder)
          peel_ql_body (substTyWith [binder] [w] body)
      | otherwise = pure cur_schema

    fun_is_out_of_scope  -- See Note [VTA for out-of-scope functions]
      = case tc_fun of
          HsHole _        -> True
          _               -> False

    inst_fun :: [HsExprArg 'TcpRn] -> ForAllTyFlag -> Bool
    -- True <=> instantiate a tyvar that has this ForAllTyFlag
    inst_fun [] | inst_final  = isInvisibleForAllTyFlag
                | otherwise   = const False
    inst_fun (EValArg {} : _) = isInvisibleForAllTyFlag
    inst_fun (ERetainedArg {} : _) = isInvisibleForAllTyFlag
    inst_fun _                = isInferredForAllTyFlag

    -----------
    go, go1 :: RetainedInstPlan          -- Immutable choices in binder order
            -> CtOrigin                 -- Of the function
            -> Int                      -- Value-argument position of next arg
            -> [HsExprArg 'TcpInst]     -- Accumulator, reversed
            -> TcSigmaType -> [HsExprArg 'TcpRn]
            -> TcM ([HsExprArg 'TcpInst], TcSigmaType)

    -- A prefix is not a whole application. Do not run final IALL or IRESULT.
    go _ _ _ acc fun_ty [] | prefix_only = return (reverse acc, fun_ty)

    -- go: If fun_ty=kappa, look it up in Theta
    go plan fun_orig pos acc fun_ty args
      | Just kappa <- getTyVar_maybe fun_ty
      , isQLInstTyVar kappa
      = do { cts <- readMetaTyVar kappa
           ; case cts of
                Indirect fun_ty' -> go plan fun_orig pos acc fun_ty' args
                Flexi            -> go1 plan fun_orig pos acc fun_ty  args }
     | otherwise
     = do { fun_ty' <- case do_ql of
              NoQL | pos > 1 || not (null acc) -> zonkAppTypeIfNeeded fun_ty
              _ -> return fun_ty
          ; go1 plan fun_orig pos acc fun_ty' args }

    written_type_arg (ETypeArg { ea_loc_span = loc })
      = not (isGeneratedSrcSpan (locA loc))
    written_type_arg _ = False

    written_prefix_arg arg = written_type_arg arg || isExplicitRetainedArg arg

    -- go1: fun_ty is not filled-in instantiation variable
    --      ('go' dealt with that case)

    -- Handle out-of-scope functions gracefully
    go1 plan fun_orig pos acc fun_ty (arg : rest_args)
      | fun_is_out_of_scope
      , looks_like_type_arg arg || isExplicitRetainedArg arg
      -- See Note [VTA for out-of-scope functions]
      = go plan fun_orig pos acc fun_ty rest_args

    -- Rule IALL from Fig 4 of the QL paper; applies even if args = []
    -- Instantiate invisible foralls and dictionaries.
    -- c.f. GHC.Tc.Utils.Instantiate.topInstantiate
    go1 plan fun_orig pos acc fun_ty args
      | (tvs,   body1) <- tcSplitSomeForAllTyVars (inst_fun args) fun_ty
      , (theta, body2) <- if inst_fun args Inferred
                          then tcSplitPhiTy body1
                          else ([], body1)
        -- inst_fun args Inferred: dictionary parameters are like Inferred foralls
        -- E.g. #22908: f :: Foo => blah
        -- No foralls!  But if inst_final=False, don't instantiate
      , let no_tvs   = null tvs
            no_theta = null theta
      , not (no_tvs && no_theta)
      = do { selected <- case plan of
                Just (_ : _) -> return plan
                _ | not prefix_only, not no_tvs
                  , DepDictTy{} <- coreFullView body2
                  -- See Note [Applying a scoped dictionary].
                  -> do { sigma <- zonkAppTypeIfNeeded fun_ty
                        ; choice <- tcRetainedKnownArgPlan (tcRetainedDeclaredArgType pure)
                                                            tc_fun sigma args
                        ; case choice of
                            Just _ -> return choice
                            Nothing -> failWithTc $ TcRnRetainedQuantificationError
                                                     (RetainedSolverType fun_ty) }
                _ -> return Nothing
           ; (wrap, fun_rho, rest_plan) <-
                -- setSrcSpan of the function: important for the class constraints
                -- that may be emitted from instantiating fun_sigma
                setSrcSpan fun_lspan $
                case selected of
                  Nothing -> do { (wrap, rho) <- instantiateSigmaQLWithAcc acc do_ql fun_orig
                                                  fun_conc_tvs tvs theta body2
                                ; case (do_ql, expected_rho) of
                                    (DoQL, Just exp_rho)
                                      | not prefix_only
                                      , Just call_res <- implicitSpineResult (count isHsValArg args) rho
                                      , isGuardedTy call_res -> do
                                          exp_view <- unwrapMetaView exp_rho
                                          schema <- liftZonkM (zonkTcType exp_view)
                                          case coreFullView schema of
                                            ExistsTy ImplicitExists _ _
                                              | supportsQuickLookType schema
                                              , supportsQuickLookType call_res
                                              , noQuickLookVars schema -> do
                                                  inst_body <- peel_ql_body schema
                                                  qlUnify call_res inst_body
                                            _ -> pure ()
                                    _ -> pure ()
                                ; return (wrap, rho, plan) }
                  Just choices -> do
                    { let (inst_tys, rest) = splitAt (length tvs) choices
                          subst = Subst.zipTvSubst (map snd tvs) inst_tys
                    ; unless (tvs `equalLength` inst_tys
                              && and (zipWith (substKindAgrees subst) (map snd tvs) inst_tys)) $
                        pprPanic "tcInstFun: invalid retained instantiations"
                                 (ppr tvs $$ ppr choices)
                    ; wrap <- instRetainedCall do_ql acc fun_orig inst_tys (substTheta subst theta)
                    ; return (wrap, substTy subst body2, Just rest) }

                  -- See Note [Representation-polymorphism checking built-ins]
                  -- in GHC.Tc.Utils.Concrete.
                  -- NB: we are doing this even when "acc" is not empty,
                  -- to handle e.g.
                  --
                  --   badTup :: forall r (a :: TYPE r). a -> (# Int, a #)
                  --   badTup = (# , #) @LiftedRep
                  --
                  -- in which we already have instantiated the first RuntimeRep
                  -- argument of (#,#) to @LiftedRep, but want to rule out the
                  -- second instantiation @r.

           ; go rest_plan fun_orig pos (addArgWrap wrap acc) fun_rho args }
                -- Going around again means we deal easily with
                -- nested  forall a. Eq a => forall b. Show b => blah

    -- Skip InferredSpec invisible foreach binders ({k :: a}) when a later
    -- SpecifiedSpec invisible binder is targeted by @arg or @{arg}.
    go1 plan fun_orig pos acc fun_ty args@(first_arg : _)
      | is_type_or_retained first_arg
      , DepFunTy DepFunInvisible binder body <- coreFullView fun_ty
      , isInferredRetainedBinder binder
      = if hasSpecifiedInvisibleArgument body
        then infer_invis_retained plan fun_orig pos acc fun_ty binder body
               (noAnnSrcSpan fun_lspan) args
        else case first_arg of
               ETypeArg { ea_hs_ty = hs_ty } ->
                 do { (_, tidy_ty) <- liftZonkM $ zonkTidyTcType emptyTidyEnv fun_ty
                    ; failWith $ TcRnInvalidTypeApplication tidy_ty hs_ty }
               ERetainedArg { ea_arg = arg } ->
                 addArgCtxt (pos + count written_prefix_arg acc) rn_head arg $
                   failWithTc (TcRnRetainedQuantificationError (RetainedExplicitArgument fun_ty))
               _ -> pprPanic "tcInstFun: impossible arg" (ppr first_arg)
      where
        is_type_or_retained ETypeArg{}     = True
        is_type_or_retained ERetainedArg{} = True
        is_type_or_retained _              = False

    -- Proposal #378 §4.8 & §4.13: canonical @arg override for SpecifiedSpec
    -- invisible foreach binders. See Note [Visible @arg override for invisible foreach].
    go1 plan fun_orig pos acc fun_ty
        (ETypeArg { ea_loc_span = ctxt, ea_hs_ty = hs_ty } : rest_args)
      | DepFunTy DepFunInvisible binder body <- coreFullView fun_ty
      = case unLoc (ignoreParens (hswc_body hs_ty)) of
          HsWildCardTy{} ->
            infer_invis_retained plan fun_orig pos acc fun_ty binder body ctxt rest_args
          _ ->
            do { mb_rep <- tcTypeRepTarget_maybe (varType binder)
               ; case mb_rep of
                   Just (kind, target) ->
                     do { ty_arg <- tcHsTypeApp hs_ty kind
                        ; _ <- unifyType Nothing ty_arg target
                        ; bndr_ty' <- zonkAppTypeIfNeeded (varType binder)
                        ; bndr_mult' <- zonkAppTypeIfNeeded (idMult binder)
                        ; body' <- zonkAppTypeIfNeeded body
                        ; let emb_arg = L ctxt (HsEmbTy noExtField hs_ty)
                              checked = ERetainedArg DepFunInvisible ctxt
                                          (Scaled bndr_mult' bndr_ty') emb_arg body' Nothing Nothing
                        ; go plan fun_orig pos (checked : acc) body' rest_args }
                   Nothing ->
                     do { arg <- type_to_retained_expr hs_ty
                        ; check_invis_retained_operand plan fun_orig pos acc fun_ty
                            ctxt binder body arg rest_args } }

    -- See Note [Explicit invisible retained arguments].
    go1 plan fun_orig pos acc fun_ty (ERetainedArg { ea_loc_span = loc, ea_arg = arg } : rest_args)
      | DepFunTy DepFunInvisible binder body <- coreFullView fun_ty
      = check_invis_retained_operand plan fun_orig pos acc fun_ty loc binder body arg rest_args
      | otherwise = addArgCtxt (pos + count written_prefix_arg acc) rn_head arg $
                    failWithTc (TcRnRetainedQuantificationError (RetainedExplicitArgument fun_ty))

    -- See Note [Inferring an invisible retained argument].
    go1 plan fun_orig pos acc fun_ty args@(EValArg { ea_arg = first } : _)
      | DepFunTy DepFunInvisible binder body <- coreFullView fun_ty
      = infer_invis_retained plan fun_orig pos acc fun_ty binder body (getLoc first) args

    -- See Note [Applying a scoped dictionary].
    go1 plan fun_orig pos acc fun_ty args
      | DepDictTy binder _ <- coreFullView fun_ty
      , not (null args), inst_fun args Inferred
      = do { givens <- mapM zonkAppIdIfNeeded =<< getLexicalGivens
           ; predicate <- zonkAppTypeIfNeeded (varType binder)
           ; dict <- case Core.projectUniqueClassDict predicate givens of
               Core.ScopedDirectDict dict -> return dict
               Core.ScopedSuperclassDict _ _ ->
                 newScopedDictWanted fun_orig predicate
               Core.ScopedNoDict | noFreeVarsOfType predicate ->
                 do { defer <- goptM Opt_DeferTypeErrors
                    ; when defer $
                        failWithTc $ TcRnRetainedQuantificationError (RetainedSolverType fun_ty)
                    ; newScopedDictWanted fun_orig predicate }
               _ -> failWithTc $ TcRnRetainedQuantificationError (RetainedSolverType fun_ty)
           ; let evidence = Core.Var dict
                 wrap = mkWpEvApps [EvExpr evidence]
           ; go plan fun_orig pos (addArgWrap wrap acc) (applyTypeToArgs fun_ty [evidence]) args }
    go1 _ _ _ _ fun_ty _
      | DepDictTy{} <- coreFullView fun_ty
      = failWithTc $ TcRnRetainedQuantificationError (RetainedSolverType fun_ty)

    -- Rule IRESULT from Fig 4 of the QL paper; no more arguments
    go1 plan fun_orig pos acc fun_ty []
       | Just (_ : _) <- plan
       = pprPanic "tcInstFun: unused retained instantiations" (ppr plan)
       | inst_final
       , DepFunTy DepFunInvisible _ _ <- coreFullView fun_ty
       , Just expected <- expected_rho
       = do { reflected <- tcInstInvisibleTypeWitness fun_orig (Just expected) fun_ty
            ; case reflected of
                Just (wrap, res) -> go plan fun_orig pos (addArgWrap wrap acc) res []
                Nothing -> return (reverse acc, fun_ty) }
       | XExpr (ConLikeTc (RealDataCon dc)) <- tc_fun
       , isNewDataCon dc
       , [Scaled _ orig_arg_ty] <- dataConOrigArgTys dc
       , n_val_args == 0
       -- If we're dealing with an unsaturated representation-polymorphic
       -- UnliftedNewype, then perform a representation-polymorphism check.
       -- See Note [Representation-polymorphism checks for unsaturated unlifted newtypes]
       -- in GHC.Tc.Utils.Concrete.
       , not $ typeHasFixedRuntimeRep orig_arg_ty
       = do { (wrap_co, ma, arg_ty, res_ty) <-
                  matchActualFunTy (FRRRepPolyUnliftedNewtype dc)
                    (Just $ HsExprTcThing tc_fun)
                    (n_val_args, fun_sigma) fun_ty
             ; let acc' = addArgWrap (mkWpCastN wrap_co) acc
             ; return (reverse acc', tcMkScaledFunTy ma arg_ty res_ty) }
      | otherwise
      = return (reverse acc, fun_ty)

    -- See Note [Retained argument application] and
    -- Note [Typed holes in retained applications].
    go1 plan fun_orig pos acc fun_ty0
        all_args@(EValArg { ea_arg = arg, ea_loc_span = ctxt } : rest_args)
      | DepFunTy DepFunVisible b0 body0 <- coreFullView fun_ty0
      = do { mb_motive <- case (do_ql, prefix_only, retained_holes_ref) of
               (NoQL, False, Just state_ref) ->
                 tcResolvePrecedingMotiveArgs state_ref rn_head fun_orig acc fun_ty0 b0 body0
               _ -> return Nothing
           ; case mb_motive of
               Just (acc_m, fun_ty_m) ->
                 go plan fun_orig pos acc_m fun_ty_m all_args
               Nothing -> do
                 { fun_ty <- pre_unify_retained_spine fun_orig fun_ty0 all_args
                 ; let (binder, body) = case coreFullView fun_ty of
                         DepFunTy DepFunVisible b r -> (b, r)
                         _ -> pprPanic "tcInstFun: expected DepFunVisible" (ppr fun_ty)
                 ; (runtime_arg, body_ty, arg_co, mb_tc) <-
                     if exprContainsHole arg
                     then do { (r_arg, b_ty, a_co) <- tcRetainedHoleOperand retained_holes_ref fun_ty binder body arg
                             ; return (r_arg, b_ty, a_co, Nothing) }
                     else do { (core_arg, checked_tc, arg_ue) <- tcRetainedOperand binder body arg
                             ; b_ty <- retainedAppResult fun_ty binder core_arg
                             ; return (arg, b_ty, Nothing, Just (checked_tc, arg_ue)) }
                 ; bndr_ty' <- zonkRetainedBinderDomain binder
                 ; bndr_mult' <- zonkAppTypeIfNeeded (idMult binder)
                 ; let arg' = ERetainedArg DepFunVisible ctxt (Scaled bndr_mult' bndr_ty') runtime_arg body_ty
                                               arg_co mb_tc
                 ; go plan fun_orig (pos + 1) (arg' : acc) body_ty rest_args } }

    -- Rule ITVDQ from the GHC Proposal #281
    go1 plan fun_orig pos acc fun_ty (EValArg { ea_arg = arg, ea_loc_span = ctxt } : rest_args)
      | Just (ma, tvb, body) <- tcSplitForAllTyVarBinder_maybe fun_ty
      = assertPpr (isVisibleForAllTyFlag (binderFlag tvb)) (ppr fun_ty $$ ppr arg) $
        -- Any invisible binders have been instantiated by IALL above,
        -- so this forall must be visible (i.e. Required)
        do { (hs_ty, ty_arg, inst_body) <- tcVDQ fun_conc_tvs (ma, tvb, body) arg
           ; let arg' = ETypeArg { ea_loc_span = ctxt, ea_hs_ty = hs_ty
                                 , ea_ty_source = Nothing, ea_ty_arg = ty_arg }
           ; go plan fun_orig pos (arg' : acc) inst_body rest_args }

    go1 plan fun_orig pos acc fun_ty (EWrap w : args)
      = go1 plan fun_orig pos (EWrap w : acc) fun_ty args

    go1 plan fun_orig pos acc fun_ty (EPrag sp prag : args)
      = go1 plan fun_orig pos (EPrag sp prag : acc) fun_ty args

    -- Rule ITYARG from Fig 4 of the QL paper
    go1 plan fun_orig pos acc fun_ty ( ETypeArg { ea_loc_span = ctxt, ea_hs_ty = hs_ty
                                             , ea_ty_source = source }
                                  : rest_args )
      = do { (ty_arg, inst_ty) <- tcVTA fun_conc_tvs fun_ty source hs_ty
           ; let arg' = ETypeArg { ea_loc_span = ctxt, ea_hs_ty = hs_ty
                                 , ea_ty_source = source, ea_ty_arg = ty_arg }
           ; go plan fun_orig pos (arg' : acc) inst_ty rest_args }

    -- Rule IVAR from Fig 4 of the QL paper:
    go1 plan fun_orig pos acc fun_ty args@(EValArg {} : _)
      | Just kappa <- getTyVar_maybe fun_ty
      , isQLInstTyVar kappa
      = -- Function type was of form   f :: forall a b. t1 -> t2 -> b
        -- with 'b', one of the quantified type variables, in the corner
        -- but the call applies it to three or more value args.
        -- Suppose b is instantiated by kappa.  Then we want to make fresh
        -- instantiation variables nu1, nu2, and set kappa := nu1 -> nu2
        --
        -- In principle what is happening here is not unlike matchActualFunTys
        -- but there are many small differences:
        --   - We know that the function type in unfilled meta-tyvar
        --     matchActualFunTys is much more general, has a loop, etc.
        --   - We must be sure to actually update the variable right now,
        --     not defer in any way, because this is a QL instantiation variable.
        -- It's easier just to do the job directly here.
        do { arg_tys <- zipWithM (new_arg_ty fun_orig) (leadingValArgs args) [pos..]
           ; res_ty  <- newOpenFlexiTyVarTyQL do_ql TauTv
           ; let fun_ty' = mkScaledFunTysU arg_tys res_ty

           -- Fill in kappa := nu_1 -> .. -> nu_n -> res_nu
           -- NB: kappa is uninstantiated ('go' already checked that)
           ; kind_co <- unifyKind Nothing liftedTypeKind (tyVarKind kappa)
                 -- unifyKind: see (UQL3) in Note [QuickLook unification] in GHC.Tc.Utils.Unify
           ; liftZonkM (writeMetaTyVar kappa (mkCastTy fun_ty' kind_co))

           ; let co_wrap = mkWpCastN (mkGReflLeftCo Nominal fun_ty' kind_co)
                 acc'    = addArgWrap co_wrap acc
                 -- Suppose kappa :: kk
                 -- Then fun_ty :: kk, fun_ty' :: Type, kind_co :: Type ~ kk
                 --      co_wrap :: (fun_ty' |> kind_co) ~ fun_ty'

           ; go plan fun_orig pos acc' fun_ty' args }

    -- Rule IARG from Fig 4 of the QL paper:
    go1 plan fun_orig pos acc fun_ty
        all_args@(EValArg { ea_arg = arg, ea_loc_span = ctxt } : rest_args)
      | NoQL <- do_ql, not prefix_only
      , Just tv <- getTyVar_maybe fun_ty
      , isTcTyVar tv && isPromotableMetaTyVar tv
      , let prev_matches = [ (prev_arg, prev_dom)
                           | EValArg { ea_arg = prev_arg, ea_arg_ty = Scaled _ prev_dom } <- reverse acc
                           , tv `elemVarSet` tyCoVarsOfType prev_dom ]
      , not (null prev_matches)
      = do { forM_ prev_matches $ \(prev_arg, prev_dom) -> do
               { d0 <- tcRetainedDeclaredArgType pure prev_arg
               ; mb_act <- case d0 of
                             Just {} -> return d0
                             Nothing -> tcRetainedConstructorArgType prev_arg
               ; forM_ mb_act $ \act ->
                   when (hasNativeDependentExpr (Core.Type act)) $
                     tcPreUnifyMetaSkeleton Nothing emptyVarSet emptyVarSet prev_dom act }
           ; fun_ty' <- zonkAppTypeIfNeeded fun_ty
           ; if isNothing (getTyVar_maybe fun_ty')
             then go plan fun_orig pos acc fun_ty' all_args
             else step_val_arg plan fun_orig pos acc fun_ty' arg ctxt rest_args }
    go1 plan fun_orig pos acc fun_ty
        (EValArg { ea_arg = arg, ea_loc_span = ctxt } : rest_args)
      = step_val_arg plan fun_orig pos acc fun_ty arg ctxt rest_args

    step_val_arg plan fun_orig pos acc fun_ty arg ctxt rest_args
      = do { let herald = mk_herald fun_orig tc_fun (unLoc arg)
           ; (fun_co, _ma, arg_ty, res_ty) <-
                -- NB: matchActualFunTy does the rep-poly check.
                -- For example, suppose we have f :: forall r (a::TYPE r). a -> Int
                -- In an application (f x), we need 'x' to have a fixed runtime
                -- representation; matchActualFunTy checks that when
                -- taking apart the arrow type (a -> Int).
                --
                -- TODO: example from T26072
                matchActualFunTy herald
                  (Just $ HsExprTcThing tc_fun)
                  (n_val_args, fun_sigma) fun_ty
           ; let arg_pos = pos + count written_prefix_arg acc
           ; selected <- case do_ql of
               DoQL | classHasUnmatchableReceiver tc_fun
                    , Just{} <- tcSplitFunTy_maybe (scaledThing arg_ty)
                    , Just domains <- receiverDomains (arg_pos + 1) res_ty rest_args
                    , not (null domains) ->
                 seedClassReceiver DoQL rn_head ((arg_pos, arg, scaledThing arg_ty) : domains)
               _ -> return False
           ; arg_ty <- if selected then liftZonkM (zonkScaledTcType arg_ty) else return arg_ty
           ; (arg_prepared, arg_ty', res_ty') <-
               prepareTypeWitnessArg arg arg_ty res_ty
           ; arg' <- look_arg do_ql arg_pos ctxt rn_head arg_prepared arg_ty'
           ; let acc' = arg' : addArgWrap (mkWpCastN fun_co) acc
           ; go plan fun_orig (pos+1) acc' res_ty' rest_args }

    pre_unify_retained_spine fun_orig fun_ty0 rem_args
      | NoQL <- do_ql, not prefix_only
      , anyFreeVarsOfType (\v -> isTcTyVar v && isPromotableMetaTyVar v) fun_ty0
      = tcPreUnifyRetainedSpine fun_orig inst_final expected_rho fun_ty0
          [v | EValArg { ea_arg = v } <- rem_args]
      | otherwise = return fun_ty0

    infer_invis_retained plan fun_orig pos acc fun_ty0 _binder0 _body0 fallback_loc next_args
      = do { fun_ty <- pre_unify_retained_spine fun_orig fun_ty0 next_args
           ; let (binder, body) = case coreFullView fun_ty of
                   DepFunTy DepFunInvisible b r -> (b, r)
                   _ -> pprPanic "infer_invis_retained" (ppr fun_ty)
           ; reflected <- tcInstInvisibleTypeWitness fun_orig Nothing fun_ty
           ; case reflected of
               Just (wrap, res) -> go plan fun_orig pos (addArgWrap wrap acc) res next_args
               Nothing ->
                 do { let val_args = [v | EValArg { ea_arg = v } <- next_args]
                          err_arg  = case val_args of
                            v : _ -> v
                            []    -> L fallback_loc (fst rn_head)
                    ; InvisibleOperand operand record_usage <-
                        addArgCtxt (pos + count written_prefix_arg acc) rn_head err_arg $
                          do { selected <- tcInferInvisibleRetained [] binder body val_args
                             ; case selected of
                                 Just (actual, _) -> return actual
                                 Nothing -> failWithTc (TcRnRetainedQuantificationError
                                                         (RetainedInvisibleArgument binder)) }
                    ; bndr_mult' <- zonkAppTypeIfNeeded (idMult binder)
                    ; tcScalingUsage bndr_mult' record_usage
                    ; let wrap = mkWpEvApps [EvExpr operand]
                    ; go plan fun_orig pos (addArgWrap wrap acc)
                         (applyTypeToArgs fun_ty [operand]) next_args } }

    check_invis_retained_operand plan fun_orig pos acc fun_ty0 ctxt _binder0 _body0 arg rest_args
      = do { fun_ty <- pre_unify_retained_spine fun_orig fun_ty0 rest_args
           ; let (binder, body) = case coreFullView fun_ty of
                   DepFunTy DepFunInvisible b r -> (b, r)
                   _ -> pprPanic "check_invis_retained_operand" (ppr fun_ty)
           ; (runtime_arg, res_ty, arg_co, mb_tc) <-
               addArgCtxt (pos + count written_prefix_arg acc) rn_head arg $
               if isVacuousRetainedBinder binder body
               then return (arg, body, Nothing, Nothing)
               else if exprContainsHole arg
               then do { (r_arg, b_ty, a_co) <- tcRetainedHoleOperand retained_holes_ref fun_ty binder body arg
                       ; return (r_arg, b_ty, a_co, Nothing) }
               else do { (operand, checked_tc, arg_ue) <- tcRetainedOperand binder body arg
                       ; res <- retainedAppResult fun_ty binder operand
                       ; return (arg, res, Nothing, Just (checked_tc, arg_ue)) }
           ; bndr_ty' <- zonkRetainedBinderDomain binder
           ; bndr_mult' <- zonkAppTypeIfNeeded (idMult binder)
           ; let checked = ERetainedArg DepFunInvisible ctxt (Scaled bndr_mult' bndr_ty')
                                        runtime_arg res_ty arg_co mb_tc
           ; go plan fun_orig pos (checked : acc) res_ty rest_args }

    -- Read only the visible domains that already follow this callback.
    receiverDomains _ _ [] = Just []
    receiverDomains pos ty (EValArg{ea_arg = arg} : rest)
      | Just (_, Scaled _ domain, result) <- tcSplitFunTy_maybe ty
      = ((pos, arg, domain) :) <$> receiverDomains (pos + 1) result rest
    receiverDomains pos ty (EWrap{} : rest) = receiverDomains pos ty rest
    receiverDomains _ _ _ = Nothing

    new_arg_ty :: CtOrigin -> LHsExpr GhcRn -> Int -> TcM (Scaled TcType)
    -- Make a fresh nus for each argument in rule IVAR
    new_arg_ty fun_orig (L _ arg) i
      = do { arg_nu <- newArgTyVarTyQL do_ql $
                       FRRExpectedFunTy (mk_herald fun_orig tc_fun arg) i
               -- Following matchActualFunTy, we create nu_i :: TYPE kappa_i[conc],
               -- thereby ensuring that the arguments have concrete runtime representations

            ; mult_ty <- newFlexiTyVarTyQL do_ql (mkTyVarOccFS (fsLit "m")) TauTv multiplicityTy
               -- mult_ty: e need variables for argument multiplicities (#18731)
               -- Otherwise, 'undefined x' wouldn't be linear in x

           ; return (mkScaled mult_ty arg_nu) }

    mk_herald :: CtOrigin -> HsExpr GhcTc -> HsExpr GhcRn -> ExpectedFunTyCtxt
    mk_herald fun_orig tc_fun arg
      = case fun_orig of
           DoStmtOrigin -> ExpectedFunTySyntaxOp DoStmtOrigin tc_fun
           _ -> ExpectedFunTyArg (HsExprTcThing tc_fun) arg


-- | Mode for extracting a 'Core.CoreExpr' from a checked 'LHsExpr' 'GhcTc'.
data RetainedCoreMode
  = RCM_RetainedArg
  | RCM_OptionalLet
  | RCM_CaseCallArg !(NameEnv Id)

-- | Recover a 'Core.CoreExpr', its certified source-table owners, and any
-- dictionary method guards from an already-typechecked expression without
-- emitting constraints or linearity usages.
--
-- See Note [Optional typed let operands],
-- Note [Checked source call arguments] in "GHC.Tc.Gen.Expr",
-- Note [Signed source call beta] in "GHC.Tc.Gen.Expr", and
-- Note [Cases on a cast callback result] in "GHC.Tc.Gen.Expr".
tcExprToRetainedCore
  :: RetainedCoreMode
  -> LHsExpr GhcTc
  -> TcM (Maybe (Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)]))
tcExprToRetainedCore mode0 lexpr0 = runMaybeT (go mode0 lexpr0)
  where
    is_case RCM_CaseCallArg{} = True
    is_case _                 = False

    liftMaybe :: Maybe a -> MaybeT TcM a
    liftMaybe = MaybeT . pure

    go :: RetainedCoreMode
       -> LHsExpr GhcTc
       -> MaybeT TcM (Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)])
    go mode (L _ expr) = case expr of
      HsPar _ body -> go mode body
      XExpr (ExpandedThingTc (HSE _ body)) -> go mode body
      XExpr (WrapExpr WpHole body) -> go mode (noLocA body)
      XExpr (WrapExpr (WpSubType w) body)
        | not (is_case mode) -> go mode (noLocA (XExpr (WrapExpr w body)))
      ExprWithTySig _ body _
        | not (is_case mode) -> go mode body
      XExpr (WrapExpr wrap body)
        | RCM_OptionalLet <- mode
        , Just (selector, domain, wanted) <- method_head wrap body -> do
            domain' <- liftMaybeT (liftZonkM (zonkTcType domain))
            (sel, dfun, ev) <- method selector domain' wanted
            target_name <- liftMaybe (sourceMethodTarget_maybe dfun sel)
            target <- MaybeT (tcLookupIdMaybe target_name) >>= tcOptionalLetCertified
            guard (varType target `tcEqType` classMethodInstTy sel [domain'])
            return (Core.Var target, [target], [(ev, dfun, sel, Just target)])
        | RCM_RetainedArg <- mode
        , Just (sel, inst, dict_expr) <- scoped_dict_head wrap body -> do
            sel'  <- liftMaybeT (liftZonkM (zonkId sel))
            inst' <- liftMaybeT (liftZonkM (zonkTcType inst))
            return (Core.mkApps (Core.Var sel') [Core.Type inst', dict_expr], [], [])
        | otherwise -> do
            (body', heads, guards) <- go mode (noLocA body)
            wrapped <- apply_wrap mode wrap body'
            return (wrapped, heads, guards)
      HsAppType ty fun _
        | not (is_case mode) -> do
            (fun', heads, guards) <- go mode fun
            ty' <- liftMaybeT (liftZonkM (zonkTcType ty))
            return (Core.App fun' (Core.Type ty'), heads, guards)
      HsAppRetained _ fun arg
        | RCM_RetainedArg <- mode -> do
            (fun', fh, fg) <- go mode fun
            (arg', ah, ag) <- go mode arg
            return (Core.App fun' arg', fh ++ ah, fg ++ ag)
      HsApp _ fun arg
        | RCM_CaseCallArg bound <- mode
        , ExprWithTySig _ inner signature <- unLoc (strip_checked fun)
        , Just domain_name <- simple_signature signature ->
            (, [], []) <$> checked_beta bound domain_name inner arg
        | RCM_RetainedArg <- mode
        , HsLam _ LamSingle MG
            { mg_alts = L _ [L _ Match
                { m_pats = L _ [L _ (VarPat _ (L _ bndr_id))]
                , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] body) NE.:| []
                                  , grhssLocalBinds = L _ EmptyLocalBinds{} } }] }
            <- unLoc (strip_checked fun) -> do
            bndr' <- liftMaybeT (liftZonkM (zonkId bndr_id))
            (body', bh, bg) <- go mode body
            (arg', ah, ag)  <- go mode arg
            return (Core.App (Core.Lam bndr' body') arg', bh ++ ah, bg ++ ag)
        | otherwise -> do
            (fun', fh, fg) <- go_fun mode fun
            (arg', ah, ag) <- go mode arg
            return (Core.App fun' arg', fh ++ ah, fg ++ ag)
      ExplicitList element_ty elements -> do
        element_ty' <- liftMaybeT (liftZonkM (zonkTcType element_ty))
        values <- mapM (go mode) elements
        let nil = Core.mkConApp nilDataCon [Core.Type element_ty']
            cons (val, _, _) rest = Core.mkConApp consDataCon [Core.Type element_ty', val, rest]
        return (foldr cons nil values,
                concatMap (\(_, h, _) -> h) values,
                concatMap (\(_, _, g) -> g) values)
      ExplicitTuple _ tup_args Boxed
        | RCM_RetainedArg <- mode
        , Just es <- tupArgsPresent_maybe tup_args -> do
            values <- mapM (go mode) es
            return (mkCoreTup [v | (v, _, _) <- values],
                    concatMap (\(_, h, _) -> h) values,
                    concatMap (\(_, _, g) -> g) values)
      HsVar _ (L _ v) -> case mode of
        RCM_CaseCallArg bound -> do
          let actual = fromMaybe v (lookupNameEnv bound (idName v))
          guard (isNativeOpaqueValueId actual)
          return (Core.Var actual, [], [])
        RCM_OptionalLet -> do
          occ <- liftMaybeT (liftZonkM (zonkId v))
          if not (isExternalName (getName occ)) && isNativeOpaqueValueId occ
            then do
              thing <- MaybeT (tcLookupLcl_maybe (getName occ))
              case thing of
                ATcId { tct_id = actual } -> do
                  actual' <- liftMaybeT (liftZonkM (zonkId actual))
                  guard (actual' == occ && isNativeOpaqueValueId actual'
                         && varType actual' `tcEqType` varType occ)
                  return (Core.Var actual', [], [])
                _ -> mzero
            else do
              owner <- tcOptionalLetCertified occ
              return (Core.Var owner, [owner], [])
        RCM_RetainedArg -> do
          occ <- liftMaybeT (liftZonkM (zonkId v))
          if not (isExternalName (getName occ))
            then do
              guard (isNativeOpaqueValueId occ || isNativeIndexFunctionId occ)
              return (Core.Var occ, [], [])
            else do
              mb_owner <- liftMaybeT (tcLookupSourceTableId occ)
              case mb_owner of
                Just owner -> do
                  owner' <- liftMaybeT (liftZonkM (zonkId owner))
                  guard (varType owner' `tcEqType` varType occ)
                  return (Core.Var owner', [owner'], [])
                Nothing
                  | isNativeOpaqueValueId occ || isNativeNamedOpaqueFunctionId occ ->
                      return (Core.Var occ, [], [])
                  | otherwise -> mzero
      XExpr (HsRecSelTc FieldOcc{foLabel = L _ sel_id}) -> case mode of
        RCM_OptionalLet -> do
          owner <- tcOptionalLetCertified sel_id
          return (Core.Var owner, [owner], [])
        RCM_RetainedArg -> do
          sel' <- liftMaybeT (liftZonkM (zonkId sel_id))
          owner <- MaybeT (tcLookupSourceTableId sel')
          owner' <- liftMaybeT (liftZonkM (zonkId owner))
          guard (varType owner' `tcEqType` varType sel')
          return (Core.Var owner', [owner'], [])
        RCM_CaseCallArg{} -> mzero
      XExpr (ConLikeTc (RealDataCon con)) -> case mode of
        RCM_OptionalLet -> return (Core.Var (dataConWorkId con), [], [])
        _               -> return (Core.Var (dataConWrapId con), [], [])
      HsLit _ (HsChar _ c)
        | RCM_RetainedArg <- mode -> return (mkCharExpr c, [], [])
      HsLit _ (HsNatural _ il)
        | RCM_RetainedArg <- mode -> do
            platform <- liftMaybeT getPlatform
            return (mkNaturalExpr platform (il_value il), [], [])
      HsOverLit _ (OverLit { ol_ext = OverLitTc { ol_rebindable = False
                                                , ol_witness = witness
                                                , ol_type = result_ty }
                           , ol_val = HsIntegral literal })
        | RCM_OptionalLet <- mode
        , let n = il_value literal, n >= 0
        , HsApp _ fun arg <- strip witness
        , XExpr (WrapExpr wrap body) <- strip (unLoc fun)
        , Just (selector, domain, wanted) <- method_head wrap body
        , HsLit _ (XLit (HsInteger _ actual integer_ty)) <- strip (unLoc arg)
        , actual == n, isIntegerTy integer_ty -> do
            domain' <- liftMaybeT (liftZonkM (zonkTcType domain))
            result_ty' <- liftMaybeT (liftZonkM (zonkTcType result_ty))
            guard (domain' `tcEqType` result_ty')
            (sel, dfun, ev) <- method selector domain' wanted
            (zero, succ_con) <- liftMaybe (sourceLiteralCons_maybe dfun sel)
            dflags <- liftMaybeT getDynFlags
            guard (n <= toInteger (maxBound :: Int)
                   && not (intGtLimit (fromInteger n) (reductionDepth dflags)))
            let build 0 = Core.Var (dataConWorkId zero)
                build k = Core.App (Core.Var (dataConWorkId succ_con)) (build (k - 1))
            return (build (fromInteger n :: Int), [], [(ev, dfun, sel, Nothing)])
      _ -> mzero

    go_fun :: RetainedCoreMode
           -> LHsExpr GhcTc
           -> MaybeT TcM (Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)])
    go_fun mode@(RCM_CaseCallArg _) fun
      | XExpr (WrapExpr (WpCast co) inner) <- unLoc (stripParensLHsExpr fun) = do
          (prefix@Core.App{}, fh, fg) <- go mode (noLocA inner)
          index <- liftMaybe (Core.exprToNativeCaseIndex prefix)
          checked <- liftMaybeT (liftZonkM (mapNativeIndexCastCoM zonkCo co))
          let source = coercionLKind checked
              target = coercionRKind checked
          guard (coercionRole checked == Representational && typeKind index `eqType` source)
          case setNominalRole_maybe Representational checked of
            Just _ | isJust (nativeIndexFunctionTypeTys_maybe target) ->
              return (Core.Cast prefix checked, fh, fg)
            _ -> do
              loc <- liftMaybeT (getCtLocM PatCheckOrigin Nothing)
              delta <- MaybeT (tryTcDiscardingErrs (pure Nothing) $
                               runMaybeT (expose_arrow loc source))
              let arrow = coercionRKind delta
              _ <- liftMaybeT (unifyType Nothing arrow target)
              target' <- liftMaybeT (liftZonkM (zonkTcType target))
              guard (target' `eqType` arrow)
              return (Core.Cast prefix (mkSubCo delta), fh, fg)
    go_fun mode fun = go mode fun

    expose_arrow :: CtLoc -> TcType -> MaybeT TcM CoercionN
    expose_arrow loc ty = do
      (family, args) <- liftMaybe (tcSplitTyConApp_maybe ty)
      guard (isTypeFamilyTyCon family && not (isConstrainedFamilyTyCon family))
      Reduction step rhs <- MaybeT (matchFamTcM family args)
      loc' <- liftMaybeT (bumpReductionDepthTcM loc ty)
      if isJust (nativeIndexFunctionTypeTys_maybe rhs)
        then pure step
        else mkTransCo step <$> expose_arrow loc' rhs

    apply_wrap :: RetainedCoreMode -> HsWrapper -> Core.CoreExpr -> MaybeT TcM Core.CoreExpr
    apply_wrap _ WpHole body = return body
    apply_wrap mode (WpSubType w) body
      | not (is_case mode) = apply_wrap mode w body
    apply_wrap mode (WpCompose outer inner) body =
      apply_wrap mode inner body >>= apply_wrap mode outer
    apply_wrap mode (WpTyApp ty) body = do
      ty' <- liftMaybeT (liftZonkM (zonkTcType ty))
      case mode of
        RCM_RetainedArg -> guard (noQuickLookVars ty')
        _               -> return ()
      return (Core.App body (Core.Type ty'))
    apply_wrap RCM_RetainedArg (WpEvApp (EvExpr (Core.Coercion co))) body = do
      co' <- liftMaybeT (liftZonkM (zonkCo co))
      guard (not (hasCoercionHole (CoercionTy co')))
      return (Core.App body (Core.Coercion co'))
    apply_wrap RCM_RetainedArg (WpCast co) body = do
      co' <- liftMaybeT (liftZonkM (mapNativeIndexCastCoM zonkCo co))
      if isReflCo co'
        then return body
        else case nativeIndexCastCo_maybe co' of
               Just delta -> return (Core.Cast body (SubCo delta))
               Nothing    -> mzero
    apply_wrap _ _ _ = mzero

    strip :: HsExpr GhcTc -> HsExpr GhcTc
    strip (HsPar _ body) = strip (unLoc body)
    strip (XExpr (ExpandedThingTc (HSE _ body))) = strip (unLoc body)
    strip (XExpr (WrapExpr WpHole body)) = strip body
    strip e = e

    strip_checked :: LHsExpr GhcTc -> LHsExpr GhcTc
    strip_checked e = case unLoc (stripParensLHsExpr e) of
      XExpr (WrapExpr WpHole inner) -> strip_checked (noLocA inner)
      XExpr (ExpandedThingTc (HSE _ inner)) -> strip_checked inner
      _ -> stripParensLHsExpr e

    scoped_dict_head :: HsWrapper -> HsExpr GhcTc -> Maybe (Id, Type, Core.CoreExpr)
    scoped_dict_head (WpCompose (WpEvApp (EvExpr dict_expr)) (WpTyApp inst)) body
      | HsVar _ (L _ sel) <- strip body
      , isJust (isClassOpId_maybe sel)
      = Just (sel, inst, dict_expr)
    scoped_dict_head _ _ = Nothing

    method_head :: HsWrapper -> HsExpr GhcTc -> Maybe (Id, Type, EvVar)
    method_head (WpCompose (WpEvApp (EvExpr (Core.Var wanted))) (WpTyApp domain)) body
      | HsVar _ (L _ selector) <- strip body = Just (selector, domain, wanted)
    method_head (WpEvApp (EvExpr (Core.Var wanted))) body
      | HsAppType domain fun _ <- strip body
      , HsVar _ (L _ selector) <- strip (unLoc fun) = Just (selector, domain, wanted)
    method_head _ _ = Nothing

    method :: Id -> Type -> EvVar -> MaybeT TcM (Id, Id, EvVar)
    method selector0 domain0 wanted0 = do
      selector <- liftMaybeT (liftZonkM (zonkId selector0))
      domain <- liftMaybeT (liftZonkM (zonkTcType domain0))
      wanted <- liftMaybeT (liftZonkM (zonkId wanted0))
      cls <- liftMaybe (isClassOpId_maybe selector)
      case tcSplitSigmaTy (varType selector) of
        ([(_, tv)], [theta], rho)
          | let (_, _, result) = splitFunTys rho
          , result `tcEqType` mkTyVarTy tv
          , theta `tcEqType` mkClassPred cls [mkTyVarTy tv]
          , noFreeVarsOfType domain, typeKind domain `tcEqType` tyVarKind tv
          , varType wanted `tcEqType` mkClassPred cls [domain] -> return ()
        _ -> mzero
      insts <- liftMaybeT tcGetInstEnvs
      dfun_name <- case lookupUniqueInstEnv insts cls [domain] of
        Right (inst, []) -> return (getName (instanceDFunId inst))
        _ -> mzero
      env <- liftMaybeT getGblEnv
      guard (not (nameIsLocalOrFrom (tcg_semantic_mod env) dfun_name))
      dfun <- MaybeT (tcLookupIdMaybe dfun_name)
      guard (isDFunId dfun && varType dfun `tcEqType` varType wanted
             && noFreeVarsOfType (varType dfun))
      return (selector, dfun, wanted)

    simple_signature :: LHsSigWcType GhcRn -> Maybe Name
    simple_signature (HsWC { hswc_ext = []
                           , hswc_body = L _ HsSig
                               { sig_bndrs = HsOuterImplicit { hso_ximplicit = [] }
                               , sig_body = signature } })
      | HsFunTy _ (HsModifiedFunArr _ [] (HsStandardArr _)) arg res <- strip_type signature
      , HsTyVar _ NotPromoted (L _ (WithUserRdr _ arg_name)) <- strip_type arg
      , HsTyVar _ NotPromoted (L _ (WithUserRdr _ res_name)) <- strip_type res
      , arg_name == res_name
      = Just arg_name
    simple_signature _ = Nothing

    strip_type :: LHsType GhcRn -> HsType GhcRn
    strip_type (L _ (HsParTy _ ty)) = strip_type ty
    strip_type (L _ ty) = ty

    checked_beta :: NameEnv Id -> Name -> LHsExpr GhcTc -> LHsExpr GhcTc -> MaybeT TcM Core.CoreExpr
    checked_beta bound domain_name inner arg
      | HsLam _ LamSingle MG
          { mg_ext = MatchGroupTc { mg_arg_tys = [pat_ty], mg_res_ty = result_ty }
          , mg_alts = L _ [L _ Match
              { m_pats = L _ [L _ (VarPat _ (L _ binder))]
              , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] body) NE.:| []
                                , grhssLocalBinds = L _ EmptyLocalBinds{} } }] }
          <- unLoc (strip_checked inner) = do
          strict <- liftMaybeT (xoptM LangExt.Strict)
          guard (not strict)
          (actual, argument, result) <- liftMaybeT $ liftZonkM $
            (,,) <$> zonkId binder <*> zonkScaledTcType pat_ty <*> zonkTcType result_ty
          guard (isManyTy (idMult actual) && isManyTy (scaledMult argument)
                 && isNativeIndexId actual
                 && eqType (idType actual) (scaledThing argument)
                 && eqType (idType actual) result)
          (tc, []) <- liftMaybe (splitTyConApp_maybe (coreFullView (idType actual)))
          guard (tyConName tc == domain_name)
          (core_body, _, _) <- go (RCM_CaseCallArg (extendNameEnv bound (idName actual) actual)) body
          (core_arg, _, _)  <- go (RCM_CaseCallArg bound) arg
          return (Core.App (Core.Lam actual core_body) core_arg)
      | otherwise = mzero

-- | Pre-unify touchable instantiation metavariables in a retained spine from
-- the enclosing expected result type and subsequent argument declarations before
-- elaborating a retained operand.
tcPreUnifyRetainedSpine
  :: CtOrigin -> Bool -> Maybe TcRhoType -> TcSigmaType -> [LHsExpr GhcRn]
  -> TcM TcSigmaType
tcPreUnifyRetainedSpine fun_orig inst_final mb_exp fun_ty0 val_args = do
  forM_ mb_exp $ \exp_rho ->
    case peel_result emptyVarSet (length val_args) fun_ty0 of
      Just (crossed, res_ty) ->
        tcPreUnifyMetaSkeleton (Just fun_orig) crossed emptyVarSet res_ty exp_rho
      Nothing -> return ()
  fun_ty1 <- zonkAppTypeIfNeeded fun_ty0
  when (anyFreeVarsOfType is_meta fun_ty1 && not (null val_args)) $
    forM_ (collect_domains emptyVarSet val_args fun_ty1) $ \(crossed, is_ret, dom0, arg) -> do
      dom <- zonkAppTypeIfNeeded dom0
      when (anyFreeVarsOfType is_meta dom) $ do
        mb_act <- if is_ret
                    then tcRetainedConstructorArgType arg
                    else do
                      d0 <- tcRetainedDeclaredArgType pure arg
                      case d0 of
                        Just {} -> return d0
                        Nothing -> tcRetainedConstructorArgType arg
        forM_ mb_act $ \act0 -> do
          let act = case tcSplitTyConApp_maybe dom of
                      Just (tc, _)
                        | is_ret, isAlgTyCon tc
                        , Just (_, params) <- matchSumPayload tc act0
                        -> mkTyConApp tc params
                      _ -> act0
          tcPreUnifyMetaSkeleton Nothing crossed emptyVarSet dom act
  zonkAppTypeIfNeeded fun_ty1
  where
    is_meta v = isTcTyVar v && isPromotableMetaTyVar v

    peel_result crossed 0 ty = go_post crossed ty
      where
        go_post cr t
          | Just t' <- coreView t = go_post cr t'
          | DepFunTy DepFunInvisible b r <- t, inst_final = go_post (extendVarSet cr b) r
          | DepDictTy d r <- t = go_post (extendVarSet cr d) r
          | FunTy af _ _ _ r <- t, isInvisibleFunArg af, inst_final = go_post cr r
          | isRetainedMonoType t = Just (cr, t)
          | otherwise = Nothing
    peel_result crossed n ty
      | n > 0 = case coreFullView ty of
          DepFunTy DepFunInvisible b r -> peel_result (extendVarSet crossed b) n r
          DepDictTy d r                -> peel_result (extendVarSet crossed d) n r
          DepFunTy DepFunVisible b r   -> peel_result (extendVarSet crossed b) (n - 1) r
          FunTy af _ _ _ r
            | isInvisibleFunArg af     -> peel_result crossed n r
            | otherwise                -> peel_result crossed (n - 1) r
          _ -> Nothing
      | otherwise = Nothing

    collect_domains _ [] _ = []
    collect_domains crossed va@(v : vs) ty = case coreFullView ty of
      DepFunTy DepFunInvisible b r -> collect_domains (extendVarSet crossed b) va r
      DepDictTy d r                -> collect_domains (extendVarSet crossed d) va r
      FunTy af _ _ _ r | isInvisibleFunArg af -> collect_domains crossed va r
      DepFunTy DepFunVisible b r   ->
        (crossed, True, varType b, v) : collect_domains (extendVarSet crossed b) vs r
      FunTy _ _ _ dom r            ->
        (crossed, False, dom, v) : collect_domains crossed vs r
      _ -> []

-- | Test whether a type is a monotype or visible @foreach@ value type.
isRetainedMonoType :: Type -> Bool
isRetainedMonoType = go False
  where
    go under_pi ty | Just ty' <- coreView ty = go under_pi ty'
    go _ ty | isTauTy ty = True
    go _ (DepFunTy DepFunVisible b r) =
      (isNativeOpaqueValueId b || isNativeIndexFunctionId b)
      && tcIsLiftedTypeKind (typeKind r)
      && go False (idMult b) && go False (varType b) && go True r
    go under_pi (ForAllTy ma (Bndr tv vis) r)
      | under_pi, isInvisibleForAllTyFlag vis
      = go False ma && go False (tyVarKind tv) && go True r
    go under_pi (FunTy af ma mult a r)
      | isVisibleFunArg af || under_pi
      = go False ma && go False mult && go under_pi a && go under_pi r
    go under_pi ty@(TyConApp tc tys)
      | isTauTyCon tc
      = let under_pi' = under_pi && isAlgTyCon tc && not (isClassTyCon tc)
                          && length tys == tyConArity tc
                          && tcIsLiftedTypeKind (typeKind ty)
        in all (go under_pi') tys
    go _ (AppTy a b) = go False a && go False b
    go _ (CastTy ty _) = go False ty
    go _ _ = False

tcPreUnifyMetaSkeleton :: Maybe CtOrigin -> VarSet -> VarSet -> Type -> Type -> TcM ()
tcPreUnifyMetaSkeleton mb_orig crossed right_bound actual0 expected0 = do
  actual   <- zonkAppTypeIfNeeded actual0
  expected <- zonkAppTypeIfNeeded expected0
  when (anyFreeVarsOfType is_meta actual) $
    case coreFullView expected of
      ExistsTy ImplicitExists w payload ->
        tcPreUnifyMetaSkeleton mb_orig crossed (extendVarSet right_bound w) actual payload
      _ -> step (coreFullView actual) (coreFullView expected)
  where
    is_meta v = isTcTyVar v && isPromotableMetaTyVar v

    can_pre_assign tv rhs = do
      touchable <- isTouchableTcM tv
      id_depths <- getIdTermDepths
      let tv_lvl   = tcTyVarLevel tv
          tv_depth = metaTyVarTermDepth tv
          fv_ok v
            | v == tv   = False
            | isCoVar v = True
            | isTyVar v =
                if isMetaTyVar v
                then isPromotableMetaTyVar v
                else not (tcTyVarLevel v `strictlyDeeperThan` tv_lvl)
            | otherwise =
                (isNativeOpaqueValueId v || isNativeIndexFunctionId v)
                && maybe False (<= tv_depth) (lookupIdTermDepth id_depths v)
      return (touchable && all fv_ok (tyCoVarsOfTypeList rhs))

    step act exp_ty
      | TyVarTy tv <- act
      , is_meta tv
      , not (tv `elemVarSet` crossed)
      , not (anyFreeVarsOfType (`elemVarSet` right_bound) exp_ty)
      , noQuickLookVars exp_ty
      , isRetainedMonoType exp_ty || isRuntimeUnkTyVar tv
      = do { ok <- can_pre_assign tv exp_ty
           ; when ok $ do
               { tcPreUnifyMetaSkeleton Nothing crossed right_bound (tyVarKind tv) (typeKind exp_ty)
               ; tv_ty'  <- zonkAppTypeIfNeeded (mkTyVarTy tv)
               ; exp_ty' <- zonkAppTypeIfNeeded exp_ty
               ; when (typeKind tv_ty' `tcEqType` typeKind exp_ty') $ do
                   { (_, eq_wc) <- captureConstraints (unifyType Nothing tv_ty' exp_ty')
                   ; unless (isEmptyWC eq_wc) $ do
                       { res_wc <- runTcSEqualities (simplifyTopWanteds eq_wc)
                       ; emitConstraints res_wc } } } }
      | TyConApp tc1 as1 <- act
      , TyConApp tc2 as2 <- exp_ty
      , tc1 == tc2, equalLength as1 as2
      = if isGenerativeTyCon tc1 Nominal
        then zipWithM_ (tcPreUnifyMetaSkeleton mb_orig crossed right_bound) as1 as2
        else case closedFamilyScope_maybe tc1 of
          Just (_, True)
            | length as1 + 1 == tyConArity tc1
            , Just axiom <- closedFamilyTyConCoAxiom_maybe tc1
            , [branch] <- fromBranches (coAxiomBranches axiom)
            , null (coAxBranchCoVars branch)
            , Just params <- traverse getTyVar_maybe (coAxBranchLHS branch)
            , length params == tyConArity tc1
            , length params == sizeVarSet (mkVarSet params) ->
                let det = injectiveVarsOfType False (coAxBranchRHS branch)
                in sequence_ [ tcPreUnifyMetaSkeleton mb_orig crossed right_bound a1 a2
                             | (p, a1, a2) <- zip3 params as1 as2
                             , p `elemVarSet` det ]
          _ -> return ()
      | TyConApp tc1 as1 <- act
      , isGenerativeTyCon tc1 Nominal
      , Just (_, params) <- matchSumPayload tc1 exp_ty
      , equalLength as1 params
      = zipWithM_ (tcPreUnifyMetaSkeleton mb_orig crossed right_bound) as1 params
      | Just orig <- mb_orig
      , TyConApp tc1 _ <- act
      , isGenerativeTyCon tc1 Nominal
      , Just (fam_tc, fam_args) <- tcSplitTyConApp_maybe exp_ty
      , isTypeFamilyTyCon fam_tc
      = do { envs <- tcGetFamInstEnvs
           ; loc0 <- getCtLocM orig (Just TypeLevel)
           ; mb_red <- tryTcDiscardingErrs (return Nothing) $
               case reduceTyFamApp_maybe envs Nominal fam_tc fam_args of
                 Just (Reduction _ rhs) -> do
                   when (isConstrainedFamilyTyCon fam_tc) $
                     checkNoErrs (checkConstrainedSignature TypeAppCtxt exp_ty)
                   _ <- bumpReductionDepthTcM loc0 exp_ty
                   return (Just rhs)
                 Nothing -> return Nothing
           ; case mb_red of
               Just exp_red -> tcPreUnifyMetaSkeleton mb_orig crossed right_bound act exp_red
               Nothing      -> return () }
      | FunTy af1 ma1 m1 d1 r1 <- act
      , FunTy af2 ma2 m2 d2 r2 <- exp_ty
      , af1 == af2
      = do { tcPreUnifyMetaSkeleton mb_orig crossed right_bound ma1 ma2
           ; tcPreUnifyMetaSkeleton mb_orig crossed right_bound m1  m2
           ; tcPreUnifyMetaSkeleton mb_orig crossed right_bound d1  d2
           ; tcPreUnifyMetaSkeleton mb_orig crossed right_bound r1  r2 }
      | DepFunTy vis1 b1 r1 <- act
      , DepFunTy vis2 b2 r2 <- exp_ty
      , vis1 == vis2
      = do { tcPreUnifyMetaSkeleton mb_orig crossed right_bound (idMult b1) (idMult b2)
           ; tcPreUnifyMetaSkeleton mb_orig crossed right_bound (varType b1) (varType b2)
           ; tcPreUnifyMetaSkeleton mb_orig (extendVarSet crossed b1) (extendVarSet right_bound b2) r1 r2 }
      | otherwise = return ()

-- | When a visible retained binder occurs in its codomain only underneath an
-- unfilled higher-order motive metavariable (as in @(.)@ or @($)@), infer any
-- preceding uncached value argument that mentions that motive so constant
-- motives collapse vacuous @foreach@ binders to ordinary arrows before checking
-- subsequent arguments.
tcResolvePrecedingMotiveArgs
  :: TcRef ([TcTyVar], RetainedArgCache)
  -> (HsExpr GhcRn, SrcSpan)
  -> CtOrigin
  -> [HsExprArg 'TcpInst]
  -> TcSigmaType -> Id -> Type
  -> TcM (Maybe ([HsExprArg 'TcpInst], TcSigmaType))
tcResolvePrecedingMotiveArgs state_ref rn_head fun_orig rev_acc fun_ty0 binder0 body0
  | Just m_tvs <- motive_metas_for binder0 body0
  , not (isEmptyVarSet m_tvs)
  = do { (_, cur_cache) <- readTcRef state_ref
       ; did_infer <- infer_matching_args cur_cache m_tvs 0 0 (reverse rev_acc)
       ; if not did_infer
         then return Nothing
         else do
           { fun_ty1 <- zonkAppTypeIfNeeded fun_ty0
           ; loc <- getCtLocM fun_orig (Just TypeLevel)
           ; Reduction co_fun fun_ty2 <- tcRewriteFamilyAxioms loc fun_ty1
           ; let acc' = if isReflCo co_fun
                        then rev_acc
                        else addArgWrap (mkWpCastN co_fun) rev_acc
           ; return (Just (acc', fun_ty2)) } }
  | otherwise = return Nothing
  where
    infer_matching_args _ _ _ _ [] = return False
    infer_matching_args cur_cache m_tvs pos vpos (arg : rest) = do
      let !vpos' = case arg of
                     EValArg{}   -> vpos + 1
                     EValArgQL{} -> vpos + 1
                     _           -> vpos
          !pos'  = if user_vis arg then pos + 1 else pos
      did1 <- case arg of
        EValArg { ea_arg = prev_arg, ea_arg_ty = Scaled _ prev_dom0 }
          | isNothing (lookup vpos' cur_cache)
          , can_pre_infer prev_arg -> do
              prev_dom <- zonkAppTypeIfNeeded prev_dom0
              if not (anyFreeVarsOfType (`elemVarSet` m_tvs) prev_dom)
                then return False
                else do
                  (usage, (tc_prev, sub_w, p_dom')) <-
                    addArgCtxt pos' rn_head prev_arg $
                    tcCollectingUsage $ do
                      (tc_p, act_rho) <- tcInferRho prev_arg
                      p_dom <- zonkAppTypeIfNeeded prev_dom
                      (sub_wrap, eq_wc) <- captureConstraints $
                        tcSubTypeSigma (lexprCtOrigin prev_arg) GenSigCtxt act_rho p_dom
                      res_wc <- runTcSEqualities (simplifyTopWanteds eq_wc)
                      emitConstraints res_wc
                      p_dom_z <- zonkAppTypeIfNeeded p_dom
                      return (tc_p, sub_wrap, p_dom_z)
                  updTcRef state_ref $ \(hs, cs) ->
                    (hs, (vpos', (fmap (mkHsWrap sub_w) tc_prev, p_dom', usage)) : cs)
                  return True
        _ -> return False
      did_rest <- infer_matching_args cur_cache m_tvs pos' vpos' rest
      return (did1 || did_rest)

    user_vis EValArg{}                   = True
    user_vis EValArgQL{}                 = True
    user_vis ERetainedArg{}              = True
    user_vis (ETypeArg{ ea_loc_span = l }) = not (isGeneratedSrcSpan (locA l))
    user_vis _                           = False

    can_pre_infer (L _ e) = case e of
      HsPar _ inner -> can_pre_infer inner
      XExpr (ExpandedThingRn (HSE (ExprCtxt SectionL{}) _)) -> True
      XExpr (ExpandedThingRn (HSE (ExprCtxt SectionR{}) _)) -> True
      XExpr (ExpandedThingRn (HSE _ inner)) -> can_pre_infer inner
      HsVar{} -> True
      HsAppType _ inner _ -> can_pre_infer inner
      HsApp _ f _ -> can_pre_infer f
      OpApp{} -> True
      ExprWithTySig{} -> True
      _ -> False

    split_app_hd args (AppTy f a) = split_app_hd (a : args) f
    split_app_hd args t           = (t, args)

    motive_metas_for b = go
      where
        go ty
          | not (anyFreeVarsOfType (== b) ty) = Just emptyVarSet
          | (TyVarTy tv, sp@(_:_)) <- split_app_hd [] ty
          , isTcTyVar tv, isPromotableMetaTyVar tv
          , any (anyFreeVarsOfType (== b)) sp
          = Just (unitVarSet tv)
          | Just ty' <- coreView ty = go ty'
          | DepFunTy _ b' r <- ty, b' /= b
          = unionVarSet <$> go (varType b') <*> go r
          | FunTy _ ma mult d r <- ty
          = foldM (\s t -> unionVarSet s <$> go t) emptyVarSet [ma, mult, d, r]
          | ForAllTy ma (Bndr tv _) r <- ty
          = foldM (\s t -> unionVarSet s <$> go t) emptyVarSet [ma, tyVarKind tv, r]
          | TyConApp tc as <- ty, not (isTypeFamilyTyCon tc)
          = foldM (\s t -> unionVarSet s <$> go t) emptyVarSet as
          | otherwise = Nothing


-- | Recover an optional alias operand from its already checked RHS.
-- See Note [Optional typed let operands].
tcOptionalLetOperand :: LHsExpr GhcTc
                     -> TcM (Maybe (Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)]))
tcOptionalLetOperand rhs = runMaybeT $ do
    result@(operand, _, _) <- MaybeT (tcExprToRetainedCore RCM_OptionalLet rhs)
    _ <- MaybeT (pure (Core.retainedArgIndex operand))
    return result

-- | Recover checked function-composition alias steps from a 0-arg or 1-arg
-- lazy local binding. Any intermediate steps are returned outermost-first.
-- See Note [Optional typed let operands].
tcOptionalLetFunctionOperand
  :: Maybe Id
  -> LHsExpr GhcTc
  -> Type
  -> TcM (Maybe ([(Id, LHsExpr GhcTc, Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)])],
                 (Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)])))
tcOptionalLetFunctionOperand mb_param expr expected_ty = runMaybeT $ do
    strict <- liftMaybeT (xoptM LangExt.Strict)
    guard (not strict)
    expected <- liftMaybeT (liftZonkM (zonkTcType expected_ty))
    go_fun mb_param expr expected
  where
    liftMaybe = MaybeT . pure

    go_fun Nothing rhs expected
      | L _ (HsLam _ LamSingle MG { mg_alts = L _ [L _ Match
          { m_pats = L _ [pat], m_grhss = grhss }] }) <- strip_loc rhs
      , Just param <- lazy_var_pat pat
      , Just body <- unguardedRhs grhss
      = go_fun (Just param) body expected
      | otherwise = do
          compose <- lookup_compose
          funs <- collect_compose_chain compose rhs
          compose_chain compose Nothing funs expected
    go_fun (Just param) body expected = do
      compose <- lookup_compose
      funs <- eta_compose compose param body <|> collect_app_chain param body
      compose_chain compose (Just param) funs expected

    eta_compose compose param body
      | HsApp _ fun arg <- unLoc (strip_loc body)
      , HsVar _ (L _ v) <- unLoc (strip_loc arg)
      , v == param
      = collect_compose_chain compose fun
      | otherwise = mzero

    collect_app_chain param e = do
      funs <- go_app e
      guard (not (null (drop 1 funs)))
      return funs
      where
        go_app expr' = case unLoc (strip_loc expr') of
          HsApp _ fun arg -> case unLoc (strip_loc arg) of
            HsVar _ (L _ v) | v == param -> return [fun]
            _ -> (fun :) <$> go_app arg
          _ -> mzero

    collect_compose_chain compose e = case unLoc (strip_loc e) of
      HsApp _ fun2 arg2
        | HsApp _ op arg1 <- unLoc (strip_loc fun2) -> do
            is_comp <- is_compose_operator compose op
            guard is_comp
            lefts <- collect_compose_chain compose arg1 <|> pure [arg1]
            rights <- collect_compose_chain compose arg2 <|> pure [arg2]
            return (lefts ++ rights)
      _ -> mzero

    is_compose_operator compose op =
      (do { (core_op, _, methods_op) <- MaybeT (tcExprToRetainedCore RCM_OptionalLet op)
          ; guard (null methods_op)
          ; case Core.collectArgs core_op of
              (Core.Var fn, args) | fn == compose, all Core.isTypeArg args -> pure True
              _ -> mzero })
      <|>
      (do { dep_fn <- liftMaybe (peel_dep_compose_var (unLoc op))
          ; guard (isDependentComposeId dep_fn)
          ; pure True })

    peel_dep_compose_var :: HsExpr GhcTc -> Maybe Id
    peel_dep_compose_var (HsPar _ body) = peel_dep_compose_var (unLoc body)
    peel_dep_compose_var (ExprWithTySig _ body _) = peel_dep_compose_var (unLoc body)
    peel_dep_compose_var (XExpr (ExpandedThingTc (HSE _ body))) = peel_dep_compose_var (unLoc body)
    peel_dep_compose_var (XExpr (WrapExpr wrap body))
      | pure_type_or_cast_wrap wrap = peel_dep_compose_var body
    peel_dep_compose_var (HsAppType _ fun _) = peel_dep_compose_var (unLoc fun)
    peel_dep_compose_var (HsVar _ (L _ fn)) = Just fn
    peel_dep_compose_var _ = Nothing

    pure_type_or_cast_wrap WpHole           = True
    pure_type_or_cast_wrap (WpSubType w)    = pure_type_or_cast_wrap w
    pure_type_or_cast_wrap (WpCompose a b)  = pure_type_or_cast_wrap a && pure_type_or_cast_wrap b
    pure_type_or_cast_wrap WpTyApp{}        = True
    pure_type_or_cast_wrap WpCast{}         = True
    pure_type_or_cast_wrap _                = False

    lookup_compose = do
      env <- liftMaybeT getGblEnv
      guard (tcg_semantic_mod env /= gHC_INTERNAL_BASE)
      compose_raw <- MaybeT (lookupBootModuleIdMaybe gHC_INTERNAL_BASE composeIdOcc)
      tcOptionalLetCertified compose_raw

    compose_chain compose mb_param funs expected = do
      resolved <- forM funs $ \f_expr0 -> do
        let f_expr = strip_nominal_casts f_expr0
        (core_f, heads_f, methods_f) <- MaybeT (tcExprToRetainedCore RCM_OptionalLet f_expr)
        forM_ mb_param $ \param ->
          guard (not (param `elemVarSet` exprFreeVars core_f))
        return (f_expr, core_f, heads_f, methods_f)
      case reverse resolved of
        inner : outer : rest -> fold_steps compose expected [] inner outer rest
        _ -> mzero

    fold_steps compose expected acc inner outer rest = do
      (step_rhs, canonical, owners, methods, result_ty) <- compose_pair compose outer inner
      if null rest
        then do
          guard (result_ty `tcEqType` expected)
          return (reverse acc, (canonical, owners, methods))
        else do
          tmp <- liftMaybeT (newSysLocalId (fsLit "compose_alias") ManyTy result_ty)
          let step = (tmp, step_rhs, canonical, owners, methods)
          case rest of
            next : remaining ->
              fold_steps compose expected (step : acc)
                (step_rhs, Core.Var tmp, [], []) next remaining
            [] -> mzero

    compose_pair compose (orig_h, core_h, heads_h, methods_h)
                         (orig_f, core_f, heads_f, methods_f) = do
      h_ty <- liftMaybeT (liftZonkM (zonkTcType (exprType core_h)))
      f_ty <- liftMaybeT (liftZonkM (zonkTcType (exprType core_f)))
      (FTF_T_T, h_ma, h_mult, b_ty, c_ty) <- liftMaybe (splitFunTy_maybe h_ty)
      guard (h_ma `eqType` unmatchableDataConTy && isManyTy h_mult)
      (FTF_T_T, f_ma, f_mult, a_ty, b'_ty) <- liftMaybe (splitFunTy_maybe f_ty)
      guard (f_ma `eqType` unmatchableDataConTy && isManyTy f_mult)
      guard (b_ty `tcEqType` b'_ty)
      guard (tcIsLiftedTypeKind (typeKind a_ty)
             && tcIsLiftedTypeKind (typeKind b_ty)
             && tcIsLiftedTypeKind (typeKind c_ty))
      let canonical = Core.mkApps (Core.mkTyApps (Core.Var compose) [b_ty, c_ty, a_ty])
                        [core_h, core_f]
      index <- liftMaybe (Core.nativeIndexFunctionArgIndex canonical)
      let result_ty = mkVisFunTyManyU a_ty c_ty
      guard (typeKind index `tcEqType` result_ty && exprType canonical `tcEqType` result_ty)
      let compose_wrap = WpTyApp a_ty <.> WpTyApp c_ty <.> WpTyApp b_ty
          compose_var = noLocA (XExpr (WrapExpr compose_wrap (HsVar noExtField (noLocA compose))))
          step_rhs = noLocA (HsApp noExtField (noLocA (HsApp noExtField compose_var orig_h)) orig_f)
      return (step_rhs, canonical, compose : heads_h ++ heads_f, methods_h ++ methods_f, result_ty)

    is_nominal_cast_wrap :: HsWrapper -> Bool
    is_nominal_cast_wrap WpHole          = True
    is_nominal_cast_wrap (WpSubType w)   = is_nominal_cast_wrap w
    is_nominal_cast_wrap (WpCompose a b) = is_nominal_cast_wrap a && is_nominal_cast_wrap b
    is_nominal_cast_wrap (WpCast co)     = isJust (setNominalRole_maybe (coercionRole co) co)
    is_nominal_cast_wrap _               = False

    drop_nominal_casts :: HsWrapper -> Maybe HsWrapper
    drop_nominal_casts WpHole          = Just WpHole
    drop_nominal_casts (WpSubType w)   = drop_nominal_casts w
    drop_nominal_casts (WpCast co)
      | isJust (setNominalRole_maybe (coercionRole co) co) = Just WpHole
      | otherwise = Nothing
    drop_nominal_casts w@WpTyApp{}     = Just w
    drop_nominal_casts w@WpEvApp{}     = Just w
    drop_nominal_casts (WpCompose a b) = (<.>) <$> drop_nominal_casts a <*> drop_nominal_casts b
    drop_nominal_casts _               = Nothing

    strip_nominal_casts :: LHsExpr GhcTc -> LHsExpr GhcTc
    strip_nominal_casts orig = case strip_loc orig of
      L loc (XExpr (WrapExpr wrap body))
        | Just wrap' <- drop_nominal_casts wrap ->
            case wrap' of
              WpHole -> strip_nominal_casts (L loc body)
              _      -> L loc (XExpr (WrapExpr wrap' body))
      stripped -> stripped

    strip_loc :: LHsExpr GhcTc -> LHsExpr GhcTc
    strip_loc original@(L loc e) = case e of
      HsPar _ body -> strip_loc body
      ExprWithTySig _ body _ -> strip_loc body
      XExpr (ExpandedThingTc (HSE _ body)) -> strip_loc body
      XExpr (WrapExpr wrap body)
        | is_nominal_cast_wrap wrap -> strip_loc (L loc body)
      _ -> original

    lazy_var_pat :: LPat GhcTc -> Maybe Id
    lazy_var_pat (L _ (ParPat _ pat)) = lazy_var_pat pat
    lazy_var_pat (L _ (VarPat _ (L _ var))) = Just var
    lazy_var_pat _ = Nothing

tcOptionalLetCertified :: Id -> MaybeT TcM Id
tcOptionalLetCertified actual = do
  env <- liftMaybeT getGblEnv
  owner <- MaybeT (pure (sourceTableOccurrence env actual))
  _ <- MaybeT (pure (sourceTableId_maybe owner))
  owner <- liftMaybeT (liftZonkM (zonkId owner))
  guard (varType owner `tcEqType` varType actual)
  return owner

{- Note [Optional typed let operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary let RHS has already selected its syntax, type applications and
class dictionary Wanteds. Recover those exact choices without checking the
source again. Replace a method by a certified target only provisionally;
the caller must guard the alias equality with the original Wanted evidence.
A literal must retain its fromInteger witness and exact Integer payload.
Primitive literal short cuts supply no such evidence and are not candidates.

Read the current source-table owner without registering an equation use.
The caller checks rewrite rules before offering the alias equality and records
uses only if its proof is used. Reject unsupported wrappers and expressions
without a diagnostic.
Finally, retainedArgIndex checks the saturated canonical tree and its domains.
This operation emits no constraints, usage obligations or method checks.
-}

{- Note [Applying a scoped dictionary]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Use declared value domains to fix erased parameters before choosing any
dictionary. Use the declaration path also with ImpredicativeTypes.
Quick Look cannot assign a type with this private evidence prefix.
The declaration pass reads through the evidence prefix. It keeps the
dictionaries bound and cannot infer an argument from a selector
application in the result. A dictionary in an expected result must
match the identity and predicate of an actual lexical Given.

A signature can also have ordinary constraints and payload parameters.
Select the parameters of every dependent dictionary before opening it.
Instantiate remaining parameters in dependency order. For example, in
(Inspect r, Label a b) => foreach (x :: r) -> a -> Payload (ordering x),
the receiver fixes r. The ordinary solver can use the functional
dependency a -> b to determine b.

At the call, prefer the unique lexical Given with the exact predicate.
A Wanted for the same predicate can produce a fresh let-bound alias.
That alias would change the index and could escape its binder.

The Core index currently represents a selector on a variable operand.
Reject a constructor or other non-variable before substituting it.

A closed predicate or unique superclass projection with no direct Given uses
a Wanted whose evidence variable carries 'neverInlinePragma'. Its fresh
evidence ID belongs to the existing evidence bindings and remains a variable
in 'ClassAppTy' during 'SimpleOpt' and 'Simplifier' passes. An ordinary
result can consume the indexed call while the ID is in scope. The
retained-index escape checks still apply if the ID occurs in the result.
Disallow deferred type errors on this path. The source prototype cannot
assert that an unsolved predicate supplies a class dictionary.

A constructor pattern can instantiate its dictionary with type metavariables
that the scrutinee has already fixed. Zonk the predicates before comparing
them. Keep the original evidence Id and reject competing lexical Givens
before asking the ordinary solver for a ground instance.
-}

{- Note [Typed holes in retained applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Interactive proof development places typed holes inside retained applications:

1. Direct retained holes: `plusComm _ _` passes a hole expression (`_`, `_x`,
   `(_ :: Nat)`, or `Succ _`) to a `foreach (x :: D) ->` binder. The expression
   has no closed or runtime index, so `tcRetainedOperand` cannot build a Core
   index for it. When deferred hole errors are disabled, allocate a fresh
   meta-variable `h :: D` for the retained binder, substitute `h` for
   `TermVarTy binder` in the residual body via `replaceTermVarInType`, and emit
   `ERetainedArg`. Step 6 then checks the hole expression at `D` and reports
   `[GHC-88464]` with the local value environment and valid hole fits. When
   `-fdefer-typed-holes`, `-fdefer-type-errors`, or
   `-fdefer-out-of-scope-variables` is active, keep rejecting non-vacuous
   retained holes with `[GHC-83605]` so Core desugaring never places an
   unresolved hole dictionary in a dependent `Core.App`.

   In Step 5 (`tcResolveRetainedHoleResult`), match the instantiated result
   type structurally against the expected type (including identical type-family
   applications such as `T:plus h1 h2` against `T:plus m k`) to substitute any
   determined `h` with in-scope retained indices. If an unfilled `h` still
   blocks a family subterm while the outer generative skeleton matches the
   expected type, unify the `h`-free subterms and suppress the redundant
   result-mismatch constraint so `[GHC-88464]` is reported cleanly.

2. Enclosing ordinary applications with hole-bearing operands:
   * Skip pre-inferring arguments that contain an expression hole in
     `tcRetainedArgInstCachedWith` (`infer_args`). Pre-inferring `cong Succ _`
     before `trans` supplies its expected type allocates premature fresh
     meta-variables that conflict with the native expected result.
   * Zonk `fun_ty` before post-prefix result matching in `tcInstFunWithPrefix`
     so assignments made by `ITVDQ` (such as `Succ` fixing `a := Nat` in
     `cong Succ _`) are visible to `tcRetainedResultInstWithCasts` and
     `tcRetainedPartialResultPlan`.
   * Allow `tcRetainedArgChoices` and `tcRetainedPartialResultPlan` to allocate
     fresh meta-variables for undetermined parameters whose substituted kind is
     any native-free tau kind (such as `Nat` in `trans _ _` or `trans _ (cong Succ _)`),
     preserving the known native endpoints from the expected result.
-}

-- | Test whether any flag that defers typed holes or unbound variables to
-- runtime is enabled.
anyDeferHoleFlags :: TcM Bool
anyDeferHoleFlags =
  or <$> sequence [ goptM Opt_DeferTypedHoles
                  , goptM Opt_DeferTypeErrors
                  , goptM Opt_DeferOutOfScopeVariables ]

-- | Expose user-named retained pattern binders on the 'TcBinderStack' while
-- typechecking a hole-bearing argument so hole diagnostics report them in
-- relevant bindings and valid hole fits.
-- See Note [Typed holes in retained applications].
tcWithRetainedHoleScope :: LHsExpr GhcRn -> TcM a -> TcM a
tcWithRetainedHoleScope larg thing_inside
  | not (exprContainsHole larg) = thing_inside
  | otherwise = do
      err_ctxts <- getErrCtxt
      case [ unLoc (m_pats m) | MatchInCtxt m <- err_ctxts ] of
        match_pats : _ -> do
          lcl_env <- getLclEnv
          givens <- getNativeGivens
          let type_env = getLclEnvTypeEnv lcl_env
              stack_names = mkNameSet [ idName b | TcIdBndr b _ <- getLclEnvBinderStack lcl_env ]
              refined_ids = mkVarSet
                [ v
                | cv <- givens
                , isCoVar cv
                , Just (_, lhs, rhs) <- [getEqPredTys_maybe (varType cv)]
                , v <- maybeToList (termVar_maybe lhs) ++ maybeToList (termVar_maybe rhs) ]
              sys_ids = sortOn (getKey . getUnique)
                [ b
                | ATcId { tct_id = b } <- nonDetNameEnvElts type_env
                , isSystemName (idName b)
                , not (isUnderscore (nameOccName (idName b)))
                , isNativeOpaqueValueId b || isNativeIndexFunctionId b
                , not (idName b `elemNameSet` stack_names)
                , not (b `elemVarSet` refined_ids) ]
              missing_pat_nms =
                [ nm
                | p <- filter isVisArgLPat match_pats
                , Just nm <- [plain_var_pat p]
                , not (isSystemName nm)
                , not (isUnderscore (nameOccName nm))
                , not (nm `elemNameEnv` type_env) ]
              paired
                | equalLength missing_pat_nms sys_ids
                = zip missing_pat_nms sys_ids
                | otherwise
                = [ (nm, b)
                  | nm <- missing_pat_nms
                  , Just b <- [find (\s -> nameOccName (idName s) == nameOccName nm) sys_ids] ]
              restored =
                [ setVarName b (mkInternalName (getUnique b) (nameOccName nm) (nameSrcSpan nm))
                | (nm, b) <- paired ]
          if null restored
            then thing_inside
            else tcExtendIdEnv restored thing_inside
        [] -> thing_inside
  where
    termVar_maybe (TermVarTy v) = Just v
    termVar_maybe _             = Nothing

    plain_var_pat (L _ (ParPat _ inner))   = plain_var_pat inner
    plain_var_pat (L _ (BangPat _ inner))  = plain_var_pat inner
    plain_var_pat (L _ (LazyPat _ inner))  = plain_var_pat inner
    plain_var_pat (L _ (VarPat _ (L _ n))) = Just n
    plain_var_pat _                        = Nothing

-- | Test whether a renamed value argument contains an expression hole (`_` or
-- an unbound `_x` / variable hole) outside embedded type arguments.
-- See Note [Typed holes in retained applications].
exprContainsHole :: LHsExpr GhcRn -> Bool
exprContainsHole (L _ expr) = case expr of
  HsHole _ -> True
  HsPar _ inner -> exprContainsHole inner
  HsPragE _ _ inner -> exprContainsHole inner
  ExprWithTySig _ inner _ -> exprContainsHole inner
  HsApp _ fun arg -> exprContainsHole fun || exprContainsHole arg
  HsAppType _ fun _ -> exprContainsHole fun
  OpApp _ lhs op rhs -> exprContainsHole lhs || exprContainsHole op || exprContainsHole rhs
  NegApp _ inner _ -> exprContainsHole inner
  SectionL _ lhs op -> exprContainsHole lhs || exprContainsHole op
  SectionR _ op rhs -> exprContainsHole op || exprContainsHole rhs
  ExplicitTuple _ args _ -> any (\case Present _ e -> exprContainsHole e; _ -> False) args
  ExplicitList _ items -> any exprContainsHole items
  XExpr (ExpandedThingRn (HSE { hse_exp = inner })) -> exprContainsHole inner
  _ -> False

-- | Substitute a fresh placeholder meta-variable for a non-vacuous retained
-- binder when its argument contains a typed hole and hole deferral is off.
-- See Note [Typed holes in retained applications].
tcRetainedHoleOperand :: Maybe (TcRef ([TcTyVar], RetainedArgCache)) -> Type -> Id -> Type -> LHsExpr GhcRn
                      -> TcM (LHsExpr GhcRn, Type, Maybe CoercionN)
tcRetainedHoleOperand retained_holes_ref fun_ty binder body arg
  | isVacuousRetainedBinder binder body
  = return (arg, body, Nothing)
  | otherwise
  = do { defer_holes <- anyDeferHoleFlags
       ; if defer_holes
         then do { (core_arg, _, _) <- tcRetainedOperand binder body arg
                 ; res <- retainedAppResult fun_ty binder core_arg
                 ; return (arg, res, Nothing) }
         else do { tcPreUnifyRetainedHoleCon binder arg
                 ; fun_ty' <- liftZonkM (zonkTcType fun_ty)
                 ; (binder', body') <- case coreFullView fun_ty' of
                     DepFunTy _ b res -> return (b, res)
                     _ -> do { dom' <- liftZonkM (zonkTcType (varType binder))
                             ; res' <- liftZonkM (zonkTcType body)
                             ; return (setVarType binder dom', res') }
                 ; hole_tv <- newFlexiTyVar (varType binder')
                 ; forM_ retained_holes_ref $ \ref ->
                     updTcRef ref (\(hs, cs) -> (hole_tv : hs, cs))
                 ; let res_ty = replaceTermVarInType binder' (mkTyVarTy hole_tv) body'
                 ; return (arg, res_ty, Nothing) } }

-- | Pre-unify flexible meta-variables in a retained binder's domain when the
-- hole-bearing argument is headed by a known data constructor (e.g. `Succ _`).
tcPreUnifyRetainedHoleCon :: Id -> LHsExpr GhcRn -> TcM ()
tcPreUnifyRetainedHoleCon binder arg
  | anyFreeVarsOfType (\v -> isTcTyVar v && isMetaTyVar v) (varType binder)
  = do { (rn_fun, rn_args) <- splitHsApps (unLoc (stripParensLHsExpr arg))
       ; case rn_fun of
           HsVar _ lname
             | isDataOcc (nameOccName (unLocWithUserRdr lname))
             , any isHsValArg rn_args -> do
                 con <- tcLookupConLike (unLoc lname)
                 case con of
                   RealDataCon dc
                     | null (dataConRepArgBinders dc)
                     , count isHsValArg rn_args == dataConSourceArity dc -> do
                         (subst, _) <- newMetaTyVars (dataConUnivTyVars dc)
                         let res_ty = Subst.substTy subst (dataConOrigResTy dc)
                         when (typeKind res_ty `tcEqType` typeKind (varType binder)) $
                           void (unifyType Nothing res_ty (varType binder))
                   _ -> return ()
           _ -> return () }
  | otherwise = return ()

-- | Resolve placeholder hole variables in an application's result type against
-- the expected result type and suppress cascading mismatch errors on stuck
-- hole-dependent family subterms.
-- See Note [Typed holes in retained applications].
tcResolveRetainedHoleResult
  :: HsExpr GhcRn -> QLFlag -> [TcTyVar] -> [HsExprArg 'TcpInst]
  -> TcRhoType -> ExpRhoType
  -> TcM (TcRhoType, [HsExprArg 'TcpInst], Maybe HsWrapper)
tcResolveRetainedHoleResult rn_expr NoQL retained_holes inst_args app_res_rho (Check expected)
  | not (null retained_holes)
  = do { actual0 <- liftZonkM (zonkTcType app_res_rho)
       ; expected0 <- liftZonkM (zonkTcType expected)
       ; let hole_set = mkVarSet retained_holes
             raw_map0 = matchRetainedHoleVars hole_set Map.empty actual0 expected0
             missing0 = [ tv | tv <- retained_holes
                             , tv `elemVarSet` tyCoVarsOfType actual0
                             , not (Map.member tv raw_map0) ]
       ; raw_map <- if null missing0 || (isFamFreeTy actual0 && isFamFreeTy expected0)
                    then return raw_map0
                    else do
                      { loc <- getCtLocM (exprCtOrigin rn_expr) (Just TypeLevel)
                      ; actual_red <- reductionReducedType <$> tcRewriteFamilyAxioms loc actual0
                      ; expected_red <- reductionReducedType <$> tcRewriteFamilyAxioms loc expected0
                      ; return (matchRetainedHoleVars hole_set raw_map0 actual_red expected_red) }
       ; let in_scope = mkInScopeSet (tyCoVarsOfTypes (actual0 : expected0 : Map.elems raw_map))
             step_subst subst tv = case Map.lookup tv raw_map of
               Just ty
                 | not (tv `elemVarSet` tyCoVarsOfType ty)
                 , noQuickLookVars ty
                 , typeKind ty `eqType` Subst.substTy subst (tyVarKind tv) -> do
                     ok <- tcRetainedInstScope [ty]
                     return $ if ok
                              then Subst.extendTvSubstAndInScope subst tv ty
                              else subst
               _ -> return subst
       ; subst <- foldM step_subst (Subst.mkEmptySubst in_scope) (reverse retained_holes)
       ; let actual1 = Subst.substTy subst actual0
             inst_args1 = substInstArgHoles subst inst_args
             still_has_hole = anyFreeVarsOfType (`elemVarSet` hole_set) actual1
       ; defer_holes <- anyDeferHoleFlags
       ; if still_has_hole && not defer_holes && isTauTy expected0
            && compatibleHoleSkeleton hole_set actual1 expected0
         then do { unifyNonHoleSubterms hole_set actual1 expected0
                 ; co_hole <- newCoercionHole (mkNomEqPred actual1 expected0)
                 ; return (actual1, inst_args1, Just (mkWpCastN (mkHoleCo co_hole))) }
         else return (actual1, inst_args1, Nothing) }
tcResolveRetainedHoleResult _ _ _ inst_args app_res_rho _
  = return (app_res_rho, inst_args, Nothing)

-- | Match placeholder hole variables structurally, including arguments of
-- identical type-family heads.
matchRetainedHoleVars :: VarSet -> Map.Map TcTyVar Type -> Type -> Type -> Map.Map TcTyVar Type
matchRetainedHoleVars hole_set acc formal actual
  | Just tv <- getTyVar_maybe formal
  , tv `elemVarSet` hole_set
  = case Map.lookup tv acc of
      Nothing -> Map.insert tv actual acc
      Just prev | prev `eqType` actual -> acc
                | otherwise -> acc
  | Just (tc1, as1) <- tcSplitTyConApp_maybe formal
  , Just (tc2, as2) <- tcSplitTyConApp_maybe actual
  , tc1 == tc2, equalLength as1 as2
  = foldl' (\m (f, a) -> matchRetainedHoleVars hole_set m f a) acc (zip as1 as2)
  | FunTy af1 ma1 _ d1 r1 <- coreFullView formal
  , FunTy af2 ma2 _ d2 r2 <- coreFullView actual
  , af1 == af2, eqType ma1 ma2
  = let acc1 = matchRetainedHoleVars hole_set acc d1 d2
    in matchRetainedHoleVars hole_set acc1 r1 r2
  | Just (f1, a1) <- tcSplitAppTyNoView_maybe formal
  , Just (f2, a2) <- tcSplitAppTyNoView_maybe actual
  = let acc1 = matchRetainedHoleVars hole_set acc f1 f2
    in matchRetainedHoleVars hole_set acc1 a1 a2
  | otherwise = acc

-- | Apply a resolved retained-hole substitution to instantiated call arguments.
substInstArgHoles :: Subst.Subst -> [HsExprArg 'TcpInst] -> [HsExprArg 'TcpInst]
substInstArgHoles subst = map subst_arg
  where
    subst_scaled (Scaled m t) = Scaled (Subst.substTy subst m) (Subst.substTy subst t)
    subst_arg (EValArg loc ty arg) = EValArg loc (subst_scaled ty) arg
    subst_arg (ERetainedArg vis loc ty arg res_ty co mb_tc) =
      ERetainedArg vis loc (subst_scaled ty) arg (Subst.substTy subst res_ty) co mb_tc
    subst_arg arg@ETypeArg{ ea_ty_arg = ty } = arg { ea_ty_arg = Subst.substTy subst ty }
    subst_arg arg@EValArgQL{ eaql_arg_ty = ty, eaql_args = inner, eaql_res_rho = rho } =
      arg { eaql_arg_ty = subst_scaled ty
          , eaql_args = substInstArgHoles subst inner
          , eaql_res_rho = Subst.substTy subst rho }
    subst_arg arg = arg

-- | Check that every outer generative 'TyCon' in 'actual' agrees with
-- 'expected', allowing differences only under hole-dependent subterms.
compatibleHoleSkeleton :: VarSet -> Type -> Type -> Bool
compatibleHoleSkeleton hole_set actual expected
  | not (anyFreeVarsOfType (`elemVarSet` hole_set) actual) = True
  | Just tv <- getTyVar_maybe actual, tv `elemVarSet` hole_set = True
  | Just (tc1, _) <- tcSplitTyConApp_maybe actual
  , isTypeFamilyTyCon tc1 = True
  | Just (tc1, as1) <- tcSplitTyConApp_maybe actual
  , isGenerativeTyCon tc1 Nominal
  = case tcSplitTyConApp_maybe expected of
      Just (tc2, as2) ->
        tc1 == tc2 && equalLength as1 as2
        && and (zipWith (compatibleHoleSkeleton hole_set) as1 as2)
      Nothing -> False
  | FunTy af1 ma1 _ d1 r1 <- coreFullView actual
  , FunTy af2 ma2 _ d2 r2 <- coreFullView expected
  = af1 == af2 && eqType ma1 ma2
    && compatibleHoleSkeleton hole_set d1 d2
    && compatibleHoleSkeleton hole_set r1 r2
  | otherwise = True

-- | Unify sibling subterms that do not mention any unfilled retained-hole
-- placeholder variable.
unifyNonHoleSubterms :: VarSet -> Type -> Type -> TcM ()
unifyNonHoleSubterms hole_set actual expected
  | not (anyFreeVarsOfType (`elemVarSet` hole_set) actual)
  = when (typeKind actual `tcEqType` typeKind expected) $
      void (unifyType Nothing actual expected)
  | Just (tc1, as1) <- tcSplitTyConApp_maybe actual
  , Just (tc2, as2) <- tcSplitTyConApp_maybe expected
  , tc1 == tc2, isGenerativeTyCon tc1 Nominal, equalLength as1 as2
  = zipWithM_ (unifyNonHoleSubterms hole_set) as1 as2
  | FunTy af1 ma1 _ d1 r1 <- coreFullView actual
  , FunTy af2 ma2 _ d2 r2 <- coreFullView expected
  , af1 == af2, eqType ma1 ma2
  = do { unifyNonHoleSubterms hole_set d1 d2
       ; unifyNonHoleSubterms hole_set r1 r2 }
  | otherwise = return ()

-- | Check a retained operand once at its binder's scalar or function domain,
-- returning its Core representation, checked syntax, and collected usage.
-- See Note [Retained argument application].
tcRetainedOperand :: Id -> Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc, UsageEnv)
tcRetainedOperand binder body arg = withAppUnpackState Nothing $ do
  result@(core_arg, _, _) <- if isNativeIndexFunctionId binder
    then tcRetainedFunctionArg domain arg
    else tcRetainedArgScaled (Scaled (idMult binder) domain) arg
  case core_arg of
    Core.Var index | isNativeOpaqueValueId index -> return result
    _ | isScopedSelectorReceiver binder body -> setSrcSpanA (getLoc arg) $
          failWithTc (TcRnRetainedQuantificationError RetainedSelectorReceiver)
      | otherwise -> return result
  where
    domain = varType binder

-- | Zonk a retained binder's domain when operand checking may have filled
-- metavariables in it.
-- See Note [Pre-unifying polymorphic retained domains in inference mode].
zonkRetainedBinderDomain :: Id -> TcM Type
zonkRetainedBinderDomain binder
  | anyFreeVarsOfType (\v -> isTcTyVar v && isMetaTyVar v) dom
  = liftZonkM (zonkTcType dom)
  | otherwise = return dom
  where
    dom = varType binder

-- | Use the same domain for index checking and codomain substitution.
-- See Note [Wildcards in retained operand domains].
retainedAppResult :: Type -> Id -> Core.CoreExpr -> TcM Type
retainedAppResult fun_ty binder arg
  | anyFreeVarsOfType (\v -> isTcTyVar v && isMetaTyVar v) (varType binder)
  = do { fun_ty' <- liftZonkM (zonkTcType fun_ty)
       ; return (applyTypeToArgs fun_ty' [arg]) }
  | otherwise = return (applyTypeToArgs fun_ty [arg])

-- | Check a value operand once and specialise a class selector to its checked method.
-- See Note [Dictionary selection in retained arguments].
tcRetainedValueOperand
  :: (LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc))
  -> (Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc))
  -> Type -> LHsExpr GhcRn
  -> TcM (Core.CoreExpr, LHsExpr GhcTc)
tcRetainedValueOperand resolve resolve_scoped_arg domain arg@(L loc original)
  | (op, args@(_:_)) <- split_call arg []
  , let (L head_loc head_expr, type_arg) = case op of
          L _ (HsAppType _ f ty) -> (stripParensLHsExpr f, Just ty)
          _ -> (op, Nothing)
  , HsVar _ lname <- head_expr
  , let name = unLocWithUserRdr lname
  , isExternalName name, isVarOcc (nameOccName name)
  = setSrcSpanA head_loc $ do
      mb_sel <- tcLookupIdMaybe name
      case mb_sel of
        Just sel | Just cls <- isClassOpId_maybe sel -> do
          let unchecked :: TcM a
              unchecked = failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall name))
          scoped <- tcScopedClassOperand domain arg
          case scoped of
            Just (selector, inst, dict_expr, index, operand) -> do
              (_, checked_operand) <- tcScalingUsage ManyTy (resolve_scoped_arg inst operand)
              let wrapper = mkWpEvApps [EvExpr dict_expr] <.> mkWpTyApps [inst]
                  method  = noLocA (mkHsWrap wrapper (mkHsVar (noLocA selector)))
                  call_tc = mkHsApps method [checked_operand]
              pure (Core.mkApps (Core.Var selector) [Core.Type inst, dict_expr, Core.Var index],
                    L loc (unLoc call_tc))
            Nothing -> do
              case tcSplitSigmaTy (varType sel) of
                ([(_, tv)], [theta], rho)
                  | let (_, _, result) = splitFunTys rho
                  , result `tcEqType` mkTyVarTy tv
                  , theta `tcEqType` mkClassPred cls [mkTyVarTy tv]
                  , noFreeVarsOfType domain, typeKind domain `tcEqType` tyVarKind tv -> return ()
                _ -> unchecked
              forM_ type_arg $ \ty -> do
                (supplied, _) <- tcVTA (idConcreteTvs sel) (varType sel) Nothing ty
                unless (supplied `tcEqType` domain) unchecked
              dfun <- tcRetainedDFun name cls domain >>= maybe unchecked return
              platform <- getPlatform
              case args of
                [operand]
                  | Just (zero, successor) <- sourceLiteralCons_maybe dfun sel
                  , HsOverLit _ (OverLit OverLitRn{ol_rebindable = False} literal)
                      <- unLoc (stripParensLHsExpr operand)
                  , Just (HsLit _ (XLit (HsInteger _ n _)))
                      <- shortCutLit platform literal integerTy
                  , n >= 0
                  -> do { (core, generated) <- buildRetainedNumeral dfun sel zero successor n
                        ; return (core, L loc (XExpr (ExpandedThingTc (HSE (ExprCtxt original) generated)))) }
                _ -> do
                  target_name <- maybe unchecked return (sourceMethodTarget_maybe dfun sel)
                  target <- tcLookupIdMaybe target_name >>= maybe unchecked return
                  unless (varType target `tcEqType` classMethodInstTy sel [domain]) unchecked
                  let target_head = L head_loc (HsVar noExtField (fmap (const (noUserRdr target_name)) lname))
                  (core, checked_call) <- resolve (mkHsApps target_head args)
                  recordSourceMethodUse dfun sel target
                  wantRetainedMethod sel dfun
                  return (core, L loc (XExpr (ExpandedThingTc (HSE (ExprCtxt original) checked_call))))
        _ -> ordinary
  | otherwise = ordinary
  where
    ordinary = resolve arg
    split_call :: LHsExpr GhcRn -> [LHsExpr GhcRn] -> (LHsExpr GhcRn, [LHsExpr GhcRn])
    split_call (L _ (HsPar _ e)) args = split_call e args
    split_call (L _ (HsApp _ f a)) args = split_call f (a:args)
    split_call (L _ (OpApp _ a op b)) args = split_call op (a:b:args)
    split_call e args = (e, args)

-- | Emit a Wanted class dictionary whose evidence binder stays let-bound as a
-- variable in 'ClassAppTy' indices across 'SimpleOpt' and 'Simplifier'.
-- See Note [Applying a scoped dictionary].
newScopedDictWanted :: CtOrigin -> PredType -> TcM EvVar
newScopedDictWanted orig pred_ty = do
  loc <- getCtLocM orig Nothing
  raw_ev <- newEvVar pred_ty
  let ev_id = setInlinePragma raw_ev neverInlinePragma
      ct_ev = CtWanted (mkWantedCt loc emptyCoHoleSet pred_ty (EvVarDest ev_id))
  emitSimple (mkNonCanonical ct_ev)
  pure ev_id

-- | Resolve a direct selector call to the exact Given (or unique superclass
-- projection) used in its index.
-- See Note [Scoped Given indices] and
-- Note [Superclass projection for scoped class selectors] in "GHC.Core".
tcScopedClassOperand :: Type -> LHsExpr GhcRn
                     -> TcM (Maybe (Id, Type, Core.CoreExpr, Id, LHsExpr GhcRn))
tcScopedClassOperand result_ty call
  | HsApp _ method operand <- unLoc (stripParensLHsExpr call)
  , HsVar _ method_name <- unLoc (stripParensLHsExpr method)
  , HsVar _ operand_name <- unLoc (stripParensLHsExpr operand)
  = do { enabled <- xoptM LangExt.DependentTypes
       ; if not enabled then return Nothing else do
       { selector <- tcLookupIdMaybe (unLocWithUserRdr method_name)
       ; actual <- tcLookupLcl_maybe (unLocWithUserRdr operand_name)
       ; index_id <- case actual of
           Just ATcId{tct_id = index} -> Just <$> liftZonkM (zonkId index)
           _ -> return Nothing
       ; givens <- liftZonkM . mapM zonkId =<< getLexicalGivens
       ; case (selector, index_id) of
           (Just sel, Just index)
             | isNativeOpaqueValueId index
             , let inst = varType index
             , Just (predicate, domain, result) <- Core.nativeClassSelectorTys sel inst
             , inst `tcEqType` domain && result `tcEqType` result_ty ->
               case Core.projectUniqueClassDict predicate givens of
                 Core.ScopedDirectDict dict ->
                   check_core sel inst (Core.Var dict) index operand
                 Core.ScopedSuperclassDict _ sc_expr ->
                   check_core sel inst sc_expr index operand
                 _ -> return Nothing
           _ -> return Nothing } }
  where
    check_core sel inst dict_expr index op =
      let core = Core.mkApps (Core.Var sel) [Core.Type inst, dict_expr, Core.Var index]
      in pure $ case Core.nativeClassSelectorIndex core of
           Just _  -> Just (sel, inst, dict_expr, index, op)
           Nothing -> Nothing
tcScopedClassOperand _ _ = return Nothing

{- Note [Scoped Given indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained arrow result can mention a direct class Given (or unique superclass
projection). Resolve the selector to that exact lexical dictionary variable in
both type instantiation and operand checking. Never introduce a fresh Wanted
dictionary let-binding, which would alter the index identity and escape scope.
-}

-- | Look up a completed, ground instance with no context.
tcRetainedDFun :: Name -> Class -> Type -> TcM (Maybe Id)
tcRetainedDFun method cls domain = do
  insts <- tcGetInstEnvs
  case lookupUniqueInstEnv insts cls [domain] of
    Right (inst, []) -> do
      env <- getGblEnv
      let dfun_name = getName (instanceDFunId inst)
      when (nameIsLocalOrFrom (tcg_semantic_mod env) dfun_name) $
        failWithTc (TcRnRetainedQuantificationError (RetainedLocalSourceMethod method))
      tcLookupIdMaybe dfun_name
    _ -> return Nothing

-- | Require the ordinary solver to select this dictionary at the use site.
wantRetainedMethod :: Id -> Id -> TcM ()
wantRetainedMethod sel dfun = do
  let name = getName sel
  ev <- newWanted (RetainedMethodOrigin name) Nothing (varType dfun)
  emitSimple (mkNonCanonical ev)
  emitDelayedErrors (unitBag (DE_SourceMethod (ctEvEvId ev) dfun name (ctEvLoc ev)))

-- | Elaborate a nonnegative literal at a known retained domain in one pass.
-- See Note [Retained integral literals].
tcRetainedNumeral :: Type -> Name -> Integer -> TcM (Core.CoreExpr, LHsExpr GhcTc)
tcRetainedNumeral domain from n = do
  let unverified :: TcM a
      unverified = failWithTc (TcRnRetainedQuantificationError (RetainedLiteralConversion from domain))
  sel <- tcLookupIdMaybe from >>= maybe unverified return
  cls <- case isClassOpId_maybe sel of
    Just cls | cls `hasKey` numClassKey, noFreeVarsOfType domain -> return cls
    _ -> unverified
  dfun <- tcRetainedDFun from cls domain >>= maybe unverified return
  if isNaturalTy domain
    then do
      platform <- getPlatform
      wantRetainedMethod sel dfun
      return (mkNaturalExpr platform n, noLocA (HsLit noExtField (HsNatural noExtField (mkIntegralLit n))))
    else do
      (zero, succ_con) <- maybe unverified return (sourceLiteralCons_maybe dfun sel)
      buildRetainedNumeral dfun sel zero succ_con n

-- | Build the Core index and checked constructor expression from a nonnegative conversion.
buildRetainedNumeral :: Id -> Id -> DataCon -> DataCon -> Integer
                     -> TcM (Core.CoreExpr, LHsExpr GhcTc)
buildRetainedNumeral dfun sel zero succ_con n = do
  dflags <- getDynFlags
  when (n > toInteger (maxBound :: Int) || intGtLimit (fromInteger n) (reductionDepth dflags)) $
    failWithTc (TcRnRetainedQuantificationError (RetainedLiteralDepth n))
  recordSourceLiteralUse dfun sel
  wantRetainedMethod sel dfun
  let zero_tc = noLocA (XExpr (ConLikeTc (RealDataCon zero)))
      succ_tc = noLocA (XExpr (ConLikeTc (RealDataCon succ_con)))
      build :: Int -> (Core.CoreExpr, LHsExpr GhcTc)
      build 0 = (Core.Var (dataConWorkId zero), zero_tc)
      build k = let (val, src) = build (k - 1)
                in (Core.App (Core.Var (dataConWorkId succ_con)) val,
                    noLocA (HsApp noExtField succ_tc src))
  return (build (fromInteger n))

{- Note [Retained integral literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Elaborate a non-negative 'HsIntegral' literal (or non-rebound @fromInteger n@)
from a certified ground 'Num' instance's zero and successor constructors within
the 'reductionDepth' limit, or as a boxed 'HsNatural' literal at 'Natural'.
Emit 'DE_SourceMethod' so the solver verifies that the lexical scope selects
that same 'DFunId'.

Note [Dictionary selection in retained arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Specialise a unary-parameter ground class method head (such as @(+)@ on @Nat@)
to its certified 'sourceMethodTarget_maybe' target at each expected operand
domain, check the rewritten call once, and attach 'DE_SourceMethod' to a fresh
Wanted so a local Given dictionary cannot silently change the runtime method.
-}

-- | Construct a checked unrestricted lambda expression for synthesized helpers.
mkCheckedManyLam :: SrcSpan -> [Id] -> Type -> LHsExpr GhcTc -> LHsExpr GhcTc
mkCheckedManyLam loc param_ids res_ty body_tc =
  let sp       = noAnnSrcSpan loc
      sp_n     = l2l sp
      pats     = [L sp (VarPat noExtField (L sp_n v)) | v <- param_ids]
      match_tc = mkSimpleMatch (LamAlt LamSingle) (L sp pats) body_tc
      mg_tc    = MatchGroupTc [Scaled ManyTy (varType v) | v <- param_ids] res_ty
                   (Generated OtherExpansion SkipPmc)
  in L sp (HsLam noAnn LamSingle (MG { mg_alts = L sp [match_tc], mg_ext = mg_tc }))

-- | Check a lexical callback, certified source function, constructor, lambda,
-- or composition in one pass, returning its Core form, checked syntax, and usage.
-- See Notes [Checking a cast retained callback] and [Named retained callbacks].
tcRetainedFunctionArg :: Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc, UsageEnv)
tcRetainedFunctionArg domain arg = do
  (arg_ue, (core_arg, checked_tc)) <- tcCollectingUsage (tcCheckRetainedFunctionArgM domain arg)
  return (core_arg, checked_tc, arg_ue)

tcCheckRetainedFunctionArgM :: Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc)
tcCheckRetainedFunctionArgM domain arg = setSrcSpanA (getLoc arg) $
  case retainedSourceHead arg of
    Just (name, written)
      | isExternalName name -> do
          mb_actual <- tcLookupIdMaybe name
          mb_owner <- maybe (pure Nothing) tcLookupSourceTableId mb_actual
          case (mb_owner, mb_actual) of
            (Just owner, _) -> instantiate_named (getLoc arg) arg owner written
            (Nothing, Just actual)
              | null written, isNativeNamedOpaqueFunctionId actual -> do
                  expected <- liftZonkM (zonkTcType domain)
                  if any isMetaTyVar (tyCoVarsOfTypeList expected)
                    then void (unifyType Nothing (varType actual) expected)
                    else unless (varType actual `tcEqType` expected) unsupported
                  let a_loc = getLoc arg
                      tc_e  = L a_loc (HsVar noExtField (L (l2l a_loc) actual))
                  pure (Core.Var actual, tc_e)
            (Nothing, _)
              | null written -> do
                  mb_simple <- lookupLocalSimpleFunBind name
                  case mb_simple of
                    Just (mb_pat, rhs_rn) ->
                      tcSynthesizeRetainedFunctionArg domain (getLocA arg) mb_pat rhs_rn
                    Nothing -> failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall name))
            _ -> failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall name))
      | null written -> do
          thing <- tcLookupLcl_maybe name
          case thing of
            Just (ATcId { tct_id = actual }) -> do
              actual' <- liftZonkM (zonkId actual)
              tcEmitBindingUsage (singleUsageUE actual')
              core_arg <- tcRetainedFunctionVar domain actual'
              let a_loc   = getLoc arg
                  base_tc = L a_loc (HsVar noExtField (L (l2l a_loc) actual'))
                  arg_tc  = case core_arg of
                    Core.Cast _ (SubCo co) -> mkLHsWrap (WpCast (SubCo co)) base_tc
                    _                      -> base_tc
              pure (core_arg, arg_tc)
            _ -> unsupported
    _
      | Just (mb_pat, body_rn) <- split_simple_lam_or_eta (unLoc (stripParensLHsExpr arg)) ->
          tcSynthesizeRetainedFunctionArg domain (getLocA arg) mb_pat body_rn
      | otherwise -> do
          mb_comp <- split_compose_app (unLoc (stripParensLHsExpr arg))
          case mb_comp of
            Just (lhs, rhs) ->
              tcRetainedComposeSinglePass domain (getLocA arg) lhs rhs
            Nothing
              | HsApp{} <- unLoc (stripParensLHsExpr arg)
              , let stripped = stripParensLHsExpr arg ->
                  if isDataConAppHead stripped
                    then tcSynthesizeRetainedFunctionArg domain (getLocA arg) Nothing stripped
                    else check_prefix
              | OpApp{} <- unLoc (stripParensLHsExpr arg) ->
                  check_prefix
              | otherwise -> unsupported
  where
    check_prefix = tcCheckRetainedScalarArgM ManyTy Core.nativeIndexFunctionArgIndex domain arg

    unsupported :: TcM a
    unsupported = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument

    split_compose_app :: HsExpr GhcRn -> TcM (Maybe (LHsExpr GhcRn, LHsExpr GhcRn))
    split_compose_app (OpApp _ lhs op rhs) = do
      is_c <- is_any_compose_expr op
      pure (if is_c then Just (lhs, rhs) else Nothing)
    split_compose_app (HsApp _ (L _ (HsApp _ op lhs)) rhs) = do
      is_c <- is_any_compose_expr op
      pure (if is_c then Just (lhs, rhs) else Nothing)
    split_compose_app _ = pure Nothing

    split_simple_lam_or_eta :: HsExpr GhcRn -> Maybe (Maybe (LocatedN Name), LHsExpr GhcRn)
    split_simple_lam_or_eta (HsLam _ LamSingle MG { mg_alts = L _ [L _ Match
        { m_pats = L _ [pat]
        , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] body) NE.:| []
                          , grhssLocalBinds = L _ EmptyLocalBinds{} } }] })
      | Just mb_pat <- simple_lam_pat (unLoc pat)
      = Just (mb_pat, body)
    split_simple_lam_or_eta (HsVar _ lname)
      | isDataOcc (nameOccName (unLocWithUserRdr lname))
      = Just (Nothing, noLocA (HsVar noExtField lname))
    split_simple_lam_or_eta _ = Nothing

    simple_lam_pat (ParPat _ inner) = simple_lam_pat (unLoc inner)
    simple_lam_pat (VarPat _ lname) = Just (Just lname)
    simple_lam_pat WildPat{}        = Just Nothing
    simple_lam_pat _                = Nothing

    instantiate_named loc orig_fun owner0 written = do
      expected0 <- liftZonkM (zonkTcType domain)
      owner <- liftZonkM (zonkId owner0)
      (written_tys, sigma) <- foldM (applyRetainedSourceType unsupported owner)
                                    ([], varType owner) written
      let (binders, body) = tcSplitForAllInvisTyVars sigma
          non_meta t = pure $
            if any (\v -> isTcTyVar v && isMetaTyVar v) (tyCoVarsOfTypeList t)
            then Nothing
            else Just t
          body_clues = case (coreFullView body, coreFullView expected0) of
            (FunTy _ _ _ b_in b_out, FunTy _ _ _ e_in e_out) ->
              [(b_in, non_meta e_in), (b_out, non_meta e_out), (body, non_meta expected0)]
            _ -> [(body, non_meta expected0)]
      (choices, expected') <- case binders of
        [] -> do
          when (any (\v -> isTcTyVar v && isMetaTyVar v) (tyCoVarsOfTypeList expected0)) $
            tcPreUnifyMetaSkeleton Nothing emptyVarSet emptyVarSet expected0 body
          expected' <- liftZonkM (zonkTcType expected0)
          if eqType body expected' then return ([], expected') else unsupported
        _ -> do
          let tvs = map snd binders
          cs <- chooseRetainedSourceTypes unsupported owner sigma tvs body_clues
          let body_inst = substTyWith tvs cs body
          when (any (\v -> isTcTyVar v && isMetaTyVar v) (tyCoVarsOfTypeList expected0)) $
            tcPreUnifyMetaSkeleton Nothing emptyVarSet emptyVarSet expected0 body_inst
          expected' <- liftZonkM (zonkTcType expected0)
          return (cs, expected')
      let all_tys  = written_tys ++ choices
          head_tc  = mkSourceHeadTc loc orig_fun owner all_tys
          arg_core = Core.mkTyApps (Core.Var owner) all_tys
      case Core.nativeIndexFunctionArgIndex arg_core of
        Just index | typeKind index `tcEqType` expected' -> return (arg_core, head_tc)
        _ -> unsupported

-- | Build a type-applied checked head preserving record selector representation.
mkSourceHeadTc :: SrcSpanAnnA -> LHsExpr GhcRn -> Id -> [Type] -> LHsExpr GhcTc
mkSourceHeadTc loc orig_fun owner all_tys =
  let inner = case unLoc (strip_ty_apps orig_fun) of
        XExpr (HsRecSelRn (FieldOcc rdr (L fl_loc _))) | isRecordSelector owner ->
          XExpr (HsRecSelTc (FieldOcc rdr (L fl_loc owner)))
        _ -> HsVar noExtField (L (l2l loc) owner)
  in mkLHsWrap (mkWpTyApps all_tys) (L loc inner)
  where
    strip_ty_apps (L _ (HsPar _ e))       = strip_ty_apps e
    strip_ty_apps (L _ (HsAppType _ e _)) = strip_ty_apps e
    strip_ty_apps e                       = e

-- | Test whether an expression spine is headed by a data constructor.
isDataConAppHead :: LHsExpr GhcRn -> Bool
isDataConAppHead (L _ (HsPar _ inner))     = isDataConAppHead inner
isDataConAppHead (L _ (HsApp _ fun _))     = isDataConAppHead fun
isDataConAppHead (L _ (HsAppType _ fun _)) = isDataConAppHead fun
isDataConAppHead (L _ (HsVar _ lname))     = isDataOcc (nameOccName (unLocWithUserRdr lname))
isDataConAppHead _                         = False

-- | Check an inline composition @lhs . rhs@ passed to @foreach (f :: a -> c)@
-- in a single deterministic O(k) pass with one intermediate metavariable,
-- using certified 'GHC.Internal.Base.(.)' when available and falling back to a
-- 1-equation synthesized helper without re-typechecking either operand.
tcRetainedComposeSinglePass
  :: Type -> SrcSpan -> LHsExpr GhcRn -> LHsExpr GhcRn
  -> TcM (Core.CoreExpr, LHsExpr GhcTc)
tcRetainedComposeSinglePass raw_domain loc lhs rhs = do
  expected <- liftZonkM (zonkTcType raw_domain)
  case nativeIndexFunctionTypeTys_maybe expected of
    Nothing -> unsupported
    Just (arg_ty0, res_ty0) -> do
      arg_ty <- liftZonkM (zonkTcType arg_ty0)
      res_ty <- liftZonkM (zonkTcType res_ty0)
      when (any isMetaTyVar (tyCoVarsOfTypeList arg_ty)
            || not (tcIsLiftedTypeKind (typeKind arg_ty))
            || not (tcIsLiftedTypeKind (typeKind res_ty)))
        unsupported
      mid_ty <- newFlexiTyVarTy liftedTypeKind
      (rhs_ue, (_, rhs_tc)) <- tcCollectingUsage $
        tcCheckRetainedFunctionArgM (mkVisFunTyManyU arg_ty mid_ty) rhs
      mid_ty' <- liftZonkM (zonkTcType mid_ty)
      when (any isMetaTyVar (tyCoVarsOfTypeList mid_ty')
            || not (tcIsLiftedTypeKind (typeKind mid_ty'))
            || not (noQuickLookVars mid_ty'))
        unsupported
      (lhs_ue, (lhs_core, lhs_tc)) <- tcCollectingUsage $
        tcCheckRetainedFunctionArgM (mkVisFunTyManyU mid_ty' res_ty) lhs
      res_ty' <- liftZonkM (zonkTcType res_ty)
      when (any isMetaTyVar (tyCoVarsOfTypeList res_ty')
            || not (noQuickLookVars res_ty'))
        unsupported
      let final_expected = mkVisFunTyManyU arg_ty res_ty'
      mb_rhs_core <- tcExprToRetainedCore RCM_RetainedArg rhs_tc
      mb_canon <- case mb_rhs_core of
        Just (rhs_core, _, _) -> runMaybeT $ do
          env <- liftMaybeT getGblEnv
          guard (tcg_semantic_mod env /= gHC_INTERNAL_BASE)
          compose_raw   <- MaybeT (lookupBootModuleIdMaybe gHC_INTERNAL_BASE composeIdOcc)
          compose_owner <- MaybeT (tcLookupSourceTableId compose_raw)
          valid_mid     <- liftMaybeT (tcRetainedInstScope [mid_ty'])
          guard valid_mid
          let call_core = Core.mkApps (Core.mkTyApps (Core.Var compose_owner) [mid_ty', res_ty', arg_ty])
                            [lhs_core, rhs_core]
          idx <- MaybeT (pure (Core.nativeIndexFunctionArgIndex call_core))
          guard (typeKind idx `tcEqType` final_expected)
          liftMaybeT $ do
            recordLocalSynthFunId compose_owner
            tcEmitBindingUsage rhs_ue
            tcEmitBindingUsage lhs_ue
          let sp        = noAnnSrcSpan loc
              sp_n      = l2l sp
              comp_wrap = WpTyApp arg_ty <.> WpTyApp res_ty' <.> WpTyApp mid_ty'
              comp_head = L sp (mkHsWrap comp_wrap (HsVar noExtField (L sp_n compose_owner)))
              call_tc   = L sp (HsApp noExtField (L sp (HsApp noExtField comp_head lhs_tc)) rhs_tc)
          pure (call_core, call_tc)
        Nothing -> pure Nothing
      case mb_canon of
        Just canon -> pure canon
        Nothing -> do
          let sp   = noAnnSrcSpan loc
              sp_n = l2l sp
          x_id <- newSysLocalId (fsLit "$x") ManyTy arg_ty
          unless (isNativeOpaqueValueId x_id) unsupported
          let x_tc    = L sp (HsVar noExtField (L sp_n x_id))
              body_tc = L sp (HsApp noExtField lhs_tc (L sp (HsApp noExtField rhs_tc x_tc)))
          mb_body_core <- tcExprToRetainedCore RCM_RetainedArg body_tc
          core_body <- maybe unsupported (\(c, _, _) -> pure c) mb_body_core
          tcRegisterCheckedSynthesizedFun loc arg_ty res_ty' x_id core_body body_tc
  where
    unsupported :: TcM a
    unsupported = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument

-- | Synthesize a certified top-level 1-equation function helper and closed
-- source family for a constructor, simple lambda, or local 1-equation function
-- passed to a retained @foreach (f :: a -> b)@ binder.
tcSynthesizeRetainedFunctionArg
  :: Type -> SrcSpan -> Maybe (LocatedN Name) -> LHsExpr GhcRn
  -> TcM (Core.CoreExpr, LHsExpr GhcTc)
tcSynthesizeRetainedFunctionArg raw_domain loc mb_pat raw_body = do
  expected <- liftZonkM (zonkTcType raw_domain)
  case nativeIndexFunctionTypeTys_maybe expected of
    Nothing -> unsupported
    Just (arg_ty0, res_ty0) -> do
      arg_ty <- liftZonkM (zonkTcType arg_ty0)
      res_ty <- liftZonkM (zonkTcType res_ty0)
      when (any isMetaTyVar (tyCoVarsOfTypeList arg_ty)) unsupported
      (x_name, body_rn) <- case mb_pat of
        Just lname -> pure (unLoc lname, raw_body)
        Nothing -> do
          fresh_x <- newSysName (mkVarOcc "$x")
          let app_rn = if isDataConAppHead raw_body
                         then nlHsApp raw_body (nlHsVar fresh_x)
                         else raw_body
          pure (fresh_x, app_rn)
      let x_id = mkLocalId x_name ManyTy arg_ty
      unless (isNativeOpaqueValueId x_id) unsupported
      (_, (core_body, body_tc)) <- tcCollectingUsage $
        tcExtendIdEnv1 x_name x_id $
          tcCheckRetainedScalarArgM ManyTy Core.retainedArgIndex res_ty body_rn
      res_ty' <- liftZonkM (zonkTcType res_ty)
      when (any isMetaTyVar (tyCoVarsOfTypeList res_ty')) unsupported
      tcRegisterCheckedSynthesizedFun loc arg_ty res_ty' x_id core_body body_tc
  where
    unsupported :: TcM a
    unsupported = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument

-- | Register a checked 1-argument body as a synthesized '$depfun$' source-table
-- binding without re-typechecking the body expression.
tcRegisterCheckedSynthesizedFun
  :: SrcSpan -> Type -> Type -> Id -> Core.CoreExpr -> LHsExpr GhcTc
  -> TcM (Core.CoreExpr, LHsExpr GhcTc)
tcRegisterCheckedSynthesizedFun loc arg_ty res_ty x_id core_body body_tc = do
  body_index <- maybe unsupported (liftZonkM . zonkTcType) (Core.retainedArgIndex core_body)
  let raw_tyvars = filter isTyVar $
                     tyCoVarsOfTypesList [arg_ty, res_ty, body_index]
                     ++ filter isTyVar (exprFreeVarsList core_body)
  when (any isMetaTyVar raw_tyvars || any isCoVar (tyCoVarsOfTypeList body_index)) unsupported
  let cap_tvs = topo_sort_tvs raw_tyvars
      raw_ids = [ v | v <- exprFreeVarsList core_body ++ filter isId (tyCoVarsOfTypeList body_index)
                    , isId v, isLocalId v, v /= x_id ]
      cap_val_ids = nub_vars raw_ids
  unless (all (\v -> isManyTy (idMult v)
                  && (isNativeOpaqueValueId v || isNativeIndexFunctionId v)
                  && not (any isMetaTyVar (tyCoVarsOfTypeList (varType v))))
              cap_val_ids)
    unsupported
  valid <- tcExtendIdEnv1 (idName x_id) x_id $
    tcRetainedInstScope (arg_ty : res_ty : body_index : map mkTyVarTy cap_tvs ++ map varType cap_val_ids)
  unless valid unsupported
  (fun_id, fam_tc) <- tcRegisterSynthesizedSourceFun loc cap_tvs cap_val_ids x_id res_ty body_index
  let rhs_tc = mkLHsWrap (mkWpTyLams cap_tvs) (mkCheckedManyLam loc (cap_val_ids ++ [x_id]) res_ty body_tc)
  tcCommitSynthesizedSourceFunBind fun_id (mkVarBind fun_id rhs_tc) fam_tc
  recordLocalSynthFunId fun_id
  mapM_ (tcEmitBindingUsage . singleUsageUE) cap_val_ids
  let sp        = noAnnSrcSpan loc
      sp_n      = l2l sp
      call_core = Core.mkApps (Core.mkTyApps (Core.Var fun_id) (map mkTyVarTy cap_tvs))
                    (map Core.Var cap_val_ids)
      head_tc   = mkLHsWrap (mkWpTyApps (map mkTyVarTy cap_tvs)) (L sp (HsVar noExtField (L sp_n fun_id)))
      call_tc   = foldl' (\acc v -> L sp (HsApp noExtField acc (L sp (HsVar noExtField (L sp_n v)))))
                         head_tc cap_val_ids
      expected  = mkVisFunTyManyU arg_ty res_ty
  case Core.nativeIndexFunctionArgIndex call_core of
    Just idx | typeKind idx `tcEqType` expected -> pure (call_core, call_tc)
    _ -> unsupported
  where
    unsupported :: TcM a
    unsupported = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument

    nub_vars :: [Var] -> [Var]
    nub_vars vs = reverse (snd (foldl' (\(s, acc) v ->
      if v `elemVarSet` s then (s, acc) else (extendVarSet s v, v : acc)) (emptyVarSet, []) vs))

    topo_sort_tvs :: [TyVar] -> [TyVar]
    topo_sort_tvs tvs = reverse (snd (foldl' visit (emptyVarSet, []) (nub_vars tvs)))
      where
        cand_set = mkVarSet tvs
        visit (seen, acc) tv
          | tv `elemVarSet` seen = (seen, acc)
          | otherwise =
              let deps = [ d | d <- tyCoVarsOfTypeList (tyVarKind tv)
                             , isTyVar d, d `elemVarSet` cand_set ]
                  (seen1, acc1) = foldl' visit (extendVarSet seen tv, acc) deps
              in (seen1, tv : acc1)

-- | Test whether a renamed operator expression denotes either 'Prelude.(.)' or
-- a certified dependent composition combinator such as 'Data.Function.Dependent.(.)'.
is_any_compose_expr :: LHsExpr GhcRn -> TcM Bool
is_any_compose_expr (L _ (HsPar _ inner)) = is_any_compose_expr inner
is_any_compose_expr (L _ (HsAppType _ inner _)) = is_any_compose_expr inner
is_any_compose_expr (L _ (HsVar _ lname))
  | let nm = unLocWithUserRdr lname
  , not (isDataOcc (nameOccName nm))
  , isExternalName nm = do
      mb_id <- tcLookupIdMaybe nm
      case mb_id of
        Just fn
          | isDependentComposeId fn -> pure True
          | nameModule_maybe (idName fn) == Just gHC_INTERNAL_BASE
          , nameOccName (idName fn) == composeIdOcc -> pure True
        _ -> pure False
is_any_compose_expr _ = pure False

{- Note [Named retained callbacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained function binder accepts a certified function owner (recording
equation and rewrite-rule dependencies via 'tcLookupSourceTableId'), a
partially applied certified source function with at least one remaining value
parameter, or a local unrestricted callback variable.

Note [Checking a cast retained callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unify a local callback variable's type with the expected arrow at nominal role.
If the resulting coercion is reflexive after zonking, retain the variable at
its resolved type; otherwise wrap it in 'SubCo'.
See Note [Wildcards in retained operand domains].
-}

-- | Check the nominal equality for the actual lexical callback.
tcRetainedFunctionVar :: TcType -> Id -> TcM Core.CoreExpr
tcRetainedFunctionVar expected actual
  | not (isExternalName (getName actual))
  , isNativeIndexFunctionId actual
  , isJust (nativeIndexFunctionTypeTys_maybe expected)
  = if actual_ty `tcEqType` expected then pure (Core.Var actual)
    else do
      co <- unifyType (Just (NameThing (idName actual))) actual_ty expected
      nominal <- liftZonkM (zonkCo co)
      if isReflCo nominal
        then pure (Core.Var (setVarType actual (coercionLKind nominal)))
        else do
          let range = Core.Cast (Core.Var actual) (SubCo nominal)
          case Core.nativeIndexFunctionArgIndex range of
            Just index | typeKind index `tcEqType` expected -> pure range
            _ -> mismatch
  | otherwise = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument
  where
    actual_ty = varType actual
    mismatch = failWithTc $ TcRnRetainedQuantificationError $
                 RetainedIndexArgumentCast actual_ty expected

-- | Check a retained argument in one pass at its known domain, returning its
-- Core representation, checked syntax, and unscaled linearity usage.
-- See Note [Retained argument application].
tcRetainedArg :: Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc, UsageEnv)
tcRetainedArg domain = tcRetainedArgScaled (Scaled ManyTy domain)

-- | Like 'tcRetainedArg', carrying the expected multiplicity of the retained binder.
tcRetainedArgScaled :: Scaled Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc, UsageEnv)
tcRetainedArgScaled (Scaled expected_mult domain) arg = do
  (arg_ue, (core_arg, checked_tc)) <- tcCollectingUsage $
    tcCheckRetainedScalarArgM expected_mult Core.retainedArgIndex domain arg
  return (core_arg, checked_tc, arg_ue)

-- | Check a scalar or partial-call retained operand once, constructing
-- 'LHsExpr' 'GhcTc' directly and extracting 'Core.CoreExpr' via 'tcExprToRetainedCore'.
tcCheckRetainedScalarArgM :: Mult -> (Core.CoreExpr -> Maybe Type) -> Type -> LHsExpr GhcRn
                          -> TcM (Core.CoreExpr, LHsExpr GhcTc)
tcCheckRetainedScalarArgM expected_mult0 reify domain0 arg0
  = setSrcSpanA (getLoc arg0) $
    do { domain <- constructor_domain domain0 arg0
       ; (inter_core, checked_tc) <- resolve emptyNameEnv expected_mult0 domain arg0
       ; target_domain <- case reify inter_core of
           Just index
             | typeKind index `tcEqType` domain -> return domain
             | otherwise -> do { domain' <- liftZonkM (zonkTcType domain)
                               ; let actual_kind = typeKind index
                               ; if actual_kind `tcEqType` domain'
                                 then return domain'
                                 else if anyFreeVarsOfType flexible domain'
                                         && not (hasNativeDependentExpr (Core.Type actual_kind))
                                      then do { co <- unifyType Nothing actual_kind domain'
                                              ; co' <- liftZonkM (zonkCo co)
                                              ; domain'' <- liftZonkM (zonkTcType domain')
                                              ; if isReflCo co' && actual_kind `tcEqType` domain''
                                                then return domain''
                                                else unsupported }
                                      else unsupported }
           _ -> unsupported
       ; mb_reified <- tcExprToRetainedCore RCM_RetainedArg checked_tc
       ; case mb_reified of
           Just (final_core, _, _)
             | Just final_idx <- reify final_core
             , typeKind final_idx `tcEqType` target_domain
             -> return (final_core, checked_tc)
           _ -> unsupported }
  where
    flexible tv = isTcTyVar v_tv && isMetaTyVar v_tv
      where v_tv = tv

    constructor_domain domain arg
      | anyFreeVarsOfType flexible domain
      , case unLoc (stripParensLHsExpr arg) of
          HsVar _ name -> isDataOcc (nameOccName (unLocWithUserRdr name))
          _ -> True
      = do { actual <- tcRetainedConstructorArgType arg
           ; case actual of
               Just ty
                 | not (hasNativeDependentExpr (Core.Type ty))
                 , isJust (tcMatchTyX_BM bind Subst.emptySubst domain ty)
                 -> do { _ <- unifyType Nothing domain ty
                       ; liftZonkM (zonkTcType domain) }
               _ -> return domain }
      | otherwise = return domain
      where
        bind tv _ | flexible tv = BindMe
                  | otherwise   = DontBindMe

    resolve :: NameEnv Id -> Mult -> Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    resolve env m_here expected = resolve_with_args env m_here expected []

    resolve_with_args :: NameEnv Id -> Mult -> Type -> [LHsExpr GhcRn] -> LHsExpr GhcRn
                      -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    resolve_with_args env m_here expected later original = do
      result@(core, checked_tc) <-
        tcRetainedValueOperand
          (resolve_expr env m_here expected later)
          (resolve env ManyTy)
          expected original
      case Core.splitNativeIndexedConApp core of
        Just _ | let actual = exprType core, not (actual `tcEqType` expected) -> do
          co <- unifyType Nothing actual expected
          co' <- liftZonkM (zonkCo co)
          if isReflCo co'
            then return result
            else return (Core.Cast core (SubCo co'), mkLHsWrap (WpCast (SubCo co')) checked_tc)
        _ -> return result

    resolve_expr :: NameEnv Id -> Mult -> Type -> [LHsExpr GhcRn] -> LHsExpr GhcRn
                 -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    resolve_expr env m_here expected later original@(L loc expr)
      = setSrcSpanA loc $ case expr of
          HsPar _ inner -> do
            (core, inner_tc) <- resolve_with_args env m_here expected later inner
            return (core, L loc (HsPar noExtField inner_tc))
          XExpr (ExpandedThingRn (HSE ctxt inner)) -> do
            mb_dot <- case ctxt of
              ExprCtxt (HsGetField _ recv (L _ dfo)) ->
                fmap (recv, dfo,) <$> tcResolveDependentDot True recv dfo
              _ -> pure Nothing
            case mb_dot of
              Just (recv_of_ctxt, dfo_of_ctxt, (_, sel_id)) -> do
                let L fl_loc _ = dfoLabel dfo_of_ctxt
                    sel_exp    = L (l2l fl_loc) (mkDependentSelectorExpr fl_loc sel_id)
                (core, app_tc) <- resolve_with_args env m_here expected later
                                    (L loc (HsApp noExtField sel_exp recv_of_ctxt))
                return (core, L loc (XExpr (ExpandedThingTc (HSE ctxt app_tc))))
              Nothing -> do
                (core, inner_tc) <- resolve_with_args env m_here expected later inner
                return (core, L loc (XExpr (ExpandedThingTc (HSE ctxt inner_tc))))
          HsLit _ (HsChar s c)
            | charTy `tcEqType` expected
            -> return (mkCharExpr c, L loc (HsLit noExtField (HsChar s c)))
          HsLit _ (HsString _ string)
            | mkListTy charTy `tcEqType` expected
            -> do { -- See Note [Quoted strings in retained operands].
                    let chars    = unpackHText string
                        char_tcs = [noLocA (HsLit noExtField (HsChar NoSourceText c)) | c <- chars]
                        list_tc  = L loc (ExplicitList charTy char_tcs)
                        expanded = L loc (XExpr (ExpandedThingTc (HSE (ExprCtxt expr) list_tc)))
                        nil_c    = Core.mkConApp nilDataCon [Core.Type charTy]
                        cons_c c r = Core.mkConApp consDataCon [Core.Type charTy, mkCharExpr c, r]
                  ; return (foldr cons_c nil_c chars, expanded) }
          HsOverLit _ (OverLit OverLitRn{ol_from_fun = L _ from} (HsIntegral lit))
            | let n = il_value lit, n >= 0
            -> do { (core, generated) <- tcRetainedNumeral expected from n
                  ; return (core, L loc (XExpr (ExpandedThingTc (HSE (ExprCtxt expr) generated)))) }
          ExprWithTySig _ operand sig
            | isCompleteHsSig sig -> do
                annotated <- tcHsSigWcType False (ExprSigCtxt NoRRC) sig
                unless (isTauTy annotated) unsupported
                expected' <- liftZonkM (zonkTcType expected)
                let cast_err :: forall a. TcM a
                    cast_err = failWithTc $ TcRnRetainedQuantificationError
                                 (RetainedIndexArgumentCast annotated expected')
                    is_num_lit = case unLoc (stripParensLHsExpr operand) of
                      HsOverLit _ (OverLit _ (HsIntegral lit)) -> il_value lit >= 0
                      _ -> False
                target <- if annotated `tcEqType` expected'
                  then return expected'
                  else if is_num_lit || hasScopedDictionaryType annotated || hasScopedDictionaryType expected'
                  then cast_err
                  else do
                    loc_ct <- getCtLocM (lexprCtOrigin original) (Just TypeLevel)
                    ann_view <- reductionReducedType <$> tcRewriteFamilyAxioms loc_ct annotated
                    exp_view <- reductionReducedType <$> tcRewriteFamilyAxioms loc_ct expected'
                    if ann_view `tcEqType` exp_view
                      then return expected'
                      else if anyFreeVarsOfType flexible exp_view
                      then do
                        co <- unifyType Nothing ann_view exp_view
                        co' <- liftZonkM (zonkCo co)
                        unless (isReflCo co') cast_err
                        liftZonkM (zonkTcType expected')
                      else cast_err
                (core, operand_tc) <- if is_num_lit
                  then resolve env m_here target operand
                  else resolve_with_args env m_here target later operand
                return (core, L loc (XExpr (ExpandedThingTc (HSE (ExprCtxt expr) operand_tc))))
          HsApp _ fun arg ->
            resolve_app env m_here expected later loc fun arg
          OpApp _ left op right -> do
            (partial_core, partial_tc) <- resolve_app env m_here expected (right : later) loc op left
            (core, call_tc) <- apply env m_here loc partial_core partial_tc right
            return (core, L loc (XExpr (ExpandedThingTc (HSE (ExprCtxt expr) call_tc))))
          ExplicitList _ elements
            | Just [element_ty] <- nativeIndexDataConTyArgs expected nilDataCon
            -> do { _ <- constructor loc expected [] nilDataCon
                  ; _ <- constructor loc expected [] consDataCon
                  ; pairs <- mapM (resolve env m_here element_ty) elements
                  ; let nil_c = Core.mkConApp nilDataCon [Core.Type element_ty]
                        cons_c (v, _) r = Core.mkConApp consDataCon [Core.Type element_ty, v, r]
                  ; return (foldr cons_c nil_c pairs, L loc (ExplicitList element_ty (map snd pairs))) }
          ExplicitTuple _ [Present _ first, Present _ second] Boxed
            | null later
            , let dc = tupleDataCon Boxed 2
            , Just [t1, t2] <- nativeIndexDataConTyArgs expected dc
            -> do { _ <- constructor loc expected [] dc
                  ; (c1, e1_tc) <- resolve env m_here t1 first
                  ; (c2, e2_tc) <- resolve env m_here t2 second
                  ; let tup_tc = L loc (ExplicitTuple noExtField [Present noExtField e1_tc, Present noExtField e2_tc] Boxed)
                  ; return (mkCoreTup [c1, c2], tup_tc) }
          HsVar _ lname
            | Just bound_id <- lookupNameEnv env (unLocWithUserRdr lname) -> do
                tcEmitBindingUsage (singleUsageUE bound_id)
                core <- tcRetainedArgVar (lexprCtOrigin original) expected bound_id
                return (core, wrap_var_cast loc core bound_id)
            | isDataOcc (nameOccName (unLocWithUserRdr lname)) -> do
                con <- tcLookupConLike (unLoc lname)
                case con of
                  RealDataCon dc -> constructor loc expected later dc
                  _ -> unsupported
            | otherwise -> do
                mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
                case mb_id of
                  Just arg_id0 -> do
                    arg_id1 <- liftZonkM (zonkId arg_id0)
                    arg_id2 <- case coreFullView (varType arg_id1) of
                      TyVarTy tv
                        | isLocalId arg_id1, isTcTyVar tv, isMetaTyVar tv
                        , not (isNativeOpaqueValueType (varType arg_id1))
                        , isNativeOpaqueValueType expected -> do
                            co <- unifyKind Nothing (tyVarKind tv) liftedTypeKind
                            co' <- liftZonkM (zonkCo co)
                            unless (isReflCo co') unsupported
                            liftZonkM (zonkId arg_id1)
                      _ -> return arg_id1
                    if isNativeOpaqueValueId (setIdMult arg_id2 ManyTy)
                      then do
                        arg_id <- if isManyTy m_here
                          then do
                            let mult = idMult arg_id2
                                flex_m = case coreFullView mult of
                                  TyVarTy tv -> isMetaTyVar tv
                                  _ -> False
                            unless (isManyTy mult || flex_m) $ failWithTc $
                              TcRnRetainedQuantificationError (RetainedIndexMultiplicity mult)
                            tcSubMult (OccurrenceOf (getName arg_id2)) ManyTy mult
                            arg_id' <- liftZonkM (zonkId arg_id2)
                            if isNativeOpaqueValueId arg_id' && isManyTy (idMult arg_id')
                              then return arg_id'
                              else failWithTc $ TcRnRetainedQuantificationError
                                     (RetainedIndexMultiplicity (idMult arg_id'))
                          else if isNativeOpaqueValueId arg_id2
                            then return arg_id2
                            else failWithTc $ TcRnRetainedQuantificationError
                                   (RetainedIndexMultiplicity (idMult arg_id2))
                        tcEmitBindingUsage (singleUsageUE arg_id)
                        core <- tcRetainedArgVar (lexprCtOrigin original) expected arg_id
                        return (core, wrap_var_cast loc core arg_id)
                      else case nameModule_maybe (getName arg_id2) of
                        Just m | isGlobalId arg_id2, isInteractiveModule m ->
                          failWithTc $ TcRnRetainedQuantificationError
                            (RetainedInteractiveIndex (getName arg_id2))
                        _ -> unsupported
                  _ -> unsupported
          _ -> unsupported

    wrap_var_cast :: SrcSpanAnnA -> Core.CoreExpr -> Id -> LHsExpr GhcTc
    wrap_var_cast loc core var_id =
      let loc_n = l2l loc
          base  = L loc (HsVar noExtField (L loc_n var_id))
      in case core of
           Core.Cast _ (SubCo co)     -> mkLHsWrap (WpCast (SubCo co)) base
           Core.Var v | v /= var_id   -> L loc (HsVar noExtField (L loc_n v))
           _                          -> base

    constructor :: SrcSpanAnnA -> Type -> [LHsExpr GhcRn] -> DataCon -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    constructor loc expected later dc = case nativeIndexDataConTyArgs expected dc of
      Just univ_tys -> do
        let user_tvs = dataConUserTyVars dc
            univ_map = zip (dataConUnivTyVars dc) univ_tys
        user_tys <- maybe unsupported pure $
          if user_tvs == dataConUnivTyVars dc
            then Just univ_tys
            else traverse (`lookup` univ_map) user_tvs
        let con_tc   = mkLHsWrap (mkWpTyApps user_tys) (L loc (XExpr (ConLikeTc (RealDataCon dc))))
            con_core = Core.mkTyApps (Core.Var (dataConWorkId dc)) univ_tys
        return (con_core, con_tc)
      Nothing -> do
        first <- indexed expected
        case first of
          Just pair -> pure (build_con pair)
          Nothing -> do
            refined <- tcRetainedDeclaredTypeView expected
            if refined `eqType` expected
              then unsupported
              else maybe unsupported (pure . build_con) =<< indexed refined
      where
        build_con (con_core, user_tys) =
          (con_core, mkLHsWrap (mkWpTyApps user_tys) (L loc (XExpr (ConLikeTc (RealDataCon dc)))))

        indexed domain = do
          let tvs = dataConUserTyVars dc
              orig_fields = dataConOrigArgTys dc
              bind tv _ | tv `elem` tvs = BindMe
                        | otherwise     = DontBindMe
          case tcMatchTyX_BM bind Subst.emptySubst (dataConOrigResTy dc) domain of
            Nothing -> return Nothing
            Just subst0 -> do
              subst <- if isJust (traverse (Subst.lookupTyVar subst0) tvs)
                          || not (orig_fields `equalLength` later)
                       then return subst0
                       else foldM (tcMatchDeclaredField tvs) subst0 (zip orig_fields later)
              return $ do
                tys <- traverse (Subst.lookupTyVar subst) tvs
                guard (substTyWith tvs tys (dataConOrigResTy dc) `eqType` domain)
                con_core <- case Core.nativeFamilyConHead_maybe dc tys of
                  Just app -> Just app
                  Nothing -> do
                    (prefix, _) <- Core.nativeIndexedConArgs dc tys
                    Just (Core.mkApps (Core.Var (dataConWorkId dc)) prefix)
                return (con_core, tys)

    resolve_app :: NameEnv Id -> Mult -> Type -> [LHsExpr GhcRn] -> SrcSpanAnnA
                -> LHsExpr GhcRn -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    resolve_app env m_here expected later app_loc (L ploc (HsPar _ fun)) arg = do
      (core, call_tc) <- resolve_app env m_here expected later app_loc fun arg
      case call_tc of
        L cloc (HsApp x f_tc a_tc) -> return (core, L cloc (HsApp x (L ploc (HsPar noExtField f_tc)) a_tc))
        _                          -> return (core, call_tc)
    resolve_app env m_here expected later app_loc (L eloc (XExpr (ExpandedThingRn (HSE ctxt fun)))) arg = do
      (core, call_tc) <- resolve_app env m_here expected later app_loc fun arg
      case call_tc of
        L cloc (HsApp x f_tc a_tc) ->
          return (core, L cloc (HsApp x (L eloc (XExpr (ExpandedThingTc (HSE ctxt f_tc)))) a_tc))
        _ -> return (core, call_tc)
    resolve_app env _ expected _ app_loc (L lam_loc (HsLam x LamSingle matches)) arg
      | L alts_loc [L _ Match
          { m_pats = L pats_loc [L pat_loc (VarPat _ lname)]
          , m_grhss = GRHSs { grhssGRHSs = grhss
                            , grhssLocalBinds = L _ (EmptyLocalBinds _) } }]
          <- mg_alts matches
      , [L _ (GRHS _ [] body)] <- NE.toList grhss
      = setSrcSpanA lam_loc $
        do { strict <- xoptM LangExt.Strict
           ; when strict $ failWithTc
               (TcRnRetainedQuantificationError RetainedIndexLambdaStrict)
           ; let name = unLoc lname
                 binder = mkLocalId name ManyTy expected
           ; (core_arg, arg_tc) <- tcScalingUsage ManyTy (resolve env ManyTy expected arg)
           ; (body_ue, (core_body, body_tc)) <- tcCollectingUsage $
               resolve (extendNameEnv env name binder) ManyTy expected body
           ; tcEmitBindingUsage (snd (popUE body_ue binder))
           ; let pat_tc   = L pat_loc (VarPat noExtField (L (getLoc lname) binder))
                 match_tc = mkSimpleMatch (LamAlt LamSingle) (L pats_loc [pat_tc]) body_tc
                 mg_tc    = MatchGroupTc [Scaled ManyTy expected] expected (Generated OtherExpansion SkipPmc)
                 lam_tc   = L lam_loc (HsLam x LamSingle (MG { mg_alts = L alts_loc [match_tc], mg_ext = mg_tc }))
           ; return (Core.App (Core.Lam binder core_body) core_arg, L app_loc (HsApp noExtField lam_tc arg_tc)) }
    resolve_app env m_here expected later app_loc fun arg
      -- See Note [Record selectors in retained operands].
      = do { let source_head = case unLoc fun of
                   XExpr (HsRecSelRn field) -> Just (unLoc (foLabel field), [])
                   _ -> retainedSourceHead fun
           ; (core_fun, fun_tc) <- case source_head of
               Just (n, []) | not (isExternalName n) -> do
                   actual <- tcLookupIdMaybe n
                   case actual of
                     Just v | isNativeIndexFunctionId v -> do
                       fun_id <- liftZonkM (zonkId v)
                       tcEmitBindingUsage (singleUsageUE fun_id)
                       let f_loc = getLoc fun
                       pure (Core.Var fun_id, L f_loc (HsVar noExtField (L (l2l f_loc) fun_id)))
                     _ -> setSrcSpanA (getLoc fun) $
                       failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall n))
               Just (n, written) -> do
                     mb_id <- if isExternalName n then tcLookupIdMaybe n else pure Nothing
                     certified0 <- maybe (pure Nothing) tcLookupSourceTableId mb_id
                     certified <- case (certified0, mb_id) of
                       (Just v, _) -> pure (Just v)
                       (Nothing, Just fn)
                         | null written, isDependentComposeId fn ->
                             lookupBootModuleIdMaybe gHC_INTERNAL_BASE composeIdOcc
                               >>= maybe (pure Nothing) tcLookupSourceTableId
                       _ -> pure Nothing
                     case certified of
                       Just v -> instantiate_source expected fun v written (arg : later)
                       _ -> setSrcSpanA (getLoc fun) $
                              if is_rec_sel fun
                              then unsupported
                              else failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall n))
               Nothing -> resolve_with_args env m_here expected (arg : later) fun
           ; apply env m_here app_loc core_fun fun_tc arg }

    -- See Note [Erased arguments of certified source operands] and
    -- Note [Pre-unifying polymorphic retained domains in inference mode].
    instantiate_source :: Type -> LHsExpr GhcRn -> Id -> [LHsWcType GhcRn] -> [LHsExpr GhcRn]
                       -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    instantiate_source expected orig_fun owner0 written args = do
      owner <- liftZonkM (zonkId owner0)
      (written_tys, sigma) <- foldM (applyRetainedSourceType unsupported owner)
                                    ([], varType owner) written
      let (binders, body) = tcSplitForAllInvisTyVars sigma
      inferred_tys <- if null binders
        then return []
        else chooseRetainedSourceTypes unsupported owner sigma (map snd binders)
               (declared_operands expected body args)
      let all_tys = written_tys ++ inferred_tys
          head_tc = mkSourceHeadTc (getLoc orig_fun) orig_fun owner all_tys
      return (Core.mkTyApps (Core.Var owner) all_tys, head_tc)

    declared_operands expected ty (arg : args)
      | Just (af, _, _, formal, result) <- splitFunTy_maybe ty
      , isVisibleFunArg af
      = (formal, tcRetainedConstructorArgTypeWith pure Nothing arg) : declared_operands expected result args
    declared_operands expected result []
      | not (isFunTy result)
      , isTyFamFree result
      = [(result, known_expected expected)]
    declared_operands _ _ _ = []

    known_expected expected = do
      expected' <- liftZonkM (zonkTcType expected)
      if not (anyFreeVarsOfType flexible expected')
         && isRetainedValueType expected'
         && noQuickLookVars expected'
        then return (Just expected')
        else return Nothing

    apply :: NameEnv Id -> Mult -> SrcSpanAnnA -> Core.CoreExpr -> LHsExpr GhcTc -> LHsExpr GhcRn
          -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    apply env m_here app_loc core_fun fun_tc arg
      | DepFunTy DepFunVisible binder _ <- coreFullView (exprType core_fun)
      = do { let b_mult = idMult binder
           ; (core_arg, arg_tc) <- tcScalingUsage b_mult $
               resolve env (mkMultMul m_here b_mult) (varType binder) arg
           ; case Core.retainedArgIndex core_arg of
               Just index | typeKind index `tcEqType` varType binder ->
                 let res_ty = applyTypeToArgs (exprType core_fun) [core_arg]
                 in return (Core.App core_fun core_arg,
                            L app_loc (HsAppRetained (DepFunVisible, res_ty) fun_tc arg_tc))
               _ -> setSrcSpanA (getLoc arg) unsupported }
      | otherwise = apply_ordinary env m_here app_loc core_fun fun_tc arg

    apply_ordinary :: NameEnv Id -> Mult -> SrcSpanAnnA -> Core.CoreExpr -> LHsExpr GhcTc -> LHsExpr GhcRn
                   -> TcM (Core.CoreExpr, LHsExpr GhcTc)
    apply_ordinary env m_here app_loc core_fun fun_tc arg = case splitFunTy_maybe (exprType core_fun) of
      Just (_, _, f_mult, arg_ty, _) -> do
        (core_arg, arg_tc) <- tcScalingUsage f_mult $ case Core.collectArgs core_fun of
          (Core.Var owner, _)
            | Just _ <- sourceTableId_maybe owner
            , Just _ <- nativeIndexFunctionTypeTys_maybe arg_ty
            , Just (name, _) <- retainedSourceHead arg
            , isExternalName name
            -> tcCheckRetainedFunctionArgM arg_ty arg
          _ -> resolve env (mkMultMul m_here f_mult) arg_ty arg
        let app_core = Core.App core_fun core_arg
            app_tc   = L app_loc (HsApp noExtField fun_tc arg_tc)
        -- See Note [Retained argument cast diagnostics].
        case core_arg of
          Core.Cast (Core.Var actual) co
            | (Core.Var owner, args) <- Core.collectArgs app_core
            , Just family <- sourceTableId_maybe owner
            , length args == tyConArity family
            , Nothing <- Core.nativeSourceCallIndex app_core
            , not (varType actual `tcEqType` coercionRKind co)
            -> setSrcSpanA (getLoc arg) $
                 failWithTc (TcRnRetainedQuantificationError
                   (RetainedIndexArgumentCast (varType actual) (coercionRKind co)))
          _ -> return (app_core, app_tc)
      _ -> unsupported

    unsupported :: TcM a
    unsupported = failWithTc (TcRnRetainedQuantificationError RetainedIndexExpression)

    is_rec_sel (L _ (HsPar _ inner)) = is_rec_sel inner
    is_rec_sel (L _ (HsAppType _ inner _)) = is_rec_sel inner
    is_rec_sel (L _ (XExpr (HsRecSelRn{}))) = True
    is_rec_sel _ = False

-- | Split a written source head into its name and visible type arguments.
-- See Note [Erased arguments of certified source operands].
retainedSourceHead :: LHsExpr GhcRn -> Maybe (Name, [LHsWcType GhcRn])
retainedSourceHead = go []
  where
    go written (L _ (HsPar _ fun)) = go written fun
    go written (L _ (HsAppType _ fun ty)) = go (ty : written) fun
    go written (L _ (HsVar _ name))
      | let n = unLocWithUserRdr name
      , not (isDataOcc (nameOccName n)) = Just (n, written)
    go written (L _ (XExpr (HsRecSelRn field))) = Just (unLoc (foLabel field), written)
    go _ _ = Nothing

-- | Check a written type without inferring an earlier erased argument.
-- See Note [Erased arguments of certified source operands].
applyRetainedSourceType :: TcM ([Type], TcSigmaType) -> Id
                        -> ([Type], TcSigmaType) -> LHsWcType GhcRn
                        -> TcM ([Type], TcSigmaType)
applyRetainedSourceType unsupported owner (tys, sigma) ty
  | Just (_, binder, _) <- tcSplitForAllTyVarBinder_maybe sigma
  , isSpecifiedForAllTyFlag (binderFlag binder)
  = do { (choice, body) <- tcVTA (idConcreteTvs owner) sigma Nothing ty
       ; choice <- liftZonkM (zonkTcType choice)
       ; body <- liftZonkM (zonkTcType body)
       ; valid <- tcRetainedInstScope [choice, body]
       ; if valid && all noQuickLookVars [choice, body]
                  && not (any (\v -> isTcTyVar v && isMetaTyVar v)
                              (tyCoVarsOfTypesList [choice, body]))
         then return (tys ++ [choice], body)
         else unsupported }
  | otherwise = unsupported

-- | Determine erased type arguments from known operand types.
-- See Note [Erased arguments of certified source operands].
chooseRetainedSourceTypes :: TcM [Type] -> Id -> Type -> [TyVar]
                          -> [(Type, TcM (Maybe Type))] -> TcM [Type]
chooseRetainedSourceTypes unsupported owner sigma tvs operands
  | isEmptyNameEnv (idConcreteTvs owner) = go Subst.emptySubst [] operands
  | otherwise = unsupported
  where
    go subst matched rest
      | Just choices <- traverse (Subst.lookupTyVar subst) tvs
      = validate subst matched choices
      | (formal, declared) : later <- rest
      = do
          actual <- declared
          case actual of
            Nothing -> go subst matched later
            Just ty
              | Just next <- matchRetainedAppType tvs subst formal ty
              -> go next ((formal, ty) : matched) later
              | otherwise -> unsupported
      | otherwise = unsupported

    validate subst matched choices
      | not (any (`elemVarSet` tyCoVarsOfTypes choices) tvs)
      , and (zipWith (\tv ty -> eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty))
                     tvs choices)
      , all (\(formal, actual) -> eqType (Subst.substTy subst formal) actual) matched
      , let checked = sigma : choices ++ map snd matched
      , all noQuickLookVars checked
      = do { valid <- tcRetainedInstScope checked
           ; if valid then return choices else unsupported }
      | otherwise = unsupported

-- | Resolve an index variable at its expected domain. Function values keep
-- their complete fixed type and lexical identity.
-- See Note [Retained argument application].
tcRetainedArgVar :: CtOrigin -> TcType -> Id -> TcM Core.CoreExpr
tcRetainedArgVar origin expected arg_id
  | isNativeIndexFunctionId arg_id = tcRetainedFunctionVar expected arg_id
  | varType arg_id `tcEqType` expected = return (Core.Var arg_id)
  | otherwise
  = do { branch_co <- tcRetainedDomainConversion origin (varType arg_id) expected
       ; co <- maybe (unifyType Nothing (varType arg_id) expected) return branch_co
       ; co' <- liftZonkM (zonkCo co)
       ; if isReflCo co'
         then return (Core.Var (setVarType arg_id (coercionLKind co')))
         else return (Core.Cast (Core.Var arg_id) (SubCo co')) }

-- | Prove a domain conversion from lexical constructor evidence and family axioms.
-- See Note [Retained argument application].
tcRetainedDomainConversion :: CtOrigin -> TcType -> TcType -> TcM (Maybe CoercionN)
tcRetainedDomainConversion origin actual expected
  | anyFreeVarsOfType isNonCoVarId actual
  = tryTcDiscardingErrs (return Nothing) $ runMaybeT $ do
      branch_co <- liftMaybeT (tcRetainedTypeCoercion (const True) actual)
      guard (not (isReflCo branch_co))
      let actual' = coercionRKind branch_co
      if actual' `tcEqType` expected
        then return branch_co
        else do
          loc <- liftMaybeT (getCtLocM origin (Just TypeLevel))
          Reduction actual_co actual_nf <- liftMaybeT (tcRewriteFamilyAxioms loc actual')
          Reduction expected_co expected_nf <- liftMaybeT (tcRewriteFamilyAxioms loc expected)
          guard (actual_nf `tcEqType` expected_nf)
          return (branch_co `mkTransCo` actual_co `mkTransCo` mkSymCo expected_co)
  | otherwise = return Nothing

{- Note [Wildcards in retained operand domains]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a retained binder's domain contains flexible metavariables (such as
@keepMaybe \@_ m x@), unify the operand's type with the domain first and zonk
both the variable's type and the dependent arrow before substituting into the
codomain.

Note [Retained argument application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check each non-vacuous, hole-free retained argument once in 'tcInstFun':

1. Construct 'LHsExpr' 'GhcTc' directly inside 'tcCollectingUsage', expanding
   plain 'HsString' literals at @[Char]@, elaborating non-negative numerals via
   'tcRetainedNumeral', specialising ground class methods, and resolving lexical
   Given class selectors.
2. Recover the 'Core.CoreExpr' index via 'tcExprToRetainedCore'
   ('RCM_RetainedArg') so 'applyTypeToArgs' substitutes the exact index that
   'dsLExpr' emits for 'HsAppRetained'.
3. Store @Just (checked_tc, arg_ue)@ in 'ea_retained_tc' on 'ERetainedArg'.
   'tcValArg' scales @arg_ue@ once by the retained binder's multiplicity
   ('OneTy', 'ManyTy', or a multiplicity variable) without re-typechecking the
   expression: type formation uses grade 0 while runtime application consumes
   the binder's grade; see Atkey, "The Syntax and Semantics of Quantitative
   Type Theory", LICS 2018, pp. 56-65; Choudhury, Eades, Eisenberg & Weirich,
   "A Graded Dependent Type System with a Usage-Aware Semantics", POPL 2021
   (PACMPL 5(POPL):50); Brady, "Idris 2: Quantitative Type Theory in Practice",
   ECOOP 2021 (LIPIcs 194:9); Norell, PhD thesis, Chalmers, 2007; and GHC
   Proposal #378 (§4.7).
4. Keep @ea_retained_tc = Nothing@ only for vacuous invisible 'DepFunTy'
   binders, @TypeRep@ '@ty' overrides, and hole-bearing operands so 'tcValArg'
   checks them once after Step 5 result refinement.

Note [Explicit invisible retained arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In @f \@Int \@{n} xs@, @\@{n}@ supplies a term-namespace expression to a
'DepFunInvisible' binder (GHC Proposal #378, §4.7; Norell, PhD thesis,
Chalmers, 2007). Non-vacuous written operands use the same single-pass retained
check as visible retained arguments; vacuous binders defer to a single
'tcPolyExpr' check in 'tcValArg'.

Note [Quoted strings in retained operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A plain quoted string at @[Char]@ has the same index as its written list of
characters. Decode the parsed 'HText' with its modified UTF-8 decoder to
preserve embedded NUL and each Unicode code point. With @OverloadedStrings@,
the renamer produces 'HsOverLit', so only a plain 'HsString' uses this
expansion.
-}

-- | Is this a written runtime argument for an invisible retained binder?
isExplicitRetainedArg :: HsExprArg p -> Bool
isExplicitRetainedArg ERetainedArg{ ea_retained_vis = DepFunInvisible } = True
isExplicitRetainedArg _ = False

-- | Check that a known application head has no concreteness requirements.
hasNoConcreteTyVars :: HsExpr GhcTc -> Bool
hasNoConcreteTyVars (HsVar _ (L _ var)) = isEmptyNameEnv (idConcreteTvs var)
hasNoConcreteTyVars (XExpr (HsRecSelTc (FieldOcc _ (L _ sel))))
  = isEmptyNameEnv (idConcreteTvs sel)
hasNoConcreteTyVars (XExpr (ConLikeTc (RealDataCon dc)))
  = isEmptyNameEnv (dataConConcreteTyVars dc)
hasNoConcreteTyVars _ = False

-- | Admit known retained values and complete erased package types.
-- Constraint arrows are supported only below a retained binder.
-- See Note [Known retained function instantiation].
isRetainedValueType :: Type -> Bool
isRetainedValueType = go False
  where
    go _ ty | isTauTy ty = True
    go under_pi ty = case coreFullView ty of
      ExistsTy{} -> supportsQuickLookType ty
      DepFunTy _ index body -> (isNativeOpaqueValueId index || isNativeIndexFunctionId index)
                         && typeKind body `eqType` liftedTypeKind
                         && go True body
      ForAllTy ma (Bndr tv vis) body
        | under_pi, isTauTy ma, isInvisibleForAllTyFlag vis, isTauTy (varType tv)
        -> go True body
      FunTy flag ma mult arg res -> isTauTy ma && isTauTy mult
                              && (isVisibleFunArg flag || under_pi)
                              && go under_pi arg && go under_pi res
      TyConApp tc args
        | isAlgTyCon tc, not (isClassTyCon tc)
        , length args == tyConArity tc
        , typeKind ty `eqType` liftedTypeKind
        -> all (go under_pi) args
      _ -> False

{- Note [Known retained function instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary polymorphic value can be checked at a known retained function type.
Accept runtime function types through ordinary arrows and supported retained
binders. Also accept constraint arrows below a retained binder. An exactly
saturated algebraic container of kind Type can hold these values. Recurse
through each container argument with the incoming under_pi flag so that a Pi in
one argument does not admit a qualified type in a sibling argument.
-}

-- | Find the result after the written ordinary value arguments. A remaining
-- visible arrow is allowed, but erased foralls and constraint arrows are not.
tcRetainedOrdinaryResult :: Int -> Type -> Maybe Type
tcRetainedOrdinaryResult 0 ty
  | isTauTy ty = Just ty
tcRetainedOrdinaryResult n ty
  | n > 0, Just (_, res') <- splitVisibleFunTy_maybe ty
  = tcRetainedOrdinaryResult (n-1) res'
  | otherwise = Nothing

-- | Classify arguments without elaboration.
-- See Note [Inference of native tuple fields] in GHC.Tc.Gen.Expr for tuples.
-- See Note [Native list anchors] for lists.
tcRetainedInferableArg :: LHsExpr GhcRn -> TcM (Bool, Bool)
tcRetainedInferableArg (L _ expr)
  = case expr of
      HsVar _ lname
        | isDataOcc (nameOccName (unLocWithUserRdr lname))
        -> do { con <- tcLookupConLike (unLoc lname)
              ; return (case con of RealDataCon dc -> (True, dataConHasRetainedResult dc)
                                    _ -> (False, False)) }
        | otherwise
        -> do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
              ; return (case mb_id of
                          Just var -> (True, hasNativeDependentExpr (Core.Type (varType var)))
                          Nothing -> (False, False)) }
      XExpr (HsRecSelRn (FieldOcc _ (L _ name)))
        -> do { mb_id <- tcLookupIdMaybe name
              ; return $ case mb_id of
                  Just sel
                    | (_, body) <- tcSplitForAllInvisTyVars (varType sel)
                    , DepFunTy DepFunVisible _ _ <- coreFullView body
                    -> (True, True)
                  _ -> (False, False) }
      HsApp _ fun arg -> application fun [arg]
      OpApp _ lhs op rhs ->
        do { ql <- wantQuickLook (unLoc op)
           ; case ql of
               DoQL -> return (False, False)
               NoQL -> application op [lhs, rhs] }
      HsAppType _ fun _ -> tcRetainedInferableArg fun
      ExplicitList _ fields
        -> do { overloaded <- xoptM LangExt.OverloadedLists
              ; anchor <- if overloaded then return Nothing
                          else tcRetainedListElementType declared_field fields
              ; return (False, isJust anchor) }
      ExplicitTuple _ args Boxed
        -> do { shapes <- mapM tcRetainedInferableArg [arg | Present _ arg <- args]
              ; return (True, any snd shapes) }
      ExprWithTySig _ arg sig@(HsWC { hswc_body = L _ (HsSig { sig_bndrs = bndrs
                                                            , sig_body = body }) })
        | isCompleteHsSig sig, null (hsOuterTyVarNames bndrs), simple_type body
        -> do { (_, anchor) <- tcRetainedInferableArg arg
              ; return (True, anchor) }
      HsPar _ arg -> tcRetainedInferableArg arg
      HsPragE _ _ arg -> tcRetainedInferableArg arg
      XExpr (ExpandedThingRn (HSE { hse_ctxt = ExprCtxt (HsGetField _ recv (L _ dfo)), hse_exp = arg })) ->
        do { mb_sel <- tcResolveDependentDot False recv dfo
           ; case mb_sel of
               Just (_, sel_id) ->
                 let L fl_loc _ = dfoLabel dfo
                 in application (L (l2l fl_loc) (mkDependentSelectorExpr fl_loc sel_id)) [recv]
               Nothing -> tcRetainedInferableArg arg }
      XExpr (ExpandedThingRn (HSE { hse_exp = arg })) -> tcRetainedInferableArg arg
      _ -> return (False, False)
  where
    application fun args =
      do { (inferable, anchor) <- tcRetainedInferableArg fun
         ; if not inferable then return (False, False)
           else do { shapes <- mapM tcRetainedInferableArg args
                   ; return (True, anchor || any snd shapes) } }

    declared_field (L _ (HsVar _ lname))
      | not (isDataOcc (nameOccName name))
      = do { mb_id <- tcLookupIdMaybe name
           ; traverse (liftZonkM . zonkTcType . varType) mb_id }
      where name = unLocWithUserRdr lname
    declared_field _ = return Nothing

    -- Exclude checking-only syntax without traversing compiler type graphs.
    simple_type :: Data a => a -> Bool
    simple_type thing
      | Just (_ :: SrcSpanAnnA) <- cast thing = True
      | Just (_ :: SrcSpanAnnN) <- cast thing = True
      | Just (_ :: SrcSpan)     <- cast thing = True
      | Just ty <- (cast thing :: Maybe (HsType GhcRn))
      , not (simple_shape ty) = False
      | Just ty <- (cast thing :: Maybe Type) = isTauTy ty
      | Just _ <- (cast thing :: Maybe Name) = True
      | otherwise = and (gmapQ simple_type thing)

    -- Keep explicit quantifiers, contexts and type splices on the usual path.
    simple_shape :: HsType GhcRn -> Bool
    simple_shape HsForAllTy{} = False
    simple_shape HsForEachTy{} = False
    simple_shape HsQualTy{} = False
    simple_shape HsSpliceTy{} = False
    simple_shape _ = True

{- Note [Native list anchors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A built-in list literal such as [vector] can contain an argument whose
declared type mentions a runtime index. Report an anchor without marking the
literal itself inferable so enclosing native capability checks detect the
index without forcing standalone list inference.
-}

-- | Immutable choices for successive invisible binder groups. Nothing uses
-- ordinary instantiation. Just [] records a fully consumed immutable plan.
type RetainedInstPlan = Maybe [Type]

-- | Collect invisible foralls and contexts before the first visible argument.
-- Keep their original order in the application; this split is for matching only.
tcRetainedInstTelescope :: Type -> ([TyVar], ThetaType, Type)
tcRetainedInstTelescope sigma
  | (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , (theta, tau) <- tcSplitPhiTy body
  , not (null tvs && null theta)
  = let (rest_tvs, rest_theta, result) = tcRetainedInstTelescope tau
    in (tvs ++ rest_tvs, theta ++ rest_theta, result)
  | otherwise = ([], [], sigma)

-- | Arguments cached for single-pass checking and usage emission.
type RetainedArgCache = [(Int, (LHsExpr GhcTc, TcRhoType, UsageEnv))]

-- | Recognize an external dependent composition combinator '(.)'.
isDependentComposeId :: Id -> Bool
isDependentComposeId fn
  | Just rhs <- Core.maybeUnfoldingTemplate (realIdUnfolding fn)
  , (bndrs, body) <- Core.collectBinders rhs
  , (ty_bndrs, [g, f, x]) <- span isTyVar bndrs
  , length ty_bndrs `elem` [3, 4]
  , all isId [g, f, x]
  , Core.App (Core.Var outer) (Core.App (Core.Var inner) (Core.Var arg)) <- body
  , outer == g && inner == f && arg == x
  = True
  | isExternalName (idName fn)
  , nameOccName (idName fn) == composeIdOcc
  , let (tvs, theta, tau) = tcRetainedInstTelescope (varType fn)
  , null theta
  , matches_dep_compose_type tvs tau
  = True
  | otherwise
  = False
  where
    matches_dep_compose_type tvs tau
      | length tvs `elem` [3, 4]
      , FunTy FTF_T_T ma1 mult1 arg1 rest1 <- coreFullView tau
      , ma1 `eqType` unmatchableDataConTy, isManyTy mult1
      , DepFunTy DepFunVisible y_id (AppTy c_head (TermVarTy y_occ)) <- coreFullView arg1
      , y_occ == y_id
      , Just c_tv <- getTyVar_maybe c_head, c_tv `elem` tvs
      , Just b_tv <- getTyVar_maybe (varType y_id), b_tv `elem` tvs
      , FunTy FTF_T_T ma_c mult_c c_dom c_res <- coreFullView (tyVarKind c_tv)
      , isManyTy mult_c, c_dom `eqType` varType y_id, tcIsLiftedTypeKind c_res
      , let has_m = case getTyVar_maybe ma_c of
              Just m_tv -> m_tv `elem` tvs && tyVarKind m_tv `eqType` matchabilityTy
              Nothing   -> False
      , (ma_c `eqType` unmatchableDataConTy && length tvs == 3) || (has_m && length tvs == 4)
      , DepFunTy DepFunVisible f_id rest2 <- coreFullView rest1
      , FunTy FTF_T_T ma_f mult_f a_ty b_ty' <- coreFullView (varType f_id)
      , ma_f `eqType` unmatchableDataConTy, isManyTy mult_f
      , b_ty' `eqType` varType y_id
      , Just a_tv <- getTyVar_maybe a_ty, a_tv `elem` tvs
      , a_tv /= b_tv, a_tv /= c_tv, b_tv /= c_tv
      , DepFunTy DepFunVisible x_id (AppTy c_head2 (TermAppTy f_occ (TermVarTy x_occ)))
          <- coreFullView rest2
      , getTyVar_maybe c_head2 == Just c_tv, f_occ == f_id, x_occ == x_id
      , varType x_id `eqType` a_ty
      = True
      | otherwise
      = False

-- | Recognize a bare or parenthesized variable callback without collecting usage.
retainedNamedCallback :: LHsExpr GhcRn
                      -> Maybe (HsExpr GhcTc -> LHsExpr GhcTc, LHsExpr GhcRn)
retainedNamedCallback (L loc (HsPar annotation arg))
  = do { (build, head_arg) <- retainedNamedCallback arg
       ; return (L loc . HsPar annotation . build, head_arg) }
retainedNamedCallback arg@(L loc (HsVar _ lname))
  | not (isDataOcc (nameOccName (unLocWithUserRdr lname))) = Just (L loc, arg)
retainedNamedCallback _ = Nothing

-- | Check runtime identifiers against their current lexical or module bindings.
-- Normalize type and multiplicity metadata without changing identifier identity.
-- See Note [Native index bindings during typechecking].
tcRetainedInstScope :: [Type] -> TcM Bool
tcRetainedInstScope tys
  | null runtime_ids = return True
  | otherwise
  = do { local_env <- getLclTypeEnv
       ; global_env <- tcg_type_env <$> getGblEnv
       ; givens <- mapM zonkAppIdIfNeeded =<< getNativeGivens
       ; let -- The outer Maybe distinguishes an absent binding from a
             -- present binding of the wrong sort. Only absence permits
             -- the lexical Given fallback.
             current_binding v
               = case lookupNameEnv local_env (getName v) of
                   Just (ATcId { tct_id = local_id }) -> Just (Just (True, local_id))
                   Just _ -> Just Nothing
                   Nothing -> case lookupTypeEnv global_env (getName v) of
                     Just (AnId module_id) -> Just (Just (False, module_id))
                     Just _ -> Just Nothing
                     Nothing -> Nothing
             given_predicates = map varType givens
             given_prototypes = filter isNonCoVarId $
                                  concatMap tyCoVarsOfTypeList given_predicates
             valid_binding lexical occurrence binding
               = occurrence == binding
                 && ((isNativeIndexId occurrence && isNativeIndexId binding)
                     || (lexical && not (isExternalName (getName occurrence))
                         && isNativeOpaqueValueId occurrence
                         && isNativeOpaqueValueId binding))
                 && eqType (varType occurrence) (varType binding)
                 && eqType (idMult occurrence) (idMult binding)
             valid_runtime_id v
               | isNativeClassDictionaryId v
               = do { scoped <- getLexicalGivens
                    ; occurrence <- zonkAppIdIfNeeded v
                    ; actuals <- mapM zonkAppIdIfNeeded (filter (== v) scoped)
                    ; return $ case actuals of
                        [actual] -> isNativeClassDictionaryId actual
                                 && varType occurrence `eqType` varType actual
                        _ -> False }
               | isNativeIndexFunctionId v
               = do { actual <- tcLookupIdMaybe (getName v)
                    ; return (maybe False (\binding -> binding == v
                                             && isNativeIndexFunctionId binding
                                             && varType binding `eqType` varType v) actual) }
               | otherwise
               = do { occurrence <- zonkAppIdIfNeeded v
                    ; case current_binding v of
                        Just (Just (lexical, binding)) ->
                          do { binding <- zonkAppIdIfNeeded binding
                             ; return (valid_binding lexical occurrence binding) }
                        Just Nothing -> return False
                        Nothing -> do
                          { prototypes <- mapM zonkAppIdIfNeeded
                                            (filter (== v) given_prototypes)
                          ; return (not (null prototypes)
                                    && all (valid_binding True occurrence) prototypes
                                    && not (any (anyFreeVarsOfType (bad_prototype occurrence))
                                                given_predicates)) } }
             bad_prototype occurrence prototype
               = isNonCoVarId prototype && prototype == occurrence
                 && not (valid_binding True occurrence prototype)
       ; and <$> mapM valid_runtime_id runtime_ids }
  where
    runtime_ids = filter isNonCoVarId (tyCoVarsOfTypesList tys)

{- Note [Native index bindings during typechecking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A source IO binding such as n <- readIORef cell can still have type or
multiplicity metadata that contains filled meta-variables. Before testing its
native index domain and Many multiplicity, use the within-TC zonkId to read
those assignments. This updates only the Id's type and multiplicity. It does
not clone the Name or Unique, allocate a new index, or fill a native meta.
Do not use the final zonker's zonkIdOcc here.

Immutable instantiation must also validate each free runtime index against
its current binding. Lexical bindings are ATcIds in the local environment.
Same-module top-level LocalIds can instead be AnIds in tcg_type_env. For
example, evaluate (sameNat bottomLength Z) and PackVec bottomLength leftVector
need immutable instantiation at the actual bottomLength index. Checking only
the local environment would reject that binding and fall back to a forbidden
native meta-variable fill.

A hidden retained binder can instead have its exact prototype certified by
an actual lexical Given predicate. Use that fallback only when no ordinary
binding exists. See Note [Declared retained argument refinement]. CoVars do
not enter this runtime-value judgment.

Structural indices can also contain opaque local values. Check their exact
lifted types and lexical identities. An eligible ordinary function head keeps
its separate closed U/Many call policy. Keep the existing module lookup for
structural indices. See Note [Source opaque call indices] in GHC.Tc.Gen.HsType.

Look in the local environment first, then the existing module type environment.
Do not load interfaces for a missing or stale name. Normalize both the index
occurrence and its authoritative binding before checking their identity,
exact domain and Many multiplicity. Imported GlobalIds, linear indices and
missing bindings still fail. The existing
Core Tidy alias step keeps accepted module indices local when Tidy globalises
their top-level bindings; see Note [Local indices across Core Tidy] in
GHC.Iface.Tidy and GHC Proposal #378 (§4.6). Keep the Core index grammar unchanged.
-}

-- Is the argument supposed to instantiate a forall?
--
-- In other words, given a function application `fn arg`,
-- can we look at the `arg` and conclude that `fn :: forall x. t`
-- or `fn :: forall x -> t`?
--
-- This is a conservative heuristic that returns `False` for "don't know".
-- Use it to mark written type operands before checking the application.
-- See Note [VTA for out-of-scope functions] and Note [Dependent RULE binder domains]
-- in GHC.Tc.Gen.Sig.
looks_like_type_arg :: HsExprArg 'TcpRn -> Bool
looks_like_type_arg ETypeArg{} =
  -- The argument is clearly supposed to instantiate an invisible forall,
  -- i.e. when we see `f @a`, we expect `f :: forall x. t`.
  True
looks_like_type_arg EValArg{ ea_arg = L _ e } =
  -- Check if the argument is supposed to instantiate a visible forall,
  -- i.e. when we see `f (type Int)`, we expect `f :: forall x -> t`,
  --      but not if we see `f True`.
  -- We can't say for sure though. Part 2 of GHC Proposal #281 allows
  -- type arguments without the `type` qualifier, so `f True` could
  -- instantiate `forall (b :: Bool) -> t`.
  case stripParensHsExpr e of
    HsEmbTy _ _ -> True
    _           -> False
looks_like_type_arg _ = False

addArgCtxt :: Int -> (HsExpr GhcRn, SrcSpan) -> LHsExpr GhcRn
           -> TcM a -> TcM a
-- There are 2 cases:
-- 1. In the normal case, we add an informative context
--     (<=> location span of f or head of application chain is user located)
--     "In the third argument of f, namely blah"
-- 2. If head of the application chain is generated
--    "In the expression: arg"

--  See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr
--  See Note [Expanding HsDo with XXExprGhcRn] in GHC.Tc.Gen.Do
addArgCtxt arg_no (app_head, app_head_lspan) (L arg_loc arg) thing_inside
  | not (isGeneratedSrcSpan app_head_lspan)
  = do { traceTc "addArgCtxt" (vcat [text "not generated Head"
                                    , ppr app_head
                                    , ppr app_head_lspan
                                    , ppr arg_loc
                                    , ppr arg
                                    , ppr arg_no])
       ; setSrcSpanA arg_loc $
         addErrCtxt (FunAppCtxt (FunAppCtxtExpr app_head arg) arg_no) $
         thing_inside
       }
  | otherwise
  = do { traceTc "addArgCtxt" (vcat [text "generated Head"
                                    , ppr app_head
                                    , ppr app_head_lspan
                                    , ppr arg_loc
                                    , ppr arg])
       ; setSrcSpanA arg_loc $
         addExprCtxt arg $
         thing_inside
       }


{- *********************************************************************
*                                                                      *
              Instantiating fresh type variables

      Functions in here use getTcLevelQL to decide what level
      to put on fresh unification variables.  If do_ql = DoQL, we
      ignore the level in the monad, and use QLInstVar instead,
      thereby giving birth to a Quick Look instantiation varaible
*                                                                      *
********************************************************************* -}

{- Note [Instantiating type variables in QuickLook]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During QuickLook, when we instantiate a function's type (specifically, in
`tcInstFun`), we must instantiate it with so-called "instantiation variables".
See Note [QuickLook instantiation variables] in GHC.Tc.Utils.TcType.

So `tcInstFun` uses a family of specialised functions, defined below, like
   instantiateSigmaQL
   newFlexiTyVarTyQL
   etc
that create fresh instantiation variables rather than regular unification
variables.   But only if QuickLook is on!  So they all take a `QLFlag` to
tell them what to do; that flag is ultimately used in `getTcLevelQL`.

There is some code duplication between these functions and their friends
in GHC.Tc.Utils.TcMType, but that's just too bad.

Note that `tcInstFun` calls `quickLookArg` which calls `tcInstFun` recursively.
Consider (f (g (h x))).`tcApp` instantiates the call to `f`, and in doing so
quick-looks at the argument(s), in this case (g (h x)).  But `quickLookArg` on (g
(h x)) in turn instantiates `g` and quick-looks at /its/ argument(s), in this
case (h x).  And so on recursively.  Key point: all these instantiations make
instantiation variables.
-}

getTcLevelQL :: QLFlag -> TcM TcLevel
-- If Quick Look is on, instantiate all fresh unification variables
-- at level QLInstVar; they are instantiation variables
-- See Note [Instantiating type variables in QuickLook]
getTcLevelQL DoQL = return QLInstVar
getTcLevelQL NoQL = getTcLevel

newFlexiTyVarQL :: QLFlag -> OccName -> MetaInfo -> TcKind -> TcM TcTyVar
newFlexiTyVarQL do_ql occ info kind
  = do { lvl   <- getTcLevelQL do_ql
       ; depth <- getTermScopeDepth
       ; ref   <- newMutVar Flexi
       ; name <- newSysName occ -- See Note [Name of a unification variable]
                                -- in GHC.Tc.Utils.TcMType
       ; let details = MetaTv { mtv_info       = info
                              , mtv_ref        = ref
                              , mtv_tclvl      = lvl
                              , mtv_term_depth = depth }
       ; return (mkTcTyVar name kind details) }

newFlexiTyVarTyQL :: QLFlag -> OccName -> MetaInfo -> TcKind -> TcM TcType
newFlexiTyVarTyQL do_ql occ info kind
  = mkTyVarTy <$> newFlexiTyVarQL do_ql occ info kind

newOpenFlexiTyVarTyQL :: QLFlag -> MetaInfo -> TcM TcType
newOpenFlexiTyVarTyQL do_ql rr_info
  = do { let rr_occ = mkTyVarOccFS (fsLit "cx")
             tv_occ = mkTyVarOccFS (fsLit "q")
        ; rr_ty  <- newFlexiTyVarTyQL do_ql rr_occ rr_info runtimeRepTy
        ; arg_nu <- newFlexiTyVarTyQL do_ql tv_occ TauTv   (mkTYPEapp rr_ty)
        ; return arg_nu }

newArgTyVarTyQL :: QLFlag -> FixedRuntimeRepContext -> TcM TcType
newArgTyVarTyQL do_ql frr_ctxt
  = mdo { let conc_orig = ConcreteFRR $
                          FixedRuntimeRepOrigin
                            { frr_context = frr_ctxt
                            , frr_type    = arg_nu }
        ; rr_info <- mkConcreteInfo conc_orig
        ; arg_nu  <- newOpenFlexiTyVarTyQL do_ql rr_info
        ; return arg_nu }

instantiateSigmaQLWithAcc :: [HsExprArg 'TcpInst]
                          -> QLFlag
                          -> CtOrigin
                          -> ConcreteTyVars
                          -> [(Matchability, TyVar)]
                          -> TcThetaType -> TcSigmaType
                          -> TcM (HsWrapper, TcSigmaType)
-- (instantiateSigmaQL orig tvs theta ty)
--     instantiates the type variables tvs, emits the (instantiated)
--     constraints theta, and returns the (instantiated) type ty
-- See Note [Instantiating type variables in QuickLook]
instantiateSigmaQLWithAcc rev_acc do_ql orig concs tvs theta body_ty
  = do { rec (subst, inst_tvs) <- mapAccumLM (new_meta subst) empty_subst (map snd tvs)
       ; let inst_theta  = substTheta subst theta
             inst_body   = substTy subst body_ty

       ; wrap <- instRetainedCall do_ql rev_acc orig (mkTyVarTys inst_tvs) inst_theta
       ; traceTc "Instantiating"
                 (vcat [ text "origin" <+> pprCtOrigin orig
                       , text "tvs"   <+> ppr tvs
                       , text "theta" <+> ppr theta
                       , text "type" <+> debugPprType body_ty
                       , text "with" <+> ppr inst_tvs
                       , text "theta:" <+> ppr inst_theta ])

      ; return (wrap, inst_body) }
  where
    in_scope = mkInScopeSet (tyCoVarsOfType (mkSpecSigmaTy tvs theta body_ty))
               -- mkSpecSigmaTy: Inferred vs Specified is not important here;
               --                We just want an accurate free-var set
    empty_subst = mkEmptySubst in_scope

    new_meta :: Subst -> Subst -> TyVar -> TcM (Subst, TcTyVar)
    new_meta final_subst subst tv
      = do { let occ = getOccName tv
                 substd_kind = substTy subst (tyVarKind tv)
           ; info   <- get_info final_subst tv
           ; new_tv <- newFlexiTyVarQL do_ql occ info substd_kind
           ; let new_subst   = extendTvSubstWithClone subst tv new_tv
           ; return (new_subst, new_tv) }

    get_info :: Subst -> TyVar -> TcM MetaInfo
    get_info final_subst tv
      -- Is this a type variable that must be instantiated to a concrete type?
      -- If so, create a ConcreteTv metavariable instead of a plain TauTv.
      -- See Note [Representation-polymorphism checking built-ins]
      --     in GHC.Tc.Utils.Concrete.

      | Just conc_orig0 <- lookupNameEnv concs (tyVarName tv)
      , let conc_orig = substConcreteTvOrigin final_subst body_ty conc_orig0
                        -- See Note [substConcreteTvOrigin].
      = mkConcreteInfo conc_orig

      -- The vastly common case
      | otherwise
      = return TauTv

{- *********************************************************************
*                                                                      *
              Visible type application
*                                                                      *
********************************************************************* -}

-- See Note [Visible type application and abstraction]
tcVTA :: ConcreteTyVars
         -- ^ Type variables that must be instantiated to concrete types.
         --
         -- See Note [Representation-polymorphism checking built-ins]
         -- in GHC.Tc.Utils.Concrete.
      -> TcType            -- ^ Function type
      -> Maybe HsAppTypeSource  -- ^ Written argument, if lifted
      -> LHsWcType GhcRn   -- ^ Argument type
      -> TcM (TcType, TcType)
-- Deal with a visible type application
-- The function type has already had its Inferred binders instantiated
tcVTA conc_tvs fun_ty source hs_ty
  | Just (ma, tvb, inner_ty) <- tcSplitForAllTyVarBinder_maybe fun_ty
  , isSpecifiedForAllTyFlag (binderFlag tvb)
  = do { tc_inst_forall_arg conc_tvs (ma, tvb, inner_ty) source hs_ty }

  | otherwise
  = do { (_, fun_ty) <- liftZonkM $ zonkTidyTcType emptyTidyEnv fun_ty
       ; failWith $ TcRnInvalidTypeApplication fun_ty hs_ty }

-- See Note [Visible type application and abstraction]
tcVDQ :: ConcreteTyVars              -- See Note [Representation-polymorphism checking built-ins]
      -> (Matchability, ForAllTyBinder, TcType)    -- Function type
      -> LHsExpr GhcRn               -- Argument type
      -> TcM (LHsWcType GhcRn, TcType, TcType)
tcVDQ conc_tvs (ma, tvb, inner_ty) arg
  = do { hs_wc_ty <- expr_to_type arg
       ; (ty_arg, inst_ty) <- tc_inst_forall_arg conc_tvs (ma, tvb, inner_ty) Nothing hs_wc_ty
       ; return (hs_wc_ty, ty_arg, inst_ty) }

-- Convert a HsExpr into the equivalent HsType.
-- See [RequiredTypeArguments and the T2T mapping]
expr_to_type :: LHsExpr GhcRn -> TcM (LHsWcType GhcRn)
expr_to_type earg =
  case stripParensLHsExpr earg of
    L _ (HsEmbTy _ hs_ty) ->
      -- The entire type argument is guarded with the `type` herald,
      -- e.g. `vfun (type (Maybe Int))`. This special case supports
      -- named wildcards. See Note [Wildcards in the T2T translation]
      return hs_ty
    _ ->
      -- The type argument is not guarded with the `type` herald, or perhaps
      -- only parts of it are, e.g. `vfun (Maybe Int)` or `vfun (Maybe (type Int))`.
      -- Apply a recursive T2T transformation, preserving `data` heralds.
      HsWC [] <$> go False earg
        <* failIfErrsM   -- Suppress unhelpful errors that arise after a failed T2T
  where
    is_data_herald_par :: SrcSpanAnnA -> Bool
    is_data_herald_par (EpAnn _ items _) =
      any is_herald_mark items
      where
        is_herald_mark (AddSemiAnn (EpTok (EpaDelta _ (SameLine 1) _))) = True
        is_herald_mark _                                                = False

    go :: Bool -> LHsExpr GhcRn -> TcM (LHsType GhcRn)
    go _ (L _ (HsEmbTy _ t)) =
      -- HsEmbTy means there is an explicit `type` herald, e.g. vfun :: forall a -> blah
      -- and the call   vfun (type Int)
      --           or   vfun (Int -> type Int)
      -- The T2T transformation can simply discard the herald and use the embedded type.
      unwrap_wc t
    go in_data (L l (HsFunArr _ mult arg res)) =
      do { arg' <- go in_data arg
         ; mult' <- go_arrow
         ; res' <- go in_data res
         ; return (L l (HsFunTy noExtField mult' arg' res'))}
         where
          go_arrow = do
            let HsModifiedFunArr _ mods arr = mult
            mods' <- mapM go_modifier mods
            pure $ HsModifiedFunArr noExtField mods' arr
          go_modifier (L lm (HsModifier x ty)) = L lm . HsModifier x <$> go in_data ty
    go in_data (L l (HsForAll _ tele expr)) =
      do { ty <- go in_data expr
         ; return (L l (HsForAllTy noExtField tele ty))}
    go in_data (L l (HsQual _ (L lc (HsContext an ctxt)) expr)) =
      do { ctxt' <- mapM (go in_data) ctxt
         ; ty <- go in_data expr
         ; return (L l (HsQualTy noExtField (L lc (HsContext an ctxt')) ty)) }
    go in_data (L l (HsVar _ lname)) =
      -- GHC Proposal #281, section 7.5 "T2T-Mapping":
      --   variables and constructors (regardless of their namespace)
      --   are mapped directly, without modification.
      -- Proposal #378 §4.3.4: an explicit `data` herald selects the
      -- value/data namespace without triggering pun ambiguity errors.
      do { unless in_data (detect_puns lname)
         ; let prom = if in_data && isDataConName (unLocWithUserRdr lname) then IsPromoted else NotPromoted
         ; return (L l (HsTyVar noAnn prom lname)) }
    go in_data (L l (HsApp _ lhs rhs)) =
      do { lhs' <- go in_data lhs
         ; rhs' <- go in_data rhs
         ; return (L l (HsAppTy noExtField lhs' rhs')) }
    go in_data (L l (HsAppType _ lhs rhs)) =
      do { lhs' <- go in_data lhs
         ; rhs' <- unwrap_wc rhs
         ; return (L l (HsAppKindTy noExtField lhs' rhs')) }
    go in_data (L l (OpApp _ lhs op rhs)) =
      do { is_comp <- is_any_compose_expr op
         ; dep <- xoptM LangExt.DependentTypes
         ; if dep && is_comp
             then do
               { lhs0 <- go in_data lhs >>= promote_section_tree
               ; rhs0 <- go in_data rhs >>= promote_section_tree
               ; sec_name <- newSysName (mkTyVarOcc "$comp")
               ; let sec_loc = noAnnSrcSpan (locA l)
                     sec_tvb = L (l2l sec_loc) (HsTvb noAnn () (HsBndrVar noExtField (L sec_loc sec_name)) (HsBndrNoKind noExtField))
                     sec_ty  = L l (HsTyVar noAnn NotPromoted (L sec_loc (noUserRdr sec_name)))
                     inner   = L l (HsAppTy noExtField rhs0 sec_ty)
                     body    = L l (HsAppTy noExtField lhs0 inner)
               ; return (L l (XHsType (HsTypeLambdaQuoteRn [sec_tvb] body))) }
             else do
               { lhs' <- go in_data lhs
               ; op'  <- go in_data op
               ; rhs' <- go in_data rhs
               ; return (L l (HsOpTy noExtField lhs' op' rhs')) } }
    go in_data e@(L l (HsLam _ LamSingle MG { mg_alts = L _ [L _ Match
        { m_pats = L _ pats@(_:_)
        , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] body_expr) NE.:| []
                          , grhssLocalBinds = L _ EmptyLocalBinds{} } }] })) =
      do { dep <- xoptM LangExt.DependentTypes
         ; unless dep $ failWith (TcRnIllformedTypeArgument e)
         ; let go_pat p = case unLoc p of
                 ParPat _ inner -> go_pat inner
                 VarPat _ (L vloc vname) -> do
                   tv_name <- newSysName (mkTyVarOccFS (occNameFS (nameOccName vname)))
                   let tvb = L (l2l vloc) (HsTvb noAnn () (HsBndrVar noExtField (L vloc tv_name)) (HsBndrNoKind noExtField))
                   pure (tvb, Just (vname, tv_name))
                 WildPat _ -> do
                   tv_name <- newSysName (mkTyVarOcc "_")
                   let wloc = noAnnSrcSpan (getLocA p)
                       tvb  = L (l2l wloc) (HsTvb noAnn () (HsBndrVar noExtField (L wloc tv_name)) (HsBndrNoKind noExtField))
                   pure (tvb, Nothing)
                 SigPat _ inner (HsPS _ sig_ty) -> do
                   (L tloc (HsTvb x () bvar _), mb_sub) <- go_pat inner
                   pure (L tloc (HsTvb x () bvar (HsBndrKind noExtField sig_ty)), mb_sub)
                 _ -> failWith $ TcRnIllformedTypeArgument e
         ; pairs <- mapM go_pat pats
         ; raw_body <- go in_data body_expr
         ; let (tvbs, subs) = (map fst pairs, catMaybes (map snd pairs))
               sub_body = foldl' (\acc (old_nm, new_nm) -> subst_tyvar_occ old_nm new_nm acc) raw_body subs
         ; body <- promote_section_tree sub_body
         ; return (L l (XHsType (HsTypeLambdaQuoteRn tvbs body))) }
    go in_data (L l (SectionL _ lhs op)) =
      -- See Note [Operator sections in T2T].
      do { lhs' <- go in_data lhs
         ; op0  <- go in_data op
         ; op'  <- promote_section_op op0
         ; sec_name <- newSysName (mkTyVarOcc "$sec")
         ; let sec_loc = noAnnSrcSpan (locA l)
               sec_tvb = L (l2l sec_loc) (HsTvb noAnn () (HsBndrVar noExtField (L sec_loc sec_name)) (HsBndrNoKind noExtField))
               sec_ty  = L l (HsTyVar noAnn NotPromoted (L sec_loc (noUserRdr sec_name)))
               body    = L l (HsOpTy noExtField lhs' op' sec_ty)
         ; return (L l (XHsType (HsTypeLambdaQuoteRn [sec_tvb] body))) }
    go in_data (L l (SectionR _ op rhs)) =
      -- See Note [Operator sections in T2T].
      do { op0  <- go in_data op
         ; op'  <- promote_section_op op0
         ; rhs' <- go in_data rhs
         ; sec_name <- newSysName (mkTyVarOcc "$sec")
         ; let sec_loc = noAnnSrcSpan (locA l)
               sec_tvb = L (l2l sec_loc) (HsTvb noAnn () (HsBndrVar noExtField (L sec_loc sec_name)) (HsBndrNoKind noExtField))
               sec_ty  = L l (HsTyVar noAnn NotPromoted (L sec_loc (noUserRdr sec_name)))
               body    = L l (HsOpTy noExtField sec_ty op' rhs')
         ; return (L l (XHsType (HsTypeLambdaQuoteRn [sec_tvb] body))) }
    go _ (L l (XExpr (HsRecSelRn (FieldOcc rdr (L fl_loc sel_name))))) =
      promote_section_op (L l (HsTyVar noAnn NotPromoted (L fl_loc (WithUserRdr rdr sel_name))))
    go in_data e@(L l (HsGetField _ recv (L _ dfo))) =
      do { mb_sel <- tcResolveDependentDot True recv dfo
         ; case mb_sel of
             Just (_, sel_id) ->
               do { recv' <- go in_data recv
                  ; let L fl_loc _ = dfoLabel dfo
                        sel_ty0 = L (l2l fl_loc) (HsTyVar noAnn NotPromoted (L fl_loc (noUserRdr (idName sel_id))))
                  ; sel_ty <- promote_section_op sel_ty0
                  ; return (L l (HsAppTy noExtField sel_ty recv')) }
             Nothing -> failWith $ TcRnIllformedTypeArgument e }
    go _ (L l (HsOverLit _ ol))
      = do { let lit = tyLitFromOverloadedLit (ol_val ol)
           ; return (L l (HsTyLit noExtField lit)) }
    go _ (L l (HsLit _ lit))
      = return (L l (HsTyLit noExtField lit))
    go in_data (L l (ExplicitTuple _ tup_args boxity))
      -- Neither unboxed tuples (#e1,e2#) nor tuple sections (e1,,e2,) can be promoted
      | isBoxed boxity
      , Just es <- tupArgsPresent_maybe tup_args
      = do { ts <- traverse (go in_data) es
           ; let prom = if in_data then IsPromoted else NotPromoted
           ; return (L l (HsExplicitTupleTy noExtField prom ts)) }
    go in_data (L l (ExplicitList _ es)) =
      do { ts <- traverse (go in_data) es
         ; let prom = if in_data then IsPromoted else NotPromoted
         ; return (L l (HsExplicitListTy noExtField prom ts)) }
    go in_data (L l (ExprWithTySig _ e sig_ty)) =
      do { t <- go in_data e
         ; sig_ki <- (unwrap_sig <=< unwrap_wc) sig_ty
         ; return (L l (HsKindSig noAnn t sig_ki)) }
      where
        unwrap_sig :: LHsSigType GhcRn -> TcM (LHsType GhcRn)
        unwrap_sig (L _ (HsSig _ HsOuterImplicit{hso_ximplicit=bndrs} body))
          | null bndrs = return body
          | otherwise  = illegal_implicit_tvs bndrs
        unwrap_sig (L ls (HsSig _ HsOuterExplicit{hso_matchability=ma,hso_bndrs=bndrs} body)) =
          return $ L ls (HsForAllTy noExtField (HsForAllInvis noAnn ma bndrs) body)
    go in_data (L l (HsPar _ e)) =
      do { let in_data' = in_data || is_data_herald_par l
         ; t <- go in_data' e
         ; return (L l (HsParTy noAnn t)) }
    go in_data (L l (HsUntypedSplice splice_result splice))
      | HsUntypedSpliceTop finalizers e <- splice_result
      = do { t <- go in_data (L l e)
           ; let splice_result' = HsUntypedSpliceTop finalizers t
           ; return (L l (HsSpliceTy splice_result' splice)) }
    go _ (L l (HsStar x))
      = return (L l (HsStarTy x))
    go _ (L l (HsHole h@(HoleVar (L _ rdr))))
      | isUnderscore occ = return (L l (HsWildCardTy h))
      | startsWithUnderscore occ =
          -- See Note [Wildcards in the T2T translation]
          do { wildcards_enabled <- xoptM LangExt.NamedWildCards
             ; if wildcards_enabled
               then illegal_wc rdr
               else not_in_scope }
      | otherwise = not_in_scope
      where occ = occName rdr
            not_in_scope = failWith $ TcRnNotInScope NotInScope rdr
    go in_data (L l (XExpr (ExpandedThingRn (HSE (ExprCtxt orig) _)))) =
      -- Use the original, user-written expression (before expansion).
      -- Example. Say we have   vfun :: forall a -> blah
      --          and the call  vfun (Maybe [1,2,3])
      --          expanded to   vfun (Maybe (fromListN 3 [1,2,3]))
      -- (This happens when OverloadedLists is enabled).
      -- The expanded expression can't be promoted, as there is no type-level
      -- equivalent of fromListN, so we must use the original.
      go in_data (L l orig)
    go _ e = failWith $ TcRnIllformedTypeArgument e

    detect_puns :: LocatedN (WithUserRdr Name) -> TcM ()
      -- GHC Proposal #281, section 7.5 "T2T-Mapping":
      --   there should be no variable of the same name but from
      --   a different namespace, or else raise an ambiguity error
      --   (does not apply to constructors)
    detect_puns (L l (WithUserRdr rdr _))
      | isTermVarOrFieldNameSpace (rdrNameSpace rdr)
      , Just promoted_rdr <- promoteRdrName rdr
      = do { envs <- getRdrEnvs
           ; whenIsJust (lookup_rdr envs rdr) $ \unpromoted@(ResolvedNameInfo _ _ u_nm) ->
             whenIsJust (lookup_rdr envs promoted_rdr) $ \promoted@(ResolvedNameInfo _ _ p_nm) ->
               unless (u_nm == p_nm) $
               do { same_witness <- is_same_retained_type_witness u_nm p_nm
                  ; unless same_witness $
                      addErrAt (locA l) $
                      TcRnIllegalPunnedVarOccInTypeArgument { illegalPunTermName = unpromoted
                                                            , illegalPunTypeName = promoted } } }
      | otherwise = return ()

    is_same_retained_type_witness :: Name -> Name -> TcM Bool
    is_same_retained_type_witness u_nm p_nm = do
      dep <- xoptM LangExt.DependentTypes
      if not dep
        then pure False
        else do
          mb_u <- tcLookupLcl_maybe u_nm
          mb_p <- tcLookupLcl_maybe p_nm
          case (mb_u, mb_p) of
            (Just (ATcId { tct_id = u_id }), Just (ATyVar _ p_tv)) -> do
              mb_rep <- tcTypeRepTarget_maybe (varType u_id)
              case mb_rep of
                Just (_, target_ty) -> do
                  p_ty <- zonkAppTypeIfNeeded (mkTyVarTy p_tv)
                  pure (tcEqType target_ty p_ty)
                Nothing -> pure False
            _ -> pure False

    lookup_rdr :: (GlobalRdrEnv, LocalRdrEnv) -> RdrName -> Maybe ResolvedNameInfo
    lookup_rdr (gbl_env, lcl_env) rdr
      | Just name <- lookupLocalRdrEnv lcl_env rdr
      = Just (ResolvedNameInfo [] rdr name)
      | gres@(gre:_) <- lookupGRE gbl_env (LookupRdrName rdr (RelevantGREsFOS WantNormal))
      = Just (ResolvedNameInfo gres rdr (gre_name gre))
      | otherwise = Nothing

    unwrap_wc :: HsWildCardBndrs GhcRn t -> TcM t
    unwrap_wc (HsWC wcs t)
      = do { mapM_ (illegal_wc . nameRdrName) wcs
           ; return t }

    illegal_wc :: RdrName -> TcM t
    illegal_wc rdr = failWith $ TcRnIllegalNamedWildcardInTypeArgument rdr

    illegal_implicit_tvs :: [Name] -> TcM t
    illegal_implicit_tvs tvs
      = do { mapM_ (addErr . TcRnIllegalImplicitTyVarInTypeArgument . nameRdrName) tvs
           ; failM }

    promote_section_op :: LHsType GhcRn -> TcM (LHsType GhcRn)
    promote_section_op (L l (HsParTy ann inner)) =
      L l . HsParTy ann <$> promote_section_op inner
    promote_section_op op_ty@(L l (HsTyVar ann prom (L op_loc (WithUserRdr rdr nm))))
      | not (isTyConName nm || isTyVarName nm)
      = do { mb_id <- tcLookupIdMaybe nm
           ; case mb_id >>= sourceTableId_maybe of
               Just table -> do
                 _ <- maybe (pure Nothing) tcLookupSourceTableId mb_id
                 pure (L l (HsTyVar ann prom (L op_loc (WithUserRdr rdr (tyConName table)))))
               Nothing -> pure op_ty }
    promote_section_op op_ty = pure op_ty

    promote_section_tree :: LHsType GhcRn -> TcM (LHsType GhcRn)
    promote_section_tree (L l (HsParTy ann inner)) =
      L l . HsParTy ann <$> promote_section_tree inner
    promote_section_tree (L l (HsAppTy x f a)) =
      L l <$> (HsAppTy x <$> promote_section_tree f <*> promote_section_tree a)
    promote_section_tree (L l (HsAppKindTy x f a)) =
      L l <$> (HsAppKindTy x <$> promote_section_tree f <*> promote_section_tree a)
    promote_section_tree (L l (HsOpTy x a op b)) =
      L l <$> (HsOpTy x <$> promote_section_tree a <*> promote_section_op op <*> promote_section_tree b)
    promote_section_tree ty = promote_section_op ty

    subst_tyvar_occ :: Name -> Name -> LHsType GhcRn -> LHsType GhcRn
    subst_tyvar_occ old_nm new_nm = go_sub
      where
        go_sub (L l (HsParTy ann inner)) = L l (HsParTy ann (go_sub inner))
        go_sub (L l (HsAppTy x f a)) = L l (HsAppTy x (go_sub f) (go_sub a))
        go_sub (L l (HsAppKindTy x f a)) = L l (HsAppKindTy x (go_sub f) (go_sub a))
        go_sub (L l (HsOpTy x a op b)) = L l (HsOpTy x (go_sub a) (go_sub op) (go_sub b))
        go_sub (L l (HsFunTy x mult a b)) = L l (HsFunTy x mult (go_sub a) (go_sub b))
        go_sub (L l (HsTyVar ann prom (L vl (WithUserRdr rdr nm))))
          | nm == old_nm = L l (HsTyVar ann prom (L vl (WithUserRdr rdr new_nm)))
        go_sub other = other

{- Note [Visible @arg override for invisible foreach]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Proposal #378 §4.8 and §4.13 use standard @arg syntax at call sites to supply
an explicit value for a SpecifiedSpec invisible foreach binder, while skipping
InferredSpec {k :: a} binders just like forall {k}. Legacy @{arg} syntax stays
available for compatibility.

When @arg instantiates a retained term binder, type_to_retained_expr translates
the renamed HsWcType back into a renamed HsExpr before passing it to
tcRetainedOperand. A source-table family name T:f is mapped back to its
underlying function f so @(f x) checks identically to @{f x}.

For foreach (a :: Type) -> ..., Proposal #378 §4.9 allows unadorned type
arguments in term syntax such as ex5 Int 3 and idDep Nat Zero. When an ordinary
argument has expected type TypeRep @k target, prepareTypeWitnessArg resolves
the type argument eagerly during tcInstFun so later dependent foreach (x :: a)
binders in the same application see the instantiated domain immediately.
-}

-- | Test whether a function type still has a SpecifiedSpec invisible binder
-- after any leading InferredSpec binders or dictionaries.
hasSpecifiedInvisibleArgument :: Type -> Bool
hasSpecifiedInvisibleArgument ty = case coreFullView ty of
  ForAllTy _ (Bndr _ vis) body
    | isSpecifiedForAllTyFlag vis -> True
    | isInferredForAllTyFlag vis  -> hasSpecifiedInvisibleArgument body
    | otherwise                   -> False
  FunTy { ft_af = af, ft_res = body }
    | not (isVisibleFunArg af)    -> hasSpecifiedInvisibleArgument body
    | otherwise                   -> False
  DepDictTy _ body                -> hasSpecifiedInvisibleArgument body
  DepFunTy DepFunInvisible b body
    | isInferredRetainedBinder b  -> hasSpecifiedInvisibleArgument body
    | otherwise                   -> True
  _                               -> False

-- | Eagerly resolve a type witness argument at @TypeRep \@k target@ so later
-- dependent binders in the same call see @target@ immediately.
prepareTypeWitnessArg :: LHsExpr GhcRn -> Scaled TcType -> TcType
                      -> TcM (LHsExpr GhcRn, Scaled TcType, TcType)
prepareTypeWitnessArg arg sc_arg_ty@(Scaled mult raw_arg_ty) res_ty = do
  dep <- xoptM LangExt.DependentTypes
  if not dep
    then pure (arg, sc_arg_ty, res_ty)
    else do
      mb_rep <- tcTypeRepTarget_maybe raw_arg_ty
      case mb_rep of
        Just (kind, target)
          | anyFreeVarsOfType (\v -> isTcTyVar v && isMetaTyVar v) target ->
            case strip_parens_and_expansions arg of
              L _ (HsEmbTy _ wc_ty) -> do
                ty_arg <- tcHsTypeApp wc_ty kind
                solve_witness ty_arg target
                arg_ty' <- zonkAppTypeIfNeeded raw_arg_ty
                res_ty' <- zonkAppTypeIfNeeded res_ty
                pure (arg, Scaled mult arg_ty', res_ty')
              L _ (HsVar _ lname) -> do
                let nm = unLocWithUserRdr lname
                mb_lcl <- tcLookupLcl_maybe nm
                case mb_lcl of
                  Just (ATcId { tct_id = v }) -> do
                    mb_v_rep <- tcTypeRepTarget_maybe (varType v)
                    case mb_v_rep of
                      Just (_, v_target) -> do
                        solve_witness v_target target
                        arg_ty' <- zonkAppTypeIfNeeded raw_arg_ty
                        res_ty' <- zonkAppTypeIfNeeded res_ty
                        pure (arg, Scaled mult arg_ty', res_ty')
                      Nothing -> pure (arg, sc_arg_ty, res_ty)
                  _ -> wrap_unadorned kind target
              _ -> wrap_unadorned kind target
        _ -> pure (arg, sc_arg_ty, res_ty)
  where
    strip_parens_and_expansions :: LHsExpr GhcRn -> LHsExpr GhcRn
    strip_parens_and_expansions (L _ (HsPar _ e)) = strip_parens_and_expansions e
    strip_parens_and_expansions (L _ (XExpr (ExpandedThingRn (HSE _ e)))) = strip_parens_and_expansions e
    strip_parens_and_expansions e = e

    solve_witness :: TcType -> TcType -> TcM ()
    solve_witness ty_arg target = do
      (_, eq_wc) <- captureConstraints (unifyType Nothing ty_arg target)
      unless (isEmptyWC eq_wc) $ do
        res_wc <- runTcSEqualities (simplifyTopWanteds eq_wc)
        emitConstraints res_wc

    wrap_unadorned kind target = do
      as_ty <- isUnadornedTypeExpr (unLoc arg)
      if not as_ty
        then pure (arg, sc_arg_ty, res_ty)
        else do
          wc_ty <- expr_to_type_at_kind kind arg
          ty_arg <- tcHsTypeApp wc_ty kind
          solve_witness ty_arg target
          arg_ty' <- zonkAppTypeIfNeeded raw_arg_ty
          res_ty' <- zonkAppTypeIfNeeded res_ty
          let loc = getLoc arg
              emb = L loc (mkExpandedLExpr (unLoc arg) (L loc (HsEmbTy noExtField wc_ty)))
          pure (emb, Scaled mult arg_ty', res_ty')

-- | Lower a term-syntax expression to a surface type at a known target kind,
-- interpreting unpromoted list and boxed tuple syntax as @Type@-valued list
-- and tuple types when @-XListTuplePuns@ is enabled and the target kind is @Type@.
expr_to_type_at_kind :: TcKind -> LHsExpr GhcRn -> TcM (LHsWcType GhcRn)
expr_to_type_at_kind kind earg = do
  puns <- xoptM LangExt.ListTuplePuns
  kind' <- zonkAppTypeIfNeeded kind
  HsWC wcs hs_ty0 <- expr_to_type earg
  let hs_ty = if puns && tcIsLiftedTypeKind kind'
              then pun_lifted_types hs_ty0
              else hs_ty0
  pure (HsWC wcs hs_ty)
  where
    pun_lifted_types :: LHsType GhcRn -> LHsType GhcRn
    pun_lifted_types (L l ty) = L l $ case ty of
      HsParTy ann t -> HsParTy ann (pun_lifted_types t)
      HsTyVar _ NotPromoted (L _ (WithUserRdr _ nm))
        | nm == dataConName unitDataCon
        -> HsTupleTy noAnn HsBoxedOrConstraintTuple []
      HsExplicitListTy _ NotPromoted [t] -> HsListTy noAnn (pun_lifted_types t)
      HsExplicitTupleTy _ NotPromoted ts ->
        HsTupleTy noAnn HsBoxedOrConstraintTuple (map pun_lifted_types ts)
      HsAppTy x f a -> HsAppTy x (pun_lifted_types f) (pun_lifted_types a)
      HsFunTy x m a r -> HsFunTy x m (pun_lifted_types a) (pun_lifted_types r)
      other -> other

-- | Test whether a renamed expression denotes an unadorned type argument
-- suitable for a @foreach (a :: Type) ->@ parameter.
isUnadornedTypeExpr :: HsExpr GhcRn -> TcM Bool
isUnadornedTypeExpr expr = case expr of
  HsPar _ (L _ inner) -> isUnadornedTypeExpr inner
  HsEmbTy{}  -> pure True
  HsFunArr{} -> pure True
  HsForAll{} -> pure True
  HsQual{}   -> pure True
  HsStar{}   -> pure True
  ExplicitList _ [L _ elem_e] -> isUnadornedTypeExpr elem_e
  ExplicitTuple _ tup_args Boxed
    | Just es <- tupArgsPresent_maybe tup_args
    -> and <$> mapM (isUnadornedTypeExpr . unLoc) es
  ExprWithTySig _ (L _ inner) _ -> isUnadornedTypeExpr inner
  HsApp _ (L _ f) _     -> isUnadornedTypeExpr f
  HsAppType _ (L _ f) _ -> isUnadornedTypeExpr f
  OpApp _ _ (L _ op) _  -> isUnadornedTypeExpr op
  SectionL _ _ (L _ op) -> isUnadornedTypeExpr op
  SectionR _ (L _ op) _ -> isUnadornedTypeExpr op
  HsVar _ lname -> do
    let nm = unLocWithUserRdr lname
    puns <- xoptM LangExt.ListTuplePuns
    if puns && nm == dataConName unitDataCon
      then pure True
      else do
        mb_lcl <- tcLookupLcl_maybe nm
        case mb_lcl of
          Just ATyVar{}       -> pure True
          Just ATcTypeLet{}   -> pure True
          Just ATcId{}        -> pure False
          Just _              -> pure True
          Nothing
            | isExternalName nm -> do
                thing <- tcLookupGlobal nm
                pure $ case thing of
                  ATyCon{} -> True
                  _        -> False
            | otherwise -> pure (isTyVarName nm || isTcClsNameSpace (occNameSpace (nameOccName nm)))
  XExpr (ExpandedThingRn (HSE (ExprCtxt orig) _)) -> isUnadornedTypeExpr orig
  _ -> pure False

-- | Convert a visible @\@arg@ type argument into a retained term expression.
type_to_retained_expr :: LHsWcType GhcRn -> TcM (LHsExpr GhcRn)
type_to_retained_expr wc_ty = do
  let body = hswc_body wc_ty
  unless (null (hswc_ext wc_ty)) $
    failWithTc (TcRnRetainedQuantificationError RetainedPartialTypeArgument)
  go body
  where
    go :: LHsType GhcRn -> TcM (LHsExpr GhcRn)
    go (L l (HsParTy _ t)) = L l . HsPar noExtField <$> go t
    go (L _ (HsKindSig _ t _)) = go t
    go (L l (HsSpliceTy (HsUntypedSpliceTop _ t) _)) = setSrcSpanA l (go t)
    go (L l (HsTyVar _ _ (L lv (WithUserRdr rdr nm)))) = do
      nm' <- resolve_term_name nm
      pure (L l (HsVar noExtField (L lv (WithUserRdr rdr nm'))))
    go (L l (HsAppTy _ f a)) = do
      f' <- go f
      a' <- go a
      pure (L l (HsApp noExtField f' a'))
    go (L l (HsAppKindTy _ f k)) = do
      f' <- go f
      pure (L l (HsAppType Nothing f' (HsWC [] k)))
    go (L l (HsOpTy _ lhs op rhs)) = do
      lhs' <- go lhs
      op'  <- go op
      rhs' <- go rhs
      pure (L l (HsApp noExtField (L l (HsApp noExtField op' lhs')) rhs'))
    go (L l (HsTyLit _ lit)) = case lit of
      HsNatural _ il -> do
        from_id <- tcLookupKnownOccId fromIntegerClassOpOcc
        let from_fun = L (noAnnSrcSpan (locA l)) (idName from_id)
            over_rn  = OverLitRn { ol_rebindable = False, ol_from_fun = from_fun }
        pure (L l (HsOverLit noExtField (OverLit over_rn (HsIntegral il))))
      HsChar src c   -> pure (L l (HsLit noExtField (HsChar src c)))
      HsString src s -> pure (L l (HsLit noExtField (HsString src s)))
      _ -> failWithTc (TcRnInvalidTypeApplication liftedTypeKind wc_ty)
    go (L l (HsExplicitListTy _ _ tys)) = do
      es <- mapM go tys
      pure (L l (ExplicitList noExtField es))
    go (L l (HsExplicitTupleTy _ _ tys)) = do
      es <- mapM go tys
      pure (L l (ExplicitTuple noExtField [Present noExtField e | e <- es] Boxed))
    go (L l _) = setSrcSpanA l $
      failWithTc (TcRnRetainedQuantificationError RetainedPartialTypeArgument)

    resolve_term_name :: Name -> TcM Name
    resolve_term_name nm
      | isExternalName nm
      , Just term_occ <- sourceTableTermOcc (nameOccName nm)
      , Just mod_nm <- nameModule_maybe nm
      = lookupOrig mod_nm term_occ
      | not (isExternalName nm)
      , isTyVarName nm
      = do { mb <- tcLookupLcl_maybe nm
           ; pure $ case mb of
               Just (ATcId { tct_id = v }) -> idName v
               _ -> nm }
      | otherwise
      = pure nm

{- Note [Dependent OverloadedRecordDot resolution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's `HasField x r a` class gives `getField :: r -> a`, where the result type
`a` cannot depend on the runtime value of `r`. For a dependent record selector
`cells :: foreach (r :: Columns a) -> Vec a (storedLength r)` or `GHC.Sigma`
`second :: foreach (p :: Sigma a b) -> b (first p)`, `HasField` cannot express
the value-dependent result type. Likewise, resolving the witness field
`r.storedLength` or `p.first` directly to `storedLength r` / `first p` preserves
its `SourceTable` promotion so `r.storedLength` and `p.first` work both as
retained `foreach` index arguments and in `T2T` required type arguments.

When `splitHsApps` expands `r.field` (or `(.field) r`), `tcRewriteDependentDotApp`
inspects the preserved `HsGetField` / `HsProjection` node in `EExpand`. It looks
up in-scope `GlobalRdrElt`s matching `field` whose `Id` is either:
  1. a visible `DepFunTy` selector (`foreach (r :: R ...) -> B r`), or
  2. a certified `SourceTable` witness projection of a dependent record or `Sigma`.
If such candidates exist and `r`'s head `TyCon` matches a unique candidate, the
application rewrites directly to that selector applied to `r` while preserving
the `EExpand` wrapper for diagnostics and recording GRE / keep-alive usage.
Ordinary non-dependent records have no matching dependent selector candidate and
continue through `HasField` unchanged.

Note [Operator sections in T2T]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Proposal #378 and PLFA use left and right operator sections such as `cong (+ p) eq`,
`cong (m +) eq`, and `cong (: ys) eq` as required type arguments. The renamer
preserves `SectionL _ lhs op` and `SectionR _ op rhs` inside `ExpandedThingRn`.
In `expr_to_type`, lower `(lhs op)` to `\ $sec -> lhs `op` $sec` and `(op rhs)`
to `\ $sec -> $sec `op` rhs` via `HsTypeLambdaQuoteRn`, which `tcInlineSomeLambda`
elaborates into a closed 1-equation scoped type family capturing any free type
variables or ambient runtime indices.
-}

-- | Build the renamed head expression for a resolved dependent selector 'Id'.
mkDependentSelectorExpr :: SrcSpanAnnN -> Id -> HsExpr GhcRn
mkDependentSelectorExpr fl_loc sel_id
  | isRecordSelector sel_id
  = XExpr (HsRecSelRn (FieldOcc (nameRdrName (idName sel_id)) (L fl_loc (idName sel_id))))
  | otherwise
  = HsVar noExtField (L fl_loc (noUserRdr (idName sel_id)))

-- | Rewrite a `getField @"field" r` application chain from `OverloadedRecordDot`
-- to a direct dependent selector application `field r` when `r` has a matching
-- dependent selector in scope.
-- See Note [Dependent OverloadedRecordDot resolution].
tcRewriteDependentDotApp :: [HsExprArg 'TcpRn]
                         -> TcM (Maybe (HsExpr GhcRn, [HsExprArg 'TcpRn]))
tcRewriteDependentDotApp rn_args
  | Just (outer_wraps, exp_wrap, dfo, val_loc, recv_arg, rest_args) <- split_dot_args [] rn_args
  = do { mb_sel <- tcResolveDependentDot True recv_arg dfo
       ; case mb_sel of
           Just (_, sel_id) ->
             let L fl_loc _ = dfoLabel dfo
                 sel_fun    = mkDependentSelectorExpr fl_loc sel_id
                 new_args   = outer_wraps ++ [EValArg val_loc noExtField recv_arg, exp_wrap] ++ rest_args
             in pure (Just (sel_fun, new_args))
           Nothing -> pure Nothing }
  | otherwise = pure Nothing
  where
    split_dot_args outer (w@(EWrap EPar{}) : rest)  = split_dot_args (w : outer) rest
    split_dot_args outer (p@EPrag{} : rest)         = split_dot_args (p : outer) rest
    split_dot_args outer (ETypeArg{}
                          : EValArg { ea_loc_span = val_loc, ea_arg = recv_arg }
                          : exp_wrap@(EWrap (EExpand _ (ExprCtxt orig)))
                          : rest)
      | Just dfo <- extract_dfo orig
      = Just (reverse outer, exp_wrap, dfo, val_loc, recv_arg, rest)
    split_dot_args outer (exp_wrap@(EWrap (EExpand _ (ExprCtxt orig)))
                          : ETypeArg{}
                          : EValArg { ea_loc_span = val_loc, ea_arg = recv_arg }
                          : rest)
      | Just dfo <- extract_dfo orig
      = Just (reverse outer, exp_wrap, dfo, val_loc, recv_arg, rest)
    split_dot_args outer (ETypeArg{}
                          : exp_wrap@(EWrap (EExpand _ (ExprCtxt orig)))
                          : EValArg { ea_loc_span = val_loc, ea_arg = recv_arg }
                          : rest)
      | Just dfo <- extract_dfo orig
      = Just (reverse outer, exp_wrap, dfo, val_loc, recv_arg, rest)
    split_dot_args _ _ = Nothing

    extract_dfo (HsGetField _ _ (L _ dfo))        = Just dfo
    extract_dfo (HsProjection _ (dfo NE.:| []))   = Just dfo
    extract_dfo _                                 = Nothing

-- | Resolve `recv.dfo` to an in-scope dependent selector `(gre, sel_id)` when
-- the receiver's datatype matches a unique dependent selector or `Sigma`
-- projection.
-- See Note [Dependent OverloadedRecordDot resolution].
tcResolveDependentDot :: Bool
                      -> LHsExpr GhcRn
                      -> DotFieldOcc GhcRn
                      -> TcM (Maybe (GlobalRdrElt, Id))
tcResolveDependentDot record_use recv dfo = do
  let lbl    = unLoc (dfoLabel dfo)
      lbl_fs = mkFastStringShortText (field_label lbl)
  rdr_env <- getGlobalRdrEnv
  let gres = lookupGRE rdr_env (LookupOccName (mkVarOccFS lbl_fs) (RelevantGREsFOS WantBoth))
  candidates <- catMaybes <$> mapM classify_candidate gres
  case candidates of
    [] -> pure Nothing
    _  -> do
      mb_recv_tc <- peekDotReceiverTyCon recv
      case mb_recv_tc of
        Nothing -> pure Nothing
        Just recv_tc ->
          let matches = [ (gre, sel_id)
                        | (gre, sel_id, cand_tc) <- candidates
                        , cand_tc == recv_tc ]
          in case removeDupsOn (idName . snd) matches of
               ([(gre, sel_id)], _) -> do
                 when record_use $ do
                   addUsedGRE AllDeprecationWarnings gre
                   keepAlive (idName sel_id)
                 pure (Just (gre, sel_id))
               _ -> pure Nothing
  where
    removeDupsOn :: Eq b => (a -> b) -> [a] -> ([a], ())
    removeDupsOn f = foldr (\x (acc, ()) ->
      if any (\y -> f x == f y) acc then (acc, ()) else (x : acc, ())) ([], ())

    classify_candidate :: GlobalRdrElt -> TcM (Maybe (GlobalRdrElt, Id, TyCon))
    classify_candidate gre = do
      mb_id <- tcLookupIdMaybe (greName gre)
      case mb_id of
        Nothing -> pure Nothing
        Just sel_id -> do
          sel_ty <- liftZonkM (zonkTcType (idType sel_id))
          let (_tvs, theta, rho) = tcRetainedInstTelescope sel_ty
          if not (null theta)
            then pure Nothing
            else case coreFullView rho of
              DepFunTy DepFunVisible bndr _
                | Just (rec_tc, args) <- tcSplitTyConApp_maybe (varType bndr) -> do
                    fam_envs <- tcGetFamInstEnvs
                    let (r_tc, _, _) = tcLookupDataFamInst fam_envs rec_tc args
                    pure (Just (gre, sel_id, r_tc))
              FunTy { ft_af = af, ft_arg = arg_ty }
                | isVisibleFunArg af
                , isJust (sourceTableId_maybe sel_id)
                , Just (rec_tc, args) <- tcSplitTyConApp_maybe arg_ty
                , is_dep_witness_projection sel_id rec_tc -> do
                    fam_envs <- tcGetFamInstEnvs
                    let (r_tc, _, _) = tcLookupDataFamInst fam_envs rec_tc args
                    pure (Just (gre, sel_id, r_tc))
              _ -> pure Nothing

    is_dep_witness_projection :: Id -> TyCon -> Bool
    is_dep_witness_projection sel_id rec_tc
      | Just (RecSelData parent_tc) <- recordSelectorTyCon_maybe sel_id
      , [con] <- tyConDataCons parent_tc
      = not (null (dataConRepArgBinders con))
      | otherwise
      = occNameFS (getOccName rec_tc) == fsLit "Sigma"

    peekDotReceiverTyCon :: LHsExpr GhcRn -> TcM (Maybe TyCon)
    peekDotReceiverTyCon e = case unLoc (stripParensLHsExpr e) of
      HsVar _ lname -> do
        mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
        case mb_id of
          Just v  -> liftZonkM (zonkTcType (varType v)) >>= tycon_of_type
          Nothing -> probe_infer e
      XExpr (ExpandedThingRn (HSE (ExprCtxt (HsGetField _ inner_recv (L _ inner_dfo))) _)) -> do
        mb_inner_sel <- tcResolveDependentDot False inner_recv inner_dfo
        case mb_inner_sel of
          Just (_, inner_sel) -> do
            sel_ty <- liftZonkM (zonkTcType (idType inner_sel))
            let (_tvs, _theta, rho) = tcRetainedInstTelescope sel_ty
                mb_res = case coreFullView rho of
                  DepFunTy DepFunVisible _ body -> Just body
                  FunTy { ft_res = body }       -> Just body
                  _                             -> Nothing
            maybe (probe_infer e) tycon_of_type mb_res
          Nothing -> do
            mb_inner_tc <- peekDotReceiverTyCon inner_recv
            case mb_inner_tc of
              Just inner_tc
                | let inner_lbl = unLoc (dfoLabel inner_dfo)
                , Just fl <- lookupTyConFieldLabel inner_lbl inner_tc -> do
                    mb_fld_id <- tcLookupIdMaybe (flSelector fl)
                    case mb_fld_id of
                      Just fld_id -> do
                        let (_tvs, _theta, rho) = tcSplitSigmaTy (idType fld_id)
                        case tcSplitFunTy_maybe rho of
                          Just (_, _, res) -> tycon_of_type res
                          Nothing          -> probe_infer e
                      Nothing -> probe_infer e
              _ -> probe_infer e
      _ -> probe_infer e

    probe_infer :: LHsExpr GhcRn -> TcM (Maybe TyCon)
    probe_infer expr = do
      (_, mb_ty) <- tcCollectingUsage $
                      fst <$> captureConstraints (fst <$> tryTc (snd <$> tcInferRho expr))
      case mb_ty of
        Just ty -> liftZonkM (zonkTcType ty) >>= tycon_of_type
        Nothing -> pure Nothing

    tycon_of_type :: TcType -> TcM (Maybe TyCon)
    tycon_of_type ty = case tcSplitTyConApp_maybe ty of
      Just (tc, args) -> do
        fam_envs <- tcGetFamInstEnvs
        let (r_tc, _, _) = tcLookupDataFamInst fam_envs tc args
        pure (Just r_tc)
      Nothing -> pure Nothing

{- Note [RequiredTypeArguments and the T2T mapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The "T2T-Mapping" section of GHC Proposal #281 introduces a term-to-type transformation
that comes into play when we typecheck function applications to required type arguments.
Say we have a function that expects a required type argument, vfun :: forall a -> ...
then it is possible to call it as follows:

  vfun (Maybe Int)

The Maybe Int argument is parsed and renamed as a term. There is no syntactic marker
to tell GHC that it is actually a type argument.  We only discover this by the time
we get to type checking, where we know that f's type has a visible forall at the front,
so we are expecting a type argument. More precisely, this happens in tcVDQ in GHC/Tc/Gen/App.hs:

  tcVDQ :: ConcreteTyVars              -- See Note [Representation-polymorphism checking built-ins]
        -> (Matchability, ForAllTyBinder, TcType)    -- Function type
        -> LHsExpr GhcRn               -- Argument type
        -> TcM (LHsWcType GhcRn, TcType, TcType)

What we want is a type to instantiate the forall-bound variable. But what we have is an HsExpr,
and we need to convert it to an HsType in order to reuse the same code paths as we use for
checking f @ty (see tc_inst_forall_arg).

  f (Maybe Int)
  -- ^^^^^^^^^
  -- parsed and renamed as:   HsApp   (HsVar   "Maybe") (HsVar   "Int")  ::  HsExpr GhcRn
  -- must be converted to:    HsTyApp (HsTyVar "Maybe") (HsTyVar "Int")  ::  HsType GhcRn

We do this using a helper function:

  expr_to_type :: LHsExpr GhcRn -> TcM (LHsWcType GhcRn)

This conversion is in the TcM monad because
* It can fail, if the expression is not convertible to a type.
      vfun [x | x <- xs]     Can't convert list comprehension to a type
      vfun (\x -> x)         Can't convert a lambda to a type
* It needs to check for LangExt.NamedWildCards to generate an appropriate
  error message for HsHole (HsVar ...).
     vfun _a    Not in scope: ‘_a’
                   (NamedWildCards disabled)
     vfun _a    Illegal named wildcard in a required type argument: ‘_a’
                   (NamedWildCards enabled)

Note [Wildcards in the T2T translation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose f1 :: forall a b. blah
        f2 :: forall a b -> blah

Consider the terms
  f1 @_ @(Either _ _)
  f2 (type _) (type (Either _ _))
Those `_` wildcards are type wildcards, each standing for a monotype.
All good.

Now consider this, with -XNamedWildCards:
  f1 @_a @(Either _a _a)
  f2 (type _a) (type (Either _a _a))
Those `_a` are "named wildcards", specified by the user manual like this: "All
occurrences of the same named wildcard within one type signature will unify to
the same type".  Note "within one signature".  So each type argument is considered
separately, and the examples mean the same as:
  f1 @_a1 @(Either _a2 _a2)
  f2 (type _a1) (type (Either _a2 _a2))
The repeated `_a2` ensures that the two arguments of `Either` are the same type;
but there is no connection with `_a1`.  (NB: `_a1` and `_a2` only scope within
their respective type, no further.)

Now, consider the T2T translation for
   f2 _ (Either _ _)
This is fine: the term wildcard `_` is translated to a type wildcard, so we get
the same as if we had written
   f2 (type _) (type (Either _ _))

But what about /named/ wildcards?
   f2 _a (Either _a _a)
Now we are in difficulties.  The renamer looks for a /term/ variable `_a` in scope,
and won't find one.  Even if it did, the three `_a`'s would not be renamed separately
as above.

Conclusion: we treat a named wildcard in the T2T translation as an error.  If you
want that, use a `(type ty)` argument instead.
-}

tc_inst_forall_arg :: ConcreteTyVars            -- See Note [Representation-polymorphism checking built-ins]
                   -> (Matchability, ForAllTyBinder, TcType)  -- Function type
                   -> Maybe HsAppTypeSource                -- Written argument, if lifted
                   -> LHsWcType GhcRn           -- Argument type
                   -> TcM (TcType, TcType)
tc_inst_forall_arg conc_tvs (ma, tvb, inner_ty) source hs_ty
  = do { let tv   = binderVar tvb
             kind = tyVarKind tv
             tv_nm   = tyVarName tv
             mb_conc = lookupNameEnv conc_tvs tv_nm
       ; ty_arg0 <- tcHsTypeAppWithSource source hs_ty kind

       -- Is this type variable required to be instantiated to a concrete type?
       -- If so, ensure that that is the case.
       --
       -- See [Wrinkle: VTA] in Note [Representation-polymorphism checking built-ins]
       -- in GHC.Tc.Utils.Concrete.
       ; th_lvl <- getThLevel
       ; ty_arg <- case mb_conc of
           Nothing   -> return ty_arg0
           Just conc
             -- See [Wrinkle: Typed Template Haskell]
             -- in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
             | TypedBrack {} <- th_lvl
             -> return ty_arg0
             | otherwise
             ->
             -- Example: user wrote e.g. (#,#) @(F Bool) for a type family F.
             -- Emit [W] F Bool ~ kappa[conc] and pretend the user wrote (#,#) @kappa.
               coercionRKind <$> unifyConcrete (occNameFS $ getOccName $ tv_nm) conc ty_arg0

       ; let fun_ty    = mkForAllTy ma tvb inner_ty
             insted_ty = piResultTy fun_ty ty_arg
               -- This substitution is well-kinded even when inner_ty
               -- is not fully zonked, because ty_arg is fully zonked.
               -- See Note [Type application substitution].

       ; lvl <- getTcLevel
       ; traceTc "tc_inst_forall_arg (VTA/VDQ)" (
                  vcat [ text "fun_ty" <+> ppr fun_ty
                       , text "tv" <+> ppr tv <+> dcolon <+> debugPprType kind
                       , text "ty_arg" <+> debugPprType ty_arg <+> dcolon
                                       <+> debugPprType (typeKind ty_arg)
                       , text "inner_ty" <+> debugPprType inner_ty
                       , text "insted_ty" <+> debugPprType insted_ty
                       , text "lvl:" <+> ppr lvl ])
       ; return (ty_arg, insted_ty) }

{- Note [Visible type application and abstraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC supports the types
    forall {a}.  a -> t     -- ForAllTyFlag is Inferred
    forall  a.   a -> t     -- ForAllTyFlag is Specified
    forall  a -> a -> t     -- ForAllTyFlag is Required

The design of type abstraction and type application for those types has gradually
evolved over time, and is based on the following papers and proposals:
  - "Visible Type Application"
    https://richarde.dev/papers/2016/type-app/visible-type-app.pdf
  - "Type Variables in Patterns"
    https://richarde.dev/papers/2018/pat-tyvars/pat-tyvars.pdf
  - "Modern Scoped Type Variables"
    https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst
  - "Visible forall in types of terms"
    https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst

Here we offer an overview of the design mixed with commentary on the
implementation status. The proposals have not been fully implemented at the
time of writing this Note (see "not implemented" in the rest of this Note).

Now consider functions
    fi :: forall {a}. a -> t     -- Inferred:  type argument cannot be supplied
    fs :: forall a. a -> t       -- Specified: type argument may    be supplied
    fr :: forall a -> a -> t     -- Required:  type argument must   be supplied

At a call site we may have calls looking like this
    fi             True  -- Inferred: no visible type argument
    fs             True  -- Specified: type argument omitted
    fs      @Bool  True  -- Specified: type argument supplied
    fr (type Bool) True  -- Required: type argument is compulsory, `type` qualifier used
    fr       Bool  True  -- Required: type argument is compulsory, `type` qualifier omitted

At definition sites we may have type /patterns/ to abstract over type variables
   fi           x       = rhs   -- Inferred: no type pattern
   fs           x       = rhs   -- Specified: type pattern omitted
   fs @a       (x :: a) = rhs   -- Specified: type pattern supplied
   fr (type a) (x :: a) = rhs   -- Required: type pattern is compulsory, `type` qualifier used
   fr a        (x :: a) = rhs   -- Required: type pattern is compulsory, `type` qualifier omitted

Type patterns in lambdas mostly work the same way as they do in a function LHS,
except for @-binders
   OK:  fs = \           x       -> rhs   -- Specified: type pattern omitted
   Bad: fs = \ @a       (x :: a) -> rhs   -- Specified: type pattern supplied
   OK:  fr = \ (type a) (x :: a) -> rhs   -- Required: type pattern is compulsory, `type` qualifier used
   OK:  fr = \ a        (x :: a) -> rhs   -- Required: type pattern is compulsory, `type` qualifier omitted

When it comes to @-binders in lambdas, they do work, but only in a limited set
of circumstances:
  * the lambda occurs as an argument to a higher-rank function or constructor
      higher-rank function:  h :: (forall a. blah) -> ...
      call site:             x = h (\ @a -> ... )
  * the lambda is annotated with an inline type signature:
      (\ @a -> ... ) :: forall a. blah
  * the lambda is a field in a data structure, whose type is impredicative
      [ \ @a -> ... ] :: [forall a. blah]
  * the @-binder is not the first binder in the lambda:
      \ x @a -> ...

Type patterns may also occur in a constructor pattern. Consider the following data declaration
   data T where
     MkTI :: forall {a}. Show a => a -> T   -- Inferred
     MkTS :: forall a.   Show a => a -> T   -- Specified
     MkTR :: forall a -> Show a => a -> T   -- Required  (NB: not implemented)

Matching on its constructors may look like this
   f (MkTI           x)       = rhs  -- Inferred: no type pattern
   f (MkTS           x)       = rhs  -- Specified: type pattern omitted
   f (MkTS @a       (x :: a)) = rhs  -- Specified: type pattern supplied
   f (MkTR (type a) (x :: a)) = rhs  -- Required: type pattern is compulsory, `type` qualifier used    (NB: not implemented)
   f (MkTR a        (x :: a)) = rhs  -- Required: type pattern is compulsory, `type` qualifier omitted (NB: not implemented)

The moving parts are as follows:
  (abbreviations used: "c.o." = "constructor of")

Syntax of types
---------------
* The types are all initially represented with HsForAllTy (c.o. HsType).
  The binders are in the (hst_tele :: HsForAllTelescope pass) field of the HsForAllTy
  At this stage, we have
      forall {a}. t    -- HsForAllInvis (c.o. HsForAllTelescope) and InferredSpec  (c.o. Specificity)
      forall a. t      -- HsForAllInvis (c.o. HsForAllTelescope) and SpecifiedSpec (c.o. Specificity)
      forall a -> t    -- HsForAllVis (c.o. HsForAllTelescope)

* By the time we get to checking applications/abstractions (e.g. GHC.Tc.Gen.App)
  the types have been kind-checked (e.g. by tcLHsType) into ForAllTy (c.o. Type).
  At this stage, we have:
      forall {a}. t    -- ForAllTy (c.o. Type) and Inferred  (c.o. ForAllTyFlag)
      forall a. t      -- ForAllTy (c.o. Type) and Specified (c.o. ForAllTyFlag)
      forall a -> t    -- ForAllTy (c.o. Type) and Required  (c.o. ForAllTyFlag)

Syntax of applications in HsExpr
--------------------------------
* We represent type applications in HsExpr like this (ignoring parameterisation)
    data HsExpr = HsApp HsExpr HsExpr      -- (f True)    (plain function application)
                | HsAppType HsExpr HsType  -- (f @True)   (function application with `@`)
                | HsEmbTy HsType           -- (type Int)  (embed a type into an expression with `type`)
                | ...

* So (f @ty) is represented, just as you might expect:
    HsAppType f ty

* But (f (type ty)) is represented by:
    HsApp f (HsEmbTy ty)

  Why the difference?  Because we /also/ need to express these /nested/ uses of `type`:

      g (Maybe (type Int))               -- valid for g :: forall (a :: Type) -> t
      g (Either (type Int) (type Bool))  -- valid for g :: forall (a :: Type) -> t

  This nesting makes `type` rather different from `@`. Remember, the HsEmbTy mainly just
  switches namespace, and is subject to the term-to-type transformation.

Syntax of abstractions in Pat
-----------------------------
* Type patterns are represented in Pat roughly like this
     data Pat = ConPat   ConLike [Pat]  -- (Con @tp1 @tp2 p1 p2)  (constructor pattern)
              | EmbTyPat HsTyPat        -- (type tp)              (embed a type into a pattern with `type`)
              | InvisPat HsTyPat        -- (@tp)                  (invisible type pattern)
              | ...
     data HsTyPat = HsTP LHsType
  (In ConPat, the type and term arguments are actually inside HsConPatDetails.)

  * Similar to HsAppType in HsExpr, InvisPat in ConPat is used for @ty arguments
  * Similar to HsEmbTy   in HsExpr, EmbTyPat lets you embed a type in a pattern

* Examples:
      \ (MkT @a  (x :: a)) -> rhs    -- ConPat (c.o. Pat) and InvisPat (c.o. Pat)
      \ (type a) (x :: a)  -> rhs    -- EmbTyPat (c.o. Pat)
      \ a        (x :: a)  -> rhs    -- VarPat (c.o. Pat)
      \ @a       (x :: a)  -> rhs    -- InvisPat (c.o. Pat)

* A HsTyPat is not necessarily a plain variable. At the very least,
  we support kind signatures and wildcards:
      \ (type _)           -> rhs
      \ (type (b :: Bool)) -> rhs
      \ (type (_ :: Bool)) -> rhs
  But in constructor patterns we also support full-on types
      \ (P @(a -> Either b c)) -> rhs
  All these forms are represented with HsTP (c.o. HsTyPat).

Renaming type applications
--------------------------
rnExpr delegates renaming of type arguments to rnHsWcType if possible:
    f @t        -- HsAppType,         t is renamed with rnHsWcType
    f (type t)  -- HsApp and HsEmbTy, t is renamed with rnHsWcType

But what about:
    f t         -- HsApp, no HsEmbTy
We simply rename `t` as a term using a recursive call to rnExpr; in particular,
the type of `f` does not affect name resolution (Lexical Scoping Principle).
We will later convert `t` from a `HsExpr` to a `Type`, see "Typechecking type
applications" later in this Note. The details are spelled out in the "Resolved
Syntax Tree" and "T2T-Mapping" sections of GHC Proposal #281.

Renaming type abstractions
--------------------------
rnPat delegates renaming of type arguments to rnHsTyPat if possible:
  f (P @t)   = rhs  -- ConPat,   t is renamed with rnHsTyPat
  f (type t) = rhs  -- EmbTyPat, t is renamed with rnHsTyPat

But what about:
  f t = rhs   -- VarPat
The solution is as before (see previous section), mutatis mutandis.
Rename `t` as a pattern using a recursive call to `rnPat`, convert it
to a type pattern later.

One particularly prickly issue is that of implicit quantification. Consider:

  f :: forall a -> ...
  f t = ...   -- binding site of `t`
    where
      g :: t -> t   -- use site of `t` or a fresh variable?
      g = ...

Does the signature of `g` refer to `t` bound in `f`, or is it a fresh,
implicitly quantified variable? This is normally controlled by
ScopedTypeVariables, but in this example the renamer can't tell `t` from a term
variable.  Only later (in the type checker) will we find out that it stands for
the forall-bound type variable `a`.  So when RequiredTypeArguments is in effect,
we change implicit quantification to take term variables into account; that is,
we do not implicitly quantify the signature of `g` to `g :: forall t. t->t`
because of the term-level `t` that is in scope.
See Note [Term variable capture and implicit quantification].

Typechecking type applications
------------------------------
Type applications are checked alongside ordinary function applications
in tcInstFun.

First of all, we assume that the function type is known (i.e. not a metavariable)
and contains a `forall`. Consider:
  f :: forall a. a -> a
  f x = const x (f @Int 5)
If the type signature is removed, the definition results in an error:
  Cannot apply expression of type ‘t1’
  to a visible type argument ‘Int’

The same principle applies to required type arguments:
  f :: forall a -> a -> a
  f (type a) x = const x (f (type Int) 5)
If the type signature is removed, the error is:
  Illegal type pattern.
  A type pattern must be checked against a visible forall.

When the type of the function is known and contains a `forall`, all we need to
do is instantiate the forall-bound variable with the supplied type argument.
This is done by tcVTA (if Specified) and tcVDQ (if Required).

tcVDQ unwraps the HsEmbTy and uses the type contained within it.  Crucially, in
tcVDQ we know that we are expecting a type argument.  This means that we can
support
    f (Maybe Int)   -- HsApp, no HsEmbTy
The type argument (Maybe Int) is represented as an HsExpr, but tcVDQ can easily
convert it to HsType.  This conversion is called the "T2T-Mapping" in GHC
Proposal #281.

Typechecking type abstractions
------------------------------
Type abstractions are checked alongside ordinary patterns in GHC.Tc.Gen.Pat.tcMatchPats.
One of its inputs is a list of ExpPatType that has two constructors
  * ExpFunPatTy    ...   -- the type A of a function A -> B
  * ExpForAllPatTy ...   -- the binder (a::A) of forall (a::A) -> B
so when we are checking
  f :: forall a b -> a -> b -> ...
  f (type a) (type b) (x :: a) (y :: b) = ...
our expected pattern types are
  [ ExpForAllPatTy ...      -- forall a ->
  , ExpForAllPatTy ...      -- forall b ->
  , ExpFunPatTy    ...      -- a ->
  , ExpFunPatTy    ...      -- b ->
  ]

The [ExpPatType] is initially constructed by GHC.Tc.Utils.Unify.matchExpectedFunTys,
by decomposing the type signature for `f` in our example.  If we are given a
definition
   g (type a) = ...
we never /infer/ a type g :: forall a -> blah.  We can only /check/
explicit type abstractions in terms.

The [ExpPatType] allows us to use different code paths for type abstractions
and ordinary patterns:
  * tc_pat :: Scaled ExpSigmaTypeFRR -> Checker (Pat GhcRn) (Pat GhcTc)
  * tc_forall_pat :: Checker (Pat GhcRn, TcTyVar) (Pat GhcTc)

tc_forall_pat unwraps the EmbTyPat and uses the type pattern contained
within it. This is another spot where the "T2T-Mapping" can take place,
allowing us to support
  f a (x :: a) = rhs    -- no EmbTyPat

Type patterns in constructor patterns are handled in with tcConTyArg.
Both tc_forall_pat and tcConTyArg delegate most of the work to tcHsTyPat.

Note [VTA for out-of-scope functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose 'wurble' is not in scope, and we have
   (wurble @Int @Bool True 'x')

Then the renamer will make (HsHole (HsVar "wurble")) for 'wurble',
and the typechecker will typecheck it with tcUnboundId, giving it
a type 'alpha', and emitting a deferred Hole constraint, to
be reported later.

But then comes the visible type application. If we do nothing, we'll
generate an immediate failure (in tc_app_err), saying that a function
of type 'alpha' can't be applied to Bool.  That's insane!  And indeed
users complain bitterly (#13834, #17150.)

The right error is the Hole, which has /already/ been emitted by
tcUnboundId.  It later reports 'wurble' as out of scope, and tries to
give its type.

Fortunately in tcInstFun we still have access to the function, so we
can check if it is a HsHole.  We use this info to simply skip
over any visible type arguments.  We'll /already/ have emitted a
Hole constraint; failing preserves that constraint.

We do /not/ want to fail altogether in this case (via failM) because
that may abandon an entire instance decl, which (in the presence of
-fdefer-type-errors) leads to leading to #17792.

What about required type arguments?  Suppose we see
    f (type Int)
where `f` is out of scope.  Then again we don't want to crash because f's
type (which will be just a fresh unification variable) isn't a visible forall.
Instead we just skip the `(type Int)` argument, as before.

Downside: the typechecked term has lost its visible type arguments; we
don't even kind-check them.  But let's jump that bridge if we come to
it.  Meanwhile, let's not crash!

Note [Type application substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `tc_inst_forall_arg`, suppose we are checking a visible type
application `f @hs_ty`, where `f :: forall (a :: k). body`.  We will:
  * Compute `ty <- tcHsTypeApp hs_ty k`
  * Then substitute `a :-> ty` in `body`.
Now, you might worry that `a` might not have the same kind as `ty`, so that the
substitution isn't kind-preserving.  How can that happen?  The kinds will
definitely be the same after zonking, and `ty` will be zonked (as this is
a postcondition of `tcHsTypeApp`). But the function type `forall a. body`
might not be fully zonked (hence the worry).

But it's OK!  During type checking, we don't require types to be well-kinded (without
zonking); we only require them to satsisfy the Purely Kinded Type Invariant (PKTI).
See Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType.

In the case of a type application:
  * `forall a. body` satisfies the PKTI
  * `ty` is zonked
  * If we substitute a fully-zonked thing into an un-zonked Type that
    satisfies the PKTI, the result still satisfies the PKTI.

This last statement isn't obvious, but read
Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType.
The tricky case is when `body` contains an application of the form `a b1 ... bn`,
and we substitute `a :-> ty` where `ty` has fewer arrows in its kind than `a` does.
That can't happen: the call `tcHsTypeApp hs_ty k` would have rejected the
type application as ill-kinded.

Historical remark: we used to require a stronger invariant than the PKTI,
namely that all types are well-kinded prior to zonking. In that context, we did
need to zonk `body` before performing the substitution above. See test case
#14158, as well as the discussion in #23661.
-}

{- *********************************************************************
*                                                                      *
              Quick Look
*                                                                      *
********************************************************************* -}

{- Note [Quick Look at value arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function quickLookArg implements the "QL argument" judgement of
the QL paper, in Fig 5 of "A quick look at impredicativity" (ICFP 2020),
rather directly.  The key rule, implemented by `quickLookArg` is

   G |-h h:sg                         -- Find the type of the head
   G |-inst sg;pis ~> phis;rho_r      -- tcInstFun on the args
   (A) rho = T sgs  OR  (B) fiv(phis) = emptyset  -- can_do_ql
   -------------------------------------- APP-QL
   G |-ql h pis : rho ~> qlUnify( rho, rho_r )

(The paper uses a lightning-bolt where we use "ql".)  The most straightforward
way to implement this rule for a call (f e1 ... en) would be:

   1. Take a quick look at the argumets e1..en to guide instantiation
      of the function f.
   2. Then typecheck e1..en from scratch.

That's wasteful, because in Step 1, the quick look at each argument, say (g
h1..hm), involves instantiating `h` and taking a quick look at /its/
arguments.  Then in Step 2 we typecheck (g h1..hm) and again take a quick look
at its arguments.  This is quadratic in the nesting depth of the arguments.

Instead, after the quick look, we /save/ the work we have done in an EValArgQL
record, and /resume/ it later.  The way to think of it is this:

  * `tcApp` typechecks an application.  It uses `tcInstFun`, which in turn
    calls `quickLookArg` on each value argument.

  * `quickLookArg` (which takes a quick look at the argument)

      - Does the "initial" part of `tcApp`, especially `tcInstFun`

      - Captures the result in an EValArgQL record

      - Later, `tcValArg` starts from the EValArgQL record, and
        completes the job of typechecking the application

This turned out to be more subtle than I expected.  Wrinkles:

(QLA1) `quickLookArg` decides whether or not premises (A) and (B) of the
  quick-look-arg judgement APP-QL are satisfied; this is captured in
  `arg_influences_enclosing_call`.

(QLA2) We avoid zonking, so the `arg_influences_enclosing_call` sees the
  argument type /before/ the QL substitution Theta is applied to it. So we
  achieve argument-order independence for free (see 5.7 in the paper).  See the
  `isGuardedTy orig_arg_rho` test in `quickLookArg`.

(QLA3) Deciding whether the premises are satisfied involves calling `tcInstFun`
  (which takes quite some work becuase it calls quickLookArg on nested calls).
  That's why we want to capture the work done, in EValArgQL.

  Do we really have to call `tcInstFun` before deciding (B) of
  `arg_influences_enclosing_call`? Yes (#24686).
  Suppose ids :: [forall a. a->a], and consider
     (:) (reverse ids) blah
  `tcApp` on the outer call will instantiate (:) with `kappa`, and take a
  quick look at (reverse ids). Only after instantiating `reverse` with kappa2,
  quick-looking at `ids` can we discover that (kappa2:=forall a. a->a), which
  satisfies premise (B) of `arg_influence_enclosing_call`.

(QLA4) When we resume typechecking an argument, in `tcValArg` on `EValArgQL`

  - Calling `tcInstFun` on the argument may have emitted some constraints, which
    we carefully captured in `quickLookArg` and stored in the EValArgQL.  We must
    now emit them with `emitConstraints`.  This must be done /under/ the skolemisation
    of the argument's type (see `tcSkolemise` in `tcValArg` for EValArgQL { ...}.
    Example:   f :: (forall b. Ord b => b -> b -> Bool) -> ...
       Call:   f (==)
    we must skolemise the argument type (forall b. Ord b => b -> b -> Bool)
    before emitting the [W] Eq alpha constraint arising from the call to (==).
    It will be solved from the Ord b!

  - quickLookArg may or may not have done `qlUnify` with the calling context.
    If not (eaql_encl = False) must do so now.  Example:  choose [] ids,
            where ids :: [forall a. a->a]
                  choose :: a -> a -> a
    We instantiate choose with `kappa` and discover from `ids` that
    (kappa = [forall a. a->a]).  Now we resume typechecking argument [], and
    we must take advantage of what we have now discovered about `kappa`,
    to typecheck   [] :: [forall a. a->a]

(QLA5) In the quicklook pass, we don't scale multiplicities. Since arguments
    aren't typechecked yet, we don't know their free variable usages
    anyway. But, in a nested call, the head of an application chain is fully
    typechecked.

    In order for the multiplicities in the head to be properly scaled, we store
    the head's usage environment in the eaql_fun_ue field. Then, when we do the
    full-typechecking pass, we can emit the head's usage environment where we
    would have typechecked the head in a naive algorithm.

(QLA6) `quickLookArg` is supposed to capture the result of partially typechecking
   the argument, so it can be resumed later.  "Capturing" should include all
   generated type-class/equality constraints and Linear-Haskell usage info. There
   are two calls in `quickLookArg1` that might generate such constraints:

     - `tcInferAppHead_maybe`.  This can generat Linear-Haskell usage info, via
       the call to `tcEmitBindingUsage` in `check_local_id`, which is called
       indirectly by `tcInferAppHead_maybe`.

       In contrast, `tcInferAppHead_maybe` does not generate any type-class or
       equality constraints, because it doesn't instantiate any functions.  [But
       see #25493 and #25494 for why this isn't quite true today.]

    - `tcInstFun` generates lots of type-class and equality constraints, as it
      instantiates the function.  But it generates no usage info, because that
      comes only from the call to `check_local_id`, whose usage info is captured
      in the call to `tcInferAppHead_maybe` in `quickLookArg1`.

  Conclusion: in quickLookArg1:
    - capture usage information (but not constraints)
        for the call to `tcInferAppHead_maybe`
    - capture constraints (but not usage information)
        for the call to `tcInstFun`

-}

-- | Take a quick look at a value argument during application instantiation.
type QuickLookArg = QLFlag -> Int
                 -> HsExprLoc -- ^ Location span of the whole application
                 -> (HsExpr GhcRn, SrcSpan) -- ^ Head and source span
                 -> LHsExpr GhcRn          -- ^ Argument
                 -> Scaled TcSigmaTypeFRR  -- ^ Type expected by the function
                 -> TcM (HsExprArg 'TcpInst)

quickLookArg :: QuickLookArg
-- See Note [Quick Look at value arguments]
quickLookArg NoQL _ app_lspan _ larg orig_arg_ty
  = skipQuickLook app_lspan larg orig_arg_ty
quickLookArg DoQL pos app_lspan fun_and_lspan larg orig_arg_ty
  = do { setSrcSpanA (getLoc larg) $ rejectNativeQuickLook (scaledThing orig_arg_ty)
       ; is_rho <- qlArgHasRhoType (scaledThing orig_arg_ty)
       ; traceTc "qla" (ppr orig_arg_ty $$ ppr is_rho)
       ; if not is_rho
         then skipQuickLook app_lspan larg orig_arg_ty
         else quickLookArg1 pos app_lspan fun_and_lspan larg orig_arg_ty }

-- | Run Quick Look over outer-scoped lambda result arms to guide impredicative
-- existential witness instantiation before 'tcLambdaMatches' enters the
-- pattern scope, then monomorphise all witness instantiation variables.
-- See Note [Quick Look before lambda patterns].
tcQuickLookLambdaMatches
  :: MatchGroup GhcRn (LHsExpr GhcRn)
  -> [TyVar]
  -> [TcType]
  -> TcType
  -> TcM ()
tcQuickLookLambdaMatches matches binders witnesses inst_body = do
  forwarded <- try_forwarding_input
  unless forwarded $
    case witnesses of
      [_] -> mapM_ (uncurry preview_arm) arms
      _   -> preview_multi_arms arms
  forM_ witnesses $ \w -> do
    rejectNativeQuickLook w
    liftZonkM (qlMonoTcType w)
  where
    arms = collect_arms inst_body matches

    try_forwarding_input
      | [binder] <- binders
      , [witness] <- witnesses
      , Just w_tv <- getTyVar_maybe witness
      , Just (input, _) <- splitVisibleFunTy_maybe inst_body
      , ForAllTy {} <- coreFullView input
      , [L _ Match { m_pats = L _ [L _ (VarPat _ (L _ param))], m_grhss = grhss }]
          <- unLoc (mg_alts matches)
      , Just rhs <- unguardedRhs grhss
      , HsApp _ fun arg <- unLoc (stripParensLHsExpr rhs)
      , HsVar _ arg_name <- unLoc (stripParensLHsExpr arg)
      , unLocWithUserRdr arg_name == param
      , HsVar _ fun_name <- unLoc (stripParensLHsExpr fun)
      , let name = unLocWithUserRdr fun_name
      , name /= param
      = do { mb_id <- tcLookupIdMaybe name
           ; mb_ty <- traverse (liftZonkM . zonkTcType . varType) mb_id
           ; let orig_formal = substTyWith [w_tv] [mkTyVarTy binder] inst_body
           ; orig_formal' <- liftZonkM (zonkTcType orig_formal)
           ; case do { ty <- mb_ty
                     ; guard (noFreeVarsOfType ty)
                     ; exact <- matchExactPolytypeWitness binder orig_formal' ty
                     ; guard (case coreFullView exact of { ForAllTy {} -> True; _ -> False })
                     ; pure exact } of
               Just exact -> liftZonkM (writeMetaTyVar w_tv exact) >> pure True
               Nothing    -> pure False }
      | otherwise = pure False

    preview_arm :: TcType -> LHsExpr GhcRn -> TcM ()
    preview_arm formal rhs = do
      mb_actual <- preview_rhs rhs
      case mb_actual of
        Just actual | supportsQuickLookType actual -> qlUnify formal actual
        _ -> pure ()

    preview_multi_arms :: [(TcType, LHsExpr GhcRn)] -> TcM ()
    preview_multi_arms pairs = do
      let w_tvs = mapMaybe getTyVar_maybe witnesses
      acc_ref <- newTcRef (Map.empty :: Map.Map Name (Maybe TcType))
      forM_ pairs $ \(formal, rhs) -> do
        -- Probe each arm with fresh temporary QLInstVars so conflicting
        -- polytypes across arms reset that witness to monomorphic inference.
        tmp_ws <- mapM (newMetaTyVarTyAtLevel QLInstVar . tyVarKind) w_tvs
        let tmp_formal = substTyWith w_tvs tmp_ws formal
        preview_arm tmp_formal rhs
        forM_ (zip w_tvs tmp_ws) $ \(w_tv, tmp_w) -> do
          val <- liftZonkM (zonkTcType tmp_w)
          let has_poly = not (isTauTy val) && noFreeVarsOfType val
                         && supportsQuickLookType val && noQuickLookVars val
          when has_poly $ do
            acc <- readTcRef acc_ref
            let nm = tyVarName w_tv
                merged = case Map.lookup nm acc of
                  Nothing           -> Just val
                  Just (Just prev)  -> if eqType prev val then Just prev else Nothing
                  Just Nothing      -> Nothing
            writeTcRef acc_ref (Map.insert nm merged acc)
      acc <- readTcRef acc_ref
      forM_ w_tvs $ \w_tv ->
        case Map.lookup (tyVarName w_tv) acc of
          Just (Just agreed) -> liftZonkM (writeMetaTyVar w_tv agreed)
          _                  -> pure ()

    preview_rhs :: LHsExpr GhcRn -> TcM (Maybe TcRhoType)
    preview_rhs expr =
      discardErrs $ discardConstraints $ tryTcDiscardingErrs (pure Nothing) $
        runMaybeT $ do
          res <- preview_expr expr >>= liftMaybeT . liftZonkM . zonkTcType
          guard (supportsQuickLookType res && not (hasCoercionHole res))
          pure res

    preview_expr :: LHsExpr GhcRn -> MaybeT TcM TcRhoType
    preview_expr expr = do
      (rn_fun, args) <- liftMaybeT (splitHsApps (unLoc expr))
      guard (not (has_splice rn_fun) && all no_arg_splice args)
      (tc_fun, declared) <- preview_head rn_fun
      sigma <- liftMaybeT (liftZonkM (zonkTcType declared))
      guard (supportsQuickLookType sigma && not (hasCoercionHole sigma))
      loc <- liftMaybeT (getFunSrcSpan args)
      snd <$> liftMaybeT
        (tcInstFunWithPrefix preview_arg False [] Nothing Nothing Nothing
          DoQL True (rn_fun, loc) tc_fun sigma args)

    preview_head :: HsExpr GhcRn -> MaybeT TcM (HsExpr GhcTc, TcSigmaType)
    preview_head (HsVar _ (L loc (WithUserRdr _ name))) = do
      thing <- if isExternalName name
               then liftMaybeT (tcLookup name)
               else MaybeT (tcLookupLcl_maybe name)
      case thing of
        ATcId { tct_id = ident } -> pure (mkHsVar (L loc ident), varType ident)
        AGlobal (AnId ident)     -> pure (mkHsVar (L loc ident), varType ident)
        AGlobal (AConLike (RealDataCon dc)) ->
          pure (XExpr (ConLikeTc (RealDataCon dc)), varType (dataConWrapId dc))
        _ -> mzero
    preview_head _ = mzero

    preview_arg :: QuickLookArg
    preview_arg _ _ loc _ arg scaled = do
      let expected = scaledThing scaled
      is_rho <- qlArgHasRhoType expected
      when is_rho $ void $ runMaybeT $ do
        actual <- preview_expr arg
        influences <- liftMaybeT (quickLookInfluencesCall expected actual)
        when influences (liftMaybeT (qlUnify actual expected))
      skipQuickLook loc arg scaled

    has_splice HsUntypedSplice{} = True
    has_splice HsTypedSplice{}   = True
    has_splice _                 = False

    no_arg_splice (EValArg { ea_arg = L _ e }) = not (has_splice e)
    no_arg_splice _                            = True

    collect_arms :: TcType -> MatchGroup GhcRn (LHsExpr GhcRn) -> [(TcType, LHsExpr GhcRn)]
    collect_arms body (MG { mg_alts = L _ alts }) = concatMap match_arms alts
      where
        match_arms (L _ (Match { m_pats = L _ pats
                               , m_grhss = GRHSs _ rhss (L _ EmptyLocalBinds{}) }))
          | all isVisArgLPat pats
          , Just formal <- drop_funs (length pats) body
          = foldr (grhs_arms formal) [] rhss
        match_arms _ = []

        grhs_arms formal (L _ (GRHS _ _ rhs)) = expr_arms rhs
          where
            expr_arms e rest = case unLoc (stripParensLHsExpr e) of
              HsIf _ _ yes no      -> expr_arms yes (expr_arms no rest)
              HsMultiIf _ sub_rhss -> foldr (grhs_arms formal) rest sub_rhss
              HsCase _ _ (MG { mg_alts = L _ case_alts }) ->
                foldr (case_arms formal) rest case_alts
              _ -> (formal, e) : rest

        case_arms formal (L _ (Match { m_grhss = GRHSs _ rhss (L _ EmptyLocalBinds{}) })) rest
          = foldr (grhs_arms formal) rest rhss
        case_arms _ _ rest = rest

        drop_funs 0 ty = Just ty
        drop_funs n ty = splitVisibleFunTy_maybe ty >>= drop_funs (n - 1) . snd

{- Note [Quick Look before lambda patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking a lambda at an implicit existential arrow with
'ImpredicativeTypes', 'tcQuickLookLambdaMatches' runs Quick Look over the
outer-scoped result expressions before 'tcLambdaMatches' enters the pattern
scope (Eisenberg et al., ICFP 2021, Fig. 10; Serrano et al., ICFP 2020).
Pattern-bound and guard-bound names are absent from the outer environment and
therefore cannot escape into the existential witnesses. All preview constraints
and errors are discarded, and every witness instantiation variable is
monomorphised before checking the lambda equations once.
-}

qlArgHasRhoType :: TcType -> TcM Bool
-- `qlArgHasRhoType` checks that the expected argument type in rule
-- App-lightning-bolt (Fig 5 in the paper) is indeed a rho-type.
--
-- It must apply the current QL substitution, so it any QLInstTyVar that it
-- comes across.   Why?  See Section 5.7 in the paper; argument order matters.
--
-- What if we find an /un-filled/ QLInstVar?  We treat this as a rho-type
-- even though a later argument might force it to be sigma-type.  See
-- Section 9 in the paper.
--
-- With -XDeepSubsunption we need a /deep/ rho-type.
-- (We don't need getDeepSubsumptionFlag_DataConHead here because this
-- is only about QuickLook.)

qlArgHasRhoType ty
  = do { ds_flag <- getDeepSubsumptionFlag
       ; go ds_flag ty }
  where
    go ds_flag ty
      | isSigmaTy ty
      = return False

      | Just kappa <- getTyVar_maybe ty
      , isQLInstTyVar kappa
      = do { info <- readMetaTyVar kappa
           ; case info of
               Indirect arg_ty' -> go ds_flag arg_ty'
               Flexi            -> return True }

      | Deep {} <- ds_flag
      , Just (_, _, res_ty) <- tcSplitFunTy_maybe ty
      = go ds_flag res_ty

      | otherwise
      = return True

-- | Check premises (A) and (B) of APP-QL using the original argument type.
-- See Note [Quick Look at value arguments] and
-- Note [The fiv test in quickLookArg] in GHC.Tc.Utils.Unify.
quickLookInfluencesCall :: TcRhoType -> TcRhoType -> TcM Bool
quickLookInfluencesCall expected actual
  | isGuardedTy expected = return True
  | otherwise = not <$> anyFreeKappa actual

isGuardedTy :: TcType -> Bool
isGuardedTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | otherwise                               = False

{- Note [Quick Look and ordinary argument checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A plain call such as print (run reflect input) can check run's higher-rank
retained argument using the same ordinary path as a direct call. Defer only
when its declared telescope supplies that argument type and its result is an
ordinary monotype. Restrict this classifier to written value arguments and
parentheses so the ordinary check never replays an untyped Template Haskell
expansion.
-}

-- | Leave checked native arguments in plain named calls to the ordinary checker.
-- See Note [Quick Look and ordinary argument checking].
tcQuickLookNeedsOrdinaryArgs :: HsExpr GhcRn -> [HsExprArg 'TcpRn] -> TcM Bool
tcQuickLookNeedsOrdinaryArgs (HsVar _ lname) args
  | let name = unLocWithUserRdr lname
  , not (isDataOcc (nameOccName name))
  , any isHsValArg args, all isPlainQuickLookArg args
  = do { ident <- tcLookupIdMaybe name
       ; return $ case ident of
           Just var -> isGroundRankedRetainedCall (count isHsValArg args) (varType var)
           Nothing  -> False }
tcQuickLookNeedsOrdinaryArgs _ _ = return False

-- | These arguments can be left for ordinary checking without replaying an expansion.
isPlainQuickLookArg :: HsExprArg 'TcpRn -> Bool
isPlainQuickLookArg EValArg{} = True
isPlainQuickLookArg (EWrap EPar{}) = True
isPlainQuickLookArg _ = False

-- | Select a constructor result from a complete expected type.
-- See Note [Retained constructors with a known Quick Look result].
tcKnownRetainedConstructorResult :: [HsExprArg 'TcpRn] -> HsExpr GhcTc
                                 -> TcSigmaType -> TcRhoType -> TcM (Maybe TcRhoType)
tcKnownRetainedConstructorResult args (XExpr (ConLikeTc (RealDataCon dc))) _sigma expected
  | let fields = dataConRepArgBinders dc
  , any isJust fields
  , length fields == count isHsValArg args
  , all isPlainQuickLookArg args
  = runMaybeT $ do
      expected <- liftMaybeT (liftZonkM (zonkTcType expected))
      guard (not (anyFreeVarsOfType (\v -> isTcTyVar v && isMetaTyVar v) expected))
      result <- MaybeT (resolve (dataConTyCon dc) expected)
      guard (isRetainedValueType result)
      pure result
  where

    resolve parent ty
      | Just (tc, _) <- tcSplitTyConApp_maybe ty
      , tc == parent
      = return (Just ty)
      | Just (family, _) <- tcSplitTyConApp_maybe ty
      , isTypeFamilyTyCon family
      = do { envs <- tcGetFamInstEnvs
           ; loc <- getCtLocM (OccurrenceOf (tyConName family)) (Just TypeLevel)
           ; tryTcDiscardingErrs (return Nothing) (follow envs loc ty) }
      | otherwise = return Nothing
      where
        follow envs loc ty
          = case tcSplitTyConApp_maybe ty of
              Just (tc, _) | tc == parent -> return (Just ty)
              Just (tc, tys)
                | isTypeFamilyTyCon tc
                , Just (Reduction _ rhs) <- reduceTyFamApp_maybe envs Nominal tc tys
                -> do { when (isConstrainedFamilyTyCon tc) $
                          checkNoErrs (checkConstrainedSignature TypeAppCtxt ty)
                      ; loc <- bumpReductionDepthTcM loc ty
                      ; follow envs loc rhs }
              _ -> return Nothing
tcKnownRetainedConstructorResult _ _ _ _ = return Nothing

{- Note [Retained constructors with a known Quick Look result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A fully applied retained constructor can get its erased parameters from a
known expected result, as in consume (Pair True payload). A type family in
the expected type can hide the constructor's parent. Follow nominal family
equations at the head only. An associated family's nominal equation can
match without evidence for its domain. Check that domain in the current
lexical scope before using the result of each such step. This is the same
domain check used by retained package patterns. A missing domain leaves
the application to ordinary checking. The preview supplies no evidence
or type variable assignment. Final subsumption still checks the original
result and records any evidence use.

Quick Look on the argument would instantiate the constructor without this
expected result. If the existing worker matcher finds every erased
parameter, leave the entire application to ordinary argument checking.
It checks the retained operand and each field against the constructor
telescope. No part of this preview assigns a metavariable or inspects a
written operand. Restrict it to plain value applications so the final
check does not repeat a source expansion.
-}

quickLookArg1 :: Int
              -> HsExprLoc
              -> (HsExpr GhcRn, SrcSpan)
              -> LHsExpr GhcRn
              -> Scaled TcRhoType  -- Deeply skolemised
              -> TcM (HsExprArg 'TcpInst)
-- quickLookArg1 implements the "QL Argument" judgement in Fig 5 of the paper
quickLookArg1 pos app_lspan rn_head larg@(L _ arg) sc_arg_ty@(Scaled _ orig_arg_rho)
  = addArgCtxt pos rn_head larg $ -- Context needed for constraints
                                           -- generated by calls in arg
    do { traceTc "qla1" (ppr arg)

       ; (rn_fun_arg, rn_args) <- splitHsApps arg

       ; traceTc "qla2" (ppr arg)

       ; fun_lspan_arg <- getFunSrcSpan rn_args

       -- Step 1: get the type of the head of the argument
       ; ordinary_args <- tcQuickLookNeedsOrdinaryArgs rn_fun_arg rn_args
       ; (fun_ue, mb_fun_ty) <- if ordinary_args || any isExplicitRetainedArg rn_args
                               then return (zeroUE, Nothing)
                               else tcCollectingUsage $ tcInferAppHead_maybe rn_fun_arg
         -- tcCollectingUsage: the use of an Id at the head generates usage-info
         -- See the call to `tcEmitBindingUsage` in `check_local_id`.  So we must
         -- capture and save it in the `EValArgQL`.  See (QLA6) in
         -- Note [Quick Look at value arguments]

       ; lvl <- getTcLevel
       ; traceTc "quickLookArg {" $
         vcat [ text "arg:" <+> ppr arg
              , text "orig_arg_rho:" <+> ppr orig_arg_rho
              , text "head:" <+> ppr rn_fun_arg <+> dcolon <+> ppr mb_fun_ty
              , text "args:" <+> ppr rn_args
              , text "level:" <+> ppr lvl ]

       ; known_result <- case mb_fun_ty of
           Just (fun, sigma) -> setSrcSpan fun_lspan_arg $ do
             result <- tcKnownRetainedConstructorResult rn_args fun sigma orig_arg_rho
             if isJust result then return True else
               case fun of
                 _ | is_named_or_con fun
                   , all isPlainQuickLookArg rn_args
                   , let (_tvs, _theta, tau) = tcRetainedInstTelescope sigma
                         arity = count isHsValArg rn_args
                   , takesDirectIndexArgument tau
                   , isJust (tcRetainedValueResultShape arity tau) ->
                   return (hasNativeDependentExpr (Core.Type tau)
                           || tcAllInvisTvsInArgs sigma rn_args)
                 _ -> return False
           Nothing -> return False

       ; let has_pkg_head = case mb_fun_ty of
               Just (_, sigma)
                 | any isHsValArg rn_args
                 , ExistsTy ImplicitExists _ _ <- implicitSpineHead sigma -> True
               _ -> False
       ; case mb_fun_ty of {
           _ | known_result || has_pkg_head -> skipQuickLook app_lspan larg sc_arg_ty ;
           Nothing -> skipQuickLook app_lspan larg sc_arg_ty ;    -- fun is too complicated
           Just (tc_fun_arg_head, fun_sigma_arg_head) ->

       -- step 2: use |-inst to instantiate the head applied to the arguments
    do { let arg_tc_head = (tc_fun_arg_head, fun_lspan_arg)
       ; do_ql <- wantQuickLook rn_fun_arg

       ; ((inst_args, app_res_rho), wanted)
             <- captureConstraints $
                tcInstFun do_ql True (rn_fun_arg, fun_lspan_arg) tc_fun_arg_head fun_sigma_arg_head rn_args
                -- We must capture type-class and equality constraints here, but
                -- not usage constraints.  See (QLA6) in Note [Quick Look at
                -- value arguments]

       ; traceTc "quickLookArg 2" $
         vcat [ text "arg:" <+> ppr arg
              , text "orig_arg_rho:" <+> ppr orig_arg_rho
              , text "app_res_rho:" <+> ppr app_res_rho ]

       -- Step 3: Check the two other premises of APP-lightning-bolt (Fig 5 in the paper)
       --         Namely: (A) is orig_arg_rho is guarded
       --             or: (B) fiv(app_res_rho) = emptyset
       -- then Step 4: do quick-look unification if either (A) or (B) hold
       --
       -- For (B) see Note [The fiv test in quickLookArg] in GHC.Tc.Utils.Unify
       -- This tells us if the quick look at the argument yields information that
       -- influences the enclosing function call
       -- NB: `isGuardedTy` is computed based on the original,
       --         unzonked orig_arg_rho, so that we deliberately do not exploit
       --         guardedness that emerges a result of QL on earlier args
       --     In contrast, we must do the `anyFreeKappa` test /after/ tcInstFun; see (QLA3).
       ; arg_influences_enclosing_call <- quickLookInfluencesCall orig_arg_rho app_res_rho

       -- Step 4: do quick-look unification if either (A) or (B) hold
       -- This is part of APP-lightning-bolt (Fig 5 in the paper)
       -- NB: orig_arg_rho may not be zonked, but that's ok
       ; when arg_influences_enclosing_call $
         qlUnify app_res_rho orig_arg_rho

       ; traceTc "quickLookArg done }" (ppr rn_fun_arg)

       ; return (EValArgQL { eaql_loc_span = app_lspan
                           , eaql_arg_ty   = sc_arg_ty
                           , eaql_larg     = larg
                           , eaql_tc_fun   = arg_tc_head
                           , eaql_rn_fun   = rn_fun_arg
                           , eaql_fun_ue   = fun_ue
                           , eaql_args     = inst_args
                           , eaql_wanted   = wanted
                           , eaql_encl     = arg_influences_enclosing_call
                           , eaql_res_rho  = app_res_rho }) }}}
  where
    is_named_or_con HsVar{}                            = True
    is_named_or_con (XExpr (ConLikeTc (RealDataCon _))) = True
    is_named_or_con _                                  = False

{- *********************************************************************
*                                                                      *
                 Folding over instantiation variables
*                                                                      *
********************************************************************* -}

qlMonoInstArgs :: [HsExprArg 'TcpInst] -> ZonkM ()
-- See Note [Monomorphise instantiation variables] in GHC.Tc.Utils.Unify
qlMonoInstArgs args = mapM_ qlMonoInstArg args

qlMonoInstArg :: HsExprArg 'TcpInst -> ZonkM ()
-- See Note [Monomorphise instantiation variables] in GHC.Tc.Utils.Unify
qlMonoInstArg (EWrap (EHsWrap w)) = qlMonoHsWrapper w
qlMonoInstArg _                   = return ()

qlMonoHsWrapper :: HsWrapper -> ZonkM ()
-- See Note [Monomorphise instantiation variables] in GHC.Tc.Utils.Unify
qlMonoHsWrapper (WpCompose w1 w2)   = qlMonoHsWrapper w1 >> qlMonoHsWrapper w2
qlMonoHsWrapper (WpTyApp ty)        = qlMonoTcType ty
qlMonoHsWrapper (WpExistsPack s w)  = qlMonoTcType s >> qlMonoTcType w
qlMonoHsWrapper _                   = return ()

-- | Check the type arguments that Quick Look will monomorphise.
rejectNativeQLInstArgs :: [HsExprArg 'TcpInst] -> TcM ()
rejectNativeQLInstArgs = mapM_ check_arg
  where
    check_arg (EWrap (EHsWrap wrap)) = check_wrap wrap
    check_arg _ = return ()

    check_wrap (WpCompose wrap1 wrap2) = check_wrap wrap1 >> check_wrap wrap2
    check_wrap (WpTyApp ty)            = rejectNativeQuickLook ty
    check_wrap (WpExistsPack s w)      = rejectNativeQuickLook s >> rejectNativeQuickLook w
    check_wrap _                       = return ()

{- *********************************************************************
*                                                                      *
                 tagToEnum#
*                                                                      *
********************************************************************* -}

{- Note [tagToEnum#]
~~~~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  It's crude, because it relies on our
knowing *now* that the type is ok, which in turn relies on the
eager-unification part of the type checker pushing enough information
here.  In theory the Right Thing to do is to have a new form of
constraint but I definitely cannot face that!  And it works ok as-is.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

When data type families are involved it's a bit more complicated.
     data family F a
     data instance F [Int] = A | B | C
Then we want to generate something like
     tagToEnum# R:FListInt 3# |> co :: R:FListInt ~ F [Int]
Usually that coercion is hidden inside the wrappers for
constructors of F [Int] but here we have to do it explicitly.

It's all grotesquely complicated.
-}

isTagToEnum :: HsExpr GhcTc -> Bool
isTagToEnum (HsVar _ (L _ fun_id)) = fun_id `hasKey` tagToEnumKey
isTagToEnum _ = False

tcTagToEnum :: HsExpr GhcTc -> [HsExprArg 'TcpTc] -> TcRhoType
            -> TcM (HsExpr GhcTc)
-- tagToEnum# :: forall a. Int# -> a
-- See Note [tagToEnum#]   Urgh!
tcTagToEnum tc_fun tc_args res_ty
  | [val_arg] <- dropWhile (not . isHsValArg) tc_args
  = do { res_ty <- liftZonkM $ zonkTcType res_ty

       -- Check that the type is algebraic
       ; case tcSplitTyConApp_maybe res_ty of {
           Nothing -> do { addErrTc (TcRnTagToEnumUnspecifiedResTy res_ty)
                         ; vanilla_result } ;
           Just (tc, tc_args) ->

    do { -- Look through any type family
       ; fam_envs <- tcGetFamInstEnvs
       ; case tcLookupDataFamInst_maybe fam_envs tc tc_args of {
           Nothing -> do { check_enumeration res_ty tc
                         ; vanilla_result } ;
           Just (rep_tc, rep_args, coi) ->

    do { -- coi :: tc tc_args ~R rep_tc rep_args
         check_enumeration res_ty rep_tc
       ; let rep_ty  = mkTyConApp rep_tc rep_args
             tc_fun' = mkHsWrap (WpTyApp rep_ty) tc_fun
             df_wrap = mkWpCastR (mkSymCo coi)
             tc_expr = rebuildHsApps tc_fun' [val_arg]
       ; return (mkHsWrap df_wrap tc_expr) }}}}}

  | otherwise
  = failWithTc TcRnTagToEnumMissingValArg

  where
    vanilla_result = return (rebuildHsApps tc_fun tc_args)

    check_enumeration ty' tc
      | -- isTypeDataTyCon: see wrinkle (W1) in
        -- Note [Type data declarations] in GHC.Rename.Module
        isTypeDataTyCon tc    = addErrTc (TcRnTagToEnumResTyTypeData ty')
      | isEnumerationTyCon tc = return ()
      | otherwise             = addErrTc (TcRnTagToEnumResTyNotAnEnum ty')

{- *********************************************************************
*                                                                      *
             Pragmas on expressions
*                                                                      *
********************************************************************* -}

tcExprPrag :: HsPragE GhcRn -> HsPragE GhcTc
tcExprPrag (HsPragSCC x1 ann) = HsPragSCC x1 (tcStringLit ann)

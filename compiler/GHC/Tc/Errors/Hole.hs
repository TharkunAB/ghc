{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Tc.Errors.Hole
   ( findValidHoleFits
   , tcCheckHoleFit
   , withoutUnification
   , tcSubsumes
   , isFlexiTyVar
   , tcFilterHoleFits
   , getLocalBindings
   , addHoleFitDocs
   , getHoleFitSortingAlg
   , getHoleFitDispConfig
   , HoleFitDispConfig (..)
   , HoleFitSortingAlg (..)
   , relevantCtEvidence
   , zonkSubs

   , sortHoleFitsByGraph
   , sortHoleFitsBySize


   -- Re-exported from GHC.Tc.Errors.Hole.Plugin
   , HoleFitPlugin (..), HoleFitPluginR (..)
   )
where

import GHC.Prelude

import GHC.Tc.Errors.Types ( HoleFitDispConfig(..), FitsMbSuppressed(..)
                           , ValidHoleFits(..), noValidHoleFits )
import GHC.Tc.Types
import GHC.Tc.Types.LclEnv ( setLclEnvRdrEnv )
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcMType
import GHC.Tc.TyCl.PatSyn (patSynBuilderOcc)
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.CtLoc
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.TcType
import GHC.Core.TyCon ( TyCon, isGenerativeTyCon, isInjectiveTyCon, isAlgTyCon, isTypeFamilyTyCon
                      , isPromotedDataCon_maybe, tyConDataCons )
import GHC.Core.TyCo.Rep ( Type(..) )
import GHC.Core.Type ( funTyFlagTyCon, coreFullView, splitTyConApp_maybe
                     , isFamFreeTy, isForAllTy, typeKind, replaceTermVarInType )
import GHC.Core.Coercion ( abstractNativeIndices, coercionRole )
import GHC.Core.Dependent ( hasNativeDependentExpr )
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Unify ( tcMatchTyX_BM, BindFlag(..) )
import qualified GHC.Core as Core
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.DataCon
import GHC.Core.PatSyn (patSynName)
import GHC.Core.Predicate ( Pred(..), classifyPredType, eqRelRole
                          , isIPClass )
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Builtin.Modules ( gHC_INTERNAL_ERR, gHC_INTERNAL_UNSAFE_COERCE )
import GHC.Builtin.WiredIn.Types ( tupleDataConName, unboxedSumDataConName )
import GHC.Types.Id
import GHC.Types.Name.Env ( NameEnv, emptyNameEnv, extendNameEnv, lookupNameEnv, elemNameEnv )
import GHC.Types.Name.Set ( extendNameSet, NameSet, emptyNameSet )
import GHC.Unit.External ( eps_PTE )
import GHC.Unit.Module ( moduleUnit, moduleName, moduleNameString )
import GHC.Unit.Types ( unitString )
import GHC.Types.Var ( TyVar, mkTyVar, setTyVarKind, isVisibleFunArg, isInvisibleForAllTyFlag
                     , DepFunTyFlag(..), VarBndr(..), isNonCoVarId
                     , tyVarKind, varType )
import GHC.Types.Var.Set
import GHC.Types.Var.Env ( TidyEnv )
import GHC.Types.TyThing
import GHC.Data.Bag
import GHC.Core.ConLike ( ConLike(..) )
import GHC.Utils.Misc
import GHC.Tc.Utils.Env ( tcLookup, tcExtendIdEnv )
import GHC.Utils.Outputable
import GHC.Driver.DynFlags
import GHC.Data.Maybe

import Control.Arrow ( (&&&) )

import Control.Monad    ( filterM, replicateM, foldM )
import Data.Function    ( on )
import Data.List        ( partition, sort, sortOn, nubBy, isPrefixOf )
import Data.Graph       ( graphFromEdges, topSort )


import GHC.Tc.Solver    ( simplifyTopWanteds, tcRewriteFamilyAxioms )
import GHC.Tc.Solver.Monad ( runTcSEarlyAbort, matchFamTcM )
import GHC.Tc.Instance.Class ( matchGlobalInst, ClsInstResult(..) )
import GHC.Tc.Utils.Unify
  ( DeepSubsumptionFlag(..), DeepSubsumptionDepth(..)
  , tcSubTypeHoleFit
  )

import GHC.HsToCore.Docs ( extractDocs )
import GHC.Hs.Doc
import GHC.Unit.Module.ModIface ( mi_docs )
import GHC.Iface.Load  ( loadInterfaceForName )

import GHC.Builtin (wiredInNames)

import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Errors.Hole.Plugin
import qualified Data.Set as Set
import GHC.Types.SrcLoc
import GHC.Data.FastString (NonDetFastString(..))
import GHC.Types.Unique.Map

import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions as LangExt

{-
Note [Valid hole fits include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`findValidHoleFits` returns the "Valid hole fits include ..." message.
For example, look at the following definitions in a file called test.hs:

   import Data.List (inits)

   f :: [String]
   f = _ "hello, world"

The hole in `f` would generate the message:

  • Found hole: _ :: [Char] -> [String]
  • In the expression: _
    In the expression: _ "hello, world"
    In an equation for ‘f’: f = _ "hello, world"
  • Relevant bindings include f :: [String] (bound at test.hs:6:1)
    Valid hole fits include
      lines :: String -> [String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      words :: String -> [String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      inits :: forall a. [a] -> [[a]]
        with inits @Char
        (imported from ‘Data.List’ at mpt.hs:4:19-23
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      repeat :: forall a. a -> [a]
        with repeat @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.List’))
      fail :: forall (m :: * -> *). Monad m => forall a. String -> m a
        with fail @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      return :: forall (m :: * -> *). Monad m => forall a. a -> m a
        with return @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      pure :: forall (f :: * -> *). Applicative f => forall a. a -> f a
        with pure @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      read :: forall a. Read a => String -> a
        with read @[String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘Text.Read’))
      mempty :: forall a. Monoid a => a
        with mempty @([Char] -> [String])
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))

Valid hole fits are found by checking top level identifiers and local bindings
in scope for whether their type can be instantiated to the type of the hole.
Additionally, we also need to check whether all relevant constraints are solved
by choosing an identifier of that type as well, see Note [Relevant constraints]

Since checking for subsumption results in the side-effect of type variables
being unified by the simplifier, we need to take care to restore them after
to being flexible type variables after we've checked for subsumption.
This is to avoid affecting the hole and later checks by prematurely having
unified one of the free unification variables.

When outputting, we sort the hole fits by the size of the types we'd need to
apply by type application to the type of the fit to make it fit. This is done
in order to display "more relevant" suggestions first. Another option is to
sort by building a subsumption graph of fits, i.e. a graph of which fits subsume
what other fits, and then outputting those fits which are subsumed by other
fits (i.e. those more specific than other fits) first. This results in the ones
"closest" to the type of the hole to be displayed first.

To help users understand how the suggested fit works, we also display the values
that the quantified type variables would take if that fit is used, like
`mempty @([Char] -> [String])` and `pure @[] @String` in the example above.
If -XTypeApplications is enabled, this can even be copied verbatim as a
replacement for the hole.

Note [Checking hole fits]
~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a hole of type hole_ty, we want to know whether a variable
of type ty is a valid fit for the whole. This is a subsumption check:
we wish to know whether ty <: hole_ty. But, of course, the check
must take into account any givens and relevant constraints.
(See also Note [Relevant constraints]).

For the simplifier to be able to use any givens present in the enclosing
implications to solve relevant constraints, we nest the wanted subsumption
constraints and relevant constraints within the enclosing implications.

As an example, let's look at the following code:

  f :: Show a => a -> String
  f x = show _

Suppose the hole is assigned type a0_a1pd[tau:2].
Here the nested implications are just one level deep, namely:

  [Implic {
      TcLevel = 2
      Skolems = a_a1pa[sk:2]
      No-eqs = True
      Status = Unsolved
      Given = $dShow_a1pc :: Show a_a1pa[sk:2]
      Wanted =
        WC {wc_simple =
              [W] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CDictCan(psc))}
      Binds = EvBindsVar<a1pi>
      Needed inner = []
      Needed outer = []
      the type signature for:
        f :: forall a. Show a => a -> String }]

As we can see, the givens say that the skolem
`a_a1pa[sk:2]` fulfills the Show constraint, and that we must prove
the [W] Show a0_a1pd[tau:2] constraint -- that is, whatever fills the
hole must have a Show instance.

When we now check whether `x :: a_a1pa[sk:2]` fits the hole in
`tcCheckHoleFit`, the call to `tcSubType` will end up unifying the meta type
variable `a0_a1pd[tau:2] := a_a1pa[sk:2]`. By wrapping the wanted constraints
needed by tcSubType_NC and the relevant constraints (see Note [Relevant
constraints] for more details) in the nested implications, we can pass the
information in the givens along to the simplifier. For our example, we end up
needing to check whether the following constraints are soluble.

  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems = a_a1pa[sk:2]
          No-eqs = True
          Status = Unsolved
          Given = $dShow_a1pc :: Show a_a1pa[sk:2]
          Wanted =
            WC {wc_simple =
                  [W] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)}
          Binds = EvBindsVar<a1pl>
          Needed inner = []
          Needed outer = []
          the type signature for:
            f :: forall a. Show a => a -> String }}

But since `a0_a1pd[tau:2] := a_a1pa[sk:2]` and we have from the nested
implications that Show a_a1pa[sk:2] is a given, this is trivial, and we end up
with a final WC of WC {}, confirming x :: a0_a1pd[tau:2] as a match.

To avoid side-effects on the nested implications, we create a new EvBindsVar so
that any changes to the ev binds during a check remains localised to that check.
In addition, we call withoutUnification to reset any unified metavariables; this
call is actually done outside tcCheckHoleFit so that the results can be formatted
for the user before resetting variables.

Note [Deep subsumption in tcCheckHoleFit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To check that a candidate fits in a hole, we perform a subsumption check, as
detailed in Note [Checking hole fits]. However, should we also perform deep
subsumption? Well, certainly if the user has enabled deep subsumption, and also
in cases where deep subsumption is required such as to perform eta-expansion
of data constructors, e.g.

  data T = MkT Int Bool -- so that MkT :: Int %1 -> Bool %1 -> T

  foo :: Int %1 -> Bool -> T
  foo = _

We should suggest MkT as a valid hole fit, because deep subsumption will
eta expand to make the multiplicities line up, as per
Note [Typechecking data constructors] in GHC.Tc.Gen.Head.

Note [Valid refinement hole fits include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the `-frefinement-level-hole-fits=N` flag is given, we additionally look
for "valid refinement hole fits"", i.e. valid hole fits with up to N
additional holes in them.

With `-frefinement-level-hole-fits=0` (the default), GHC will find all
identifiers 'f' (top-level or nested) that will fit in the hole.

With `-frefinement-level-hole-fits=1`, GHC will additionally find all
applications 'f _' that will fit in the hole, where 'f' is an in-scope
identifier, applied to single argument.  It will also report the type of the
needed argument (a new hole).

And similarly as the number of arguments increases

As an example, let's look at the following code:

  f :: [Integer] -> Integer
  f = _

with `-frefinement-level-hole-fits=1`, we'd get:

  Valid refinement hole fits include

    foldl1 (_ :: Integer -> Integer -> Integer)
      with foldl1 @[] @Integer
      where foldl1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    foldr1 (_ :: Integer -> Integer -> Integer)
      with foldr1 @[] @Integer
      where foldr1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    const (_ :: Integer)
      with const @Integer @[Integer]
      where const :: forall a b. a -> b -> a
    ($) (_ :: [Integer] -> Integer)
      with ($) @'GHC.Types.LiftedRep @[Integer] @Integer
      where ($) :: forall a b. (a -> b) -> a -> b
    fail (_ :: String)
      with fail @((->) [Integer]) @Integer
      where fail :: forall (m :: * -> *).
                    Monad m =>
                    forall a. String -> m a
    return (_ :: Integer)
      with return @((->) [Integer]) @Integer
      where return :: forall (m :: * -> *). Monad m => forall a. a -> m a
    (Some refinement hole fits suppressed;
      use -fmax-refinement-hole-fits=N or -fno-max-refinement-hole-fits)

Which are hole fits with holes in them. This allows e.g. beginners to
discover the fold functions and similar, but also allows for advanced users
to figure out the valid functions in the Free monad, e.g.

  instance Functor f => Monad (Free f) where
      Pure a >>= f = f a
      Free f >>= g = Free (fmap _a f)

Will output (with -frefinment-level-hole-fits=1):
    Found hole: _a :: Free f a -> Free f b
          Where: ‘a’, ‘b’ are rigid type variables bound by
                  the type signature for:
                    (>>=) :: forall a b. Free f a -> (a -> Free f b) -> Free f b
                  at fms.hs:25:12-14
                ‘f’ is a rigid type variable bound by
    ...
    Relevant bindings include
      g :: a -> Free f b (bound at fms.hs:27:16)
      f :: f (Free f a) (bound at fms.hs:27:10)
      (>>=) :: Free f a -> (a -> Free f b) -> Free f b
        (bound at fms.hs:25:12)
    ...
    Valid refinement hole fits include
      ...
      (=<<) (_ :: a -> Free f b)
        with (=<<) @(Free f) @a @b
        where (=<<) :: forall (m :: * -> *) a b.
                      Monad m =>
                      (a -> m b) -> m a -> m b
        (imported from ‘Prelude’ at fms.hs:5:18-22
        (and originally defined in ‘GHC.Base’))
      ...

Where `(=<<) _` is precisely the function we want (we ultimately want `>>= g`).

We find these refinement suggestions by considering hole fits that don't
fit the type of the hole, but ones that would fit if given an additional
argument. We do this by creating a new type variable with `newOpenFlexiTyVar`
(e.g. `t_a1/m[tau:1]`), and then considering hole fits of the type
`t_a1/m[tau:1] -> v` where `v` is the type of the hole.

Since the simplifier is free to unify this new type variable with any type, we
can discover any identifiers that would fit if given another identifier of a
suitable type. This is then generalized so that we can consider any number of
additional arguments by setting the `-frefinement-level-hole-fits` flag to any
number, and then considering hole fits like e.g. `foldl _ _` with two additional
arguments.

To make sure that the refinement hole fits are useful, we check that the types
of the additional holes have a concrete value and not just an invented type
variable. This eliminates suggestions such as `head (_ :: [t0 -> a]) (_ :: t0)`,
and limits the number of less than useful refinement hole fits.

Additionally, to further aid the user in their implementation, we show the
types of the holes the binding would have to be applied to in order to work.
In the free monad example above, this is demonstrated with
`(=<<) (_ :: a -> Free f b)`, which tells the user that the `(=<<)` needs to
be applied to an expression of type `a -> Free f b` in order to match.
If -XScopedTypeVariables is enabled, this hole fit can even be copied verbatim.

Note [Relevant constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
As highlighted by #14273, we need to check any relevant constraints as well
as checking for subsumption. Relevant constraints are the simple constraints
whose free unification variables are mentioned in the type of the hole.

In the simplest case, these are all non-hole constraints in the simples, such
as is the case in

  f :: String
  f = show _

Here, the hole is given type a0_a1kv[tau:1]. Then, the emitted constraint is:

  [W] $dShow_a1kw {0}:: Show a0_a1kv[tau:1] (CNonCanonical)

However, when there are multiple holes, we need to be more careful. As an
example, Let's take a look at the following code:

  f :: Show a => a -> String
  f x = show (_b (show _a))

Here there are two holes, `_a` and `_b`. Suppose _a :: a0_a1pd[tau:2] and
_b :: a1_a1po[tau:2]. Then, the simple constraints passed to
findValidHoleFits are:

  [[W] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical),
    [W] $dShow_a1pp {0}:: Show a1_a1po[tau:2] (CNonCanonical)]

When we are looking for a match for the hole `_a`, we filter the simple
constraints to the "Relevant constraints", by throwing out any constraints
which do not mention a variable mentioned in the type of the hole. For hole
`_a`, we will then only require that the `$dShow_a1pe` constraint is solved,
since that is the only constraint that mentions any free type variables
mentioned in the hole constraint for `_a`, namely `a_a1pd[tau:2]`, and
similarly for the hole `_b` we only require that the `$dShow_a1pe` constraint
is solved.

Note [Leaking errors]
~~~~~~~~~~~~~~~~~~~~~
When considering candidates, GHC believes that we're checking for validity in
actual source. However, As evidenced by #15321, #15007 and #15202, this can
cause bewildering error messages. The solution here is simple: if a candidate
would cause the type checker to error, it is not a valid hole fit, and thus it
is discarded.

Note [Speeding up valid hole-fits]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When computing valid hole fits, we want to quickly rule out identifiers that
clearly don't fit the type of the hole, without doing too much work.
This is important for the performance of the "valid hole fits" feature,
which is known to be slow in some cases (#16875).

The valid hole fits machinery is given the type of the hole and a possible
candidate identifier, and it computes whether the identifier can be used by
performing a subtype check

  tcSubTypeHoleFit .. cand_ty hole_ty

which checks that the type of the candidate identifier is more general than
the type of the hole.

We currently use the following shortcuts.

  1. When computing a set of candidate identifiers for a hole:

    (FastHoles1)
      If 'hole_ty' is an immutable type variable (i.e. a skolem type variable),
      then the only possible (useful) fits are fits of exactly that type, which
      can only come from locally bound variables for which that skolem is in scope.
      In that case, only include local identifiers in the list of candidate Ids.

      This speeds things up quite a bit when we don't know anything about the type
      of the hole, and helps with degenerate fits like (`id (_ :: a)` and `head (_ :: [a])`)
      when looking for fits of type `a`, where `a` is a skolem.

  2. When checking whether a particular candidate 'cand_ty :: cand_ty' fits 'hole_ty':

    (FastHoles2)
      Abort early if 'cand_ty' is obviously not a subtype of 'hole_ty',
      according to a cheap test. The current cheap test is in 'definitelyNotSubType',
      which detects the following cases:

        1. 'cand_ty' and 'hole_ty' have a different TyCon at the head, after
           looking through foralls and (=>) arrows, e.g.:
              hole_ty = Int     -- headed by Int
              cand_ty = Maybe a -- headed by Maybe
           or
              hole_ty = forall a b. a -> b               -- headed by (->)
              cand_ty = forall x y. Num x => Either x y  -- headed by Either
        2. 'hole_ty' is polymorphic but 'cand_ty' has no polymorphism, e.g.
              hole_ty = forall a. a -> a
              cand_ty = Int -> Int

    (FastHoles3)
      After calling 'tcSubTypeHoleFit' but before running the solver on the
      constraints that it generated, do a quick check to see if any constraint
      is obviously insoluble. See Note [tcCheckHoleFit: fast insolubility check].

    (FastHoles4)
      When running the solver on the constraints generated by 'tcSubTypeHoleFit',
      do it in a special mode that stops immediately as soon as it spots an
      insoluble constraint, using 'runTcSEarlyAbort'.
-}

-- We read the various -no-show-*-of-hole-fits flags
-- and set the display config accordingly.
getHoleFitDispConfig :: TcM HoleFitDispConfig
getHoleFitDispConfig
  = do { sWrap <- goptM Opt_ShowTypeAppOfHoleFits
       ; sWrapVars <- goptM Opt_ShowTypeAppVarsOfHoleFits
       ; sType <- goptM Opt_ShowTypeOfHoleFits
       ; sProv <- goptM Opt_ShowProvOfHoleFits
       ; sMatc <- goptM Opt_ShowMatchesOfHoleFits
       ; return HFDC{ showWrap = sWrap, showWrapVars = sWrapVars
                    , showProv = sProv, showType = sType
                    , showMatches = sMatc } }

-- Which sorting algorithm to use
data HoleFitSortingAlg = HFSNoSorting      -- Do not sort the fits at all
                       | HFSBySize         -- Sort them by the size of the match
                       | HFSBySubsumption  -- Sort by full subsumption
                deriving (Eq, Ord)

getHoleFitSortingAlg :: TcM HoleFitSortingAlg
getHoleFitSortingAlg =
    do { shouldSort <- goptM Opt_SortValidHoleFits
       ; subsumSort <- goptM Opt_SortBySubsumHoleFits
       ; sizeSort <- goptM Opt_SortBySizeHoleFits
       -- We default to sizeSort unless it has been explicitly turned off
       -- or subsumption sorting has been turned on.
       ; return $ if not shouldSort
                    then HFSNoSorting
                    else if subsumSort
                         then HFSBySubsumption
                         else if sizeSort
                              then HFSBySize
                              else HFSNoSorting }

-- If enabled, we go through the fits and add any associated documentation,
-- by looking it up in the module or the environment (for local fits)
addHoleFitDocs :: [HoleFit] -> TcM [HoleFit]
addHoleFitDocs fits =
  do { showDocs <- goptM Opt_ShowDocsOfHoleFits
     ; if showDocs
       then do { dflags <- getDynFlags
               ; mb_local_docs <- extractDocs dflags =<< getGblEnv
               ; (mods_without_docs, fits') <- mapAccumLM (upd mb_local_docs) Set.empty fits
               ; report mods_without_docs
               ; return fits' }
       else return fits }
  where
   msg = text "GHC.Tc.Errors.Hole addHoleFitDocs"
   upd mb_local_docs mods_without_docs (TcHoleFit fit@(HoleFit {hfCand = cand})) =
     let name = getName cand in
     do { mb_docs <- if hfIsLcl fit
                     then pure mb_local_docs
                     else mi_docs <$> loadInterfaceForName msg name
        ; case mb_docs of
            { Nothing -> return (Set.insert (nameOrigin name) mods_without_docs, TcHoleFit fit)
            ; Just docs -> do
                { let doc = lookupUniqMap (docs_decls docs) name
                ; return $ (mods_without_docs, TcHoleFit (fit {hfDoc = map (tcHsDocString . hsDocString) <$> doc})) }}}
   upd _ mods_without_docs fit@(RawHoleFit {}) = pure (mods_without_docs, fit)
   nameOrigin name = case nameModule_maybe name of
     Just m  -> Right m
     Nothing ->
       Left $ case nameSrcLoc name of
         -- Nondeterminism is fine, this is used only to display a warning
         RealSrcLoc r _ -> NonDetFastString $ srcLocFile r
         UnhelpfulLoc s -> NonDetFastString s
   report mods = do
     { let warning =
             text "WARNING: Couldn't find any documentation for the following modules:" $+$
             nest 2
                  (pprWithCommas (either ppr ppr) (Set.toList mods) $+$
                   text "Make sure the modules are compiled with '-haddock'.")
     ; warnPprTrace (not $ Set.null mods) "addHoleFitDocs" warning (pure ())
     }

getLocalBindings :: TidyEnv -> CtLoc -> TcM [Id]
getLocalBindings tidy_orig ct_loc
 = do { (env1, _) <- liftZonkM $ zonkTidyOrigin tidy_orig (ctLocOrigin ct_loc)
      ; go env1 [] (removeBindingShadowing $ ctl_bndrs lcl_env) }
  where
    lcl_env = ctLocEnv ct_loc

    go :: TidyEnv -> [Id] -> [TcBinder] -> TcM [Id]
    go _ sofar [] = return (reverse sofar)
    go env sofar (tc_bndr : tc_bndrs) =
        case tc_bndr of
          TcIdBndr id _
            | isSystemName (idName id)
            , isUnderscore (nameOccName (idName id))
            -> discard_it
            | otherwise
            -> keep_it id
          _ -> discard_it
     where
        discard_it = go env sofar tc_bndrs
        keep_it id = go env (id:sofar) tc_bndrs

-- | Restore the hole constraint's local reader, binder, type, and Given scope.
-- See Note [Dependent hole fits and subgoal splitting].
withHoleLclEnv :: CtLoc -> [Id] -> [Implication] -> TcM a -> TcM a
withHoleLclEnv ct_loc extra_ids implics thing_inside
  = setCtLocM ct_loc $
    updLclEnv (setLclEnvRdrEnv (ctl_rdr lcl_env)) $
    tcExtendIdEnv scope_ids $
    tcExtendLexicalGivens all_givens $
    thing_inside
  where
    lcl_env = ctLocEnv ct_loc
    bndr_ids = [ bndr_id | TcIdBndr bndr_id _ <- ctl_bndrs lcl_env ]
    all_givens = concatMap ic_given implics
    given_ids = filter isNonCoVarId $
                concatMap (tyCoVarsOfTypeList . varType) all_givens
    scope_ids = nubBy ((==) `on` idName) (bndr_ids ++ extra_ids ++ given_ids)

-- | Pre-normalize top-level and nested family redexes in a dependent hole type
-- once before checking candidates.
-- See Note [Dependent hole fits and subgoal splitting].
preNormalizeHoleTy :: Bool -> CtLoc -> TcType -> TcM TcType
preNormalizeHoleTy dep_exts ct_loc ty
  | dep_exts
  , not (isFamFreeTy ty)
  = tryTcDiscardingErrs (pure ty) $
    setCtLocM ct_loc $
    do { Reduction _ norm_ty <- tcRewriteFamilyAxioms ct_loc ty
       ; pure norm_ty }
  | otherwise
  = pure ty

-- See Note [Valid hole fits include ...]
findValidHoleFits :: TidyEnv        -- ^ The tidy_env for zonking
                  -> [Implication]  -- ^ Enclosing implications for givens
                  -> [CtEvidence]
                  -- ^ The  unsolved simple constraints in the implication for
                  -- the hole.
                  -> Hole
                  -> TcM (TidyEnv, ValidHoleFits)
findValidHoleFits tidy_env implics simples h@(Hole { hole_sort = ExprHole _
                                                   , hole_loc  = ct_loc
                                                   , hole_ty   = raw_hole_ty }) =
  do { rdr_env <- getGlobalRdrEnv
     ; lclBinds0 <- getLocalBindings tidy_env ct_loc
     ; maxVSubs <- maxValidHoleFits <$> getDynFlags
     ; sortingAlg <- getHoleFitSortingAlg
     ; dflags <- getDynFlags
     ; let exts = extensionFlags dflags
           dep_exts = EnumSet.member LangExt.ExperimentalDependentTypes exts
           hole_term_ids
             | dep_exts
             = [ v
               | v <- tyCoVarsOfTypeList raw_hole_ty
               , isNonCoVarId v
               , not (isGlobalId v)
               , not (isUnderscore (nameOccName (idName v)))
               ]
             | otherwise = []
           bndr_term_ids
             | dep_exts
             = [ b
               | TcIdBndr b _ <- ctl_bndrs (ctLocEnv ct_loc)
               , not (isUnderscore (nameOccName (idName b)))
               ]
             | otherwise = []
           lclBinds = nubBy (==) (lclBinds0 ++ bndr_term_ids ++ hole_term_ids)
     ; withHoleLclEnv ct_loc lclBinds implics $ do
     { hole_ty <- preNormalizeHoleTy dep_exts ct_loc raw_hole_ty
     ; tcg_env <- getGblEnv
     ; plugins <- readTcRef (tcg_plugins tcg_env)
     ; let hfPlugs = holeFitTcMPlugins plugins
           findVLimit = if sortingAlg > HFSNoSorting then Nothing else maxVSubs
           refLevel = refLevelHoleFits dflags
           hole = TypedHole { th_relevant_cts =
                                listToBag (relevantCtEvidence hole_ty simples)
                            , th_implics      = implics
                            , th_hole         = Just (h { hole_ty = hole_ty }) }
           (candidatePlugins, fitPlugins) =
             unzip $ map (\p-> ((candPlugin p) hole, (fitPlugin p) hole)) hfPlugs
     ; traceTc "findingValidHoleFitsFor { " $ ppr hole
     ; traceTc "hole_lvl is:" $ ppr hole_lvl
     ; traceTc "simples are: " $ ppr simples
     ; traceTc "locals are: " $ ppr lclBinds
     ; let (lcl, gbl) = partition gre_lcl (globalRdrEnvElts rdr_env)
           -- We remove binding shadowings here, but only for the local level.
           -- this is so we e.g. suggest the global fmap from the Functor class
           -- even though there is a local definition as well, such as in the
           -- Free monad example.
           locals = removeBindingShadowing $
                      map IdHFCand lclBinds ++ map GreHFCand lcl
           globals = map GreHFCand gbl
           syntax = map NameHFCand (builtIns exts)

           -- If the hole is a rigid type variable, then we only check the
           -- locals, since only they can match the type (in a meaningful way).
           -- See (FastHoles1) in Note [Speeding up valid hole-fits].
           only_locals = any isImmutableTyVar $ getTyVar_maybe hole_ty
           gadt_proof_hole = dep_exts && isGadtHoleTy hole_ty
           to_check
             | only_locals     = locals
             | gadt_proof_hole = locals ++ globals
             | otherwise       = locals ++ syntax ++ globals
     ; cands <- foldM (flip ($)) to_check candidatePlugins
     ; lookup_fn <- mkCachedCandLookup gadt_proof_hole
     ; traceTc "numPlugins are:" $ ppr (length candidatePlugins)
     ; (searchDiscards, subs0) <-
        tcFilterHoleFitsWith lookup_fn findVLimit hole (hole_ty, []) cands
     ; (tidy_env, tidy_subs) <- liftZonkM $ zonkSubs tidy_env subs0
     ; tidy_sorted_subs <- sortFits sortingAlg tidy_subs
     ; (tidy_env, tidy_hole_ty) <- liftZonkM $ zonkTidyTcType tidy_env hole_ty
     -- Refinement hole fits. See Note [Valid refinement hole fits include ...]
     ; (tidy_env, rsubs) <-
       if refLevel >= Just 0
       then
         do { maxRSubs <- maxRefHoleFits <$> getDynFlags
            -- We can use from just, since we know that Nothing >= _ is False.
            ; let refLvls = [1..(fromJust refLevel)]
            -- We make a new refinement type for each level of refinement, where
            -- the level of refinement indicates number of additional arguments
            -- to allow.
            ; ref_tys <- mapM (mkRefTy hole_ty) refLvls
            ; traceTc "ref_tys are" $ ppr ref_tys
            ; let findRLimit = if sortingAlg > HFSNoSorting then Nothing
                                                            else maxRSubs
            ; refDs :: [(Bool, [TcHoleFit])]
                 <- mapM (flip (tcFilterHoleFitsWith lookup_fn findRLimit hole) cands) ref_tys
            ; (tidy_env, tidy_rsubs :: [TcHoleFit])
                 <- liftZonkM $ zonkSubs tidy_env $ concatMap snd refDs
            ; tidy_sorted_rsubs :: [TcHoleFit] <- sortFits sortingAlg tidy_rsubs
            -- For refinement substitutions we want matches
            -- like id (_ :: t), head (_ :: [t]), asTypeOf (_ :: t),
            -- and others in that vein to appear last, since these are
            -- unlikely to be the most relevant fits.
            ; let hasExactApp = any (tcEqType tidy_hole_ty) . hfWrap
                  exact, not_exact :: [TcHoleFit]
                  (exact, not_exact) = partition hasExactApp tidy_sorted_rsubs
                  fits :: [HoleFit] = map TcHoleFit (not_exact ++ exact)
            ; plugin_handled_rsubs <- foldM (\fs plug -> plug fs) fits fitPlugins
            ; let (pRDisc, exact_last_rfits) =
                    possiblyDiscard maxRSubs $ plugin_handled_rsubs
                  rDiscards = pRDisc || any fst refDs
            ; rsubs_with_docs <- addHoleFitDocs exact_last_rfits
            ; return (tidy_env, Fits rsubs_with_docs rDiscards) }
       else return (tidy_env, Fits [] False)
     ; plugin_handled_subs <- foldM (\fs plug -> plug fs) (map TcHoleFit tidy_sorted_subs) fitPlugins
     ; let (pVDisc, limited_subs) = possiblyDiscard maxVSubs plugin_handled_subs
           vDiscards = pVDisc || searchDiscards
     ; subs_with_docs <- addHoleFitDocs limited_subs
     ; let subs = Fits subs_with_docs vDiscards
     ; traceTc "findingValidHoleFitsFor }" empty
     ; let hole_fits = ValidHoleFits subs rsubs
     ; return (tidy_env, hole_fits) } }
  where
    -- We extract the TcLevel from the constraint.
    hole_lvl = ctLocLevel ct_loc

    -- BuiltInSyntax names like (:) and []
    builtIns :: EnumSet LangExt.Extension -> [Name]
    builtIns exts = filter isBuiltInSyntax (wiredInNames ++ infFamNames)
      where
        -- Tuples and sums of are not included in knownKeyName as there are infinitely many of them.
        -- See Note [Infinite families of known-key names] in GHC.Builtin.KnownKeys.
        infFamNames =
             [tupleDataConName Boxed   n | n <- [0..max_tup]]
          ++ [tupleDataConName Unboxed n | unboxedTuples, n <- [0..max_tup]]
          ++ [unboxedSumDataConName k  n | unboxedSums, n <- [2..max_sum], k <- [1..n]]

        -- The upper limits on arity are small to avoid bloating the list with
        -- too many names, which would incur a performance cost.
        -- Users are unlikely to care about larger tuples/sums anyway.
        max_tup = 7
          -- Tuples up to (,,,,,,). Common limit, e.g. this is the highest tuple
          -- arity that has a Functor instance.
        max_sum = 7
          -- Sums up to (#||||||#). Results in 27 sum constructors because there
          -- are [1..n] data constructors at each arity.

        unboxedTuples = EnumSet.member LangExt.UnboxedTuples exts
        unboxedSums   = EnumSet.member LangExt.UnboxedSums exts

    -- We make a refinement type by adding a new type variable in front
    -- of the type of t h hole, going from e.g. [Integer] -> Integer
    -- to t_a1/m[tau:1] -> [Integer] -> Integer. This allows the simplifier
    -- to unify the new type variable with any type, allowing us
    -- to suggest a "refinement hole fit", like `(foldl1 _)` instead
    -- of only concrete hole fits like `sum`.
    mkRefTy :: TcType -> Int -> TcM (TcType, [TcTyVar])
    mkRefTy base_ty refLvl = (wrapWithVars &&& id) <$> newTyVars
      where newTyVars = replicateM refLvl $ setLvl <$> newOpenFlexiTyVar
            setLvl = flip setMetaTyVarTcLevel hole_lvl
            wrapWithVars vars = mkVisFunTysManyU (map mkTyVarTy vars) base_ty

    sortFits :: HoleFitSortingAlg    -- How we should sort the hole fits
             -> [TcHoleFit]     -- The subs to sort
             -> TcM [TcHoleFit]
    sortFits HFSNoSorting subs = return subs
    sortFits HFSBySize subs
        = (++) <$> sortHoleFitsBySize (sort lclFits)
               <*> sortHoleFitsBySize (sort gblFits)
        where (lclFits, gblFits) = span hfIsLcl subs
    -- To sort by subsumption, we invoke the sortByGraph function, which
    -- builds the subsumption graph for the fits and then sorts them using a
    -- graph sort.  Since we want locals to come first anyway, we can sort
    -- them separately. The substitutions are already checked in local then
    -- global order, so we can get away with using span here.
    -- We use (<*>) to expose the parallelism, in case it becomes useful later.
    sortFits HFSBySubsumption subs
        = (++) <$> sortHoleFitsByGraph (sort lclFits)
               <*> sortHoleFitsByGraph (sort gblFits)
        where (lclFits, gblFits) = span hfIsLcl subs

    -- Based on the flags, we might possibly discard some or all the
    -- fits we've found.
    possiblyDiscard :: Maybe Int -> [HoleFit] -> (Bool, [HoleFit])
    possiblyDiscard (Just max) fits = (fits `lengthExceeds` max, take max fits)
    possiblyDiscard Nothing fits = (False, fits)

-- We don't (as of yet) handle holes in types, only in expressions.
findValidHoleFits env _ _ _ = return (env, noValidHoleFits)

-- See Note [Relevant constraints]
relevantCtEvidence :: Type -> [CtEvidence] -> [CtEvidence]
relevantCtEvidence hole_ty simples
  = if isEmptyVarSet hole_fvs
    then []
    else filter isRelevant simples
  where hole_fvs = tyCoVarsOfType hole_ty
        -- We filter out those constraints that have no variables (since
        -- they won't be solved by finding a type for the type variable
        -- representing the hole) and also other holes, since we're not
        -- trying to find hole fits for many holes at once.
        isRelevant ctev = not (isEmptyVarSet fvs) &&
                          (fvs `intersectsVarSet` hole_fvs)
          where fvs = tyCoVarsOfCtEv ctev

-- We zonk the hole fits so that the output aligns with the rest
-- of the typed hole error message output.
zonkSubs :: TidyEnv -> [TcHoleFit] -> ZonkM (TidyEnv, [TcHoleFit])
zonkSubs = zonkSubs' []
  where zonkSubs' zs env [] = return (env, reverse zs)
        zonkSubs' zs env (hf:hfs) = do { (env', z) <- zonkSub env hf
                                        ; zonkSubs' (z:zs) env' hfs }

        zonkSub :: TidyEnv -> TcHoleFit -> ZonkM (TidyEnv, TcHoleFit)
        zonkSub env hf@HoleFit{hfType = ty, hfMatches = m, hfWrap = wrp}
            = do { (env, ty') <- zonkTidyTcType env ty
                ; (env, m')   <- zonkTidyTcTypes env m
                ; (env, wrp') <- zonkTidyTcTypes env wrp
                ; let zFit = hf {hfType = ty', hfMatches = m', hfWrap = wrp'}
                ; return (env, zFit ) }

-- | Sort by size uses as a measure for relevance the sizes of the different
-- types needed to instantiate the fit to the type of the hole.
-- This is much quicker than sorting by subsumption, and gives reasonable
-- results in most cases.
sortHoleFitsBySize :: [TcHoleFit] -> TcM [TcHoleFit]
sortHoleFitsBySize = return . sortOn sizeOfFit
  where sizeOfFit :: TcHoleFit -> TypeSize
        sizeOfFit = holeFitTypesSize . nubBy tcEqType . hfWrap

-- | Compute the 'TypeSize' of instantiating wrapper types while handling
-- native dependent type forms ('TermVarTy', 'DepFunTy', 'TermAppTy') without
-- triggering 'pSizeTypeX' panics.
holeFitTypesSize :: [Type] -> TypeSize
holeFitTypesSize tys
  | not (any needsDepHoleCheck tys)
  = sizeTypes tys
  | otherwise
  = case traverse dep_sz tys of
      Nothing -> infinity
      Just ns -> mkIntWithInf (sum ns)
  where
    dep_sz :: Type -> Maybe Int
    dep_sz ty | Just ty' <- coreView ty = dep_sz ty'
    dep_sz (TyVarTy _)        = Just 1
    dep_sz (TermVarTy _)      = Just 1
    dep_sz (LitTy _)          = Just 1
    dep_sz (CoercionTy _)     = Just 1
    dep_sz (TyConApp tc args)
      | isTypeFamilyTyCon tc  = Nothing
      | otherwise             = (1 +) . sum <$> traverse dep_sz args
    dep_sz (AppTy t1 t2)      = (+) <$> dep_sz t1 <*> dep_sz t2
    dep_sz (FunTy _ ma w a r) = (\x y u v -> x + y + u + v)
                                  <$> dep_sz ma <*> dep_sz w <*> dep_sz a <*> dep_sz r
    dep_sz (ForAllTy ma (Bndr tv _) b)
                              = (\x y z -> x + y + z)
                                  <$> dep_sz ma <*> dep_sz (tyVarKind tv) <*> dep_sz b
    dep_sz (ExistsTy _ tv b)  = (+) <$> dep_sz (tyVarKind tv) <*> dep_sz b
    dep_sz (DepFunTy _ v b)   = (+) <$> dep_sz (varType v) <*> dep_sz b
    dep_sz (DepDictTy v b)    = (+) <$> dep_sz (varType v) <*> dep_sz b
    dep_sz (TermAppTy _ a)    = (1 +) <$> dep_sz a
    dep_sz (ClassAppTy _ i d o k)
                              = (\x y u v -> 1 + x + y + u + v)
                                  <$> dep_sz i <*> dep_sz d <*> dep_sz o <*> dep_sz k
    dep_sz (CastTy t _)       = dep_sz t

-- Based on a suggestion by phadej on #ghc, we can sort the found fits
-- by constructing a subsumption graph, and then do a topological sort of
-- the graph. This makes the most specific types appear first, which are
-- probably those most relevant. This takes a lot of work (but results in
-- much more useful output), and can be disabled by
-- '-fno-sort-valid-hole-fits'.
sortHoleFitsByGraph :: [TcHoleFit] -> TcM [TcHoleFit]
sortHoleFitsByGraph fits = go [] fits
  where tcSubsumesWCloning :: TcSigmaType -> TcSigmaType -> TcM Bool
        tcSubsumesWCloning fit_ty cand_ty =
          withoutUnification (tyCoVarsOfTypes [fit_ty, cand_ty]) $
            tcSubsumes fit_ty cand_ty
        go :: [(TcHoleFit, [TcHoleFit])] -> [TcHoleFit] -> TcM [TcHoleFit]
        go sofar [] = do { traceTc "subsumptionGraph was" $ ppr sofar
                         ; return $ uncurry (++) $ partition hfIsLcl topSorted }
          where toV (hf, adjs) = (hf, hfName hf, map hfName adjs)
                (graph, fromV, _) = graphFromEdges $ map toV sofar
                topSorted = map ((\(h,_,_) -> h) . fromV) $ topSort graph
        go sofar (hf:hfs) =
          do { adjs <- filterM (tcSubsumesWCloning (hfType hf) . hfType) fits
             ; go ((hf, adjs):sofar) hfs }

-- | tcFilterHoleFits filters the candidates by whether, given the implications
-- and the relevant constraints, they can be made to match the type by
-- running the type checker. Stops after finding limit matches.
tcFilterHoleFits :: Maybe Int
               -- ^ How many we should output, if limited
               -> TypedHole -- ^ The hole to filter against
               -> (TcType, [TcTyVar])
               -- ^ The type to check for fits and a list of refinement
               -- variables (free type variables in the type) for emulating
               -- additional holes.
               -> [HoleFitCandidate]
               -- ^ The candidates to check whether fit.
               -> TcM (Bool, [TcHoleFit])
               -- ^ We return whether or not we stopped due to hitting the limit
               -- and the fits we found.
tcFilterHoleFits = tcFilterHoleFitsWith lookupCandType

tcFilterHoleFitsWith :: (HoleFitCandidate -> TcM (Maybe (Name, Bool, Type)))
                     -> Maybe Int
                     -> TypedHole
                     -> (TcType, [TcTyVar])
                     -> [HoleFitCandidate]
                     -> TcM (Bool, [TcHoleFit])
tcFilterHoleFitsWith _ (Just 0) _ _ _ = return (False, []) -- Stop right away on 0
tcFilterHoleFitsWith lookup_cand limit typed_hole ht@(hole_ty, _) candidates =
  do { traceTc "checkingFitsFor {" $ ppr hole_ty
     ; (discards, subs) <- go [] emptyNameSet limit ht candidates
     ; traceTc "checkingFitsFor }" empty
     ; return (discards, subs) }
  where
    hole_fvs :: VarSet
    hole_fvs = tyCoVarsOfType hole_ty

    -- Kickoff the checking of the elements.
    -- We iterate over the elements, checking each one in turn for whether
    -- it fits, and adding it to the results if it does.
    go :: [TcHoleFit]         -- What we've found so far.
       -> NameSet             -- Names of identifiers we have already checked
       -> Maybe Int           -- How many we're allowed to find, if limited
       -> (TcType, [TcTyVar]) -- The type, and its refinement variables.
       -> [HoleFitCandidate]  -- The elements we've yet to check.
       -> TcM (Bool, [TcHoleFit])
    go subs _ _ _ [] = return (False, reverse subs)
    go subs _ (Just 0) _ _ = return (True, reverse subs)
    go subs seen maxleft ty (el:elts) =
        -- See Note [Leaking errors]
        tryTcDiscardingErrs discard_it $
        do { traceTc "lookingUp" $ ppr el
           ; maybeThing <- lookup_cand el
           ; case maybeThing of
               Just cand@(_, is_dc, cand_ty) ->
                       do { fits <- fitsHole ty cand_ty is_dc
                          ; case fits of
                              Just (wrp, matches) -> keep_it cand wrp matches
                              _ -> discard_it }
               _ -> discard_it }
        where
          discard_it = go subs seen maxleft ty elts
          keep_it (enm, _, ety) wrp ms = go (fit:subs) (extendNameSet seen enm)
                                 ((\n -> n - 1) <$> maxleft) ty elts
            where
              fit = HoleFit { hfName = enm, hfCand = el, hfType = ety
                            , hfRefLvl = length (snd ty)
                            , hfWrap = wrp, hfMatches = ms
                            , hfDoc = Nothing }

    unfoldWrapper :: HsWrapper -> [Type]
    unfoldWrapper = reverse . unfWrp'
      where
        unfWrp' (WpTyApp ty)      = [ty]
        unfWrp' (WpSubType w)     = unfWrp' w
        unfWrp' (WpCompose w1 w2) = unfWrp' w1 ++ unfWrp' w2
        unfWrp' _                  = []

    -- The real work happens here, where we invoke the type checker using
    -- tcCheckHoleFit to see whether the given type fits the hole.
    fitsHole :: (TcType, [TcTyVar]) -- The type of the hole wrapped with the
                                    -- refinement variables created to simulate
                                    -- additional holes (if any), and the list
                                    -- of those variables (possibly empty).
                                    -- As an example: If the actual type of the
                                    -- hole (as specified by the hole
                                    -- constraint CHoleExpr passed to
                                    -- findValidHoleFits) is t and we want to
                                    -- simulate N additional holes, h_ty will
                                    -- be  r_1 -> ... -> r_N -> t, and
                                    -- ref_vars will be [r_1, ... , r_N].
                                    -- In the base case with no additional
                                    -- holes, h_ty will just be t and ref_vars
                                    -- will be [].
             -> TcType -- The type of the hole fit candidate
             -> Bool   -- Is the hole fit candidate a data constructor?
             -> TcM (Maybe ([TcType], [TcType])) -- If it is not a match, we
                                                 -- return Nothing. Otherwise,
                                                 -- we Just return the list of
                                                 -- types that quantified type
                                                 -- variables in cand_ty would take
                                                 -- if used in place of h_ty,
                                                 -- and the list types of any
                                                 -- additional holes simulated
                                                 -- with the refinement
                                                 -- variables in ref_vars.
    fitsHole (h_ty, ref_vars) cand_ty cand_is_datacon =
      do { dep_exts <- xoptM LangExt.ExperimentalDependentTypes
         ; let n_ref = length ref_vars
               base_hole_ty = peelRefinementArrows n_ref h_ty
         ; if | dep_exts
              , needsDepHoleCheck base_hole_ty
                  || needsDepHoleCheck cand_ty
                  || hasRequiredForAllBndr cand_ty
              -> withoutUnification fvs $
                 tcDepCheckCand hole n_ref base_hole_ty cand_ty cand_is_datacon
              | hasRequiredForAllBndr cand_ty
              -> return Nothing
              -- (FastHoles2) hoisted before withoutUnification to avoid
              -- scanning free variables for obviously incompatible candidates.
              | definitelyNotSubType cand_ty h_ty
              -> return Nothing
              | otherwise
              ->
                -- We wrap this with the withoutUnification to avoid having side-effects
                -- beyond the check, but we rely on the side-effects when looking for
                -- refinement hole fits, so we can't wrap the side-effects deeper than this.
                withoutUnification fvs $
                do { traceTc "checkingFitOf {" $ ppr cand_ty

                     -- Compute 'ds_flag' with the same logic as 'getDeepSubsumptionFlag_DataConHead'.
                   ; user_ds <- xoptM LangExt.DeepSubsumption
                   ; let ds_flag
                           | user_ds
                           = Deep DeepSub
                           | cand_is_datacon
                           = Deep TopSub
                           | otherwise
                           = Shallow
                   ; mbWrap <- tcCheckHoleFit ds_flag hole h_ty cand_ty
                   ; traceTc "Did it fit?" $ ppr mbWrap
                   ; traceTc "checkingFitOf }" empty
                   -- We'd like to avoid refinement suggestions like `id _ _` or
                   -- `head _ _`, and only suggest refinements where our all phantom
                   -- variables got unified during the checking. This can be disabled
                   -- with the `-fabstract-refinement-hole-fits` flag.
                   -- Here we do the additional handling when there are refinement
                   -- variables, i.e. zonk them to read their final value to check for
                   -- abstract refinements, and to report what the type of the simulated
                   -- holes must be for this to be a match.
                   ; case mbWrap of
                      { Just wrp -> do {
                        -- Zonking is expensive, so we only do it if required.
                        z_wrp_tys <- liftZonkM $ zonkTcTypes (unfoldWrapper wrp)
                      ; if null ref_vars
                        then return (Just (z_wrp_tys, []))
                        else do { let -- To be concrete matches, matches have to
                                      -- be more than just an invented type variable.
                                      notAbstract :: TcType -> Bool
                                      notAbstract t = case getTyVar_maybe t of
                                                        Just tv -> tv `elemVarSet` fvs
                                                        _ -> True
                                      allConcrete = all notAbstract z_wrp_tys
                                ; z_vars  <- liftZonkM $ zonkTcTyVars ref_vars
                                ; let z_mtvs = mapMaybe getTyVar_maybe z_vars
                                ; allFilled <- not <$> anyM isFlexiTyVar z_mtvs
                                ; allowAbstract <- goptM Opt_AbstractRefHoleFits
                                ; if allowAbstract || (allFilled && allConcrete )
                                  then return $ Just (z_wrp_tys, z_vars)
                                  else return Nothing }}
                     ; Nothing -> return Nothing } } }
     where
       fvs = mkVarSet ref_vars `unionVarSet` hole_fvs `unionVarSet` tyCoVarsOfType cand_ty
       hole = typed_hole { th_hole = Nothing }

-- | Checks whether a MetaTyVar is flexible or not.
isFlexiTyVar :: TcTyVar -> TcM Bool
isFlexiTyVar tv | isMetaTyVar tv = isFlexi <$> readMetaTyVar tv
isFlexiTyVar _ = return False

-- | Takes a list of free variables and restores any Flexi type variables in
-- free_vars after the action is run.
withoutUnification :: TyCoVarSet -> TcM a -> TcM a
withoutUnification free_vars action =
  do { flexis <- filterM isFlexiTyVar (nonDetVarSetElems free_vars)
                 -- nonDetEltsUFM: order of restoration does not matter

     ; result <- action

       -- Reset any mutated free variables
     ; mapM_ restore flexis
     ; return result }
  where
    restore tv = do { traceTc "withoutUnification: restore flexi" (ppr tv)
                    ; writeTcRef (metaTyVarRef tv) Flexi }

-- | Reports whether first type (ty_a) subsumes the second type (ty_b),
-- discarding any errors. Subsumption here means that the ty_b can fit into the
-- ty_a, i.e. `tcSubsumes a b == True` if b is a subtype of a.
tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
tcSubsumes ty_a ty_b
  | needsDepHoleCheck ty_a || needsDepHoleCheck ty_b
  = tryTcDiscardingErrs (pure False) $
    isJust <$> tcDepCheckCand dummyHole 0 ty_a ty_b False
  | otherwise
  = isJust <$> tcCheckHoleFit Shallow dummyHole ty_a ty_b
  where dummyHole = TypedHole { th_relevant_cts = emptyBag
                              , th_implics      = []
                              , th_hole         = Nothing }

-- | A tcSubsumes which takes into account relevant constraints, to fix
-- #14273. This makes sure that when checking whether a type fits the hole,
-- the type has to be subsumed by type of the hole as well as fulfill all
-- constraints on the type of the hole.
tcCheckHoleFit :: DeepSubsumptionFlag
               -> TypedHole   -- ^ The hole to check against
               -> TcSigmaType
               -- ^ The type of the hole to check against (possibly modified,
               -- e.g. refined with additional holes for refinement hole-fits.)
               -> TcSigmaType
                 -- ^ The candidate fit type
               -> TcM (Maybe HsWrapper)
               -- ^ Whether it was a match, and the wrapper from hole_ty to cand_ty
tcCheckHoleFit _ _ hole_ty cand_ty
  -- (FastHoles2) from Note [Speeding up valid hole-fits]
  | definitelyNotSubType cand_ty hole_ty
  = return Nothing
  | hole_ty `eqType` cand_ty
  = return $ Just idHsWrapper
tcCheckHoleFit ds_flag (TypedHole {..}) hole_ty cand_ty = discardErrs $
  do { -- We wrap the subtype constraint in the implications to pass along the
       -- givens, and so we must ensure that any nested implications and skolems
       -- end up with the correct level. The implications are ordered so that
       -- the innermost (the one with the highest level) is first, so it
       -- suffices to get the level of the first one (or the current level, if
       -- there are no implications involved).
       innermost_lvl <- case th_implics of
                          [] -> getTcLevel
                          -- imp is the innermost implication
                          (imp:_) -> return (ic_tclvl imp)

     ; (wrap, wanted) <-
         setTcLevel innermost_lvl $ captureConstraints $
         tcSubTypeHoleFit ds_flag orig cand_ty hole_ty
           -- See Note [Deep subsumption in tcCheckHoleFit]

     ; traceTc "Checking hole fit {" empty
     ; traceTc "wanteds are: " $ ppr wanted
     ; insol <- checkInsolubleM no_givens no_given_dicts wanted
     ; if | isEmptyWC wanted, isEmptyBag th_relevant_cts
          -> do { traceTc "}" empty
                ; return $ Just wrap }

          -- (FastHoles3) from Note [Speeding up valid hole-fits]
          | insol
          -> return Nothing

          | otherwise
          -> do { fresh_binds <- newTcEvBinds
                 -- The relevant constraints may contain HoleDests, so we must
                 -- take care to clone them as well (to avoid #15370).
                ; cloned_relevants <- mapBagM cloneWantedCtEv th_relevant_cts
                  -- We wrap the WC in the nested implications, for details, see
                  -- Note [Checking hole fits]
                ; let wrapInImpls cts = foldl (flip (setWCAndBinds fresh_binds)) cts th_implics
                      final_wc  = wrapInImpls $ addSimples wanted $
                                  mapBag mkNonCanonical cloned_relevants
                  -- We add the cloned relevants to the wanteds generated
                  -- by the call to tcSubType_NC, for details, see
                  -- Note [Relevant constraints]. There's no need to clone
                  -- the wanteds, because they are freshly generated by the
                  -- call to`tcSubtype_NC`.
                ; traceTc "final_wc is: " $ ppr final_wc

                  -- runTcSEarlyAbort: (FastHoles4) from Note [Speeding up valid hole-fits]
                ; dep_exts <- xoptM LangExt.ExperimentalDependentTypes
                ; let cap_depth d
                        | dep_exts
                        = d { reductionDepth = min (reductionDepth d) (mkIntWithInf 24) }
                        | otherwise
                        = d
                ; (rem, _) <- tryTc $ updTopFlags cap_depth
                                    $ runTcSEarlyAbort
                                    $ simplifyTopWanteds final_wc
                ; traceTc "}" empty
                ; return $
                    if any isSolvedWC rem
                    then Just wrap
                    else Nothing
                } }
  where
    orig = ExprHoleOrigin (hole_occ <$> th_hole)
    is_eq_only ev = case classifyPredType (varType ev) of
                      EqPred {}       -> True
                      ClassPred cls _ -> isEqualityClass cls
                      _               -> False
    no_givens      = null (concatMap ic_given th_implics)
    no_given_dicts = all is_eq_only (concatMap ic_given th_implics)

    setWCAndBinds :: EvBindsVar         -- Fresh ev binds var.
                  -> Implication        -- The implication to put WC in.
                  -> WantedConstraints  -- The WC constraints to put implic.
                  -> WantedConstraints  -- The new constraints.
    setWCAndBinds binds imp wc
      = mkImplicWC $ unitBag $
          imp { ic_wanted = wc, ic_binds = binds, ic_status = IC_Unsolved }

{- Note [tcCheckHoleFit: fast insolubility check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `tcCheckHoleFit` we compare (with `tcSubTypeHoleFit`) the type of the hole
with the type of zillions of in-scope functions, to see which would "fit".
Most of these checks fail!  They generate obviously-insoluble constraints.
For these very-common cases we don't want to crank up the full constraint
solver.  It's much more efficient to do a quick-and-dirty check for insolubility.

Now, `tcSubTypeHoleFit` uses the on-the-fly unifier in GHC.Tc.Utils.Unify,
it has already done the dirt-simple unification. So our quick-and-dirty
check can simply look for constraints like (Int ~ Bool).  We don't need
to worry about (Maybe Int ~ Maybe Bool).

When the enclosing implications introduce no Given class dictionaries, we also
reject Wanted class constraints that have no matching or unifying top-level
instances via `matchGlobalInst` after a shallow top-level meta-variable read.
This avoids canonicalizing large evaluated type-level terms (such as deep Peano
naturals) for unrelated `Prelude` classes like `Floating` or `Monoid`.

The quick-and-dirty check is in `checkInsolubleM`. It can make a big
difference: For test hard_hole_fits, compile-time allocation goes down by 37%!
-}

checkInsolubleM :: Bool -> Bool -> WantedConstraints -> TcM Bool
-- See Note [tcCheckHoleFit: fast insolubility check]
checkInsolubleM no_givens no_given_dicts wc@(WC { wc_simple = simples })
  | checkInsoluble wc
  = return True
  | otherwise
  = do { dflags <- getDynFlags
       ; if not (xopt LangExt.ExperimentalDependentTypes dflags)
         then return False
         else anyM (is_insol_ct dflags) (bagToList simples) }
  where
    zonk_top :: TcType -> TcM TcType
    zonk_top (TyVarTy tv)
      | isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty' -> zonk_top ty'
               Flexi        -> return (TyVarTy tv) }
    zonk_top ty = return ty

    is_insol_ct :: DynFlags -> Ct -> TcM Bool
    is_insol_ct dflags ct
      = case classifyPredType (ctPred ct) of
          ClassPred cls tys
            | no_given_dicts
            , not (isEqualityClass cls)
            , not (isIPClass cls)
            -> do { tys' <- mapM zonk_top tys
                  ; res <- matchGlobalInst dflags False cls tys' Nothing
                  ; return $ case res of
                      NoInstance -> True
                      _          -> False }
          EqPred r t1 t2
            | no_givens
            , eqRelRole r == Nominal
            , noFreeVarsOfType t1
            , noFreeVarsOfType t2
            , not (isFamFreeTy t1) || not (isFamFreeTy t2)
            -> boundedGroundNeq 48 t1 t2
          _ -> return False

-- | Bounded weak-head evaluator for closed ground nominal equalities during
-- speculative hole-fit checking. Refutes unequal concrete family redexes (such
-- as @FromNatural Nat 81 ~ 5@) in a handful of head steps and aborts runaway
-- ground computations (such as @pow 3 4@) after @max_steps@ family reductions.
boundedGroundNeq :: Int -> TcType -> TcType -> TcM Bool
boundedGroundNeq max_steps lhs0 rhs0 =
  tryTcDiscardingErrs (pure False) $
  do { fuel_ref <- newTcRef max_steps
     ; let tick :: TcM Bool
           tick = do
             n <- readTcRef fuel_ref
             if n <= 0
               then return False
               else writeTcRef fuel_ref (n - 1) >> return True

           whnf :: Type -> TcM (Maybe Type)
           whnf ty | Just ty' <- coreView ty = whnf ty'
           whnf (TyConApp tc args)
             | isTypeFamilyTyCon tc = do
                 ok <- tick
                 if not ok
                   then return Nothing
                   else do
                     mb_red <- matchFamTcM tc args
                     case mb_red of
                       Just (Reduction _ rhs) -> whnf rhs
                       Nothing -> do
                         mb_args' <- step_one_arg args
                         case mb_args' of
                           Just args' -> whnf (TyConApp tc args')
                           Nothing    -> return (Just (TyConApp tc args))
           whnf ty = return (Just ty)

           step_one_arg :: [Type] -> TcM (Maybe [Type])
           step_one_arg [] = return Nothing
           step_one_arg (a : as)
             | isFamFreeTy a = fmap (a :) <$> step_one_arg as
             | otherwise = do
                 mb_a' <- step_once a
                 case mb_a' of
                   Just (Just a') -> return (Just (a' : as))
                   Just Nothing   -> fmap (a :) <$> step_one_arg as
                   Nothing        -> return Nothing

           -- Just (Just t') = stepped; Just Nothing = stuck; Nothing = out of fuel
           step_once :: Type -> TcM (Maybe (Maybe Type))
           step_once ty | Just ty' <- coreView ty = step_once ty'
           step_once (TyConApp tc args)
             | isTypeFamilyTyCon tc = do
                 ok <- tick
                 if not ok
                   then return Nothing
                   else do
                     mb_red <- matchFamTcM tc args
                     case mb_red of
                       Just (Reduction _ rhs) -> return (Just (Just rhs))
                       Nothing -> do
                         mb_args' <- step_one_arg args
                         return (Just (TyConApp tc <$> mb_args'))
             | otherwise = do
                 mb_args' <- step_one_arg args
                 return (Just (TyConApp tc <$> mb_args'))
           step_once _ = return (Just Nothing)

           go_neq :: Type -> Type -> TcM Bool
           go_neq a b
             | a `eqType` b = return False
             | otherwise = do
                 mb_a <- whnf a
                 case mb_a of
                   Nothing -> return True
                   Just a' -> do
                     mb_b <- whnf b
                     case mb_b of
                       Nothing -> return True
                       Just b'
                         | a' `eqType` b' -> return False
                         | TyConApp tc1 as1 <- a'
                         , TyConApp tc2 as2 <- b'
                         , isGenerativeTyCon tc1 Nominal
                         , isGenerativeTyCon tc2 Nominal ->
                             if tc1 /= tc2
                             then return True
                             else if isInjectiveTyCon tc1 Nominal && as1 `equalLength` as2
                                  then anyM (uncurry go_neq) (zip as1 as2)
                                  else return False
                         | otherwise -> return False
     ; go_neq lhs0 rhs0 }

checkInsoluble :: WantedConstraints -> Bool
-- See Note [tcCheckHoleFit: fast insolubility check]
checkInsoluble (WC { wc_simple = simples })
  = any is_insol simples
  where
    is_insol ct = case classifyPredType (ctPred ct) of
                    EqPred r t1 t2 -> definitelyNotEqual (eqRelRole r) t1 t2
                    _              -> False

definitelyNotEqual :: Role -> TcType -> TcType -> Bool
-- See Note [tcCheckHoleFit: fast insolubility check]
-- Bounded-depth structural check under Nominal injective generative TyCons
-- avoids invoking simplifyTopWanteds on unequal concrete evaluated terms
-- (for example pow 3 4 :~: 81 against 5 :~: 5).
definitelyNotEqual r t1 t2
  = go (24 :: Int) t1 t2
  where
    go !depth a b
      | depth <= 0 = False
      | Just a' <- coreView a = go depth a' b
      | Just b' <- coreView b = go depth a b'

    go !depth (TyConApp tc1 args1) (TyConApp tc2 args2)
      | isGenerativeTyCon tc1 r
      , isGenerativeTyCon tc2 r
      = tc1 /= tc2
        || (r == Nominal
            && isInjectiveTyCon tc1 Nominal
            && args1 `equalLength` args2
            && any (uncurry (go (depth - 1))) (zip args1 args2))

    go _ (TyConApp tc _) b | isGenerativeTyCon tc r = go_tc tc b
    go _ a (TyConApp tc _) | isGenerativeTyCon tc r = go_tc tc a
    go _ (FunTy {ft_af = af1}) (FunTy {ft_af = af2}) = af1 /= af2
    go _ _ _ = False

    go_tc :: TyCon -> TcType -> Bool
    -- The TyCon is generative, and is not a saturated FunTy
    go_tc tc1 (TyConApp tc2 _) | isGenerativeTyCon tc2 r = tc1 /= tc2
    go_tc _ (FunTy {})    = True
    go_tc _ (ForAllTy {}) = True
    go_tc _ (DepFunTy {}) = True
    go_tc _ _ = False

-- | @definitelyNotSubType cand_ty hole_ty@ computes whether @cand_ty@ is
-- **definitely not** a subtype of @hole_ty@, in order to quickly rule out
-- a possible hole fit candidate without having to do any solving.
--
-- See (FastHoles2) in Note [Speeding up valid hole-fits].
definitelyNotSubType :: TcType -> TcType -> Bool
definitelyNotSubType = go
  where
    go cand_ty hole_ty
      -- Expand type synonyms
      | Just cand_ty' <- coreView cand_ty
      = go cand_ty' hole_ty
      | Just hole_ty' <- coreView hole_ty
      = go cand_ty hole_ty'

      -- Different TyCons at the head (looking through invisible binders and =>).
      | Just tc1 <- tc_head cand_ty
      , Just tc2 <- tc_head hole_ty
      , tc1 /= tc2
      = True

      -- A visible retained function binder requires a value argument and cannot
      -- instantiate to a non-retained hole type.
      | has_vis_dep_fun cand_ty
      , not (has_vis_dep_fun hole_ty)
      = True

      | not (has_vis_dep_fun cand_ty)
      , has_vis_dep_fun hole_ty
      , Just _ <- tc_head cand_ty
      = True

      -- Non-forall type does not fit a forall-typed hole.
      | isSigmaTy hole_ty
      , isTauTy cand_ty
      = True

      -- Covariant result pruning across visible function arrows (for example
      -- when checking refinement types r_1 -> ... -> r_n -> base_hole_ty).
      | fun_res_mismatch cand_ty hole_ty
      = True

      | otherwise
      = False

    strip_invis :: Type -> Type
    strip_invis ty
      | Just ty' <- coreView ty = strip_invis ty'
    strip_invis (ForAllTy _ (Bndr _ vis) b)
      | isInvisibleForAllTyFlag vis = strip_invis b
    strip_invis (DepFunTy DepFunInvisible _ b) = strip_invis b
    strip_invis (DepDictTy _ b) = strip_invis b
    strip_invis (FunTy { ft_af = af, ft_res = res })
      | not (isVisibleFunArg af) = strip_invis res
    strip_invis ty = ty

    fun_res_mismatch :: Type -> Type -> Bool
    fun_res_mismatch c0 h0
      | FunTy { ft_af = af1, ft_res = r1 } <- strip_invis c0
      , isVisibleFunArg af1
      , FunTy { ft_af = af2, ft_res = r2 } <- strip_invis h0
      , isVisibleFunArg af2
      = gen_res_mismatch r1 r2
      | otherwise
      = False

    gen_res_mismatch :: Type -> Type -> Bool
    gen_res_mismatch c0 h0
      | FunTy { ft_af = af1, ft_res = r1 } <- c
      , isVisibleFunArg af1
      , FunTy { ft_af = af2, ft_res = r2 } <- h
      , isVisibleFunArg af2
      = gen_res_mismatch r1 r2
      | has_vis_dep_fun c
      , not (has_vis_dep_fun h)
      = True
      | Just tc1 <- gen_tc_head c
      , Just tc2 <- gen_tc_head h
      = tc1 /= tc2
      | otherwise
      = False
      where
        c = strip_invis c0
        h = strip_invis h0

    gen_tc_head :: Type -> Maybe TyCon
    gen_tc_head (FunTy { ft_af = af })
      | isVisibleFunArg af = Just (funTyFlagTyCon af)
    gen_tc_head (TyConApp tc _)
      | isGenerativeTyCon tc Nominal = Just tc
    gen_tc_head _ = Nothing

    -- Is this Type a TyConApp, after looking under invisible binders and =>?
    -- If so, return the TyCon at the head.
    tc_head :: Type -> Maybe TyCon
    tc_head (FunTy { ft_af = af, ft_res = res })
      | not $ isVisibleFunArg af
      = tc_head res
      | otherwise
      = Just $ funTyFlagTyCon af
    tc_head (TyConApp tc _)
      = Just tc
    tc_head (ForAllTy _ (Bndr _ vis) body)
      | isInvisibleForAllTyFlag vis
      = tc_head body
      | otherwise
      = Nothing
    tc_head (DepFunTy DepFunInvisible _ body)
      = tc_head body
    tc_head (DepDictTy _ body)
      = tc_head body
    tc_head _ = Nothing

    has_vis_dep_fun :: Type -> Bool
    has_vis_dep_fun ty
      | Just ty' <- coreView ty = has_vis_dep_fun ty'
    has_vis_dep_fun (DepFunTy DepFunVisible _ _) = True
    has_vis_dep_fun (DepFunTy DepFunInvisible _ b) = has_vis_dep_fun b
    has_vis_dep_fun (DepDictTy _ b) = has_vis_dep_fun b
    has_vis_dep_fun (ForAllTy _ (Bndr _ vis) b)
      | isInvisibleForAllTyFlag vis = has_vis_dep_fun b
    has_vis_dep_fun (FunTy { ft_af = af, ft_res = res })
      | not (isVisibleFunArg af) = has_vis_dep_fun res
    has_vis_dep_fun _ = False

{- Note [Dependent hole fits and subgoal splitting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under `DependentTypes`, hole checking interacts with runtime index binders
(`TermVarTy`), visible `forall a ->` binders, and retained function types
(`DepFunTy`) in three ways:

1. Local scope restoration (`withHoleLclEnv`):
   `reportUnsolved` runs outside the lexical expression environment. Restore
   the hole's `CtLoc`, `LocalRdrEnv`, local `Id` bindings, and enclosing
   `LexicalGivens` before checking candidates, and reset `ic_status = IC_Unsolved`
   when wrapping Wanted constraints inside `th_implics`.

2. Immutable index instantiation (`tcDepCheckCand`):
   Mutable unification variables cannot bind types that contain `TermVarTy`
   (see Note [Term-variable occurrences in types] in `GHC.Tc.Utils.Unify`).
   For candidates or holes with native dependent forms or visible `forall a ->`
   binders, peel invisible quantifiers, constraints, and `n_ref` visible
   parameters (`FunTy`, `DepFunTy`, or `ForAllTy Required`), abstracting
   retained binders to fresh template `TyVar`s via `abstractNativeIndices`.
   Match the candidate result type against the hole type (decomposing aligned
   `DepFunTy`, `FunTy`, and `TyConApp` spines when inner type families reduce
   differently) to select immutable arguments, check well-kindedness and local
   `Id` scope, and verify the instantiated result with `tcCheckHoleFit` inside
   `th_implics`.

3. General refinement levels and `-fabstract-refinement-hole-fits`:
   `tcDepCheckCand` works uniformly at any `-frefinement-level-hole-fits=N`
   (`N = 0, 1, 2, ...`). Ununified template variables are rejected unless they
   are unused `foreach` value binders or `-fabstract-refinement-hole-fits`
   (`Opt_AbstractRefHoleFits`) is active, in which case `completeDepSubst`
   allocates fresh flexible meta-variables (for example producing
   `trans (_ :: lhs :~: b0) (_ :: b0 :~: rhs)` at level 2).
-}

-- | Strip @n@ leading synthetic refinement arrows added by 'mkRefTy'.
peelRefinementArrows :: Int -> TcType -> TcType
peelRefinementArrows 0 ty = ty
peelRefinementArrows n (FunTy { ft_af = af, ft_res = res })
  | isVisibleFunArg af = peelRefinementArrows (n - 1) res
peelRefinementArrows _ ty = ty

-- | Does this type contain native dependent forms that require immutable
-- hole-fit instantiation rather than mutable meta-variable unification?
needsDepHoleCheck :: Type -> Bool
needsDepHoleCheck ty = hasNativeDependentExpr (Core.Type ty)

-- | Does this candidate type contain a visible @forall a ->@ binder?
hasRequiredForAllBndr :: Type -> Bool
hasRequiredForAllBndr = go
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy _ (Bndr _ Required) _) = True
    go (ForAllTy _ _ b) = go b
    go (FunTy { ft_res = r }) = go r
    go (DepFunTy _ _ b) = go b
    go (DepDictTy _ b) = go b
    go _ = False

-- | Visible parameter peeled during dependent hole-fit checking.
data DepVisParam
  = DepVisFun !Type
  | DepVisForEach !Id !TyVar !Type
  | DepVisReqForAll !TyVar !Type

depVisParamDom :: DepVisParam -> Type
depVisParamDom (DepVisFun dom)         = dom
depVisParamDom (DepVisForEach _ _ dom) = dom
depVisParamDom (DepVisReqForAll _ dom) = dom

-- | Check a candidate against a dependent hole type with @n_ref@ missing
-- visible arguments, instantiating invisible binders, visible @forall a ->@
-- binders, and 'DepFunTy' binders immutably from the expected result.
-- See Note [Dependent hole fits and subgoal splitting].
tcDepCheckCand :: TypedHole
               -> Int
               -> TcType
               -> TcType
               -> Bool
               -> TcM (Maybe ([TcType], [TcType]))
tcDepCheckCand hole n_ref orig_hole_ty cand_ty cand_is_dc
  | n_ref == 0
  , cand_ty `eqType` orig_hole_ty
  = return $ Just ([], [])
  | otherwise
  = do { user_ds <- xoptM LangExt.DeepSubsumption
       ; let ds_flag
               | user_ds      = Deep DeepSub
               | cand_is_dc   = Deep TopSub
               | otherwise    = Shallow
       -- Monomorphic candidates with 0 missing arguments need no instantiation.
       ; direct_mono <-
           if n_ref == 0 && isTauTy cand_ty && not (definitelyNotSubType cand_ty orig_hole_ty)
           then fmap (const ([], [])) <$> tcCheckHoleFit ds_flag hole orig_hole_ty cand_ty
           else return Nothing
       ; case direct_mono of
           Just ok -> return (Just ok)
           Nothing -> tcDepInstAndCheck ds_flag hole n_ref orig_hole_ty cand_ty }

tcDepInstAndCheck :: DeepSubsumptionFlag
                  -> TypedHole
                  -> Int
                  -> TcType
                  -> TcType
                  -> TcM (Maybe ([TcType], [TcType]))
tcDepInstAndCheck ds_flag hole n_ref orig_hole_ty cand_ty =
  do { mb_peeled <- peel_cand 0 [] [] [] [] [] cand_ty
     ; case mb_peeled of
         Nothing -> return Nothing
         Just (invis_tvs, bind_tvs, theta, vis_params, res_ty) ->
           if | Just tc1 <- depResHead res_ty
              , Just tc2 <- depResHead orig_hole_ty
              , tc1 /= tc2
              -> return Nothing
              | has_vis_dep res_ty /= has_vis_dep orig_hole_ty
              -> return Nothing
              | hasRequiredForAllBndr res_ty /= hasRequiredForAllBndr orig_hole_ty
              -> return Nothing
              | otherwise
              -> do { let vis_dep_set = mkVarSet [ tv | DepVisForEach _ tv _ <- vis_params ]
                          used_in_rest =
                            tyCoVarsOfTypes (res_ty : theta ++ map depVisParamDom vis_params)
                    ; case matchDepResult bind_tvs res_ty orig_hole_ty of
                        Nothing -> return Nothing
                        Just subst ->
                          do { allow_abs <- goptM Opt_AbstractRefHoleFits
                             ; mb_full <- completeDepSubst allow_abs vis_dep_set used_in_rest bind_tvs subst
                             ; case mb_full of
                                 Nothing -> return Nothing
                                 Just full_subst ->
                                   do { let choices = [ Subst.substTy full_subst (mkTyVarTy tv)
                                                      | tv <- bind_tvs
                                                      , not (tv `elemVarSet` vis_dep_set && not (tv `elemVarSet` used_in_rest)) ]
                                      ; scoped <- checkDepRuntimeScope choices
                                      ; if not scoped
                                        then return Nothing
                                        else do
                                          { let theta' = map (Subst.substTy full_subst) theta
                                                res'   = Subst.substTy full_subst res_ty
                                                inst_sigma = mkPhiTy theta' res'
                                          ; mb_w <- tcCheckHoleFit ds_flag hole orig_hole_ty inst_sigma
                                          ; case mb_w of
                                              Nothing -> return Nothing
                                              Just _  ->
                                                let wrp_tys = [ Subst.substTy full_subst (mkTyVarTy tv)
                                                              | tv <- invis_tvs ]
                                                    arg_tys = [ Subst.substTy full_subst (depVisParamDom p)
                                                              | p <- vis_params ]
                                                in return $ Just (wrp_tys, arg_tys) } } } } }
  where
    has_vis_dep ty
      | Just ty' <- coreView ty = has_vis_dep ty'
    has_vis_dep (DepFunTy DepFunVisible _ _) = True
    has_vis_dep _ = False

    -- Peel invisible binders, constraints, and exactly n_ref visible parameters
    -- in telescope order, abstracting retained binders to fresh template TyVars.
    peel_cand :: Int
              -> [TyVar]
              -> [TyVar]
              -> [(Type, TyVar)]
              -> [PredType]
              -> [DepVisParam]
              -> Type
              -> TcM (Maybe ([TyVar], [TyVar], [PredType], [DepVisParam], Type))
    peel_cand k invis_rev bind_rev dep_pairs theta_rev vis_rev ty
      | Just ty' <- coreView ty
      = peel_cand k invis_rev bind_rev dep_pairs theta_rev vis_rev ty'
      | ForAllTy _ (Bndr tv vis) body <- ty
      , isInvisibleForAllTyFlag vis
      , not (k == n_ref && isForAllTy orig_hole_ty)
      = case abstractNativeIndices dep_pairs (tyVarKind tv) of
          Nothing -> return Nothing
          Just k' ->
            let tv'   = if k' `eqType` tyVarKind tv then tv else setTyVarKind tv k'
                body' = if k' `eqType` tyVarKind tv then body else Subst.substTyWith [tv] [mkTyVarTy tv'] body
            in peel_cand k (tv' : invis_rev) (tv' : bind_rev) dep_pairs theta_rev vis_rev body'
      | DepFunTy DepFunInvisible bndr body <- ty
      = case abstractNativeIndices dep_pairs (varType bndr) of
          Nothing -> return Nothing
          Just dom' ->
            do { nm <- newSysName (mkTyVarOccFS (occNameFS (getOccName bndr)))
               ; let tv = mkTyVar nm dom'
               ; peel_cand k invis_rev (tv : bind_rev) (dep_pairs ++ [(TermVarTy bndr, tv)]) theta_rev vis_rev body }
      | FunTy { ft_af = af, ft_arg = pred_ty, ft_res = body } <- ty
      , not (isVisibleFunArg af)
      = case abstractNativeIndices dep_pairs pred_ty of
          Nothing -> return Nothing
          Just pred' ->
            peel_cand k invis_rev bind_rev dep_pairs (pred' : theta_rev) vis_rev body
      | k < n_ref
      = case ty of
          FunTy { ft_af = af, ft_arg = arg_ty, ft_res = body }
            | isVisibleFunArg af ->
              case abstractNativeIndices dep_pairs arg_ty of
                Nothing -> return Nothing
                Just arg' ->
                  peel_cand (k + 1) invis_rev bind_rev dep_pairs theta_rev
                    (DepVisFun arg' : vis_rev) body
          DepFunTy DepFunVisible bndr body ->
            case abstractNativeIndices dep_pairs (varType bndr) of
              Nothing -> return Nothing
              Just dom' ->
                do { nm <- newSysName (mkTyVarOccFS (occNameFS (getOccName bndr)))
                   ; let tv = mkTyVar nm dom'
                   ; peel_cand (k + 1) invis_rev (tv : bind_rev) (dep_pairs ++ [(TermVarTy bndr, tv)]) theta_rev
                       (DepVisForEach bndr tv dom' : vis_rev) body }
          ForAllTy _ (Bndr tv Required) body ->
            case abstractNativeIndices dep_pairs (tyVarKind tv) of
              Nothing -> return Nothing
              Just dom' ->
                let tv'   = if dom' `eqType` tyVarKind tv then tv else setTyVarKind tv dom'
                    body' = if dom' `eqType` tyVarKind tv then body else Subst.substTyWith [tv] [mkTyVarTy tv'] body
                in peel_cand (k + 1) invis_rev (tv' : bind_rev) dep_pairs theta_rev
                     (DepVisReqForAll tv' dom' : vis_rev) body'
          _ -> return Nothing
      | otherwise
      = case abstractNativeIndices dep_pairs ty of
          Nothing -> return Nothing
          Just res' ->
            let rest_invis = deepInvisTvVars res'
            in return $ Just (reverse invis_rev ++ rest_invis, reverse bind_rev, reverse theta_rev, reverse vis_rev, res')

depResHead :: Type -> Maybe TyCon
depResHead ty | Just ty' <- coreView ty = depResHead ty'
depResHead (TyConApp tc _) = Just tc
depResHead (FunTy { ft_af = af, ft_res = r })
  | isVisibleFunArg af = Just (funTyFlagTyCon af)
  | otherwise = depResHead r
depResHead (ForAllTy _ (Bndr _ vis) b)
  | isInvisibleForAllTyFlag vis = depResHead b
depResHead (DepFunTy _ _ b) = depResHead b
depResHead (DepDictTy _ b) = depResHead b
depResHead _ = Nothing

-- | Pure result matcher for dependent hole fits: tries exact matching first,
-- then independent binder/argument matching across aligned 'DepFunTy', 'FunTy',
-- and algebraic 'TyConApp' spines (for repeated indices such as 'Refl', higher-
-- order congruence templates @f x@, and @foreach@ holes whose inner type
-- families reduce differently).
matchDepResult :: [TyVar] -> Type -> Type -> Maybe Subst.Subst
matchDepResult bind_tvs formal_res actual_res
  | Just s0 <- match_one Subst.emptySubst formal_res actual_res
  = Just s0
  | Just pairs <- decomp_pairs formal_res actual_res
  , let cands = mapMaybe (uncurry (match_one Subst.emptySubst)) pairs
  , not (null cands)
  = let pick s tv = case [ ty | sub <- cands, Just ty <- [Subst.lookupTyVar sub tv] ] of
                      (ty : _) -> Subst.extendTvSubstAndInScope s tv ty
                      []       -> s
    in Just (foldl' pick Subst.emptySubst bind_tvs)
  | null bind_tvs
  = Just Subst.emptySubst
  | otherwise
  = Nothing
  where
    bind_set = mkVarSet bind_tvs
    bind_fn tv _
      | tv `elemVarSet` bind_set = BindMe
      | otherwise                = DontBindMe

    match_one subst formal actual
      | Just s1 <- tcMatchTyX_BM bind_fn subst (typeKind formal) (typeKind actual)
      , Just s2 <- tcMatchTyX_BM bind_fn s1 formal actual
      = Just s2
      | AppTy f_tmpl x_tmpl <- coreFullView formal
      , Just (con, args) <- splitTyConApp_maybe (coreFullView actual)
      , last_a : rev_prefix <- reverse args
      , let prefix = mkTyConApp con (reverse rev_prefix)
      , Just s1 <- match_one subst f_tmpl prefix
      , Just s2 <- match_one s1 x_tmpl last_a
      = Just s2
      | formal' <- coreFullView formal
      , CastTy actual' co <- coreFullView actual
      , coercionRole co == Nominal
      = match_one subst formal' actual'
      | otherwise
      = Nothing

    decomp_pairs :: Type -> Type -> Maybe [(Type, Type)]
    decomp_pairs formal actual
      | DepFunTy vis1 b1 body1 <- coreFullView formal
      , DepFunTy vis2 b2 body2 <- coreFullView actual
      , vis1 == vis2
      = let body1' = replaceTermVarInType b1 (TermVarTy b2) body1
            rest   = fromMaybe [(body1', body2)] (decomp_pairs body1' body2)
        in Just ((varType b1, varType b2) : rest)
      | FunTy { ft_af = af1, ft_arg = arg1, ft_res = res1 } <- coreFullView formal
      , FunTy { ft_af = af2, ft_arg = arg2, ft_res = res2 } <- coreFullView actual
      , af1 == af2
      = let rest = fromMaybe [(res1, res2)] (decomp_pairs res1 res2)
        in Just ((arg1, arg2) : rest)
      | Just (tc1, ts1) <- splitTyConApp_maybe (coreFullView formal)
      , Just (tc2, ts2) <- splitTyConApp_maybe (coreFullView actual)
      , tc1 == tc2
      , isAlgTyCon tc1 || isTypeFamilyTyCon tc1 || isJust (isPromotedDataCon_maybe tc1)
      , ts1 `equalLength` ts2
      = Just (concatMap (\(t1, t2) -> fromMaybe [(t1, t2)] (decomp_pairs t1 t2)) (zip ts1 ts2))
      | otherwise
      = Nothing

-- | Collect invisible 'TyVar's beneath function and retained binders.
deepInvisTvVars :: Type -> [TyVar]
deepInvisTvVars = go
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy _ (Bndr tv vis) body)
      | isInvisibleForAllTyFlag vis = tv : go body
      | otherwise                   = go body
    go (FunTy { ft_res = res })     = go res
    go (DepFunTy _ _ body)          = go body
    go (DepDictTy _ body)           = go body
    go _                            = []

completeDepSubst :: Bool -> VarSet -> VarSet -> [TyVar] -> Subst.Subst -> TcM (Maybe Subst.Subst)
completeDepSubst allow_abs vis_dep_tvs used_in_rest tvs subst0
  = go subst0 tvs
  where
    tv_set = mkVarSet tvs
    go subst [] = return (Just subst)
    go subst (tv : rest) =
      case Subst.lookupTyVar subst tv of
        Just choice ->
          let exp_k = Subst.substTy subst (tyVarKind tv)
          in if exp_k `eqType` typeKind choice
                && not (anyVarSet (\v -> v `elemVarSet` tv_set && not (v `elemVarSet` vis_dep_tvs)) (tyCoVarsOfType choice))
             then go (Subst.extendTvSubstAndInScope subst tv choice) rest
             else return Nothing
        Nothing
          | tv `elemVarSet` vis_dep_tvs
          , not (tv `elemVarSet` used_in_rest)
          -> let k'  = Subst.substTy subst (tyVarKind tv)
                 tv' = mkTyVar (getName tv) k'
             in go (Subst.extendTvSubstAndInScope subst tv (mkTyVarTy tv')) rest
          | allow_abs
          , let k' = Subst.substTy subst (tyVarKind tv)
          , not (tyCoVarsOfType k' `intersectsVarSet` mkVarSet (tv : rest))
          -> do { (subst', tv') <- newMetaTyVarX subst tv
                ; go (Subst.extendTvSubstAndInScope subst' tv (mkTyVarTy tv')) rest }
          | otherwise
          -> return Nothing

-- | Verify that every non-global runtime 'Id' occurring in the chosen
-- instantiation types refers to an in-scope local binding with matching type.
checkDepRuntimeScope :: [Type] -> TcM Bool
checkDepRuntimeScope tys =
  do { lcl_env <- getLclTypeEnv
     ; let runtime_ids = filter (\v -> isNonCoVarId v && not (isGlobalId v)) $
                         concatMap tyCoVarsOfTypeList tys
           ok_id v = case lookupNameEnv lcl_env (idName v) of
                       Just (ATcId { tct_id = bndr }) ->
                         bndr == v && (varType bndr `eqType` varType v)
                       _ -> False
     ; return (all ok_id runtime_ids) }

-- | Is the hole type headed by an indexed GADT (a datatype where at least one
-- constructor carries GADT equality specifications, such as @(:~:)@ or @LE@)?
isGadtHoleTy :: Type -> Bool
isGadtHoleTy ty
  | Just (tc, _) <- splitTyConApp_maybe (coreFullView ty)
  , isAlgTyCon tc
  , let dcs = tyConDataCons tc
  , not (null dcs)
  = any has_eq_spec dcs
  | otherwise
  = False
  where
    has_eq_spec dc =
      case dataConFullSig dc of
        (_, _, eq_spec, _, _, _) -> not (null eq_spec)

-- | Build a memoized candidate type lookup across level-0 and refinement passes.
-- On GADT proof holes under 'ExperimentalDependentTypes', skip unloaded
-- 'Prelude' modules unrelated to propositional equality so cold hole checks do
-- not deserialize irrelevant interfaces.
mkCachedCandLookup :: Bool
                   -> TcM (HoleFitCandidate -> TcM (Maybe (Name, Bool, Type)))
mkCachedCandLookup gadt_proof_hole =
  do { cache_ref <- newTcRef (emptyNameEnv :: NameEnv (Maybe (Name, Bool, Type)))
     ; eps_pte   <- eps_PTE <$> getEps
     ; let allow_base_mods =
             [ "Data.Type.Equality"
             , "GHC.Internal.Data.Type.Equality"
             , "Data.Type.Coercion"
             , "GHC.Internal.Data.Type.Coercion"
             , "GHC.Internal.Types"
             , "GHC.Internal.Base"
             ]
           skip_unloaded nm
             | not gadt_proof_hole = False
             | Just mod <- nameModule_maybe nm
             , let u_str   = unitString (moduleUnit mod)
                   mod_str = moduleNameString (moduleName mod)
             , "ghc-internal" `isPrefixOf` u_str || "base" `isPrefixOf` u_str
             = mod_str `elem`
                 [ "GHC.Internal.Enum", "GHC.Enum"
                 , "Text.Read", "GHC.Read", "GHC.Internal.Read", "GHC.Internal.Text.Read"
                 , "System.IO", "GHC.Internal.IO"
                 ]
               || (not (nm `elemNameEnv` eps_pte) && mod_str `notElem` allow_base_mods)
             | otherwise = False
     ; return $ \cand ->
         case cand of
           IdHFCand _ -> lookupCandType cand
           _ -> do
             let nm = getName cand
             cache <- readTcRef cache_ref
             case lookupNameEnv cache nm of
               Just cached -> return cached
               Nothing
                 | skip_unloaded nm -> do
                     writeTcRef cache_ref (extendNameEnv cache nm Nothing)
                     return Nothing
                 | otherwise -> do
                     res <- lookupCandType cand
                     updTcRef cache_ref (\c -> extendNameEnv c nm res)
                     return res }

lookupCandType :: HoleFitCandidate -> TcM (Maybe (Name, Bool, Type))
lookupCandType (IdHFCand i)
  | isAllowedCandId i = return $ Just (idName i, False, idType i)
  | otherwise         = return Nothing
lookupCandType hfc
  | not (isValNameSpace (occNameSpace (nameOccName (getName hfc))))
  = return Nothing
  | otherwise =
  do { let nm = getName hfc
     ; thing <- tcLookup nm
     ; return $ case thing of
         ATcId { tct_id = i } | isAllowedCandId i ->
           Just (idName i, False, idType i)
         AGlobal (AnId i) | isAllowedCandId i ->
           Just (idName i, False, idType i)
         AGlobal (AConLike (RealDataCon con)) ->
           Just (dataConName con, True, dataConWrapperType con)
         AGlobal (AConLike (PatSynCon ps))
           | Just (_, t) <- patSynBuilderOcc ps ->
           Just (patSynName ps, False, t)
         _ -> Nothing }

isAllowedCandId :: Id -> Bool
isAllowedCandId i =
  nameModule_maybe (idName i) `notElem` [Just gHC_INTERNAL_ERR, Just gHC_INTERNAL_UNSAFE_COERCE]

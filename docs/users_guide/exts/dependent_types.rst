.. _dependent-types:

Dependent types
===============

.. extension:: DependentTypes
    :shortdesc: Enable the GHC Proposal #378 Dependent Haskell extension bundle.

    :implies: :extension:`ExistentialTypes`, :extension:`ExtendedLet`,
              :extension:`GADTs`, :extension:`DataKinds`,
              :extension:`PolyKinds`, :extension:`TypeOperators`,
              :extension:`RankNTypes`, :extension:`ScopedTypeVariables`,
              :extension:`TypeApplications`, :extension:`TypeAbstractions`,
              :extension:`StandaloneKindSignatures`, :extension:`EmptyCase`,
              :extension:`Modifiers`, :extension:`RequiredTypeArguments`,
              :extension:`ExplicitNamespaces`, :extension:`NoListTuplePuns`
    :since: 10.2
    :status: Experimental

    Enable Dependent Haskell (`GHC Proposal #378`_) and its companion
    extensions. Enabling this extension also enables
    :ghc-flag:`-fwrite-source-equations` by default.

.. _GHC Proposal #378: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
.. _GHC Proposal #473: https://github.com/ghc-proposals/ghc-proposals/pull/473
.. _GHC Proposal #523: https://github.com/ghc-proposals/ghc-proposals/pull/523

.. extension:: ExistentialTypes
    :shortdesc: Enable first-class existential quantification.

    :implies: :extension:`RequiredTypeArguments`
    :since: 10.2
    :status: Experimental

    Enable first-class existential quantification (``exists``,
    `GHC Proposal #473`_).

.. extension:: ExtendedLet
    :shortdesc: Enable local let bindings in types and local type synonyms in terms.

    :implies: :extension:`RequiredTypeArguments`
    :since: 10.2
    :status: Experimental

    Enable local ``let ... in ...`` bindings in types and nonrecursive local
    ``type`` synonyms inside term-level ``let`` and ``where`` blocks
    (`GHC Proposal #523`_).

GHC disables :extension:`DependentTypes` by default. Enabling
:extension:`DependentTypes` turns on :ghc-flag:`-fwrite-source-equations` by
default. Pass :ghc-flag:`-fno-write-source-equations` to disable
source-equation publication. Place a ``No...`` flag or pragma (or
:extension:`ListTuplePuns`) after :extension:`DependentTypes` to override any
implied extension.

These extensions enable ``foreach``, ``exists``, and ``some`` in types and in
required type argument positions. With :extension:`UnicodeSyntax`, ``∏``
(``U+220F``) is an alias for ``foreach``. Telescopes in ``forall``,
``foreach``, ``exists``, and ``some`` accept shared-type binder groups such as
``(m n p :: Nat)`` and ``{m n p :: Nat}``.

GHC compiles dependent definitions to a native dependent Core representation.
Module interfaces record retained binders with scoped local identifier names.
An ordinary ``Haskell2010`` or ``GHC2024`` module can import and call dependent
functions without enabling :extension:`DependentTypes`. GHCi loads dependent
modules as interpreted source or bytecode and preserves local declarations
across commands.

Quantifiers, telescopes, and visibility
---------------------------------------

Dependent Haskell provides four explicit function quantifiers alongside
``c =>`` and ``t1 -> t2``, plus two existential and dependent-sum quantifiers:

- ``forall a.``: erased at runtime and invisible at call sites.
- ``forall a ->``: erased at runtime and visible (required) at call sites
  (see :ref:`required-type-arguments`).
- ``foreach a.``: retained at runtime and invisible at call sites.
- ``foreach a ->``: retained at runtime and visible (required) at call sites.
- ``some (a :: k) | body`` (and ``some (a :: k). body``): retained dependent
  sum (``GHC.Sigma.Sigma``).
- ``exists (a :: k). body`` (and ``exists (a :: k) | body``): erased
  existential package (``GHC.Exists``).

Telescope binders in ``foreach`` follow the same syntactic rules as ``forall``:

- Shared-type binder groups ``(m n p :: Nat)`` and ``{m n p :: Nat}`` expand in
  source order across ``forall``, ``foreach``, ``exists``, and ``some``.
- Unannotated binders ``foreach a ->``, ``foreach m n.``, and ``foreach {n}.``
  infer their domain type from uses in the body.
- Wildcard binders ``foreach (_ :: t1) -> t2`` and ``foreach _ -> t2`` require
  call arguments in the static subset without binding a name in ``t2``.
- With :extension:`UnicodeSyntax`, ``∏`` (``U+220F``) stands for ``foreach`` in
  both ``∏ (n :: Nat) ->`` and ``∏ (n :: Nat).``.
- Required type arguments accept ``foreach``, ``∏``, ``exists``, and ``some``
  directly in term syntax without a ``type`` herald.

Invisible ``foreach (n :: Nat).`` binders have specified visibility by default.
Callers can omit the argument for inference or pass an explicit override with
``@arg``. Equations and lambdas can bind or pattern-match specified invisible
retained arguments with ``@pat``, or omit ``@pat`` to keep the signature binder
name in scope::

    {-# LANGUAGE DependentTypes #-}

    vReplicateImplicit :: forall {a}. foreach (n :: Nat). a -> Vec a n
    vReplicateImplicit @Zero     _ = VNil
    vReplicateImplicit @(Succ k) x = VCons x (vReplicateImplicit @k x)

    quickLength :: forall a. foreach (n :: Nat). Vec a n -> Nat
    quickLength @_ @n _ = n

    skipInferred :: forall a. foreach {m :: Nat}. foreach (n :: Nat).
                    Vec a m -> Vec a n -> (Nat, Nat)
    skipInferred @_ @n _ _ = (m, n)

Brace-delimited ``foreach {m :: Nat}.`` binders have inferred specificity.
Explicit ``@arg`` call overrides and ``@pat`` equation patterns skip ``{...}``
binders.

When a retained binder has kind ``Type``, the bound name stays in scope as a
type variable in type positions and required type arguments. At runtime, GHC
passes ``Typeable`` evidence for that binder. Call sites accept unadorned type
expressions in the static subset as well as ``(type T)``::

    the :: forall (a :: Type) -> a -> a
    the _ x = x

    ex5 :: foreach (a :: Type) -> a -> a
    ex5 a x = the a (x :: a)

    sample :: (Int, Maybe Char)
    sample = (ex5 Int 42, ex5 (Maybe Char) (Just 'z'))

Inside the body of a ``foreach (a :: Type) ->`` function, ``case`` expressions
can match the runtime ``Type`` witness directly using unadorned type patterns
and a catch-all branch::

    describeType :: foreach (a :: Type) -> Maybe a
    describeType a = case a of
      Int    -> Just 12
      Bool   -> Just True
      [Char] -> Just "ok"
      _      -> Nothing

Required type arguments (``T2T``) also accept left and right operator sections,
including sections that capture enclosing ``foreach`` runtime binders::

    congRightPlus :: forall (m :: Nat) (n :: Nat).
                     foreach (p :: Nat) -> m :~: n -> (m + p) :~: (n + p)
    congRightPlus p eq = cong (+ p) eq

With :extension:`LinearTypes`, ``foreach`` binders accept explicit linear and
multiplicity-polymorphic annotations ``foreach (x %1 :: a) ->`` and
``foreach (x %m :: a) ->``::

    dupInTypeLinear :: foreach (n %1 :: Nat) -> Proxy '(n, n) -> Nat
    dupInTypeLinear n Proxy = n

Type-level occurrences of ``n`` are erased and do not consume runtime
multiplicity. Term-level occurrences of ``n`` must satisfy the declared
multiplicity ``%1`` or ``%m``.

Dependent pattern matching and retained arguments
-------------------------------------------------

A ``foreach`` binder retains a value argument whose identity can occur in later
parameter types and the result type::

    {-# LANGUAGE DependentTypes #-}

    keep :: foreach (b :: Bool) -> Select b -> Select b
    keep _ x = x

Here ``Select`` denotes a type family indexed by ``Bool``. The argument ``b`` is
an ordinary runtime Boolean. Under Haskell's non-strict semantics, calling
``keep`` does not force ``b``. In a module with :extension:`Strict`, the
wildcard has the same demand as a named argument.

Constructor equations specialize dependent argument and result types::

    choose :: foreach (b :: Bool) -> Select b
    choose True  = 42
    choose False = False

    observe :: foreach (b :: Bool) -> Select b -> String
    observe True  n = show (n :: Int)
    observe False p = if p then "True" else "False"

The domain can be any closed lifted algebraic datatype, GADT, parameterized
container, function type, or ``Type``::

    {-# LANGUAGE DependentTypes, TypeFamilies #-}

    data Nat = Z | S Nat

    data Vec a (n :: Nat) where
      VNil  :: Vec a Z
      VCons :: a -> Vec a n -> Vec a (S n)

    type family Add (n :: Nat) (m :: Nat) where
      Add Z     m = m
      Add (S n) m = S (Add n m)

    replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
    replicateNative n x = case n of
      Z   -> VNil
      S k -> VCons x (replicateNative k x)

    headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
    headNative _ (VCons x _) = x

    successorHead :: foreach (n :: Nat) -> Int
    successorHead n = headNative n (replicateNative (S n) 42)

    appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
    appendVec VNil         ys = ys
    appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

    appendNative :: forall a. foreach (n :: Nat) (m :: Nat)
                 -> Vec a n -> Vec a m -> Vec a (Add n m)
    appendNative _ _ xs ys = appendVec xs ys

    appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat)
                         -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)
    appendAfterSuccessor n = appendNative @Int (S n)

``headNative`` does not inspect its ``Nat`` argument. In GHCi, enable
``:seti -XDependentTypes`` to define and apply retained functions at the prompt::

    ghci> let fill :: foreach (n :: Nat) -> Vec Bool n; fill n = replicateNative n True
    ghci> toList (fill 2)
    [True,True]

Dependent definitions require an explicit value signature. Signatures allow
outer and inner erased ``forall`` binders, visible and invisible ``foreach``
binders, unannotated and wildcard ``foreach`` binders, ``Type`` domains,
function domains, and lifted algebraic or indexed GADT domains. GHC rejects
newtype and unboxed ``foreach`` domains. Retained GADT constructors support
existential ``forall`` binders, class and equality constraints, and visible or
invisible ``foreach`` fields (such as
``MkT :: forall a -> foreach (n :: Nat) -> Vec a n -> T``). Unsigned aliases of
retained functions require a complete signature and report ``GHC-83605``.

Constructor equations can match one or more ``foreach`` columns, including
nested constructor patterns (such as ``S (S k)``) and catch-all ``_`` fallback
rows. Later GADT or value arguments can distinguish equations or refute
impossible constructor combinations without writing absurd clauses. GHC checks
pattern coverage and redundancy with the standard warnings and rejects guards on
retained constructor equations::

    fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n)
    fromListExact Z     []       = Just VNil
    fromListExact Z     (_ : _)  = Nothing
    fromListExact (S _) []       = Nothing
    fromListExact (S n) (x : xs) = VCons x <$> fromListExact n xs

Source ``case`` expressions refine a named local retained index (or checked
function-application scrutinee) when GHC checks the result type, and refine
referenced local bindings whose types mention that index. Dependent ``case``
branches accept flat and nested constructor patterns and catch-all ``_``
fallback rows. GHC rejects guards and Template Haskell quotations in dependent
``case`` expressions.

Calls to ``foreach`` functions accept constructor arguments with fields
(including ``S n``, ``S (S n)``, and indexed or existential GADT constructors),
annotated expressions, local variables, immediate single-binder beta redexes,
and checked source calls without forcing ``n`` at type-checking time. Partial
applications such as ``appendNative @Int (S n)`` preserve index relationships
across remaining arguments.

Function parameters and ``case``-bound variables used in types act as rigid
term skolems unless pattern matching refines them. Global type-family equations
normalize beneath ``foreach`` binders at nominal and representational roles
without evaluating runtime indices or performing function eta-conversion::

    nativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
    nativeId _ xs = xs

    leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n
    leftUnit = nativeId

Dependent sums (``some``) and erased existentials (``exists``)
--------------------------------------------------------------

A dependent sum (``some``) stores its first component at runtime, and the type
of its second component can depend on that first value. An erased existential
(``exists``) hides a witness type chosen by the producer without storing a type
representation at runtime::

    {-# LANGUAGE DependentTypes #-}
    module Filter where
    import GHC.Sigma (Sigma(Pair))

    data Nat = Z | S Nat
    data Vec a (n :: Nat) where
      Nil  :: Vec a Z
      Cons :: a -> Vec a n -> Vec a (S n)

    type Filtered a = exists (n :: Nat). Vec a n

    filterVec :: (a -> Bool) -> Vec a n -> Filtered a
    filterVec _ Nil = Nil
    filterVec p (Cons x xs)
      | p x       = Cons x (filterVec p xs)
      | otherwise = filterVec p xs

    forgetLength :: (some (n :: Nat) | Vec a n) -> Filtered a
    forgetLength (Pair _ xs) = xs

Both ``exists`` (enabled by :extension:`ExistentialTypes` or
:extension:`DependentTypes`) and ``some`` accept multi-binder telescopes,
shared-type binder groups, and dot or bar delimiters:

- ``exists (m n :: Nat). Pair2 (Vec a m) (Vec a n)`` and
  ``exists {k} (x :: k). Proxy x`` open and pack existential packages implicitly
  through subsumption, preserving lazy evaluation of the payload.
- ``exists (m n :: Nat) | Pair2 (Vec a m) (Vec a n)`` uses explicit ``Pack``
  construction and pattern matching from ``GHC.Exists`` (reporting ``GHC-27432``
  when ``Pack`` is omitted on a bar-delimited existential).
- ``Pack @w payload`` supplies or binds an explicit witness type ``w`` in
  expressions and patterns (including ``Pack @_ payload``) for both the bar
  form and the dot form::

      showPacked :: (exists a | (a, a -> String)) -> String
      showPacked (Pack @a (x, render)) = render (x :: a)

      packedInt :: exists a | (a, a -> String)
      packedInt = Pack @Int (42, show)

- ``GHC.Exists`` (`GHC Proposal #473`_) exports ``Exists``
  (``type Exists p = exists x | p x``), ``Pack``, ``(/\)`` (constructor
  ``MkCPair``, ``infixr 2 /\``), ``discardEvidence :: (c /\ a) -> a``,
  ``withEvidence :: (c /\ a) -> (c => a -> r) -> r``, ``Witness(Witness)`` for
  erased witness tokens, ``withExists``, ``mapExists``, and ``uncurryExists``.
- ``some (m n :: Nat) | Pair2 (Vec a m) (Vec a n)`` and
  ``some (m n :: Nat). Pair2 (Vec a m) (Vec a n)`` desugar right-associatively
  to nested ``GHC.Sigma.Sigma`` pairs in any type position.
- ``GHC.Sigma`` exports ``Sigma(Pair)``, ``first :: Sigma a p -> a``,
  ``second :: foreach (p :: Sigma a b) -> b (first p)``, and
  ``withSigma :: Sigma a p -> (foreach (x :: a) -> p x -> r) -> r``.
- With :extension:`OverloadedRecordDot` (`GHC Proposal #282`), dot projection
  ``r.field`` resolves dependent record selectors and ``GHC.Sigma`` projections
  (``pkg.first`` and ``pkg.second``) by substituting ``r`` into dependent field
  result types. Dependent record update checks interrelated fields together.

Term promotion, large eliminations, and compile-time evaluation
---------------------------------------------------------------

Under :extension:`DependentTypes`, :ghc-flag:`-fwrite-source-equations` is
enabled by default. GHC stages top-level datatypes, type families, and value
bindings in dependency SCC order so functions defined earlier in a module can
appear in later type signatures in the same module as well as across ``.hi``
interfaces (including under :ghc-flag:`-fno-code` with
:ghc-flag:`-fwrite-interface`).

A term function whose result type is ``Type`` (from ``Data.Kind``) defines a
large elimination without a separate ``type family`` declaration::

    {-# LANGUAGE DependentTypes #-}
    module LargeElim where
    import Data.Kind (Type)

    data Val = Bot | Arr Val Val

    valPred :: Val -> Type
    valPred Bot       = type ()
    valPred (Arr a b) = type (valPred a -> valPred b)

    defaultVal :: foreach (v :: Val) -> valPred v
    defaultVal Bot       = ()
    defaultVal (Arr _ b) = \_ -> defaultVal b

Each equation right-hand side writes its result type with the ``type`` herald.
GHC synthesizes a promoted closed type family for type-level calls such as
``valPred v``. Multi-argument functions (``Val -> Val -> Type``), polymorphic
list eliminations (``forall a. [a] -> Type``), and functions mutually recursive
with datatypes follow the same rule.

Nonrecursive local ``let`` and ``where`` bindings in the static subset unfold
their definitions in dependent types while keeping lazy runtime sharing. This
includes checked arithmetic, constructors, and unary function compositions::

    prefixed :: foreach (rows :: Nat) (cols :: Nat) -> Vec Bool (1 + rows * cols)
    prefixed rows cols =
        let cells = rows * cols
        in replicateNative (1 + cells) True

    packIso :: forall a b c.
               foreach (f :: a -> b) (g :: b -> a)
                       (h :: b -> c) (k :: c -> b)
            -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
            -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
            -> Iso a c
    packIso f g h k left right = MkIso @a @c to from left right
      where
        to x = h (f x)
        from = g . k

Recursive local functions and effectful bindings remain opaque term skolems.

When a closed lifted Peano datatype defines a ground ``Num`` instance with a
certified three-guard ``fromInteger`` method (guarding ``n < 0``, returning the
zero constructor at ``n == 0``, and applying the successor constructor to
``fromInteger (n - 1)``), GHC promotes nonnegative type-level numerals at that
kind directly without requiring a separate ``FromNatural`` instance::

    three   :: Vec Bool 3
    three = replicateNative 3 True

    oneMore :: foreach (n :: Nat) -> Vec Bool (n + 1)
    oneMore n = replicateNative (n + 1) True

When no certified ``Num`` instance is in scope, a library can define an
explicit ``FromNatural`` instance from ``GHC.TypeNats.Experimental`` in
``ghc-experimental``.

GHC also evaluates closed ground calls to ordinary functions at compile time
during type checking (reporting ``GHC-70565`` when a closed call fails or
exceeds limits, and ``GHC-70566`` when open symbolic evaluation cannot reduce an
unsupported call)::

    {-# LANGUAGE DependentTypes, ListTuplePuns #-}
    import qualified Data.Char
    import Data.Type.Equality ((:~:)(Refl))

    upper :: Data.Char.toUpper 'x' :~: 'X'
    upper = Refl

    total :: Prelude.sum '[2, 3, 5] :~: 10
    total = Refl

    evenSum :: Prelude.odd (Prelude.sum '[2, 3, 5]) :~: 'False
    evenSum = Refl

``base`` publishes checked character equations for ``Data.Char.toUpper``, so an
exported constraint such as ``(Data.Char.toUpper c ~ uc, KnownChar uc)`` also
reduces on concrete character arguments in downstream ``Haskell2010`` modules.
Safe Haskell rejects closed bytecode execution during type checking.

Type-level ``let``, ``case``, lambdas, and the ``data`` herald
--------------------------------------------------------------

Type definitions and signatures can bind intermediate types with ``let``,
branch on constructors or ground types with ``case``, and write higher-order
type lambdas::

    {-# LANGUAGE DependentTypes, TypeFamilies #-}
    module SurfaceGuide (PairUp, Payload, ByType, WithError, Result) where
    import Data.Kind (Type)
    import GHC.Matchability (U)

    type PairUp a = let b = Maybe a in (b, b)

    type Payload b = case b of
      True  -> Int
      False -> Bool

    type ByType a = case a of
      Bool     -> Int
      Ordering -> Char

    type WithError :: Type %U -> Type
    type WithError = (\err result -> Either err result) String

    type Result = WithError (Payload 'True)

Here ``Result`` reduces to ``Either String Int``, and ``ByType Bool`` reduces
to ``Int``. With :extension:`DependentTypes` or :extension:`ExtendedLet`
(`GHC Proposal #523`_), term-level ``let`` and ``where`` blocks can also declare
nonrecursive local ``type`` synonyms that reference enclosing ``foreach``
binders::

    headSum :: foreach (n :: Nat) -> (Int, Int) -> Vec Int n -> Int
    headSum n (u, v) tailVec =
      let type PairOf a = (a, a)
          pair :: PairOf Int
          pair = (u, v)
          combined :: LocalVec Int
          combined = VCons (fst pair + snd pair) tailVec
      in case combined of
           VCons total _ -> total
      where
        type LocalVec b = Vec b (Succ n)

Type families under :extension:`DependentTypes` also support promoted primitive
``Char#`` literals (``'C# 'a'#`` under :extension:`MagicHash`), promoted unboxed
tuples ``(# a, b #)`` (under :extension:`UnboxedTuples`), and standalone open
type families with class constraints in their kind signatures (such as
``type ItemOf :: HasItem c => c -> Type``).

Dual to the ``type`` herald (:ref:`explicit-namespaces`), :extension:`ExplicitNamespaces`
(implied by :extension:`DependentTypes`) enables the ``data`` namespace herald
in type syntax, expressions, and patterns (`GHC Proposal #378`_ Section 4.3.4).
Writing ``data t`` resolves names in the data-constructor and term namespaces
first before falling back to the type namespace, and interprets list and tuple
brackets as promoted data constructors even under :extension:`NoListTuplePuns`::

    {-# LANGUAGE DependentTypes #-}
    import Data.Proxy (Proxy(Proxy))

    data Tag = Tag

    -- Proxy Tag selects the type Tag :: Type;
    -- Proxy (data Tag) selects the promoted constructor 'Tag :: Tag.
    tagProxy :: Proxy (data Tag)
    tagProxy = Proxy

    emptyListProxy :: Proxy (data [])
    emptyListProxy = Proxy

    pairProxy :: Proxy (data (True, False))
    pairProxy = Proxy

Inside ``data (...)``, explicit ``:: kind`` and ``@kind`` annotations switch
back to type syntax.

Typed holes, advisory totality, and companion packages
------------------------------------------------------

Under :extension:`DependentTypes`, typed holes (see :ref:`typed-holes`) inspect
local ``foreach`` runtime indices, local ``let`` and ``where`` bindings, and
GADT equality Givens:

- Built-in valid hole fits check individual in-scope identifiers, polymorphic
  constructors (such as ``Refl``), GADT constructors, and ``foreach`` functions
  at in-scope runtime term indices, including point-free ``foreach`` holes.
- When :ghc-flag:`-frefinement-level-hole-fits=⟨n⟩` is enabled, GHC splits a
  goal into typed subgoals by peeling visible function arrows, ``foreach``
  binders, and required ``forall a ->`` binders (supporting existential
  intermediate variables such as ``trans (_ :: lhs :~: b0) (_ :: b0 :~: rhs)``
  when :ghc-flag:`-fabstract-refinement-hole-fits` is also set).
- When a hole type contains a stuck promoted function application on an unsplit
  local variable, GHC suggests pattern-matching on that variable and lists its
  constructors under ``Suggested fixes``.

Dependent Haskell does not require termination by default. Authors who want
local structural termination feedback can enable :ghc-flag:`-freport-totality`.

.. ghc-flag:: -freport-totality
    :shortdesc: Emit an advisory structural totality report for top-level bindings.
    :type: dynamic
    :reverse: -fno-report-totality
    :category: verbosity

    :default: off
    :since: 10.2

    Inspect zonked top-level value bindings and emit one informational
    ``totality: <name>: <status>`` diagnostic per binding. The report is
    advisory: it never fails compilation under :ghc-flag:`-Werror`, and it does
    not alter Core conversion, optimization, or ``.hi`` interface files.

    GHC recomputes the top-level call graph across signed and unsigned bindings,
    checks pattern-match coverage privately without requiring
    :ghc-flag:`-Wincomplete-patterns`, and classifies each binding into one of
    two categories:

    - ``checked-structural``: exhaustive unguarded equations over closed,
      strictly positive first-order algebraic types, calling only
      ``checked-structural`` local functions, with direct self-recursion on a
      strict constructor subterm in one fixed parameter position.
    - ``unknown (<reason>)``: non-certified forms, reporting the blocking
      reason (such as ``non-structural recursive call``, ``mutual recursion``,
      ``partial or guarded equations``, ``higher-order call``,
      ``imports external function``, or ``calls non-total local function``).

Standard GHC plugins (:ghc-flag:`-fplugin=⟨module⟩`) work with
:extension:`DependentTypes`: source, renamer, splice, driver, interface,
typechecker (``tcPlugin``), defaulting, and hole-fit plugins run normally, and
``installCoreToDos`` Core-to-Core passes execute under Dependent Core Lint
before post-optimization source-equation verification. When at least one
``TcPlugin`` is active, uncertified open term calls in types elaborate to
deterministic equationless closed type families (``U:<name>``) instead of
reporting ``GHC-68947`` immediately (see
:ref:`type-family-rewriting-with-plugins`). Only active ``latePlugin`` hooks,
profiling, and JavaScript code generation reject native dependent Core.

Three external companion packages provide standard data structures, testing, and
proof search on top of :extension:`DependentTypes`:

- ``dependent-base`` (`TharkunAB/dependent-base <https://github.com/TharkunAB/dependent-base>`_):
  standard library modeled on ``linear-base``, reusing ``base`` runtime types
  without wrapper allocation and providing ``Prelude.Dependent``,
  ``Data.{Nat,Fin,Vec,List,Sigma,Exists,Type.Equality}.Dependent``,
  ``Data.Decision``, and ``Data.{Nat,List,Vec}.Dependent.Properties``.
- ``dependent-obs-eq`` (`TharkunAB/dependent-obs-eq <https://github.com/TharkunAB/dependent-obs-eq>`_):
  typechecker plugin (``-fplugin=Dependent.Plugin.ObsEq``) and library
  (``Dependent.ObsEq``) for bounded QuickCheck observational-equivalence testing
  (``obsEq``) and counterexample shrinking on uninterpreted open term calls.
- ``dependent-proof-search`` (`TharkunAB/dependent-proof-search <https://github.com/TharkunAB/dependent-proof-search>`_):
  ``HoleFitPlugin`` (``-fplugin=Dependent.Plugin.ProofSearch``) providing
  bounded backward-chaining proof synthesis and congruence/transitivity subgoal
  refinement hole fits.

Implied extensions, reserved keywords, and diagnostics
------------------------------------------------------

:extension:`ExistentialTypes` and :extension:`ExtendedLet` imply
:extension:`RequiredTypeArguments` and enable
:ghc-flag:`-fwrite-source-equations` unless
:ghc-flag:`-fno-write-source-equations` is given. :extension:`DependentTypes`
implies :extension:`ExistentialTypes`, :extension:`ExtendedLet`,
:extension:`GADTs` (which implies :extension:`GADTSyntax` and
:extension:`MonoLocalBinds`), :extension:`DataKinds`, :extension:`PolyKinds`,
:extension:`TypeOperators`, :extension:`RankNTypes` (which implies
:extension:`ExplicitForAll`), :extension:`ScopedTypeVariables`,
:extension:`TypeApplications`, :extension:`TypeAbstractions`,
:extension:`StandaloneKindSignatures` (which disables :extension:`CUSKs`),
:extension:`EmptyCase`, :extension:`Modifiers`,
:extension:`RequiredTypeArguments`, :extension:`ExplicitNamespaces`, and
:extension:`NoListTuplePuns`. A later ``No...`` or :extension:`ListTuplePuns`
flag or pragma overrides any implied extension. :extension:`DependentTypes` also
permits unsaturated type-family and type-synonym applications at unmatchable
arrow kinds without implying :extension:`TypeFamilies` or
:ghc-flag:`-fprint-explicit-matchabilities`.

When these extensions are disabled, ``foreach``, ``exists``, and ``some``
remain ordinary identifiers::

    foreach :: a -> a
    foreach x = x

When :extension:`DependentTypes`, :extension:`ExistentialTypes`, or
:extension:`ExtendedLet` is enabled, GHC reserves ``foreach``, ``exists``, and
``some`` as keywords (and reserves ``∏`` when :extension:`UnicodeSyntax` is
also enabled). With :ghc-flag:`-fwrite-ide-info`, ``.hie`` files record
retained binder visibilities, runtime index references, and opaque call indices.

Primary diagnostic codes for dependent definitions and term promotion:

- ``GHC-83605``: invalid dependent binder domain, unsupported retained pattern or
  application outside the static subset, or unsigned retained function alias.
- ``GHC-43582``: dependent quantifier used in an unsupported type context.
- ``GHC-68947`` and ``GHC-64152``: open term or callback in a type lacks
  published checked source equations (when no ``TcPlugin`` handles it).
- ``GHC-70565`` and ``GHC-70566``: closed or open compile-time term evaluation
  failed or remained stuck on an unsupported construct.
- ``GHC-27432``: explicit ``exists ... | ...`` type constructed or matched
  without ``GHC.Exists.Pack``.

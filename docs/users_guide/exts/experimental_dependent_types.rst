.. _experimental-dependent-types:

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
    :since: 10.1
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
    :since: 10.1
    :status: Experimental

    Enable first-class existential quantification (``exists``,
    `GHC Proposal #473`_).

.. extension:: ExtendedLet
    :shortdesc: Enable local let bindings in types and local type synonyms in terms.

    :implies: :extension:`RequiredTypeArguments`
    :since: 10.1
    :status: Experimental

    Enable local ``let ... in ...`` bindings in types and nonrecursive local
    ``type`` synonyms inside term-level ``let`` and ``where`` blocks
    (`GHC Proposal #523`_).

.. extension:: ExperimentalDependentTypes
    :shortdesc: Legacy compatibility alias for dependent quantification.

    :implies: :extension:`RequiredTypeArguments`
    :since: 10.1
    :status: Deprecated

    Legacy compatibility alias that enables dependent quantification without
    the full :extension:`DependentTypes` extension bundle. New code should
    enable :extension:`DependentTypes` (adding :extension:`ListTuplePuns` after
    :extension:`DependentTypes` when list or tuple type puns are desired).

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
GHCi loads dependent modules as interpreted source and preserves local
declarations across commands.

Local definitions and functions in types
----------------------------------------

A type synonym can name an intermediate type with ``let``, select a
type by matching a constructor, or partially apply a type lambda::

    {-# LANGUAGE DependentTypes, TypeFamilies #-}
    module SurfaceGuide (PairUp, Payload, ByType, WithError, Result) where
    import Data.Kind (Type)
    import GHC.Exts (U)

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

Here ``Result`` reduces to ``Either String Int``. A module importing the
synonym can use that equality without enabling the extension. The
unmatchable arrow in ``WithError`` uses :extension:`Modifiers`.

A closed type family can use ``case`` on the right-hand side of an
equation. In this example, a local binding computes a default width.
A second case selects that width or an explicit override::

    {-# LANGUAGE DependentTypes, TypeFamilies #-}
    module WidthGuide where
    import GHC.TypeNats (Nat)

    type family Width (wide :: Bool) (override :: Maybe Nat) :: Nat where
      Width wide override =
        let defaultWidth = case wide of { True -> 3; False -> 2 }
        in case override of { Nothing -> defaultWidth; Just n -> n }

``Width 'False 'Nothing`` reduces to ``2``. With an unknown ``wide``,
``Width wide ('Just 7)`` still reduces to ``7``. With an unknown
``override``, neither branch can be selected. The example uses the
ordinary type-family termination checks with :extension:`UndecidableInstances`
disabled.

A type-level let checks its bindings in source order. Each right-hand
side sees the enclosing scope and the preceding bindings. Each binding
must kind-check, even if the body ignores it. A kind signature for one
name can immediately precede that name's binding. For example,
``let { b :: Type; b = Maybe a } in (b, b)`` checks ``Maybe a`` at
kind ``Type``.

The lambda and case forms currently require a top-level synonym on the
outside. A case alternative can bind distinct fields of a constructor.
A variable or wildcard at the root accepts the scrutinee when matching
reaches that row. An earlier constructor pattern blocks reduction on an
unknown input, even if both rows return the same type.

``ByType Bool`` selects ``Int`` directly at compile time. A bare branch
head names a data constructor if one is in scope. Otherwise GHC looks
it up in the type namespace. A promotion tick always selects a data
constructor. A synonym used as a pattern must still have a constructor
head after expansion.

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

With :extension:`DependentTypes` or :extension:`ExtendedLet`
(`GHC Proposal #523`_), term-level ``let`` and ``where`` blocks can also declare
nonrecursive local ``type`` synonyms, including synonyms that mention enclosing
``foreach`` runtime binders::

    {-# LANGUAGE DependentTypes #-}
    module LocalTypeSynonym where

    data Nat = Zero | Succ Nat
    data Vec a (n :: Nat) where
      VNil  :: Vec a Zero
      VCons :: a -> Vec a n -> Vec a (Succ n)

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
``Char#`` literals (such as ``'C# 'a'#`` under :extension:`MagicHash`), promoted
unboxed tuples ``(# a, b #)`` (under :extension:`UnboxedTuples`), and
standalone open type families with class constraints in their kind signatures
(such as ``type ItemOf :: HasItem c => c -> Type``).

Retained pairs and erased lengths
---------------------------------

A retained pair stores its first component at runtime. The type of its
second component can mention the first. An existential can instead hide
a type index chosen by the producer. For example, a vector filter chooses
its result length after it checks the elements::

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

In the passing branch, GHC opens the recursive result at a fresh length
and packs the new cell at its successor. Reading the first passing cell
can leave the tail unevaluated. The caller must establish that the result
is nonempty before it can use a function that requires a successor length.

Both ``exists`` (enabled by :extension:`ExistentialTypes` or
:extension:`DependentTypes`) and ``some`` accept multi-binder telescopes,
shared-type binder groups, and dot or bar delimiters:

- ``exists (m n :: Nat). Pair2 (Vec a m) (Vec a n)`` and
  ``exists {k} (x :: k). Proxy x`` infer package introduction and elimination.
- ``exists (m n :: Nat) | Pair2 (Vec a m) (Vec a n)`` uses explicit ``Pack``
  construction and pattern matching from ``GHC.Exists``.
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
  to nested ``Sigma`` pairs. GHC accepts ``some`` in any type position,
  including function signatures, GADT fields, local ``let`` signatures,
  expression type annotations, and required type arguments.
- ``GHC.Sigma`` exports ``Sigma(Pair)``, ``first :: Sigma a p -> a``,
  ``second :: foreach (p :: Sigma a b) -> b (first p)``, and
  ``withSigma :: Sigma a p -> (foreach (x :: a) -> p x -> r) -> r``.
- With :extension:`OverloadedRecordDot`, dot projection ``r.field`` resolves
  dependent record selectors and ``GHC.Sigma`` projections (``pkg.first`` and
  ``pkg.second``) when a field's result type depends on earlier fields of ``r``.

Both the producer and an ordinary client can
compile at ``-O1``, including through a separately compiled interface.
Two uses of the same unrestricted local package can share a witness in
one application. Two independently filtered vectors have independent lengths.
A proof of element-type equality also applies under the hidden length binder.
GHC can compile that transport at ``-O1``.

Natural numerals in vector indices
----------------------------------

When a closed lifted Peano datatype defines a ground ``Num`` instance with
the certified three-guard ``fromInteger`` method under
:extension:`DependentTypes`, GHC
automatically promotes nonnegative type-level numerals at that kind without
requiring a separate ``FromNatural`` instance.

A library can also supply an explicit ``FromNatural`` fallback instance when no
certified ``Num`` instance is in scope. Import ``FromNatural`` from
``GHC.TypeNats.Experimental`` in ``ghc-experimental`` and define an instance
for the index kind::

    import GHC.TypeNats.Experimental (FromNatural)
    import qualified GHC.TypeNats as TN

    data Nat = Z | S Nat

    type family ToPeano (n :: TN.Nat) :: Nat where
      ToPeano 0 = 'Z
      ToPeano n = 'S (ToPeano (n TN.- 1))

    type instance FromNatural Nat n = ToPeano n

This definition of ``ToPeano`` requires :extension:`UndecidableInstances`.
In a module with :extension:`DependentTypes`, a numeral in
``Vec a 3`` now has the expected kind ``Nat``. The checked instance
determines its value. When GHC infers the kind of a numeral, or expects
``GHC.TypeNats.Nat``, the ordinary built-in literal rules apply.
Family instance patterns keep the ordinary rules for literals.
When a proof needs the full Peano representation of a numeral, each
successor takes a reduction step in this example. Large proofs can
reach GHC's family reduction depth limit.

The packed-vector example defines ``type n + m = plus n m`` and shares
the ordinary definition of ``plus`` with its ``Num Nat`` instance. It can
therefore write signatures such as these::

    three     :: Vec Bool 3
    safeHead  :: Vec a (1 + n) -> a
    appendOne :: Vec a n -> a -> Vec a (n + 1)

The first argument of ``plus`` controls reduction. In ``safeHead``,
``1 + n`` reduces to a successor without inspecting ``n``. Each caller
still uses the ordinary term-level ``fromInteger`` for its runtime literals.
An arbitrary ``Num`` instance cannot establish a runtime index equality.

The library also provides ``replicateAt 3 x :: Vec a 3``, using
:extension:`RequiredTypeArguments`. The first argument specifies the
length in the type. For a length ``n`` read at runtime, its retained
counterpart is ``replicateNative n x :: Vec a n``.

A retained argument can also be a sum of two lengths read from IO::

    import PackedVecLibrary (Nat, Vec, type (+), replicateNative)

    fillSum :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Bool (n + m)
    fillSum n m = replicateNative (n + m) True

The signature uses the library's type operator. The body uses the
standard overloaded ``Prelude.(+)``. The library defines ``(+) = plus``
in its ``Num Nat`` instance. Its interface records both that alias and
the checked equations for ``plus``. GHC checks that this call uses the
same dictionary before relying on the sum in the result type. The
runtime sum stays lazy.

A recursive equation can also call another checked function. For example,
``times (S n) m = plus m (times n m)`` uses the checked equations for
``plus``. The recursive call decreases the first argument of ``times``.
The vector library defines both the type operator ``(*)`` and the
ordinary ``Num Nat`` multiplication method using ``times``.
Enable ``NoStarIsType`` to write the type operator::

    flattenRows :: Vec (Vec a cols) rows -> Vec a (rows * cols)
    flattenRows VNil = VNil
    flattenRows (VCons row rest) = appendVec row (flattenRows rest)

A retained operand can contain several checked calls. Each call uses its
own function or method dictionary. For example::

    prefixed :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Bool (1 + rows * cols)
    prefixed rows cols = replicateNative (1 + rows * cols) True

The two operators have their ordinary fixities. A caller can also write
``plus 1 (times rows cols)``. The first cell can be produced before
evaluating the product.

A checked result can use the defining index of a lazy local name::

    prefixed rows cols =
        let cells = rows * cols
        in replicateNative (1 + cells) True

The compiler checks the ordinary binding first. If its actual ``Num Nat``
dictionary supplies the checked multiplication function, the body can use
``cells`` at the index ``times rows cols``. Each use shares the local value.
The same check applies to a numeral in a local formula, such as
``let cells = times rows 3``.

A lazy let expression can also name more than one intermediate length.
The bindings can have complete monomorphic signatures::

    prefixed rows cols =
        let total, cells :: Nat
            total = 1 + cells
            cells = rows * cols
        in replicateNative total True

GHC checks the signatures first and follows the dependencies between
bindings. Here the body can use ``total`` at the length
``1 + rows * cols``. This rule applies to nonrecursive value definitions
in a let expression with a checked dependent result or a fixed
monomorphic result. A function with one unguarded equation can also
use these definitions in its ``where`` clause::

    safeHead :: Vec a (1 + n) -> a
    safeHead (VCons x _) = x

    localHead :: foreach (r :: Nat) -> foreach (c :: Nat) -> Char
    localHead r c = safeHead (replicateNative total 'x')
      where
        total, area :: Nat
        total = 1 + area
        area = r * c

GHC can use a defining RHS to choose the omitted index for ``safeHead``.
It checks the original vector against the resulting argument type.
With the checked arithmetic from this library, either dimension can be
unknown at compile time. The outer successor suffices to take the head.
If the local length is ``0``, type checking fails at the vector argument.
The mismatch refers to the constructors ``Z`` and ``S`` of this ``Nat`` type.

Each RHS that supplies a length equality must yield a monomorphic value.
A different dictionary can still supply the ordinary runtime operation.
If the body needs the index equality, the compiler also checks rewrite
rules that can affect the selected method.

Nonrecursive local ``let`` and ``where`` bindings also unfold lazy unary
function compositions in the static subset::

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

GHC unfolds ``to`` and ``from`` in dependent types across ``let`` blocks and
single-clause or multi-clause ``where`` blocks. GHC also unfolds ``where``
bindings on ``case`` branches when no guard binds a referenced variable. GHC
accepts signed and unsigned definitions, point-free ``h . f``, lambda
``\x -> h (f x)``, and chained compositions ``p . h . f``. Runtime evaluation
remains lazy. Recursive local functions remain opaque term skolems.

The vector library's ``Num Nat`` instance also supplies checked equations
for ``fromInteger``. A caller can use those equations at a retained
argument, including inside an addition::

    three :: Vec Bool 3
    three = replicateNative 3 True

    oneMore :: foreach (n :: Nat) -> Vec Bool (n + 1)
    oneMore n = replicateNative (n + 1) True

The checker recognises a ground instance whose ``fromInteger n``
first guards against ``n < 0``, returns the zero constructor for
``n == 0``, and otherwise applies the successor constructor to
``fromInteger (n - 1)``. Both guards use the standard ``Integer``
operations. The instance module publishes the constructors in its
interface. Each successor counts against ``-freduction-depth``.
The ordinary solver must select the checked dictionary at the literal.
Result signatures use the certified ``Num`` conversion directly. If no
certified ``Num`` instance is in scope, GHC falls back to an explicit
``FromNatural`` instance. When both instances are in scope, GHC checks that the
two interpretations agree.

A consumer can infer the tail of a vector produced by another call::

    firstCopy :: foreach (n :: Nat) -> a -> a
    firstCopy n x = safeHead (replicateNative (1 + n) x)

The addition equations reduce the formal length ``1 + k`` of ``safeHead``
to ``S k`` and the argument length to ``S n``. GHC can then choose ``k``
to be the caller's runtime ``n``. The first element is available without
forcing that tail length. With an unknown left addend, ``n + 1`` needs a
proof of nonemptiness or a pattern match on the vector.

A constructor argument can determine the erased type of a retained parameter.
For example, the fields of a list cell determine its element type::

    firstCell :: a -> [a] -> a
    firstCell x xs = safeHead (sourceVector (x : xs))

The equation for ``sourceLength (x : xs)`` gives a successor for any tail
``xs``. The caller can therefore take the head without inspecting the tail.

A call can also use the source function directly, as in
``replicateNative (plus n m) True``. The function must be monomorphic
and fully applied. Its operands can be local values, supported constructors,
or further checked calls. A user-defined operator can supply equations in
the same way as a named function.

GHC can execute a call to an imported ordinary function while checking
a type. An unqualified name uses the existing scope rules. For example,
with :extension:`RequiredTypeArguments`, ``Vec (length [1,2,3]) Bool``
can refer to an imported ``length :: [Natural] -> Natural``. An explicit
type binder named ``length`` takes precedence.

GHC uses a published character equation for ``toUpper`` and executes
``isDigit``, ``sum``, ``reverse`` and ``odd`` in the following examples::

    {-# LANGUAGE DependentTypes, ListTuplePuns #-}
    import qualified Data.Char
    import Data.Type.Equality ((:~:)(Refl))

    upper :: Data.Char.toUpper 'x' :~: 'X'
    upper = Refl

    digit :: Data.Char.isDigit '7' :~: 'True
    digit = Refl

    total :: Prelude.sum '[2, 3, 5] :~: 10
    total = Refl

    reversed :: Prelude.reverse '[2, 3, 5] :~: '[5, 3, 2]
    reversed = Refl

    nested :: Prelude.sum (Prelude.reverse '[2, 3, 5]) :~: 10
    nested = Refl

    evenSum :: Prelude.odd (Prelude.sum '[2, 3, 5]) :~: 'False
    evenSum = Refl

In a nested call, GHC checks the argument and uses the kind of its result
to specialize the outer function. Here the inner call returns a list of
``Natural``, so the outer call uses the list and ``Natural`` instances.
GHC checks the operand kinds and the required instances throughout the
chain before it executes any call. It then evaluates each inner argument
once. If that call fails, the error
points to the inner function, even when the outer function would ignore
its argument at runtime.

At the innermost call, write a natural or character literal, a Boolean
constructor, or a promoted list of those values. For example,
``Prelude.odd 5 :~: 'True`` and ``Prelude.and '[ 'True, 'False] :~: 'False``
both admit ``Refl``. GHC uses the declared function argument to select
``[Bool]`` for ``Prelude.and '[]``. A polymorphic ``Prelude.reverse '[]``
uses ``[Natural]`` as before.

Each function takes one unrestricted argument. An intermediate result
can have any of the same six kinds. An empty result list carries its
element kind to the next call. GHC forces
every result element before constructing the promoted list. The producer may use
ordinary Haskell. The compiler needs its compiled code and a target
interpreter. Safe Haskell rejects execution of these calls. If an imported
function diverges, checking its application can also diverge.

A literal can specialize an imported polymorphic function. In ``total``,
the list fixes the container to ``[]`` and the element type to
``Natural``. GHC uses ordinary instance resolution for ``Foldable []`` and
``Num Natural``. Each type parameter must be determined by the input.
A missing or ambiguous instance causes a type error at the call, even if
the module enables :ghc-flag:`-fdefer-type-errors`. Pure recursive functions defined earlier in the same module (such as
``fibClosed :: Natural -> Natural``) can also be evaluated on closed arguments
in later type signatures within that module.

With :extension:`TypeApplications`, the expression
``charVal (Proxy @(Data.Char.toUpper 'x'))`` reflects the result as a
character. A visible type argument containing such a call must be complete.

A pattern can determine the input after GHC reads a partial signature.
For example, with :extension:`NamedWildCards`, :extension:`PartialTypeSignatures`,
:extension:`ScopedTypeVariables` and :extension:`TypeApplications`::

    import Data.Proxy (Proxy(..))
    import qualified Data.Char

    upper :: Proxy _c -> Proxy (Data.Char.toTitle _c)
    upper (_ :: Proxy 'a') = Proxy @'A'

GHC learns that ``_c`` is ``'a'`` from the pattern. Before it generalizes
the binding, it evaluates the call and checks that the body returns
``Proxy 'A'``. A body that returns ``Proxy 'Z'`` fails at the call in
the signature. A body that leaves ``_c`` unknown also fails there.
An importing module sees ``upper :: Proxy 'a' -> Proxy 'A'``.

This form requires a named wildcard in the value type of a partial
signature with no leading constraints. Each input must become a
character literal or a list of natural number literals within the binding
group. An equality available only in one GADT branch cannot determine
the input for the whole group. A local pattern binding with an explicit
multiplicity cannot postpone a call.

An imported polymorphic function can also occur in a type when its argument
is a rigid type variable. For example, this definition checks because
reducing the ordinary Prelude function ``id`` returns the type variable
``a`` unchanged::

    {-# LANGUAGE DependentTypes #-}

    x :: forall a. Num a => id a
    x = 42

The compiler also accepts projections with several operands. For example::

    left :: forall a b. a -> b -> const a b
    left x _ = x

The imported function must have one or more invisible, lifted type
parameters. Each operand type and the result type must be one of those
parameters. Supply all the operands as rigid type variables of kind
``Type``. The function must return one of them during evaluation. If
evaluation reaches a case directly on a rigid operand, the error names
that operand at the source call.
A user-defined function can participate if its interface provides the
Core unfoldings that reduction needs. Compile the producer with
:ghc-flag:`-fno-omit-interface-pragmas` to retain available unfoldings.
The client records each definition it uses. The usual type variable
lookup takes precedence. For example, an explicit type binder named
``id`` still denotes that binder.

The function can select an operand by inspecting a known character.
For example, an ordinary worker can implement ``route 'x' l _ = l``
and ``route _ _ r = r``. If an imported ``routeLeft`` has type
``t -> t -> t`` and definition ``route 'x'``, then
``routeLeft a b`` reduces to ``a`` in the client signature.
The consumer records the worker definition it used. A producer loaded
as bytecode in GHCi can supply its unfoldings with
:ghc-flag:`-fno-unoptimized-core-for-interpreter` and
:ghc-flag:`-fno-omit-interface-pragmas`.

An exported equality can also mention a checked character function.
For example, module ``CharacterCase`` can define::

    upper :: Char -> Char
    upper 'x' = 'X'
    upper 'é' = 'É'
    upper c   = c

A second module can use that function in a signature::

    p3 :: forall c uc. (CharacterCase.upper c ~ uc, KnownChar uc)
       => Proxy c -> Char
    p3 _ = charVal (Proxy @uc)

Enable :extension:`DependentTypes` in those two modules.
A client with the extension switched off can call ``p3 (Proxy @'x')``
and get ``'X'``. Calling it at ``'!'`` returns ``'!'``. The producer's
interface carries the checked equations even with
:ghc-flag:`-fomit-interface-pragmas`. GHC can use the default equation
when it can rule out every earlier incompatible literal. This form
supports a finite set of character literals and an identity default.
An exported function can also forward to an imported function with checked
equations. The fork's in-tree build uses this for ``Data.Char.toUpper``.
Replacing ``CharacterCase.upper c`` with ``Data.Char.toUpper c`` in the
signature above lets an ordinary client reflect the Unicode mapping.

For a class method, the current implementation accepts a concrete
instance of a one-parameter class. The method must return its class
parameter and name a checked function as its complete definition.
The producer enables :extension:`DependentTypes` to
publish this information. A client reads it from the completed
interface. Within the instance module, use ``plus n m`` directly.
An abstract ``Num Nat`` dictionary passed to the client
cannot establish the addition equations.

Quantifiers, telescopes, and visibility
---------------------------------------

Dependent Haskell provides four explicit quantifiers alongside ``c =>`` and
``t1 -> t2``:

- ``forall a.``: erased at runtime and invisible at call sites.
- ``forall a ->``: erased at runtime and visible (required) at call sites.
- ``foreach a.``: retained at runtime and invisible at call sites.
- ``foreach a ->``: retained at runtime and visible (required) at call sites.

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
binders. GHC still accepts legacy ``@{expr}`` call syntax as a compatibility
alias for ``@arg`` on invisible ``foreach .`` parameters.

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
multiplicity; term-level occurrences of ``n`` must satisfy the declared
multiplicity ``%1`` or ``%m``.

Retained arguments
------------------

The syntax retains a value argument whose identity can occur in the
result type. For example::

    {-# LANGUAGE DependentTypes #-}

    keep :: foreach (b :: Bool) -> Select b -> Select b
    keep _ x = x

Here ``Select`` denotes a type family indexed by ``Bool``. The argument ``b`` is
an ordinary runtime Boolean whose identity can occur in the result type.
The wildcard leaves this argument unnamed in the equation. The payload
and result still have the same index. Under Haskell's usual lazy pattern
semantics, calling ``keep`` does not force the Boolean. In a module with
:extension:`Strict`, the wildcard has the same demand as a named argument.

Applications retain that value argument. They accept supported constructors,
named local values and checked source calls as described above. The
compiler checks their types without evaluating the runtime arguments.

In GHCi, a retained definition can be used in later commands. For example,
with the vector library loaded, enter the definition and use it at the
next prompt::

    ghci> let fill :: foreach (n :: Nat) -> Vec Bool n; fill n = replicateNative n True
    ghci> toList (fill 2)
    [True,True]

A multiline declaration group between ``:{`` and ``:}`` works as well.
Use ``:seti -XDependentTypes`` to enable the syntax at the
prompt. A length obtained from IO can be bound in the same expression
that applies the retained function, as in
``do { n <- readIORef ref; print (toList (fill n)) }``.

Constructor equations can specialise the result type::

    choose :: foreach (b :: Bool) -> Select b
    choose True = 42
    choose False = False

The checker substitutes the constructor into later argument types and the
result type. A consumer can therefore receive the selected value::

    observe :: foreach (b :: Bool) -> Select b -> String
    observe True n = show (n :: Int)
    observe False p = if p then "True" else "False"

The domain can also be a user-defined closed algebraic type::

    {-# LANGUAGE DependentTypes, TypeFamilies #-}

    data Nat = Z | S Nat

    data Vec a (n :: Nat) where
      VNil :: Vec a Z
      VCons :: a -> Vec a n -> Vec a (S n)

    type family Add (n :: Nat) (m :: Nat) where
      Add Z m = m
      Add (S n) m = S (Add n m)

    replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
    replicateNative n x = case n of
      Z -> VNil
      S k -> VCons x (replicateNative k x)

    headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
    headNative n (VCons x _) = x

    successorHead :: foreach (n :: Nat) -> Int
    successorHead n = headNative n (replicateNative (S n) 42)

    appendNative :: forall a. foreach (n :: Nat) (m :: Nat)
                 -> Vec a n -> Vec a m -> Vec a (Add n m)
    appendNative n m xs ys = appendVec xs ys

    appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat)
                         -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)
    appendAfterSuccessor n = appendNative @Int (S n)

    appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
    appendVec VNil ys = ys
    appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

The input Nat remains a runtime value. ``headNative`` does not inspect its
Nat argument. ``appendNative`` uses its vector arguments to select equations.
Nat constructor indices use ordinary DataKinds promotion.

Dependent definitions require an explicit value signature. Signatures allow
outer and inner erased ``forall`` binders, visible and invisible ``foreach``
binders, unannotated and wildcard ``foreach`` binders, ``Type`` domains,
function domains, and lifted algebraic or indexed GADT domains. GHC rejects
newtype and unboxed ``foreach`` domains. Retained GADT constructors support
existential ``forall`` binders, class and equality constraints, and visible or
invisible ``foreach`` fields (such as
``MkT :: forall a -> foreach (n :: Nat) -> Vec a n -> T``).

One variable equation can bind the retained telescope and match an ordinary
GADT payload, as in ``headNative``. An equation with a complete signature can
also stop before the remaining retained arguments, as in
``appendAfterSuccessor``. Unsigned aliases of retained functions require a
complete signature and report GHC-83605.

Constructor equations can match one or more ``foreach`` columns, including
nested constructor patterns (such as ``S (S k)``) and catch-all ``_`` fallback
rows. Later ordinary arguments can distinguish equations for the same retained
constructors. GHC checks pattern coverage and redundancy with the standard
warnings. GHC rejects guards on retained constructor equations. This adapter
checks a list against a requested length::

    fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n)
    fromListExact Z [] = Just VNil
    fromListExact Z (_ : _) = Nothing
    fromListExact (S _) [] = Nothing
    fromListExact (S n) (x : xs) = VCons x <$> fromListExact n xs

In each equation, matching a retained constructor refines dependent argument
and result types (including rank-2 ``forall`` payloads and results) for that
equation. Dedicated rejection checks report GHC-83605. Ordinary payload
mismatches use the standard type error. Unsupported type positions report
GHC-43582.

Source ``case`` expressions refine a named local retained index when GHC checks
the result type. The match also refines referenced local values whose types
depend on that index. Dependent ``case`` branches accept flat and nested
constructor patterns and catch-all ``_`` fallback rows. GHC rejects guards and
Template Haskell quotations in dependent ``case`` expressions and does not
infer general motives. The case inspects its index even when the result is a
function. Captured payloads remain lazy until the selected branch uses them.

Calls accept constructor arguments with fields (including ``S n``, ``S (S n)``,
and indexed or existential GADT constructors), annotated expressions, local
names, and checked source calls. These calls do not force ``n`` to check its
index. Partial applications such as ``appendNative @Int (S n)`` preserve index
relationships across remaining arguments. GHC does not perform structural
``foreach`` unification, normalization with ambient Given evidence or plugins,
or function eta conversion. Strict wildcard matching on a partial function with
named indices preserves ordinary forcing behavior. It does not force an unused
index or the function body. An empty match on a function type remains
non-exhaustive.

Function parameters and ``case``-bound variables used in types act as rigid
term skolems unless pattern matching refines them. Nonrecursive local ``let``
and ``where`` bindings in the static subset (including checked arithmetic,
constructors, and lazy unary function compositions) unfold to their defining
expressions in dependent types. Recursive local bindings and bindings outside
the static subset remain opaque skolems. Producers and consumers of opaque
local bindings must share the same named variable.

Global type-family equations can normalize beneath a retained binder with an
unchanged closed algebraic domain and unrestricted multiplicity. Complete
signatures can therefore check aliases with no source argument patterns::

    nativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
    nativeId n xs = xs

    leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n
    leftUnit = nativeId

    leftUnitAppend :: foreach (n :: Nat) (m :: Nat)
                   -> Vec Int (Add Z n) -> Vec Int m -> Vec Int (Add n m)
    leftUnitAppend = appendNative @Int

A constant family can remove the retained dependency::

    type family ConstantNat (n :: Nat) where
      ConstantNat n = Int

    ordinaryNat :: Nat -> Int
    ordinaryNat _ = 42

    constantAlias :: foreach (n :: Nat) -> ConstantNat n
    constantAlias = ordinaryNat

The normalized signature of ``constantAlias`` is an ordinary ``Nat -> Int``
arrow. This normalization does not evaluate a runtime index. A family that needs
an unknown constructor remains stuck. Solver normalization under the retained
binder excludes ambient Given evidence and plugin rewriting. It does not enable
unsigned aliases, structural retained unification, or a function eta law.
After normalization, identical runtime indices and alpha-equal retained
function types can satisfy reflexivity. This does not decompose a retained
function or assign a type meta-variable. Family axioms reduce at nominal and
representational roles. Existing phantom normalization leaves family heads
unreduced; ordinary forall binder kinds normalize at nominal role.

Dependent typed holes and advisory totality
-------------------------------------------

Under :extension:`DependentTypes`,
typed holes (see :ref:`typed-holes`) inspect local ``foreach`` runtime indices,
local ``let`` and ``where`` bindings, and GADT equality Givens:

- Valid hole fits instantiate polymorphic constructors (such as ``Refl``),
  GADT constructors, and ``foreach`` lemmas at runtime term indices, including
  bounded one-step local closure such as ``cong Succ (plusAssoc m n p)``.
- Refinement hole fits default to level 1 (see
  :ghc-flag:`-frefinement-level-hole-fits=⟨n⟩`) and split a goal into typed
  subgoals through ``foreach`` functions and required-type-argument congruence
  wrappers such as ``cong Succ (_ :: plus (plus m n) p :~: plus m (plus n p))``.
- Point-free ``foreach`` holes (such as ``lengthRev = _``) match polymorphic and
  monomorphic ``foreach`` candidates even when inner type families reduce
  differently on each side.
- When a hole type contains a stuck promoted function application on an unsplit
  local variable bound in the enclosing clause or expression, GHC suggests
  pattern-matching on that variable and lists its constructors under
  ``Suggested fixes``.

Dependent Haskell does not require termination by default. Authors who want
local structural termination feedback can enable :ghc-flag:`-freport-totality`.

.. ghc-flag:: -freport-totality
    :shortdesc: Emit an advisory structural totality report for top-level bindings.
    :type: dynamic
    :reverse: -fno-report-totality
    :category: verbosity

    :default: off
    :since: 10.1

    Inspect zonked top-level value bindings and emit one informational
    ``totality: <name>: <status>`` diagnostic per binding. The report is
    advisory: it never fails compilation under :ghc-flag:`-Werror`, and it does
    not alter Core conversion, optimization, or ``.hi`` interface files.

    GHC recomputes the top-level call graph across signed and unsigned bindings.
    GHC checks pattern-match coverage privately without requiring
    :ghc-flag:`-Wincomplete-patterns`. GHC classifies each binding into one of
    two categories:

    - ``checked-structural``: exhaustive unguarded equations over closed,
      strictly positive first-order algebraic types, calling only
      ``checked-structural`` local functions, with direct self-recursion on a
      strict constructor subterm in one fixed parameter position.
    - ``unknown (<reason>)``: non-certified forms, reporting the blocking
      reason (such as ``non-structural recursive call``, ``mutual recursion``,
      ``partial or guarded equations``, ``higher-order call``,
      ``imports external function``, or ``calls non-total local function``).

Source equations, build options, and reserved syntax
----------------------------------------------------

:extension:`DependentTypes` (along with :extension:`ExistentialTypes` and
:extension:`ExtendedLet`) enables :ghc-flag:`-fwrite-source-equations` by
default. Pass :ghc-flag:`-fno-write-source-equations` to disable
source-equation publication for a module. An ordinary module without dependent
extensions can pass :ghc-flag:`-fwrite-source-equations` to publish checked
equations for downstream dependent clients. GHC stages checked source equations
and closure tables in dependency order within the defining module and across
``.hi`` interfaces (including with :ghc-flag:`-fno-code` and
:ghc-flag:`-fwrite-interface`).

The native dependent Core pipeline runs Core Lint and STG Lint, serializes
validated ``INLINE`` and ``INLINABLE`` ``DepFunTy`` unfoldings across ``.hi``
interfaces, and supports optimization on spine-strict list proofs and
existential packages. GHCi can interpret dependent modules in memory or load
compiled bytecode.

Standard GHC plugins (:ghc-flag:`-fplugin=⟨module⟩`) work with
:extension:`DependentTypes`: source, renamer, splice, driver, interface,
typechecker (``tcPlugin``), defaulting, and hole-fit plugins run normally, and
``installCoreToDos`` Core-to-Core passes execute under Dependent Core Lint
before post-optimization source-equation verification. Only active
``latePlugin`` hooks, profiling, and JavaScript code generation reject native
dependent Core.

Use libraries and packages built with a matching compiler. The configured
compiler at this revision is ``10.1.20261115.1``. Its ordinary interface
version is ``100120261115168``. With :ghc-flag:`-fwrite-ide-info`, ``.hie``
files also record retained binder visibilities, runtime index references, and
opaque call indices.

Standard library (``dependent-base``)
-------------------------------------

The standalone `dependent-base <https://github.com/TharkunAB/dependent-base>`_
package provides the standard library for Dependent Haskell, modeled on
``linear-base``. It reuses GHC ``base`` runtime datatypes directly without
wrapper allocation or data copying and exports:

- ``Prelude.Dependent`` and ``Data.Function.Dependent`` (including dependent
  ``(.)`` and ``($)``),
- ``Data.Nat.Dependent``, ``Data.Fin.Dependent``, ``Data.Vec.Dependent``,
  ``Data.List.Dependent``, and ``Data.List.Dependent.All``,
- ``Data.{Bool,Maybe,Either,Tuple,Sigma,Exists}.Dependent``,
- ``Data.Type.Equality.Dependent`` (visible ``cong``, ``subst``, ``rewrite``,
  ``Inspect``, and ASCII equational reasoning),
- ``Data.Decision`` (``Dec``, ``DecEq``, and Boolean reflection), and
- ``Data.{List,Nat,Vec}.Dependent.Properties``.

Ordinary ``Haskell2010`` and ``GHC2024`` modules can import ``dependent-base``
interfaces without enabling :extension:`DependentTypes`.

Implied extensions and reserved keywords
----------------------------------------

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
flag or pragma overrides any implied extension.

When these extensions are disabled, ``foreach``, ``exists``, and ``some``
remain ordinary identifiers::

    foreach :: a -> a
    foreach x = x

When :extension:`DependentTypes`, :extension:`ExistentialTypes`, or
:extension:`ExtendedLet` is enabled, GHC reserves ``foreach``, ``exists``, and
``some`` as keywords (and reserves ``∏`` when :extension:`UnicodeSyntax` is
also enabled). Programs that use these names as identifiers must leave the
dependent extensions disabled.

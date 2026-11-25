# Dependent Haskell in GHC

This fork lets a Haskell function name a runtime argument in its result
type. The compiler tracks that argument through Core and generates native
code. Enable `-XDependentTypes` to write Dependent Haskell (`GHC Proposal #378`).
An ordinary `Haskell2010` or `GHC2024` module can import and call a checked
function without enabling `-XDependentTypes`.

The example library exports `replicateNative`. Its client defines
`safeHead` by matching the first cell.

```haskell
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
safeHead        :: Vec a (1 + n) -> a
safeHead (VCons x _) = x
```

A caller can take `safeHead (replicateNative (1 + n) x)` even when `n`
comes from IO. The compiler knows the vector has a first cell. The same
call on a vector of length zero fails to typecheck. For an ordinary list
of unknown length, `fromListExact 1 xs` checks the length at runtime.
The `Just` branch gets a `Vec a 1` and can use `safeHead` directly.
[Vector library](examples/packed-vector/PackedVecLibrary.hs) and
[working examples](Showcase.hs).

A prefix GADT can take a field before a required type argument.
For example, `ChoosePayload :: Bool -> forall a -> PayloadKey a` is a
`type data` constructor in the Showcase. Applying `ChoosePayload 'True Int`
selects a key for an `Int` payload.
[Ordered constructor example](roadmap/R41-nested-quantification-in-gadts.md#release-check).

For the current compiler revision, test results, and restart procedure, see
the [project handoff](HANDOFF.md).

## Standard library: `dependent-base`

The standalone [`dependent-base`](https://github.com/TharkunAB/dependent-base)
package provides the standard library for Dependent Haskell, modeled on
[`linear-base`](https://github.com/tweag/linear-base). It reuses GHC `base`
runtime datatypes (`Bool`, `Maybe`, `Either`, `(,)`, `[]`) directly without
wrapper allocation or data copying, and adds indexed containers (`Nat`, `Fin`,
`Vec`, `All`, `Any`, `Elem`), dependent pairs (`Sigma`), erased existentials
(`Exists`, `Pack`, `/\`, `Witness`), propositional equality (`cong`, `subst`,
`rewrite`, `Inspect`, ASCII equational reasoning), constructive decidability
(`Dec`, `DecEq`), and algebraic proof modules (`*.Properties`).

Import `Prelude.Dependent` for the ergonomic Dependent Haskell prelude
(including first-class dependent `(.)` and `($)`), or import individual
modules such as `Data.Nat.Dependent`, `Data.List.Dependent`,
`Data.List.Dependent.Properties`, `Data.Vec.Dependent`, and
`Data.Type.Equality.Dependent`.

| Lemma (`Data.List.Dependent.Properties`) | Length of the result |
| --- | --- |
| `lengthMap f xs` | `size xs` |
| `lengthReverse xs` | `size xs` |
| `lengthTakeSize n xs` | `minSize n (size xs)` for `takeSize n xs` |
| `lengthAppend xs ys` | `plus (size xs) (size ys)` |
| `lengthZip xs ys` | `minSize (size xs) (size ys)` |
| `lengthZipWith f xs ys` | `minSize (size xs) (size ys)` |
| `lengthZipMap pre combine xs ys` | `minSize (size xs) (size ys)` for `zipWith combine (map pre xs) ys` |

An ordinary client can use the zip equality with lists read at runtime.
A dependent client can also name it in its signature.

```haskell
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Client where

import Data.List.Dependent (size, zip)
import Data.List.Dependent.Properties (lengthZip)
import Data.Nat.Dependent (minSize)
import Data.Type.Equality.Dependent ((:~:))

zipLaw :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
       -> size (zip xs ys) :~: minSize (size xs) (size ys)
zipLaw xs ys = lengthZip xs ys
```

This proof and the corresponding `zipWith` proof compile at ordinary
`-O2`. The compiler checks the installed zipper fusion rules against
the functions' equations. [Zipper proofs](examples/open-evaluation/PreludeZipRuntime.hs)
and [map pipeline](examples/open-evaluation/PreludeZipMapRuntime.hs).

For a bounded prefix, the result type records both inputs.

```haskell
lengthTakeSize :: forall a. foreach (n :: Nat) -> foreach (xs :: [a])
               -> size (takeSize n xs) :~: minSize n (size xs)
```

At a zero bound, an ordinary caller can obtain both the empty prefix
and its length witness without evaluating the input list. At a finite
bound, the witness also terminates on an infinite list.
[Proof and Agda precedent](REFERENCES.md#length-of-a-bounded-list-prefix).

The proofs use the ordinary definitions of `size`, `plus`, and `minSize`
over `Nat` from `Data.Nat.Dependent`. The same `Nat` type accepts
ordinary numerals at runtime and natural literals in indices, as in
`plus 2 3 :~: 5` and `(2 + 3 :: Nat) :~: 5`.
[`dependent-base` repository](https://github.com/TharkunAB/dependent-base) and
[proof methods](REFERENCES.md#parametricity-and-the-list-length-laws).

### Compose equality proofs

Use `trans` (or ASCII `(===)` / `stepEq`) from `Data.Type.Equality.Dependent`
to join two proofs with a shared endpoint. Use `cong` with a visible
required function argument (`forall f ->`) to apply any matchable or
unmatchable function, constructor, composition, or operator section to both
sides of an equality:

```haskell
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Client where

import Data.List.Dependent (map, size, zipWith)
import Data.List.Dependent.Properties (lengthMap, lengthZipWith)
import Data.Nat.Dependent (Nat (..), minSize, plus)
import Data.Nat.Dependent.Properties (congFirst, plusSucc)
import Data.Type.Equality.Dependent ((:~:), cong, trans)

addUnder :: foreach (m n p :: Nat)
         -> plus m (plus n (Succ p)) :~: plus m (Succ (plus n p))
addUnder m n p = cong (plus m) (plusSucc n p)

zipMapLaw :: forall a b c d.
             foreach (pre :: a -> b) -> foreach (combine :: b -> c -> d)
          -> foreach (xs :: [a]) -> foreach (ys :: [c])
          -> size (zipWith combine (map pre xs) ys) :~: minSize (size xs) (size ys)
zipMapLaw pre combine xs ys =
    lengthZipWith combine (map pre xs) ys
        `trans` congFirst @minSize @(size ys) (lengthMap pre xs)
```

`rewrite proof body` brings a propositional equality into scope as a nominal
constraint for `body`. In the Showcase, `sameNat count (S Z)` returns a
witness when the runtime count is one:

```haskell
Just proof -> rewrite proof (Just (safeHead (replicateNative count 'x')))
```

The branch returns `Just 'x'`. [Value-returning example](Showcase.hs) and
[compiled laws](examples/open-evaluation/BookWave3Proposal378.hs).

### Pass a named measurement

The Showcase passes `Data.List.Dependent.size` to a retained callback. Its
premise states that the measurement of the input list equals `size xs`.
Passing `size` makes both sides equal, so `Refl` supplies the premise. The
provider uses `lengthReverse` and returns the measured size.

```haskell
case measureReverseLength DepBase.size xs Refl of
    (n, Refl) -> n
```

The Showcase reads the list at runtime, counts two cells, and leaves the
elements unevaluated. [Runnable definition](Showcase.hs) and
[checked callback rules](roadmap/R03-dependent-products.md#pass-a-named-function-with-checked-equations).

## Run `dependent-base` and the Showcase

From the repository root, verify `dependent-base` (`DEPENDENT_BASE_DIR`
defaults to `/home/tritlo/Code/dependent-base`) at `-O0` and `-O2`, including
separate `Haskell2010` `.hi`-only interoperability:

```sh
bash docs/dependent-haskell/check-dependent-base.sh _build/stage1/bin/ghc _build/stage1/lib
```

Run the 642-observation Showcase at `-O0`, `--default-o2`, or `--ghci`:

```sh
bash docs/dependent-haskell/check-showcase.sh _build/stage1/bin/ghc _build/stage1/lib
```

[Showcase source](Showcase.hs) and [expected output](Showcase.stdout).

### Try a retained function in GHCi

Run `_build/stage1/bin/ghc --interactive` from the repository root.

```text
:set -fbyte-code -O0 -fomit-interface-pragmas -fno-write-if-simplified-core
:load *docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs
:seti -XDependentTypes
let fill :: foreach (n :: Nat) -> Vec Bool (1 + n); fill n = replicateNative (1 + n) True
toList (fill 2)
[True,True,True]
```

## Where to look

| Question | Entry point |
| --- | --- |
| What runs on the fork? | [Verified checkpoints and roadmap closure](ROADMAP.md#where-this-fork-stands) |
| How far along is the whole program? | [All 50 Serokell cards and their dependencies](ROADMAP.md#upstream-checklist-and-complete-dependency-index) |
| How does the fork align with Proposal `#378`? | [Proposal `#378` conformance record](PROPOSAL-378.md) |
| What can I write in Haskell? | [Extension guide](../users_guide/exts/experimental_dependent_types.rst), [`dependent-base`](https://github.com/TharkunAB/dependent-base), [indexed vectors](examples/packed-vector/README.md), [higher-order functions](examples/qualified-pi/README.md) |
| How does the compiler represent a retained value? | [Core contract](CORE.md) and [Eisenberg branch port](PORT.md) |
| Which designs and papers did we use? | [Reference ledger](REFERENCES.md) and [paper notes](papers/README.md) |
| How was an older result checked? | [Implementation log](PLAN.md) and [historical verification records](CHECKPOINTS.md) |

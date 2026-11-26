# Dependent Haskell in GHC

GHC supports Dependent Haskell (`GHC Proposal #378`) under `-XDependentTypes`.
A function can name a runtime argument in its result type, and GHC tracks that
argument through a native typed Core representation to machine code. An
ordinary `Haskell2010` or `GHC2024` module can import and call a dependent
function without enabling `-XDependentTypes`.

The vector example exports `replicateNative`, and its client defines `safeHead`
by matching the first cell:

```haskell
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
safeHead        :: Vec a (1 + n) -> a
safeHead (VCons x _) = x
```

A caller can write `safeHead (replicateNative (1 + n) x)` even when `n` comes
from `IO`. GHC verifies that the vector has at least one cell. Calling
`safeHead` on a vector of length zero fails at compile time. For a list of
unknown runtime length, `fromListExact 1 xs` checks the spine dynamically and
returns `Maybe (Vec a 1)`. See the
[vector library](examples/packed-vector/PackedVecLibrary.hs) and the executable
[Showcase](Showcase.hs).

Prefix GADT constructors can also interleave fields, constraints, and `forall`
or `foreach` quantifiers (`GHC Proposal #402`). For example,
`ChoosePayload :: Bool -> forall a -> PayloadKey a` takes a `Bool` field before
a required type argument.

## Companion packages

Three external packages build on `-XDependentTypes`:

- [`dependent-base`](https://github.com/TharkunAB/dependent-base): standard
  library modeled on `linear-base`. It reuses `base` runtime datatypes (`Bool`,
  `Maybe`, `Either`, `(,)`, `[]`) without wrapper allocation and provides
  `Prelude.Dependent`, `Nat`, `Fin`, `Vec`, `All`, `Any`, `Elem`, `Sigma`,
  `Exists`, propositional equality (`cong`, `subst`, `rewrite`, `Inspect`),
  constructive decidability (`Dec`, `DecEq`), and proof modules
  (`Data.{Nat,List,Vec}.Dependent.Properties`).
- [`dependent-obs-eq`](https://github.com/TharkunAB/dependent-obs-eq):
  typechecker plugin (`-fplugin=Dependent.Plugin.ObsEq`) and combinator library
  (`Dependent.ObsEq`) for QuickCheck-style bounded Kleene observational
  equivalence (`obsEq`) and counterexample shrinking over uninterpreted open
  term calls.
- [`dependent-proof-search`](https://github.com/TharkunAB/dependent-proof-search):
  hole-fit plugin (`-fplugin=Dependent.Plugin.ProofSearch`) for bounded
  backward-chaining proof synthesis and congruence/transitivity subgoal
  refinements on typed holes.

### List length laws in `dependent-base`

| Lemma (`Data.List.Dependent.Properties`) | Length of the result |
| --- | --- |
| `lengthMap f xs` | `size xs` |
| `lengthReverse xs` | `size xs` |
| `lengthTakeSize n xs` | `minSize n (size xs)` for `takeSize n xs` |
| `lengthAppend xs ys` | `plus (size xs) (size ys)` |
| `lengthZip xs ys` | `minSize (size xs) (size ys)` |
| `lengthZipWith f xs ys` | `minSize (size xs) (size ys)` |
| `lengthZipMap pre combine xs ys` | `minSize (size xs) (size ys)` for `zipWith combine (map pre xs) ys` |

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

These proofs compile at `-O0` and `-O2`; GHC checks rewrite-rule interactions
against each function's published source equations. See the
[zipper proofs](examples/open-evaluation/PreludeZipRuntime.hs) and
[map pipeline](examples/open-evaluation/PreludeZipMapRuntime.hs).

### Compose equality proofs

Use `trans` (or `(===)` / `stepEq`) from `Data.Type.Equality.Dependent` to
chain equalities, and use `cong` (`forall (f :: a %m -> b) ->`) to apply any
matchable or unmatchable function, constructor, composition, or operator
section to both sides of an equality:

```haskell
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
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

`rewrite proof body` brings a propositional equality `lhs :~: rhs` into scope
as a nominal Given constraint when checking `body`:

```haskell
Just proof -> rewrite proof (Just (safeHead (replicateNative count 'x')))
```

See the [Showcase](Showcase.hs) and
[Proposal #378 examples](examples/open-evaluation/BookProposal378.hs).

## Run `dependent-base` and the Showcase

Set `DEPENDENT_BASE_DIR` to a local checkout of `dependent-base` (by default,
the scripts look for `../dependent-base` next to the GHC repository root).

Verify `dependent-base` at `-O0` and `-O2`, including `.hi`-only `Haskell2010`
interoperability:

```sh
bash docs/dependent-haskell/check-dependent-base.sh _build/stage1/bin/ghc _build/stage1/lib
```

Run the 642-observation Showcase at `-O0`, `--default-o2`, or `--ghci`:

```sh
bash docs/dependent-haskell/check-showcase.sh _build/stage1/bin/ghc _build/stage1/lib
```

### Try a retained function in GHCi

Run `_build/stage1/bin/ghc --interactive` from the repository root:

```text
:load *docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs
:seti -XDependentTypes
let fill :: foreach (n :: Nat) -> Vec Bool (1 + n); fill n = replicateNative (1 + n) True
toList (fill 2)
[True,True,True]
```

## Documentation index

| Topic | Document |
| --- | --- |
| Surface language and flags | [GHC User's Guide (`-XDependentTypes`)](../users_guide/exts/dependent_types.rst) |
| Proposal conformance | [Proposal `#378` and companion proposals](PROPOSAL-378.md) |
| Typed Core and `.hi` invariants | [Dependent Core specification](CORE.md) |
| End-to-end runnable examples | [Showcase.hs](Showcase.hs), [Showcase.stdout](Showcase.stdout), [indexed vectors](examples/packed-vector/README.md), [qualified retained functions](examples/qualified-pi/README.md) |

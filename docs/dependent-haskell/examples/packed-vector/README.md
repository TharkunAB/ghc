# Indexed Vector Examples (`packed-vector`)

`PackedVecLibrary.hs` defines Peano naturals (`Nat`), length-indexed vectors
(`Vec a n`), and retained vector packages (`PackedVec a`) using
`-XDependentTypes`.

## Core definitions

```haskell
data PackedVec a where
  PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a
```

- `PackVec` stores the runtime `Nat` length and `Vec a n` payload in two
  ordinary lazy heap fields without a singleton witness.
- `replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n` builds a
  vector of runtime length `n`.
- `fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n)` checks a list
  against a requested runtime length `n` without forcing list elements.
- `packList :: [a] -> PackedVec a` packages `sourceLength xs` alongside
  `sourceVector xs`.
- `filterVec :: (a -> Bool) -> Vec a n -> PackedVec a` filters a vector and
  packs the dynamically computed result length with the surviving elements.
- `appendNative`, `reverseNative`, `sameNat`, and `zipVec` demonstrate checked
  addition equations, length-preserving reversal, runtime length equality
  decision, and aligned zipping.

## Ordinary Haskell interoperability

`OrdinaryVecClient.hs` compiles without `-XDependentTypes` and calls the
exported vector APIs against `.hi` interfaces alone, verifying that `Haskell2010`
and `GHC2024` callers can construct, inspect, and align runtime-indexed vectors.

## Running the example

Run the standalone vector suite or the full Showcase from the repository root:

```sh
mkdir -p _build/packed-vec
_build/stage1/bin/ghc --make -O0 -dcore-lint -dstg-lint \
  -idocs/dependent-haskell/examples/packed-vector \
  -outputdir _build/packed-vec \
  docs/dependent-haskell/examples/packed-vector/Main.hs \
  -o _build/packed-vec/packed-vec
_build/packed-vec/packed-vec
```

# Qualified and Higher-Order Retained Functions (`qualified-pi`)

The modules in this directory test qualified retained function types,
function-valued `foreach` parameters and constructor fields, closed
parameterized runtime index domains, and safe list indexing.

## Highlights

- **Qualified retained functions (`QualifiedPiLibrary.hs`):**
  ```haskell
  type QPi = foreach (n :: Nat) -> Eq Int => Vec Int n

  exposeQ :: forall value. value ~ QPi => value -> QPi
  exposeQ f = f
  ```
  Transports a retained function with an inner class constraint across nominal
  equalities while keeping dictionary passing and runtime length retention
  distinct and lazy.
- **Function-valued fields and signatures (`FunctionPackageLibrary.hs`, `FunctionSignatureLibrary.hs`):**
  Stores `f :: Nat -> Nat` and `n :: Nat` alongside `Vec a (f n)` and exports
  `idCall :: forall a. foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)`.
- **Parameterized runtime index domains (`ParameterizedIndexLibrary.hs`):**
  Uses `[Bool]`, `Maybe Nat`, `Tree Bool`, and generic `[a]` as `foreach`
  domains, including `indexGeneric`, `mkFinGeneric`, and `lookupGeneric` bounded
  by `Fin (sourceLength xs)`.
- **Runtime `Type` witnesses (`RuntimeTypeColumn.hs`):**
  Passes runtime `Type` witnesses via `foreach (a :: Type) ->`.
- **Aligned input pairing (`PairInputLines.hs`):**
  Reads two lines from standard input, checks runtime length equality, and zips
  the characters with `checkedZipWith`.

## Running the standalone suite

From the repository root:

```sh
bash docs/dependent-haskell/examples/qualified-pi/run-example.sh "$PWD"
```

Set `QUALIFIED_PI_GHC` to select a specific `ghc` binary and `QUALIFIED_PI_WORK`
to select an output directory.

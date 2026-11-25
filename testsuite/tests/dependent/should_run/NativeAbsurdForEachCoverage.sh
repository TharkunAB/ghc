#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

# 1. Positive suite at -O0 and -O2 (both codegen and -fno-code -fwrite-interface)
#    with -Wall -Werror: zero stdout and zero stderr.
for opt in -O0 -O2; do
  mkdir -p "$out/$opt"
  cp "$src/NativeAbsurdForEachCoverage.hs" "$out/$opt/"
  (
    cd "$out/$opt"
    sed 's/module Main (main) where/module NativeAbsurdForEachCoverage (main) where/' \
      NativeAbsurdForEachCoverage.hs > NativeAbsurdNoCode.hs
    if ! "${ghc[@]}" "$opt" -Wall -Werror -fno-code -fwrite-interface -c NativeAbsurdNoCode.hs >nocode.log 2>&1 || [[ -s nocode.log ]]; then
      cat nocode.log >&2
      exit 1
    fi
    if ! "${ghc[@]}" "$opt" -Wall -Werror NativeAbsurdForEachCoverage.hs -o run >build.log 2>&1 || [[ -s build.log ]]; then
      cat build.log >&2
      exit 1
    fi
    if ! ./run >run.log 2>&1 || [[ -s run.log ]]; then
      cat run.log >&2
      exit 1
    fi
  )
done

# 2. Negative controls: genuinely reachable omitted clauses and uninspected
#    lazy witnesses MUST be rejected with GHC-83605 even with -Wno-incomplete-patterns.
mkdir -p "$out/neg"
cd "$out/neg"

cat > ZipBad.hs <<'HS'
{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ZipBad where
import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat
data Vec a (n :: Nat) where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

zipBad :: foreach (m n :: Nat) -> Vec a m -> Vec b n -> (m :~: n) -> Vec (a, b) m
zipBad Zero Zero VNil VNil Refl = VNil
HS
if "${ghc[@]}" -O0 -c ZipBad.hs >zipbad.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' zipbad.log; then
  cat zipbad.log >&2
  exit 1
fi

cat > TriBad.hs <<'HS'
{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TriBad where
import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat
data Vec a (n :: Nat) where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

triBad :: foreach (m n p :: Nat) -> Vec a m -> Vec b n -> Vec c p
       -> (m :~: n) -> (n :~: p) -> Vec (a, b, c) m
triBad Zero Zero Zero VNil VNil VNil Refl Refl = VNil
HS
if "${ghc[@]}" -O0 -c TriBad.hs >tribad.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' tribad.log; then
  cat tribad.log >&2
  exit 1
fi

cat > LazyWitnessBad.hs <<'HS'
{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LazyWitnessBad where
import Data.Type.Equality ((:~:))

data Nat = Zero | Succ Nat

lazyWitnessBad :: foreach (m n :: Nat) -> (m :~: n) -> Int
lazyWitnessBad Zero     Zero     _ = 0
lazyWitnessBad (Succ _) (Succ _) _ = 1
HS
if "${ghc[@]}" -O0 -c LazyWitnessBad.hs >lazybad.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' lazybad.log; then
  cat lazybad.log >&2
  exit 1
fi

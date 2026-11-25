#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client,nocode}
    cd "$out/$opt/provider"
    cp "$src/NativeAutoNumLiteralsProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeAutoNumLiteralsProvider.hs
    cp ./*.hi "$src/NativeAutoNumLiterals.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c -fno-code -fwrite-interface NativeAutoNumLiterals.hs
    "${ghc[@]}" "-$opt" -c NativeAutoNumLiterals.hs
    "${ghc[@]}" "-$opt" NativeAutoNumLiterals.o ../provider/NativeAutoNumLiteralsProvider.o -o run
    ./run

    cd ../nocode
    cp "$src/NativeAutoNumLiteralsProvider.hs" "$src/NativeAutoNumLiterals.hs" .
    sed -i 's/module Main (main) where/module NativeAutoNumLiterals (main) where/' NativeAutoNumLiterals.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -c NativeAutoNumLiteralsProvider.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -c NativeAutoNumLiterals.hs
done

# Explicit FromNatural application also reduces for Peano.
mkdir -p "$out/explicit_fam"
cd "$out/explicit_fam"
cp "$out/O0/provider/NativeAutoNumLiteralsProvider.hi" .
cat >ExplicitFam.hs <<'HS'
{-# LANGUAGE DataKinds, DependentTypes #-}
module ExplicitFam where
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeNats.Experimental (FromNatural)
import NativeAutoNumLiteralsProvider (Peano(..))

explicitFour :: FromNatural Peano 4 :~: 'PSucc ('PSucc ('PSucc ('PSucc 'PZero)))
explicitFour = Refl
HS
"${ghc[@]}" -O0 -c ExplicitFam.hs

# Ordinary MTrans kind-arrow synonym: works both as a kind and as a function type.
mkdir -p "$out/mtrans"/{provider,client}
cd "$out/mtrans/provider"
cat >LegacyMTransProvider.hs <<'HS'
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures, PolyKinds #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module LegacyMTransProvider (Endo, MTrans, Tag(..), Lifting(..), idEndo) where

import Data.Kind (Type)

type Endo = Type -> Type
type MTrans = (Type -> Type) -> (Type -> Type)

data Tag :: Endo where
  MkTag :: Tag Int

class Lifting (t :: MTrans) where
  liftUnit :: Monad m => m a -> t m a

idEndo :: Endo
idEndo x = x
HS
"${ghc[@]}" -O2 -c LegacyMTransProvider.hs
cp LegacyMTransProvider.hi ../client/
cd ../client
cat >LegacyMTransClient.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module LegacyMTransClient (UseTag(..), useIdEndo) where

import LegacyMTransProvider (Endo, MTrans, idEndo)

newtype UseTag :: MTrans where
  MkUseTag :: m a -> UseTag m a

useIdEndo :: Endo
useIdEndo = idEndo
HS
"${ghc[@]}" -O2 -c LegacyMTransClient.hs

# Negative controls.
mkdir -p "$out/neg"
cd "$out/neg"

# 1. Uncertified Num instance must NOT promote numerals at the type level.
cat >UncertifiedNum.hs <<'HS'
{-# LANGUAGE DataKinds, DependentTypes #-}
module UncertifiedNum where
import Data.Type.Equality ((:~:)(Refl))
data Bad = BZero | BSucc Bad
instance Num Bad where
  (+) _ _ = BZero
  (*) _ _ = BZero
  (-) _ _ = BZero
  negate _ = BZero
  abs _ = BZero
  signum _ = BZero
  fromInteger _ = BZero

bad :: (3 :: Bad) :~: 'BZero
bad = Refl
HS
if "${ghc[@]}" -O0 -c UncertifiedNum.hs >uncert.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' uncert.log; then
    cat uncert.log >&2
    exit 1
fi

# 2. Without DependentTypes / ExperimentalDependentTypes on client, (3 :: Peano) fails.
cp "$out/O0/provider/NativeAutoNumLiteralsProvider.hi" .
cat >OptInOff.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators, NoExperimentalDependentTypes #-}
module OptInOff where
import Data.Type.Equality ((:~:)(Refl))
import NativeAutoNumLiteralsProvider (Peano(..))

bad :: (0 :: Peano) :~: 'PZero
bad = Refl
HS
if "${ghc[@]}" -O0 -c OptInOff.hs >optin.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' optin.log; then
    cat optin.log >&2
    exit 1
fi

# 3. With UnsaturatedTypeFamilies enabled, bare synonym RHS arrows stay fixed to U (#242).
cat >UnsatFixedU.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, UnsaturatedTypeFamilies #-}
module UnsatFixedU where
import Data.Kind (Type)
type UEndo = Type -> Type
data BadTag :: UEndo where
  MkBadTag :: BadTag Int
HS
if "${ghc[@]}" -O0 -c UnsatFixedU.hs >unsat.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' unsat.log; then
    cat unsat.log >&2
    exit 1
fi

#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeLinearForEachAndExtendedLet.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

expected='(35,(True,False,True,True),2,(Succ (Succ Zero),Succ Zero,(Succ (Succ (Succ Zero)),Succ (Succ (Succ Zero))),Succ Zero,(Zero,Succ Zero),Succ (Succ Zero),(Zero,99)),(37,42,15))'

for opt in -O0 -O2; do
    dir="$work/$opt"
    mkdir -p "$dir"/{native,readback}

    # 1. Compile Provider + Client at -O0 and -O2 with Core/STG Lint
    cd "$dir/native"
    cp "$inputs/NativeLinearForEachAndExtendedLetProvider.hs" \
       "$inputs/NativeLinearForEachAndExtendedLet.hs" .
    "${ghc[@]}" "$opt" -c NativeLinearForEachAndExtendedLetProvider.hs
    "${ghc[@]}" "$opt" -c NativeLinearForEachAndExtendedLet.hs
    "${ghc[@]}" "$opt" NativeLinearForEachAndExtendedLetProvider.o \
        NativeLinearForEachAndExtendedLet.o -o run
    actual=$(./run)
    [[ "$actual" == "$expected" ]]

    # 2. Separate .hi readback check (Provider.hs absent)
    cd "$dir/readback"
    cp "$dir/native/NativeLinearForEachAndExtendedLetProvider.hi" \
       "$inputs/NativeLinearForEachAndExtendedLet.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeLinearForEachAndExtendedLet.hs
done

# 3. GHCi interactive check
mkdir -p "$work/ghci"
cd "$work/ghci"
cp "$inputs/NativeLinearForEachAndExtendedLetProvider.hs" \
   "$inputs/NativeLinearForEachAndExtendedLet.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci NativeLinearForEachAndExtendedLet.hs -e 'main' >ghci.out
diff -u <(printf '%s\n' "$expected") ghci.out

# 4. Standalone -XExtendedLet (without -XDependentTypes)
mkdir -p "$work/standalone"
cd "$work/standalone"
cat >StandaloneExtendedLet.hs <<'HS'
{-# LANGUAGE ExtendedLet, ExplicitForAll, KindSignatures #-}
module StandaloneExtendedLet where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))

polySyn :: forall (k :: Type) (a :: k). Proxy a -> (Proxy a, Proxy a)
polySyn p = res
  where
    type ProxyPair (x :: k) = (Proxy x, Proxy x)
    res :: ProxyPair a
    res = (p, p)
HS
"${ghc[@]}" -O0 -fno-code -c StandaloneExtendedLet.hs

# 5. Negative linearity & ExtendedLet & canonical -XDependentTypes diagnostic controls
mkdir -p "$work/neg"
cd "$work/neg"
cp "$work/-O0/native/NativeLinearForEachAndExtendedLetProvider.hi" .

expect_reject() {
    local name=$1
    local pattern=$2
    if "${ghc[@]}" -O0 -fno-code -c "$name.hs" >"$name.log" 2>&1; then
        echo "Expected $name.hs to fail, but it succeeded" >&2
        exit 1
    fi
    if ! grep -Fq "$pattern" "$name.log"; then
        echo "Expected '$pattern' in $name.log:" >&2
        cat "$name.log" >&2
        exit 1
    fi
}

# 5a. Discarding a %1 foreach binder at runtime (even when used in types!) must fail linearity check:
cat >DiscardLinear.hs <<'HS'
{-# LANGUAGE DependentTypes, LinearTypes #-}
module DiscardLinear where
import Data.Proxy (Proxy (..))
import GHC.Tuple (Unit)
import NativeLinearForEachAndExtendedLetProvider (Nat)

discardLinear :: foreach (x %1 :: Nat) -> Proxy x -> Unit
discardLinear x Proxy = ()
HS
expect_reject DiscardLinear '[GHC-18872]'

# 5b. Duplicating a %1 foreach binder at runtime must fail linearity check:
cat >DupLinear.hs <<'HS'
{-# LANGUAGE DependentTypes, LinearTypes #-}
module DupLinear where
import Data.Proxy (Proxy (..))
import GHC.Tuple (Tuple2)
import NativeLinearForEachAndExtendedLetProvider (Nat)

dupLinear :: foreach (x %1 :: Nat) -> Proxy x -> Tuple2 Nat Nat
dupLinear x Proxy = (x, x)
HS
expect_reject DupLinear '[GHC-18872]'

# 5c. Duplicating a linear argument across a %1 foreach call site must fail linearity check:
cat >CallerDupLinear.hs <<'HS'
{-# LANGUAGE DependentTypes, LinearTypes #-}
module CallerDupLinear where
import Data.Proxy (Proxy (..))
import GHC.Tuple (Tuple2)
import NativeLinearForEachAndExtendedLetProvider (Nat, dupInTypeLinearInTerm)

callerDup :: Nat %1 -> Tuple2 Nat Nat
callerDup n = (dupInTypeLinearInTerm n Proxy, n)
HS
expect_reject CallerDupLinear '[GHC-18872]'

# 5d. Self-recursive and mutually recursive local type synonyms must be rejected:
cat >SelfRecSyn.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module SelfRecSyn where
bad :: Int
bad = let type Loop a = Maybe (Loop a) in 0
HS
expect_reject SelfRecSyn '[GHC-97522]'

cat >MutualRecSyn.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module MutualRecSyn where
bad :: Int
bad = let { type A = B; type B = A } in 0
HS
expect_reject MutualRecSyn '[GHC-97522]'

# 5e. Unused ill-kinded local type synonym must be rejected:
cat >UnusedIllKindedSyn.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module UnusedIllKindedSyn where
bad :: Int
bad = let type Wrong = Maybe Int Bool in 42
HS
expect_reject UnusedIllKindedSyn '[GHC-83865]'

# 5f. Undersaturated local type synonym must be rejected:
cat >UndersaturatedSyn.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module UndersaturatedSyn where
import GHC.Tuple (Tuple2)
bad :: Int
bad = let { type PairOf a = Tuple2 a a; x :: PairOf; x = 0 } in x
HS
expect_reject UndersaturatedSyn 'should have 1 argument'

# 5g. Canonical -XDependentTypes / -XExtendedLet / -XExistentialTypes diagnostics (never ExperimentalDependentTypes):
cat >LocalSynDisabled.hs <<'HS'
{-# LANGUAGE NoDependentTypes #-}
module LocalSynDisabled where
bad :: Int
bad = let type T = Int in (1 :: T)
HS
expect_reject LocalSynDisabled 'requires -XDependentTypes or -XExtendedLet.'
if grep -Fq 'ExperimentalDependentTypes' LocalSynDisabled.log; then
    cat LocalSynDisabled.log >&2
    exit 1
fi

cat >TypeLamDisabled.hs <<'HS'
{-# LANGUAGE NoDependentTypes #-}
module TypeLamDisabled where
type F = \a -> a
HS
expect_reject TypeLamDisabled 'Type lambdas require -XDependentTypes.'
if grep -Fq 'ExperimentalDependentTypes' TypeLamDisabled.log; then
    cat TypeLamDisabled.log >&2
    exit 1
fi

cat >ExistsPackDisabled.hs <<'HS'
{-# LANGUAGE NoDependentTypes #-}
module ExistsPackDisabled where
import GHC.Exists
x = Pack
HS
expect_reject ExistsPackDisabled 'GHC.Exists.Pack requires -XDependentTypes or -XExistentialTypes.'
if grep -Fq 'ExperimentalDependentTypes' ExistsPackDisabled.log; then
    cat ExistsPackDisabled.log >&2
    exit 1
fi

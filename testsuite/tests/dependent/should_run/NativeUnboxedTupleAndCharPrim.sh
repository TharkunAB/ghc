#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)

refuse() {
    local code=$1
    shift
    if "$@" >"$out/diag" 2>&1; then
        echo "Expected GHC-$code for $*" >&2
        exit 1
    fi
    grep -Fq "[GHC-$code]" "$out/diag" || { cat "$out/diag" >&2; exit 1; }
}

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,client}
    cp "$src/NativeUnboxedTupleAndCharPrim.hs" "$out/$opt/provider/"
    (
        cd "$out/$opt/provider"
        "${ghc[@]}" "$opt" -c NativeUnboxedTupleAndCharPrim.hs
        cp NativeUnboxedTupleAndCharPrim.hi ../client/
    )
    (
        cd "$out/$opt/client"
        cat >Main.hs <<'EOF'
{-# LANGUAGE DataKinds, MagicHash, NoDependentTypes, NoExperimentalDependentTypes, TypeOperators, UnboxedTuples #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import NativeUnboxedTupleAndCharPrim

clientEq :: EqCharLit 'a' :~: 'True
clientEq = Refl

clientFst :: FstU PairSyn :~: 'True
clientFst = Refl

clientSwap :: FstU (SwapU PairSyn) :~: 'x'
clientSwap = Refl

main :: IO ()
main =
  case ( charLitRefl
       , eqCharHit
       , eqCharMiss
       , eqCharOther
       , matchCharVarProof
       , matchCharWildProof
       , matchCharAnnotProof
       , fstUProof
       , sndUProof
       , swapUFstProof
       , swapUSndProof
       , clientEq
       , clientFst
       , clientSwap
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> pure ()
EOF
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/NativeUnboxedTupleAndCharPrim.o -o run
        ./run >stdout 2>stderr
        test ! -s stdout
        test ! -s stderr
    )
done

mkdir -p "$out/nopuns"
(
    cd "$out/nopuns"
    cat >NoPunsCheck.hs <<'EOF'
{-# LANGUAGE DependentTypes, MagicHash, TypeFamilies, UnboxedTuples #-}
module NoPunsCheck (FstU, SndU, p1, p2) where
import Data.Type.Equality ((:~:)(Refl))

type FstU :: (# a, b #) -> a
type family FstU p where
  FstU (# x, _ #) = x

type SndU :: (# a, b #) -> b
type family SndU p where
  SndU ((# , #) _ y) = y

p1 :: FstU (# True, 'x' #) :~: True
p1 = Refl

p2 :: SndU (# True, 'x' #) :~: 'x'
p2 = Refl
EOF
    "${ghc[@]}" -O0 -c NoPunsCheck.hs
)

mkdir -p "$out/neg"
(
    cd "$out/neg"
    cat >NoDepCharPrim.hs <<'EOF'
{-# LANGUAGE DataKinds, MagicHash, NoDependentTypes, NoExperimentalDependentTypes #-}
module NoDepCharPrim where
import Data.Proxy (Proxy)
import GHC.Exts (Char(C#))
type Bad = Proxy ('C# 'a'#)
EOF
    refuse 51819 "${ghc[@]}" -fno-code NoDepCharPrim.hs

    cat >EscapeCharPrim.hs <<'EOF'
{-# LANGUAGE DependentTypes, MagicHash, TypeFamilies #-}
module EscapeCharPrim where
import GHC.Exts (Char(C#), Char#)
type Escape :: Char -> Char#
type family Escape c where
  Escape ('C# x) = x
EOF
    if "${ghc[@]}" -fno-code EscapeCharPrim.hs >"$out/diag" 2>&1; then
        echo "Expected escaping Char# pattern binder to fail" >&2
        exit 1
    fi
    if grep -Fq "TypeFamilies" "$out/diag"; then
        cat "$out/diag" >&2
        exit 1
    fi

    cat >NoUnboxedTuples.hs <<'EOF'
{-# LANGUAGE DependentTypes, ListTuplePuns, UnboxedSums, NoUnboxedTuples #-}
module NoUnboxedTuples where
type P = '(# , #) 'True 'x'
EOF
    refuse 19590 "${ghc[@]}" -fno-code NoUnboxedTuples.hs
)

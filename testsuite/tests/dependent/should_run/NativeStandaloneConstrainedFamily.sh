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
    cp "$src/NativeStandaloneConstrainedFamily.hs" "$out/$opt/provider/"
    (
        cd "$out/$opt/provider"
        "${ghc[@]}" "$opt" -c NativeStandaloneConstrainedFamily.hs
        cp NativeStandaloneConstrainedFamily.hi ../client/
    )
    (
        cd "$out/$opt/client"
        cat >Main.hs <<'EOF'
{-# LANGUAGE NoDependentTypes, NoExperimentalDependentTypes, TypeOperators #-}
module Main (main) where

import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import NativeStandaloneConstrainedFamily

clientElem :: ElemTop [Bool] :~: Bool
clientElem = Refl

clientDelegate :: DelegateTop [Bool] :~: [Bool]
clientDelegate = Refl

main :: IO ()
main =
  case ( elemBoolProof
       , delegateBoolProof
       , forwardBoolProof
       , memberBoolProof
       , clientElem
       , clientDelegate
       , polyElem (Proxy :: Proxy [Bool]) True [True]
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, ()) -> pure ()
EOF
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/NativeStandaloneConstrainedFamily.o -o run
        ./run >stdout 2>stderr
        test ! -s stdout
        test ! -s stderr

        cat >BadClient.hs <<'EOF'
{-# LANGUAGE NoDependentTypes, NoExperimentalDependentTypes #-}
module BadClient where
import NativeStandaloneConstrainedFamily
bad :: ElemTop [Int] -> ()
bad _ = ()
EOF
        refuse 39999 "${ghc[@]}" "$opt" -c -fno-code BadClient.hs
        grep -Fq "Allowed Int" "$out/diag" || { cat "$out/diag" >&2; exit 1; }

        refuse 39999 "${ghc[@]}" "$opt" -fdefer-type-errors -c -fno-code BadClient.hs
        grep -Fq "Allowed Int" "$out/diag" || { cat "$out/diag" >&2; exit 1; }
    )
done

mkdir -p "$out/neg"
cp "$out/-O0/provider/NativeStandaloneConstrainedFamily.hi" "$out/neg/"
(
    cd "$out/neg"
    cat >BadEquation.hs <<'EOF'
{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module BadEquation where
import Data.Kind (Type)
import NativeStandaloneConstrainedFamily (Base, ElemTop)
type MissingPremise :: Base c => c -> Type
type family MissingPremise c where
  MissingPremise c = ElemTop c
EOF
    refuse 90647 "${ghc[@]}" -c -fno-code BadEquation.hs

    cat >DisabledStandaloneConstraint.hs <<'EOF'
{-# LANGUAGE NoDependentTypes, NoExperimentalDependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module DisabledStandaloneConstraint where
import Data.Kind (Type)
class C a
type F :: C a => a -> Type
type family F a
EOF
    refuse 01259 "${ghc[@]}" -c -fno-code DisabledStandaloneConstraint.hs
)

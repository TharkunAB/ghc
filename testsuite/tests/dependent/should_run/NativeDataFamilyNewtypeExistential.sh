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
    cp "$src/NativeDataFamilyNewtypeExistential.hs" "$out/$opt/provider/"
    (
        cd "$out/$opt/provider"
        "${ghc[@]}" "$opt" -c NativeDataFamilyNewtypeExistential.hs
        cp NativeDataFamilyNewtypeExistential.hi ../client/
    )
    (
        cd "$out/$opt/client"
        cat >Main.hs <<'EOF'
{-# LANGUAGE DataKinds, NoDependentTypes, NoExperimentalDependentTypes, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import NativeDataFamilyNewtypeExistential

main :: IO ()
main =
  case (unwrapProof, boxKindProof, boxPayloadProof, retainedWrapProof) of
    (Refl, Refl, Refl, Refl) -> pure ()
EOF
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/NativeDataFamilyNewtypeExistential.o -o run
        ./run >stdout 2>stderr
        test ! -s stdout
        test ! -s stderr
    )
done

mkdir -p "$out/neg"
(
    cd "$out/neg"
    cat >ConstrainedFamCon.hs <<'EOF'
{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module ConstrainedFamCon where
import Data.Proxy (Proxy)
class Pick a
instance Pick Bool
data family Stored a
data instance Stored a where
  MkStored :: Pick a => a -> Stored a
type Bad = Proxy ('MkStored 'True)
EOF
    refuse 64578 "${ghc[@]}" -fno-code ConstrainedFamCon.hs

    cat >DisabledNewtypePromotion.hs <<'EOF'
{-# LANGUAGE DataKinds, ExistentialQuantification, NoDependentTypes, NoExperimentalDependentTypes, TypeFamilies #-}
module DisabledNewtypePromotion where
import Data.Proxy (Proxy)
data family Wrap a
newtype instance Wrap [a] = MkWrap a
type Bad = Proxy ('MkWrap 'True)
EOF
    refuse 64578 "${ghc[@]}" -fno-code DisabledNewtypePromotion.hs
)

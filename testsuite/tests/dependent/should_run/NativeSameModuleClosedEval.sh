#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dynamic -dcore-lint -dstg-lint
     -fdiagnostics-color=never)

refuse() {
    local code=$1
    shift
    if "$@" >"$out/diag" 2>&1; then
        echo "Expected GHC-$code for $*" >&2
        exit 1
    fi
    grep -Fq "[GHC-$code]" "$out/diag" || { cat "$out/diag" >&2; exit 1; }
}

# 1. Compile and run at -O0 and -O2 (-dynamic, 0 stdout/stderr)
for opt in -O0 -O2; do
    mkdir -p "$out/run$opt"
    cp "$src/NativeSameModuleClosedEval.hs" "$out/run$opt/"
    (
        cd "$out/run$opt"
        "${ghc[@]}" "$opt" NativeSameModuleClosedEval.hs -o prog
        ./prog >stdout 2>stderr
        test ! -s stdout
        test ! -s stderr
    )
done

# 2. Verify -fno-code works with zero .o files on disk
mkdir -p "$out/nocode"
cp "$src/NativeSameModuleClosedEval.hs" "$out/nocode/"
(
    cd "$out/nocode"
    "${ghc[@]}" -fno-code NativeSameModuleClosedEval.hs
    test ! -e NativeSameModuleClosedEval.o
    test ! -e NativeSameModuleClosedEval.dyn_o
)

# 3. Verify stale .dyn_o / .o on disk is never loaded for same-module closed eval
mkdir -p "$out/stale"
(
    cd "$out/stale"
    cat >StaleMod.hs <<'EOF'
{-# LANGUAGE DependentTypes #-}
module StaleMod where
import Numeric.Natural (Natural)
bump :: Natural -> Natural
bump n = fromIntegral (fromEnum n + 1)
EOF
    "${ghc[@]}" -c StaleMod.hs
    cat >StaleMod.hs <<'EOF'
{-# LANGUAGE DependentTypes #-}
module StaleMod where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
bump :: Natural -> Natural
bump n = fromIntegral (fromEnum n + 2)
proof :: bump 5 :~: 7
proof = Refl
EOF
    "${ghc[@]}" -fno-code StaleMod.hs
)

# 4. Clean diagnostics on exception and ill-typed closed calls
mkdir -p "$out/neg"
(
    cd "$out/neg"
    cat >Boom.hs <<'EOF'
{-# LANGUAGE DependentTypes #-}
module Boom where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
explode :: Natural -> Natural
explode _ = error "same-module boom"
bad :: explode 0 :~: 0
bad = Refl
EOF
    refuse 70565 "${ghc[@]}" -fno-code Boom.hs
    grep -Fq "same-module boom" "$out/diag" || { cat "$out/diag" >&2; exit 1; }

    cat >IllTyped.hs <<'EOF'
{-# LANGUAGE DependentTypes #-}
module IllTyped where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
incNat :: Natural -> Natural
incNat n = fromIntegral (fromEnum n + 1)
bad :: incNat 'True :~: 1
bad = Refl
EOF
    refuse 70565 "${ghc[@]}" -fno-code IllTyped.hs
)

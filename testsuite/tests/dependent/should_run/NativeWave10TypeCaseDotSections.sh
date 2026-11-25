#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/NativeWave10TypeCaseDotSectionsProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeWave10TypeCaseDotSectionsProvider.hs
    cp ./*.hi "$src/NativeWave10TypeCaseDotSections.hs" ../client/

    cd ../client
    test ! -e NativeWave10TypeCaseDotSectionsProvider.hs
    "${ghc[@]}" "-$opt" -c NativeWave10TypeCaseDotSections.hs
    "${ghc[@]}" "-$opt" NativeWave10TypeCaseDotSections.o ../provider/NativeWave10TypeCaseDotSectionsProvider.o -o run
    ./run
done

mkdir -p "$out/ghci"
cp "$src/NativeWave10TypeCaseDotSectionsProvider.hs" "$src/NativeWave10TypeCaseDotSections.hs" "$out/ghci/"
cd "$out/ghci"
"$1" "-B$2" --interactive -ignore-dot-ghci -v0 -package-env - -i -i. NativeWave10TypeCaseDotSections.hs <<'GHCI'
:main
:quit
GHCI

mkdir -p "$out/neg"
cp "$out/O0/provider/"*.hi "$out/neg/"
cd "$out/neg"

reject() {
    local pattern=$1
    local file=$2
    if "${ghc[@]}" -O0 -c "$file" >rejected.log 2>&1 ||
       ! grep -Eq "$pattern" rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
}

# 1. Runtime Type case without a catch-all must be rejected (Type is open).
cat >MissingCatchAll.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module MissingCatchAll where
import Data.Kind (Type)

bad :: foreach (a :: Type) -> Maybe a
bad a = case a of
  Int  -> Just 0
  Bool -> Just True
HS
reject 'GHC-83605' MissingCatchAll.hs

# 2. Ambiguous superclass paths to Choose a must be rejected.
cat >AmbiguousSuper.hs <<'HS'
{-# LANGUAGE DependentTypes, GADTs #-}
module AmbiguousSuper where
import NativeWave10TypeCaseDotSectionsProvider

bad :: (SubChoose a, AltSubChoose a) => a -> Bool
bad x = case chooseTagged x of
  Tagged v -> v
HS
reject 'GHC-83605' AmbiguousSuper.hs

# 3. Missing superclass evidence for Choose a must be rejected.
cat >MissingSuper.hs <<'HS'
{-# LANGUAGE DependentTypes, GADTs #-}
module MissingSuper where
import NativeWave10TypeCaseDotSectionsProvider

bad :: Eq a => a -> Bool
bad x = case chooseTagged x of
  Tagged v -> v
HS
reject 'GHC-83605' MissingSuper.hs

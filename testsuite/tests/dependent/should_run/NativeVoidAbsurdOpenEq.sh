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
  cp "$src/NativeVoidAbsurdOpenEqProvider.hs" "$src/NativeVoidAbsurdOpenEq.hs" "$out/$opt/"
  (
    cd "$out/$opt"
    if ! "${ghc[@]}" "$opt" -Wall -Werror -fno-code -fwrite-interface \
         NativeVoidAbsurdOpenEqProvider.hs >nocode_prov.log 2>&1 || [[ -s nocode_prov.log ]]; then
      cat nocode_prov.log >&2
      exit 1
    fi
    rm -f NativeVoidAbsurdOpenEqProvider.hi
    if ! "${ghc[@]}" "$opt" -Wall -Werror \
         NativeVoidAbsurdOpenEqProvider.hs NativeVoidAbsurdOpenEq.hs -o run >build.log 2>&1 || [[ -s build.log ]]; then
      cat build.log >&2
      exit 1
    fi
    if ! ./run >run.log 2>&1 || [[ -s run.log ]]; then
      cat run.log >&2
      exit 1
    fi
  )
done

# 2. Negative controls:
mkdir -p "$out/neg"
cd "$out/neg"

# 2a. Missing uninhabited constructor row must NOT be certified as a source table.
cat > MissingVoidRow.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module MissingVoidRow where

import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void)

badTo :: Either a Void -> a
badTo (Left x) = x

badProof :: foreach (x :: a) -> badTo (Left x) :~: x
badProof _ = Refl
HS
if "${ghc[@]}" -O0 -c MissingVoidRow.hs >missing.log 2>&1 ||
   ! grep -Fq '[GHC-68947]' missing.log; then
  cat missing.log >&2
  exit 1
fi

# 2b. Uninhabited branch must stay stuck (never invent a bogus equation on Right v).
cat > StuckRightBranch.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module StuckRightBranch where

import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void)

okTo :: Either a Void -> a
okTo (Left x)  = x
okTo (Right v) = case v of {}

unsound :: foreach (v :: Void) -> foreach (x :: a) -> okTo (Right v) :~: x
unsound _ _ = Refl
HS
if "${ghc[@]}" -O0 -c StuckRightBranch.hs >stuck.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' stuck.log; then
  cat stuck.log >&2
  exit 1
fi

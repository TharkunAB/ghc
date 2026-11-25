#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -Werror=incomplete-patterns -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client,nocode}
    cd "$out/$opt/provider"
    cp "$src/NativeMutualLargeElimDataProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeMutualLargeElimDataProvider.hs
    cp ./*.hi "$src/NativeMutualLargeElimData.hs" ../client/
    cd ../client
    test ! -e NativeMutualLargeElimDataProvider.hs
    "${ghc[@]}" "-$opt" -c -fno-code -fwrite-interface NativeMutualLargeElimData.hs
    "${ghc[@]}" "-$opt" -c NativeMutualLargeElimData.hs
    "${ghc[@]}" "-$opt" NativeMutualLargeElimData.o ../provider/NativeMutualLargeElimDataProvider.o -o run
    test "$(./run)" = "(True,True,True,True,True)"

    cd ../nocode
    cp "$src/NativeMutualLargeElimDataProvider.hs" "$src/NativeMutualLargeElimData.hs" .
    sed -i 's/module Main where/module NativeMutualLargeElimData where/' NativeMutualLargeElimData.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -fwrite-source-equations -c NativeMutualLargeElimDataProvider.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -c NativeMutualLargeElimData.hs
done

mkdir -p "$out/ghci"
cp "$src/NativeMutualLargeElimDataProvider.hs" "$src/NativeMutualLargeElimData.hs" "$out/ghci/"
cd "$out/ghci"
ghci_out=$("$1" "-B$2" --interactive -ignore-dot-ghci -v0 -package-env - -i -i. NativeMutualLargeElimData.hs <<'GHCI'
:set -XDataKinds -XDependentTypes -XListTuplePuns
:kind! logVLam (Join Bot (Arr Bot Bot))
main
GHCI
)
if ! grep -Fq "= ((), LogArr Bot Bot)" <<<"$ghci_out" ||
   ! grep -Fq "(True,True,True,True,True)" <<<"$ghci_out"; then
    printf 'unexpected ghci output: %s\n' "$ghci_out" >&2
    exit 1
fi

mkdir -p "$out/neg"
cd "$out/neg"

reject() {
    local pattern=$1
    local file=$2
    shift 2
    if "${ghc[@]}" "$@" -O0 -c "$file" >rejected.log 2>&1 ||
       ! grep -Eq "$pattern" rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
}

# 1. Non-exhaustive mutual large elimination must fail with GHC-68947.
cat >NonExhaustiveMutual.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module NonExhaustiveMutual where
import Data.Kind (Type)
data Val = Bot | Arr Val Val | Join Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot       = type ()
logVLam (Arr v w) = type (LogArr v w)
bad :: logVLam Bot
bad = ()
HS
reject '\[GHC-68947\]' NonExhaustiveMutual.hs -Wno-incomplete-patterns

# 2. Non-decreasing mutual recursion must fail cleanly without looping.
cat >NonDecreasingMutual.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NonDecreasingMutual where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot       = type (logVLam Bot)
logVLam (Arr v w) = type (LogArr v w)
HS
reject '\[GHC-68947\]' NonDecreasingMutual.hs

# 3. Non-decreasing self-call hidden behind a mutual type synonym must fail.
cat >HiddenSynCycle.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module HiddenSynCycle where
import Data.Kind (Type)
data Val = Bot | Arr Val Val | Join Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
type Sneaky (u :: Val) = logVLam Bot
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot        = type ()
logVLam (Arr v w)  = type (LogArr v w)
logVLam (Join u _) = type (Sneaky u)
HS
reject '\[GHC-68947\]' HiddenSynCycle.hs

# 4. Datatype header kind mentioning mutual large elimination must fail.
cat >BadHeaderKind.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module BadHeaderKind where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
data HaltsV (x :: logVLam Bot) where
  MkHaltsV :: HaltsV x
logVLam :: Val -> Type
logVLam Bot       = type ()
logVLam (Arr _ _) = type (HaltsV ())
HS
reject '\[GHC-68947\]' BadHeaderKind.hs

# 5. Exporting mutual large elimination without exporting mutual RHS type must fail with GHC-70566.
cat >UnexportedMutualRhs.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module UnexportedMutualRhs (Val (..), HaltsV (..), logVLam) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot       = type ()
logVLam (Arr v w) = type (LogArr v w)
HS
reject '\[GHC-70566\]' UnexportedMutualRhs.hs

# 6. Active rewrite rule on mutual large elimination must fail with GHC-83605.
cat >RuledMutual.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module RuledMutual where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot       = type ()
logVLam (Arr v w) = type (LogArr v w)
{-# RULES "logVLam/Bot" forall x. logVLam x = type () #-}
proof :: logVLam Bot
proof = ()
HS
reject '\[GHC-83605\]' RuledMutual.hs -fenable-rewrite-rules

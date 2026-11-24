#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -Werror=incomplete-patterns -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/NativeIndexedForEachCtorProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeIndexedForEachCtorProvider.hs
    cp ./*.hi "$src/NativeIndexedForEachCtor.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c -fno-code NativeIndexedForEachCtor.hs
    "${ghc[@]}" "-$opt" -c NativeIndexedForEachCtor.hs
    "${ghc[@]}" "-$opt" NativeIndexedForEachCtor.o ../provider/NativeIndexedForEachCtorProvider.o -o run
    test "$(./run)" = "(True,True,True,True,True,True,True,True,True)"
done

mkdir -p "$out/neg"
cp "$out/O0/provider/"*.hi "$out/neg/"
cd "$out/neg"

cat >StrictGadt.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module StrictGadt where
import NativeIndexedForEachCtorProvider (Nat(..))

data StrictUTerm (n :: Nat) where
  SULam :: !(StrictUTerm ('Succ n)) -> StrictUTerm n

data BadWrap (t :: StrictUTerm n) where
  MkBad :: foreach (t :: StrictUTerm n) -> BadWrap t
HS
if "${ghc[@]}" -O0 -c StrictGadt.hs >strict.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' strict.log; then
    cat strict.log >&2
    exit 1
fi

cat >WrongIndex.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications #-}
module WrongIndex where
import GHC.Sigma (Sigma(..))
import NativeIndexedForEachCtorProvider

type BadVar = some (t' :: UTerm 'Zero) | BetaSteps ('UVar 'FZ) t'
bad :: BadVar
bad = Pair @(UTerm 'Zero) (UVar FZ) BRefl
HS
if "${ghc[@]}" -O0 -c WrongIndex.hs >wrongidx.log 2>&1 ||
   ! grep -Eq '\[GHC-(83865|25897|83605)\]' wrongidx.log; then
    cat wrongidx.log >&2
    exit 1
fi

cat >BadProof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module BadProof where
import Data.Type.Equality ((:~:)(Refl))
import NativeIndexedForEachCtorProvider

badProof :: foreach (t :: UTerm 'Zero) -> 'ULam ('UVar 'FZ) :~: t
badProof _ = Refl
HS
if "${ghc[@]}" -O0 -c BadProof.hs >badproof.log 2>&1 ||
   ! grep -Eq '\[GHC-(83865|25897)\]' badproof.log; then
    cat badproof.log >&2
    exit 1
fi

cat >UnliftedField.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, MagicHash #-}
module UnliftedField where
import GHC.Exts (Int#)
import NativeIndexedForEachCtorProvider (Nat(..))

data UnlGadt (n :: Nat) where
  MkUnl :: Int# -> UnlGadt 'Zero

data BadUnl where
  MkBadUnl :: foreach (u :: UnlGadt 'Zero) -> BadUnl
HS
if "${ghc[@]}" -O0 -c UnliftedField.hs >unl.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' unl.log; then
    cat unl.log >&2
    exit 1
fi

cat >FunField.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module FunField where
import NativeIndexedForEachCtorProvider (Nat(..), UTerm)

data BadFun (n :: Nat) where
  MkBadFun :: foreach (f :: UTerm n -> UTerm n) -> BadFun n
HS
if "${ghc[@]}" -O0 -c FunField.hs >fun.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' fun.log; then
    cat fun.log >&2
    exit 1
fi

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
    cp "$src/NativeLargeElimTypeProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeLargeElimTypeProvider.hs
    cp ./*.hi "$src/NativeLargeElimType.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c -fno-code -fwrite-interface NativeLargeElimType.hs
    "${ghc[@]}" "-$opt" -c NativeLargeElimType.hs
    "${ghc[@]}" "-$opt" NativeLargeElimType.o ../provider/NativeLargeElimTypeProvider.o -o run
    test "$(./run)" = "(True,True,True,True,True)"

    cd ../nocode
    cp "$src/NativeLargeElimTypeProvider.hs" "$src/NativeLargeElimType.hs" .
    sed -i 's/module Main where/module NativeLargeElimType where/' NativeLargeElimType.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -fwrite-source-equations -c NativeLargeElimTypeProvider.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -c NativeLargeElimType.hs
done

mkdir -p "$out/ghci"
cp "$src/NativeLargeElimTypeProvider.hs" "$src/NativeLargeElimType.hs" "$out/ghci/"
cd "$out/ghci"
ghci_out=$("$1" "-B$2" --interactive -ignore-dot-ghci -v0 -package-env - -i -i. NativeLargeElimType.hs <<'GHCI'
:set -XDataKinds -XExperimentalDependentTypes
:kind! valPred ('Arr 'Bot ('Arr 'Bot 'Bot))
main
GHCI
)
if ! grep -Fq "= () -> () -> ()" <<<"$ghci_out" ||
   ! grep -Fq "(True,True,True,True,True)" <<<"$ghci_out"; then
    printf 'unexpected ghci output: %s\n' "$ghci_out" >&2
    exit 1
fi

mkdir -p "$out/neg"/{vis1,vis2,vis3,vis4}
cd "$out/neg"

# 1. Non-terminating base self-call must fail.
cat >NonTermBase.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes #-}
module NonTermBase where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type (valPred Bot)
valPred (Arr a b) = type (valPred a -> valPred b)
HS
if "${ghc[@]}" -O0 -c NonTermBase.hs >nonterm1.log 2>&1 ||
   ! grep -Eq '\[GHC-(68947|70566)\]' nonterm1.log; then
    cat nonterm1.log >&2
    exit 1
fi

# 2. Non-decreasing recursive argument must fail.
cat >NonTermStep.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes #-}
module NonTermStep where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred (Arr a b))
HS
if "${ghc[@]}" -O0 -c NonTermStep.hs >nonterm2.log 2>&1 ||
   ! grep -Eq '\[GHC-(68947|70566)\]' nonterm2.log; then
    cat nonterm2.log >&2
    exit 1
fi

# 3. Mutual non-terminating large-elimination cycle must fail when used in a type.
cat >MutualCycle.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, TypeOperators #-}
module MutualCycle where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
data Val = Bot | Arr Val Val
f :: Val -> Type
f Bot       = type ()
f (Arr a _) = type (g a)
g :: Val -> Type
g Bot       = type ()
g (Arr a _) = type (f a)
bad :: f 'Bot :~: ()
bad = Refl
HS
if "${ghc[@]}" -O0 -c MutualCycle.hs >mutual.log 2>&1 ||
   ! grep -Eq '\[GHC-(70566|68947)\]' mutual.log; then
    cat mutual.log >&2
    exit 1
fi

# 4a. Export visibility: recursive provider exporting valPred with abstract Val -> GHC-70566,
#     and non-recursive provider with abstract Val withholds table -> client gets GHC-70566.
cd "$out/neg/vis1"
cat >HiddenConsRecProv.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes #-}
module HiddenConsRecProv (Val, valPred) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)
HS
if "${ghc[@]}" -O0 -c HiddenConsRecProv.hs >recprov.log 2>&1 ||
   ! grep -Fq '[GHC-70566]' recprov.log; then
    cat recprov.log >&2
    exit 1
fi

cat >HiddenConsProv.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes #-}
module HiddenConsProv (Val, flatPred) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
flatPred :: Val -> Type
flatPred Bot       = type ()
flatPred (Arr _ _) = type Int
HS
"${ghc[@]}" -O0 -c HiddenConsProv.hs
cat >HiddenConsCli.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module HiddenConsCli where
import HiddenConsProv (Val, flatPred)
use :: foreach (v :: Val) -> flatPred v -> ()
use _ _ = ()
HS
if "${ghc[@]}" -O0 -c HiddenConsCli.hs >cli.log 2>&1 ||
   ! grep -Eq '\[GHC-(70566|68947)\]' cli.log; then
    cat cli.log >&2
    exit 1
fi

# 4b. Export visibility: same-module exported type mentions valPred while hiding Val constructors -> GHC-70566.
cd "$out/neg/vis2"
cat >SameModHiddenCons.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, GADTs #-}
module SameModHiddenCons (Val, valPred, Box(..)) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)
data Box (v :: Val) = MkBox (valPred v)
HS
if "${ghc[@]}" -O0 -c SameModHiddenCons.hs >samemod1.log 2>&1 ||
   ! grep -Fq '[GHC-70566]' samemod1.log; then
    cat samemod1.log >&2
    exit 1
fi

# 4c. Export visibility: same-module exported type mentions valPred while hiding valPred itself -> GHC-70566.
cd "$out/neg/vis3"
cat >SameModHiddenFun.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, GADTs #-}
module SameModHiddenFun (Val(..), Box(..)) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)
data Box (v :: Val) = MkBox (valPred v)
HS
if "${ghc[@]}" -O0 -c SameModHiddenFun.hs >samemod2.log 2>&1 ||
   ! grep -Fq '[GHC-70566]' samemod2.log; then
    cat samemod2.log >&2
    exit 1
fi

# 4d. Export visibility: unexported RHS datatype rejects recursive export & withholds non-recursive table (GHC-70566).
cd "$out/neg/vis4"
cat >HiddenRhsRecProv.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, TemplateHaskell #-}
module HiddenRhsRecProv (Val(..), valPred) where
import Data.Kind (Type)
data Secret = Secret
$(pure [])
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type Secret
valPred (Arr a b) = type (valPred a -> valPred b)
HS
if "${ghc[@]}" -O0 -c HiddenRhsRecProv.hs >rhsrec.log 2>&1 ||
   ! grep -Fq '[GHC-70566]' rhsrec.log; then
    echo "ERROR: HiddenRhsRecProv check failed" >&2
    cat rhsrec.log >&2
    exit 1
fi

cat >HiddenRhsProv.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes #-}
module HiddenRhsProv (Val(..), flatPred) where
import Data.Kind (Type)
data Secret = Secret
data Val = Bot | Arr Val Val
flatPred :: Val -> Type
flatPred Bot       = type Secret
flatPred (Arr _ _) = type ()
HS
"${ghc[@]}" -O0 -c HiddenRhsProv.hs
cat >HiddenRhsCli.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module HiddenRhsCli where
import HiddenRhsProv (Val(..), flatPred)
use :: foreach (v :: Val) -> flatPred v -> ()
use _ _ = ()
HS
if "${ghc[@]}" -O0 -c HiddenRhsCli.hs >rhscli.log 2>&1 ||
   ! grep -Eq '\[GHC-(70566|68947)\]' rhscli.log; then
    cat rhscli.log >&2
    exit 1
fi

# 5. Active rewrite RULE on valPred must fail with GHC-83605.
cd "$out/neg"
cat >RuleConflict.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
module RuleConflict where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
data Val = Bot | Arr Val Val
{-# NOINLINE valPred #-}
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)
{-# RULES "hijackValPred" forall x. valPred x = valPred Bot #-}
bad :: valPred 'Bot :~: ()
bad = Refl
HS
if "${ghc[@]}" -O2 -c RuleConflict.hs >rule.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' rule.log; then
    cat rule.log >&2
    exit 1
fi

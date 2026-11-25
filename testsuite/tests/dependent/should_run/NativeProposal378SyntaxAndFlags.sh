#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeProposal378SyntaxAndFlags.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

expected='(Succ (Succ Zero),Succ (Succ (Succ Zero)),(Succ (Succ Zero),Zero,Zero,Succ Zero,Zero,Succ Zero,Succ Zero),("rta","rta","rta"))'

for opt in -O0 -O2; do
    dir="$work/$opt"
    mkdir -p "$dir"/{native,readback,no-eqs,aliases,neg}

    # 1. Compile single-pragma Provider + Unicode/Proposal-378 Client without -fwrite-source-equations
    cd "$dir/native"
    cp "$inputs/NativeProposal378SyntaxAndFlagsProvider.hs" \
       "$inputs/NativeProposal378SyntaxAndFlags.hs" .
    "${ghc[@]}" "$opt" -c NativeProposal378SyntaxAndFlagsProvider.hs
    "${ghc[@]}" --show-iface NativeProposal378SyntaxAndFlagsProvider.hi >provider.iface
    grep -Fq 'CheckedClosureSourceTable T:replace' provider.iface
    grep -Fq 'SourceTable T:plus' provider.iface

    "${ghc[@]}" "$opt" -c NativeProposal378SyntaxAndFlags.hs
    "${ghc[@]}" "$opt" NativeProposal378SyntaxAndFlagsProvider.o \
        NativeProposal378SyntaxAndFlags.o -o run
    actual=$(./run)
    [[ "$actual" == "$expected" ]]

    # 2. Separate .hi readback check (no Provider.hs present)
    cd "$dir/readback"
    cp "$dir/native/NativeProposal378SyntaxAndFlagsProvider.hi" \
       "$inputs/NativeProposal378SyntaxAndFlags.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeProposal378SyntaxAndFlags.hs

    # 3. Explicit -fno-write-source-equations (both CLI and OPTIONS_GHC) must override default!
    cd "$dir/no-eqs"
    cp "$inputs/NativeProposal378SyntaxAndFlagsProvider.hs" .
    "${ghc[@]}" "$opt" -fno-write-source-equations -c NativeProposal378SyntaxAndFlagsProvider.hs
    "${ghc[@]}" --show-iface NativeProposal378SyntaxAndFlagsProvider.hi >noeq.iface
    if grep -Fq 'CheckedClosureSourceTable T:replace' noeq.iface; then
        echo "CLI -fno-write-source-equations failed to suppress CheckedClosureSourceTable" >&2
        exit 1
    fi
    {
        echo '{-# OPTIONS_GHC -fno-write-source-equations #-}'
        cat "$inputs/NativeProposal378SyntaxAndFlagsProvider.hs"
    } >ProviderPragmaNoEq.hs
    sed -i 's/module NativeProposal378SyntaxAndFlagsProvider/module ProviderPragmaNoEq/' ProviderPragmaNoEq.hs
    "${ghc[@]}" "$opt" -fforce-recomp -c ProviderPragmaNoEq.hs
    "${ghc[@]}" --show-iface ProviderPragmaNoEq.hi >pragma-noeq.iface
    if grep -Fq 'CheckedClosureSourceTable T:replace' pragma-noeq.iface; then
        echo "Pragma -fno-write-source-equations failed to suppress CheckedClosureSourceTable" >&2
        exit 1
    fi
done

# 4. GHCi interactive execution check
mkdir -p "$work/ghci"
cd "$work/ghci"
cp "$inputs/NativeProposal378SyntaxAndFlagsProvider.hs" \
   "$inputs/NativeProposal378SyntaxAndFlags.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci NativeProposal378SyntaxAndFlags.hs -e 'main' >ghci.out
diff -u <(printf '%s\n' "$expected") ghci.out

# 5. ExistentialTypes (#473), ExtendedLet (#523), ListTuplePuns override, and non-Unicode (∏) operator
cd "$work/-O0/aliases"
cat >AliasChecks.hs <<'HS'
{-# LANGUAGE ExistentialTypes, ExtendedLet, GADTs, DataKinds #-}
module AliasChecks where

type LetPair a = let b = Maybe a in (b, b)
type ExShow = exists a. a
HS
"${ghc[@]}" -O0 -fno-code -c AliasChecks.hs

cat >NonUnicodeProdOp.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NonUnicodeProdOp where

(∏) :: [Int] -> [Int] -> [Int]
xs ∏ ys = xs ++ ys
HS
"${ghc[@]}" -O0 -fno-code -c NonUnicodeProdOp.hs

# 6. Parser & negative controls:
cd "$work/-O0/neg"
cp "$work/-O0/native/NativeProposal378SyntaxAndFlagsProvider.hi" .

# 6a. 'some' in infixexp2 lowers cleanly via T2T, while unadorned foreach in a value expression is rejected:
cat >SomeInInfixExp2.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module SomeInInfixExp2 where
import Data.Kind (Type)
import GHC.Types (List)
import NativeProposal378SyntaxAndFlagsProvider (Nat, Vec, Pair2)
w :: forall (t :: Type) -> List Char
w _ = "ok"
x :: List Char
x = w (some (m n :: Nat). Pair2 (Vec Int m) (Vec Int n))
HS
"${ghc[@]}" -O0 -fno-code -c SomeInInfixExp2.hs

cat >BadForEachInValueExpr.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadForEachInValueExpr where
import NativeProposal378SyntaxAndFlagsProvider (Nat, Vec)
x = (foreach (n :: Nat) -> Vec Int n)
HS
if "${ghc[@]}" -O0 -fno-code -c BadForEachInValueExpr.hs >foreach-val.log 2>&1; then
    cat foreach-val.log >&2
    exit 1
fi
grep -Fq '[GHC-31244]' foreach-val.log

# 6b. Braced specificity in visible foreach -> is rejected at parse time:
cat >BadVisBraces.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadVisBraces where
bad :: foreach {n :: Bool} -> Bool
bad b = b
HS
if "${ghc[@]}" -O0 -fno-code -c BadVisBraces.hs >vis.log 2>&1; then
    cat vis.log >&2
    exit 1
fi

# 6c. Punned [Int] type syntax is rejected by default under -XDependentTypes (NoListTuplePuns):
cat >PunRejected.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module PunRejected where
xs :: [Int]
xs = []
HS
if "${ghc[@]}" -O0 -fno-code -c PunRejected.hs >pun.log 2>&1; then
    cat pun.log >&2
    exit 1
fi

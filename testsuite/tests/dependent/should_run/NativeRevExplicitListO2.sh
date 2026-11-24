#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

reject() {
    local code=$1
    shift
    local status=0
    "${ghc[@]}" "$@" >negative.log 2>&1 || status=$?
    if [[ "$status" != 1 ]] || ! grep -Eq "\\[GHC-($code)\\]" negative.log; then
        cat negative.log >&2
        return 1
    fi
}

# 1. Native -O0 and -O2 provider with `rev (x:xs) = app (rev xs) [x]` and separate consumer.
for opt in -O0 -O2; do
    mkdir -p "$work/$opt"/{provider,client}
    cd "$work/$opt/provider"
    cat > RevOps.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module RevOps (app, rev) where

app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys

rev :: [a] -> [a]
rev []       = []
rev (x : xs) = app (rev xs) [x]
HS
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c RevOps.hs
    "${ghc[@]}" --show-iface RevOps.hi >revops.iface
    grep -Fq '  SourceTable T:app' revops.iface
    if [[ "$opt" == -O0 ]]; then
        grep -Fq '  SourceTable T:rev' revops.iface
    else
        grep -Fq '  FinalCoreSourceTable T:rev' revops.iface
    fi
    cp RevOps.{hi,o} ../client/

    cd ../client
    test ! -e RevOps.hs
    cat > RevClient.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:) (Refl))
import RevOps (app, rev)

#ifdef RULE_REV
{-# RULES "badRev" forall xs. rev xs = [] #-}
#endif
#ifdef RULE_APP
{-# RULES "badApp" forall xs ys. app xs ys = [] #-}
#endif

revNil :: forall a. rev ('[] :: [a]) :~: '[]
revNil = Refl

revStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
        -> rev (x ': xs) :~: app (rev xs) '[x]
revStep _ _ = Refl

main :: IO ()
main = print (revNil @Bool, revStep True [False], rev [True, False])
HS
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c RevClient.hs
    "${ghc[@]}" "$opt" RevClient.o RevOps.o -o run
    [[ $(./run) == '(Refl,Refl,[False,True])' ]]
    if [[ "$opt" == -O2 ]]; then
        reject 83605 "$opt" -DRULE_REV -fforce-recomp -c RevClient.hs
        reject 83605 "$opt" -DRULE_APP -fforce-recomp -c RevClient.hs
    fi
done

# 2. Job-3208 negative controls: imported (normal, -fomit-interface-pragmas, -fno-code)
#    or local hijackBuild must withhold T:rev and block consumers with GHC-83605.
for scope in imported imported-omit imported-nocode local; do
    mkdir -p "$work/hijack-$scope"
    cd "$work/hijack-$scope"
    if [[ "$scope" != local ]]; then
        cat > Hijack.hs <<'HS'
{-# LANGUAGE RankNTypes #-}
module Hijack () where
import GHC.Exts (build)
{-# RULES "hijackBuild" forall (g :: forall b. (a -> b -> b) -> b -> b). build g = [] #-}
HS
        case "$scope" in
            imported)        "${ghc[@]}" -O2 -c Hijack.hs ;;
            imported-omit)   "${ghc[@]}" -O2 -fomit-interface-pragmas -c Hijack.hs ;;
            imported-nocode) "${ghc[@]}" -O2 -fno-code -fwrite-interface -fomit-interface-pragmas -c Hijack.hs
                             test ! -e Hijack.o ;;
        esac
        cat > RevHijack.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module RevHijack (app, rev) where
import Hijack ()
app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys
rev :: [a] -> [a]
rev []       = []
rev (x : xs) = app (rev xs) [x]
HS
        cp "$work/-O2/provider/RevOps.hi" .
        cat > ConsumerHijack.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module ConsumerHijack where
import Data.Type.Equality ((:~:) (Refl))
import Hijack ()
import RevOps (rev)
revNil :: forall a. rev ('[] :: [a]) :~: '[]
revNil = Refl
HS
        reject 83605 -O2 -c ConsumerHijack.hs
    else
        cat > RevHijack.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RankNTypes #-}
module RevHijack (app, rev) where
import GHC.Exts (build)
{-# RULES "hijackBuild" forall (g :: forall b. (a -> b -> b) -> b -> b). build g = [] #-}
app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys
rev :: [a] -> [a]
rev []       = []
rev (x : xs) = app (rev xs) [x]
HS
    fi
    "${ghc[@]}" -O2 -fomit-interface-pragmas -fno-write-if-simplified-core -c RevHijack.hs
    "${ghc[@]}" --show-iface RevHijack.hi >hijack.iface
    if grep -Eq 'SourceTable T:rev\b' hijack.iface; then
        echo "RevHijack ($scope) unexpectedly exported T:rev" >&2
        exit 1
    fi
    cat > UseHijack.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module UseHijack where
import Data.Type.Equality ((:~:) (Refl))
import RevHijack (rev)
revNil :: forall a. rev ('[] :: [a]) :~: '[]
revNil = Refl
HS
    reject 70565 -O2 -c UseHijack.hs
    if [[ "$scope" != local ]]; then
        cat > SameModuleHijack.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module SameModuleHijack (app, rev, badStep, falso) where
import Data.Type.Equality ((:~:) (Refl))
import Hijack ()
app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys
rev :: [a] -> [a]
rev []       = []
rev (x : xs) = app (rev xs) [x]
badStep :: foreach (x :: a) -> foreach (xs :: [a]) -> rev (x ': xs) :~: app (rev xs) '[x]
badStep _ _ = Refl
falso :: rev '[ 'True ] :~: '[ 'True ]
falso = Refl
HS
        reject '68947|70565' -O2 -c SameModuleHijack.hs
    fi
done

# 2b. Retained provider module (compiled via core2coreRetained) also certifies FinalCoreSourceTable T:rev.
mkdir -p "$work/retained-provider"
cd "$work/retained-provider"
cat > RetainedRevOps.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module RetainedRevOps (keepList, app, rev) where
keepList :: foreach (xs :: [a]) -> [a]
keepList xs = xs
app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys
rev :: [a] -> [a]
rev []       = []
rev (x : xs) = app (rev xs) [x]
HS
"${ghc[@]}" -O2 -fomit-interface-pragmas -fno-write-if-simplified-core -c RetainedRevOps.hs
"${ghc[@]}" --show-iface RetainedRevOps.hi >retained.iface
grep -Fq '  FinalCoreSourceTable T:rev' retained.iface

# 3. INLINABLE rev (non-default inline pragma) must withhold T:rev at -O2.
mkdir -p "$work/inlinable"
cd "$work/inlinable"
cat > RevInlinable.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module RevInlinable (rev) where
app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys
{-# INLINABLE rev #-}
rev :: [a] -> [a]
rev []       = []
rev (x : xs) = app (rev xs) [x]
HS
"${ghc[@]}" -O2 -c RevInlinable.hs
"${ghc[@]}" --show-iface RevInlinable.hi >inlinable.iface
if grep -Eq 'SourceTable T:rev\b' inlinable.iface; then
    echo "RevInlinable unexpectedly exported T:rev" >&2
    exit 1
fi

# 4. Explicit matchability printing on matchability kind mismatches (only when differing).
mkdir -p "$work/matchability"
cd "$work/matchability"
cat > BoxMod.hs <<'HS'
{-# LANGUAGE DataKinds, Modifiers, PolyKinds, RankNTypes, StandaloneKindSignatures #-}
module BoxMod where
import Data.Kind (Type)
import GHC.Matchability (U)
type Box :: (forall x. @U Maybe x %U-> Maybe x) -> Type
data Box f
HS
"${ghc[@]}" -O0 -c BoxMod.hs
cat > MatchKindMismatch.hs <<'HS'
{-# LANGUAGE PolyKinds, RankNTypes #-}
module MatchKindMismatch where
import BoxMod (Box)
foo :: forall (r :: forall x. Maybe x -> Maybe x). Box r -> ()
foo _ = ()
HS
reject 83865 -O0 -c MatchKindMismatch.hs
grep -Eq 'GHC\.Internal\.Types\.(U|Unmatchable)' negative.log
grep -Fq 'Matchable' negative.log

cat > PlainKindMismatch.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds #-}
module PlainKindMismatch where
import Data.Proxy (Proxy)
f :: Proxy (Int -> Bool) -> Proxy (Char -> Bool)
f x = x
HS
reject 83865 -O0 -c PlainKindMismatch.hs
if grep -Eq 'Matchable|Unmatchable' negative.log; then
    cat negative.log >&2
    exit 1
fi

cat > AlphaMatchPolyMismatch.hs <<'HS'
{-# LANGUAGE DataKinds, Modifiers, PolyKinds, RankNTypes #-}
module AlphaMatchPolyMismatch where
import Data.Proxy (Proxy)
import GHC.Matchability (Matchability)
g :: (forall (m :: Matchability) (f :: Int %m -> Bool). Proxy f)
  -> (forall (n :: Matchability) (h :: Char %n -> Bool). Proxy h)
g x = x
HS
reject '83865|18872' -O0 -c AlphaMatchPolyMismatch.hs
if grep -Eq 'GHC\.Internal\.Types\.(M|U|Matchable|Unmatchable)' negative.log; then
    cat negative.log >&2
    exit 1
fi

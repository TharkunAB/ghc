#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc_bin=$1
ghc_lib=$2
shift 2

base_ghc=("$ghc_bin" "-B$ghc_lib" -v0 -package-env - -i -i.
          -dcore-lint -dstg-lint -fdiagnostics-color=never
          -Wall -Wno-name-shadowing -Werror "$@")

provider=NativePolyNestedDomainOpenEqProvider
client=NativePolyNestedDomainOpenEq

# 1. Stripped-interface (.hi-only) compilation + execution at -O0 and -O2.
for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cp "$src/$provider.hs" "$out/$opt/provider/"
    (
        cd "$out/$opt/provider"
        "${base_ghc[@]}" "-$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$provider.hs"
    )
    cp "$out/$opt/provider/$provider.hi" "$src/$client.hs" "$out/$opt/client/"
    (
        cd "$out/$opt/client"
        "${base_ghc[@]}" "-$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$client.hs"
        "${base_ghc[@]}" "-$opt" "$client.o" "../provider/$provider.o" -o run
        ./run
    )
done

# 2. -fno-code -fwrite-interface check on both provider and client.
mkdir -p "$out/nocode"
cp "$src/$provider.hs" "$src/$client.hs" "$out/nocode/"
(
    cd "$out/nocode"
    "${base_ghc[@]}" -O0 -fno-code -fwrite-interface -c "$provider.hs"
    "${base_ghc[@]}" -O0 -fno-code -fwrite-interface -c "$client.hs"
)

# 3. GHCi (--interactive) load and execution.
mkdir -p "$out/ghci"
cp "$src/$provider.hs" "$src/$client.hs" "$out/ghci/"
(
    cd "$out/ghci"
    printf ':load %s.hs\nmain\n:quit\n' "$client" | \
        "$ghc_bin" "-B$ghc_lib" -v0 -ignore-dot-ghci -package-env - --interactive >ghci.out 2>&1
    if [[ -s ghci.out ]]; then
        cat ghci.out >&2
        exit 1
    fi
)

# 4. Same-module reversed order (proofs BEFORE workers and helpers) + direct nested tuple pattern.
mkdir -p "$out/reverse"
cat >"$out/reverse/ReverseOrder.hs" <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module ReverseOrder (proofAssoc, proofDirectTuple, proofUnit, proofSum) where

import Data.Type.Equality ((:~:) (Refl))

proofAssoc :: foreach (p :: ((a, b), c)) -> assocProdFrom (assocProdTo p) :~: p
proofAssoc (ab, _) = case ab of
  (_, _) -> Refl

proofDirectTuple :: foreach (p :: ((a, b), c)) -> directTupleFrom (directTupleTo p) :~: p
proofDirectTuple (ab, _) = case ab of
  (_, _) -> Refl

proofUnit :: foreach (x :: a) -> unitRTo (unitRFrom x) :~: x
proofUnit _ = Refl

proofSum :: foreach (s :: Either (Either a b) c) -> sumAssocFrom (sumAssocTo s) :~: s
proofSum (Left ab) = case ab of
  Left _  -> Refl
  Right _ -> Refl
proofSum (Right _) = Refl

directTupleTo :: ((a, b), c) -> (a, (b, c))
directTupleTo ((x, y), z) = (x, (y, z))

directTupleFrom :: (a, (b, c)) -> ((a, b), c)
directTupleFrom (x, (y, z)) = ((x, y), z)

assocProdTo :: ((a, b), c) -> (a, (b, c))
assocProdTo (p, z) = assocPairR p z

assocProdFrom :: (a, (b, c)) -> ((a, b), c)
assocProdFrom (x, p) = assocPairL p x

assocPairR :: (a, b) -> c -> (a, (b, c))
assocPairR (x, y) z = (x, (y, z))

assocPairL :: (b, c) -> a -> ((a, b), c)
assocPairL (y, z) x = ((x, y), z)

unitRTo :: (a, ()) -> a
unitRTo (x, ()) = x

unitRFrom :: a -> (a, ())
unitRFrom x = (x, ())

sumAssocTo :: Either (Either a b) c -> Either a (Either b c)
sumAssocTo (Left (Left x))  = Left x
sumAssocTo (Left (Right y)) = Right (Left y)
sumAssocTo (Right z)        = Right (Right z)

sumAssocFrom :: Either a (Either b c) -> Either (Either a b) c
sumAssocFrom (Left x)          = Left (Left x)
sumAssocFrom (Right (Left y))  = Left (Right y)
sumAssocFrom (Right (Right z)) = Right z
HS
(
    cd "$out/reverse"
    "${base_ghc[@]}" -O0 -c ReverseOrder.hs
    "${base_ghc[@]}" -O2 -fforce-recomp -c ReverseOrder.hs
)

# 5. Negative controls:
mkdir -p "$out/neg"

# 5a. Active rewrite rule on exported owner or private helper rejects with GHC-83605.
cat >"$out/neg/RuleOnHelper.hs" <<'HS'
{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module RuleOnHelper (assocProdTo, badProof) where
import Data.Type.Equality ((:~:) (Refl))

assocPairR :: (a, b) -> c -> (a, (b, c))
assocPairR (x, y) z = (x, (y, z))
{-# NOINLINE assocPairR #-}
{-# RULES "assocPairR/alt" forall p z. assocPairR p z = (fst p, (snd p, z)) #-}

assocProdTo :: ((a, b), c) -> (a, (b, c))
assocProdTo (p, z) = assocPairR p z

badProof :: foreach (x :: a) -> foreach (y :: b) -> foreach (z :: c) -> assocProdTo '( '(x, y), z ) :~: '( x, '(y, z) )
badProof _ _ _ = Refl
HS
if "${base_ghc[@]}" -O1 -Wwarn -c "$out/neg/RuleOnHelper.hs" >"$out/neg/rule.log" 2>&1 ||
   ! grep -Fq '[GHC-83605]' "$out/neg/rule.log"; then
    cat "$out/neg/rule.log" >&2
    exit 1
fi

# 5b. Strictness preservation: unitRTo (x, ()) = x does NOT reduce when inner () is neutral.
cat >"$out/neg/StrictUnit.hs" <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module StrictUnit where
import Data.Type.Equality ((:~:) (Refl))

unitRTo :: (a, ()) -> a
unitRTo (x, ()) = x

unitRFrom :: a -> (a, ())
unitRFrom x = (x, ())

badRT :: foreach (p :: (a, ())) -> unitRFrom (unitRTo p) :~: p
badRT (_, _) = Refl
HS
if "${base_ghc[@]}" -O0 -Wwarn -c "$out/neg/StrictUnit.hs" >"$out/neg/strict_unit.log" 2>&1 ||
   ! grep -Eq '\[GHC-(83865|25897|05617)\]' "$out/neg/strict_unit.log"; then
    cat "$out/neg/strict_unit.log" >&2
    exit 1
fi

# 5c. Incomplete nested pattern does not publish source equations (GHC-68947).
cat >"$out/neg/IncompleteNested.hs" <<'HS'
{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module IncompleteNested where
import Data.Type.Equality ((:~:) (Refl))

partialSum :: Either (Either a b) c -> Either a (Either b c)
partialSum (Left (Left x)) = Left x
partialSum (Right z)       = Right (Right z)

bad :: foreach (x :: a) -> partialSum ('Left ('Left x)) :~: 'Left x
bad _ = Refl
HS
if "${base_ghc[@]}" -O0 -Wwarn -Wno-incomplete-patterns -c "$out/neg/IncompleteNested.hs" >"$out/neg/incomp.log" 2>&1 ||
   ! grep -Fq '[GHC-68947]' "$out/neg/incomp.log"; then
    cat "$out/neg/incomp.log" >&2
    exit 1
fi

# 5d. Bang pattern / -XStrict rejects carried/nested equations.
cat >"$out/neg/BangCarried.hs" <<'HS'
{-# LANGUAGE BangPatterns, DataKinds, DependentTypes, ListTuplePuns #-}
module BangCarried where
import Data.Type.Equality ((:~:) (Refl))

strictPairR :: (a, b) -> c -> (a, (b, c))
strictPairR (x, y) !z = (x, (y, z))

bad :: foreach (x :: a) -> foreach (y :: b) -> foreach (z :: c) -> strictPairR '(x, y) z :~: '( x, '(y, z) )
bad _ _ _ = Refl
HS
if "${base_ghc[@]}" -O0 -Wwarn -c "$out/neg/BangCarried.hs" >"$out/neg/bang.log" 2>&1 ||
   ! grep -Fq '[GHC-68947]' "$out/neg/bang.log"; then
    cat "$out/neg/bang.log" >&2
    exit 1
fi

# 5e. Unexported local constructors: withheld from .hi and rejected when exposed in exported dependent type.
cat >"$out/neg/HiddenConExposed.hs" <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module HiddenConExposed (Secret, buildSecret, exposedSig) where
import Data.Type.Equality ((:~:) (Refl))

data Secret a = MkSecret a

buildSecret :: a -> (Secret a, ())
buildSecret x = (MkSecret x, ())

exposedSig :: foreach (x :: a) -> buildSecret x :~: buildSecret x
exposedSig _ = Refl
HS
if "${base_ghc[@]}" -O0 -Wwarn -c "$out/neg/HiddenConExposed.hs" >"$out/neg/hidden.log" 2>&1 ||
   ! grep -Fq '[GHC-70566]' "$out/neg/hidden.log"; then
    cat "$out/neg/hidden.log" >&2
    exit 1
fi

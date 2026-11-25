#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

# 1. Ordinary interfaces do not record the DepFunTy unfolding capability.
mkdir -p "$out/ordinary"
cd "$out/ordinary"
cat > OrdinaryControl.hs <<'HS'
{-# LANGUAGE Haskell2010 #-}
module OrdinaryControl (inc, scale) where

{-# INLINE inc #-}
inc :: Int -> Int
inc x = x + 1

{-# INLINABLE scale #-}
scale :: Num a => a -> a -> a
scale x y = x * y + x
HS

for opt in -O1 -O2; do
    "${ghc[@]}" "$opt" -fforce-recomp -c OrdinaryControl.hs
    cp OrdinaryControl.hi first.hi
    "${ghc[@]}" "$opt" -fforce-recomp -c OrdinaryControl.hs
    cmp first.hi OrdinaryControl.hi
    "${ghc[@]}" --show-iface OrdinaryControl.hi > ordinary.iface
    if grep -Fq 'retained-features-v1' ordinary.iface ||
       grep -Fq 'retained-depfun-unfolding' ordinary.iface; then
        echo "Unexpected retained capability in ordinary interface at $opt" >&2
        exit 1
    fi
done

expected_dep_client=$'42\n7\n21\n20\n52\n120\n99\n6'
expected_h2010_client=$'42\n7\n21\n20\n52\n120\n99\n6'

# 2. Provider and both -XDependentTypes / Haskell2010 clients at -O1 and -O2.
for opt in -O1 -O2; do
    mkdir -p "$out/$opt"/{provider,dep-client,h2010-client}
    cd "$out/$opt/provider"
    cp "$src/NativeDepFunUnfolding.hs" .
    "${ghc[@]}" "$opt" -fforce-recomp -c NativeDepFunUnfolding.hs
    cp NativeDepFunUnfolding.hi first.hi
    "${ghc[@]}" "$opt" -fforce-recomp -c NativeDepFunUnfolding.hs
    cmp first.hi NativeDepFunUnfolding.hi
    "${ghc[@]}" --show-iface NativeDepFunUnfolding.hi > provider.iface

    grep -Fq 'retained-depfun-unfolding' provider.iface
    for sym in inlVarApp inlBoolCase inlVecHead inlAliasDefault \
               inlPackVec inlUnpackSum inlTopIndex inlScopedDict \
               inlinableScaleHead inlinablePack inlinableScopedDict; do
        awk -v s="  $sym ::" '
            index($0, s) == 1 { inside = 1; next }
            inside && /^  [a-zA-Z0-9_$]+ ::/ { inside = 0 }
            inside && /Unfolding:/ { found = 1 }
            END { exit(found ? 0 : 1) }
        ' provider.iface
    done
    if awk '
        index($0, "  recVecSum ::") == 1 { inside = 1; next }
        inside && /^  [a-zA-Z0-9_$]+ ::/ { inside = 0 }
        inside && /Unfolding:/ { found = 1 }
        END { exit(found ? 0 : 1) }
    ' provider.iface; then
        echo "Recursive DepFunTy binding exposed an unfolding at $opt" >&2
        exit 1
    fi

    # 2a. -XDependentTypes / -XExperimentalDependentTypes client
    cd "$out/$opt/dep-client"
    cp ../provider/NativeDepFunUnfolding.hi .
    cat > DepClient.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module Main where

import Data.Proxy (Proxy (..))
import NativeDepFunUnfolding

useHeadLazy :: Int
useHeadLazy =
  let badNat :: Nat
      badNat = error "strict runtime Nat index"
      badTail :: Vec Int badNat
      badTail = error "strict runtime Vec tail"
      vec = inlVarApp badNat 42 badTail
      vec1 = inlAliasDefault (S Z) (inlVarApp Z (inlVecHead badNat vec) VNil)
  in inlVecHead Z vec1

usePackLazy :: Int
usePackLazy =
  let badVal = error "strict packed payload"
      (_, packed) = inlPackVec True badVal
  in inlUnpackSum False Proxy packed

usePackActive :: Int
usePackActive =
  let (_, packed) = inlPackVec True 20
  in inlUnpackSum True Proxy packed

useBoolAndTop :: Int
useBoolAndTop =
  let n :: Int
      n = inlBoolCase True 9
  in snd (inlTopIndex True n)

useScopedAndInlinable :: (Int, Int, Int, Int)
useScopedAndInlinable =
  let flag = True
      t1 = case inlScopedDict flag 42 of Tagged v -> v
      v1 = inlinableScaleHead Z (3 :: Int) (VCons 40 VNil)
      t2 = case inlinableScopedDict flag of Tagged v -> v
      s  = recVecSum (S (S Z)) (VCons 2 (VCons 4 VNil))
  in (t1, v1, t2, s)

main :: IO ()
main = do
  print useHeadLazy
  print usePackLazy
  print usePackActive
  print useBoolAndTop
  let (a, b, c, d) = useScopedAndInlinable
  print a
  print b
  print c
  print d
HS
    test ! -e NativeDepFunUnfolding.hs
    "${ghc[@]}" "$opt" -funfolding-use-threshold=1500 -ddump-inlinings \
        -fforce-recomp -c DepClient.hs > dep-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlVecHead' dep-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlPackVec' dep-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlUnpackSum' dep-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlScopedDict' dep-inlinings.log
    "${ghc[@]}" "$opt" DepClient.o ../provider/NativeDepFunUnfolding.o -o dep-run
    diff -u <(printf '%s\n' "$expected_dep_client") <(./dep-run)

    # 2b. Ordinary Haskell2010 client (-XNoExperimentalDependentTypes)
    cd "$out/$opt/h2010-client"
    cp ../provider/NativeDepFunUnfolding.hi .
    cat > H2010Client.hs <<'HS'
{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes, DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}
module Main where

import Data.Proxy (Proxy (..))
import NativeDepFunUnfolding

lazyHeadH2010 :: Int
lazyHeadH2010 =
  let go :: Nat -> Int
      go badNat =
        let badTail = error "strict H2010 tail"
        in inlVecHead badNat (inlVarApp badNat 42 badTail)
  in go (error "strict H2010 Nat index")

lazyPackH2010 :: Int
lazyPackH2010 =
  let badPayload = error "strict H2010 payload"
      (_, packed) = inlPackVec True badPayload
  in inlUnpackSum False Proxy packed

activePackH2010 :: Int
activePackH2010 =
  let (_, packed) = inlPackVec True 20
  in inlUnpackSum True Proxy packed

topIdxH2010 :: Int
topIdxH2010 = snd (inlTopIndex True 10)

scopedAndSpecH2010 :: (Int, Int, Int, Int)
scopedAndSpecH2010 =
  let flag = True
      a = case inlScopedDict flag 42 of Tagged v -> v
      b = inlinableScaleHead Z (3 :: Int) (VCons 40 VNil)
      c = case inlinableScopedDict flag of Tagged v -> v
      d = recVecSum (S (S Z)) (VCons 2 (VCons 4 VNil))
  in (a, b, c, d)

main :: IO ()
main = do
  print lazyHeadH2010
  print lazyPackH2010
  print activePackH2010
  print topIdxH2010
  let (a, b, c, d) = scopedAndSpecH2010
  print a
  print b
  print c
  print d
HS
    test ! -e NativeDepFunUnfolding.hs
    "${ghc[@]}" "$opt" -funfolding-use-threshold=1500 -ddump-inlinings \
        -fforce-recomp -c H2010Client.hs > h2010-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlVecHead' h2010-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlPackVec' h2010-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlUnpackSum' h2010-inlinings.log
    grep -Fq 'Inlining done: NativeDepFunUnfolding.inlScopedDict' h2010-inlinings.log
    "${ghc[@]}" "$opt" H2010Client.o ../provider/NativeDepFunUnfolding.o -o h2010-run
    diff -u <(printf '%s\n' "$expected_h2010_client") <(./h2010-run)
done

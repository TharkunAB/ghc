#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeRetainedScrutineePoly.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

expected='(1,2,3,4,5,6,7,8,9,10,11,12,13,14)'

for opt in -O0 -O2; do
    dir="$work/$opt"
    mkdir -p "$dir"/{native,readback,impred,neg}

    # 1. Compile Provider + Client and run binary
    cd "$dir/native"
    cp "$inputs/NativeRetainedScrutineePolyProvider.hs" \
       "$inputs/NativeRetainedScrutineePoly.hs" .
    "${ghc[@]}" "$opt" -c NativeRetainedScrutineePolyProvider.hs
    "${ghc[@]}" "$opt" -c NativeRetainedScrutineePoly.hs
    "${ghc[@]}" "$opt" NativeRetainedScrutineePolyProvider.o \
        NativeRetainedScrutineePoly.o -o run
    actual=$(./run)
    [[ "$actual" == "$expected" ]]

    # 2. Separate .hi readback check without Provider.hs source
    cd "$dir/readback"
    cp "$dir/native/NativeRetainedScrutineePolyProvider.hi" \
       "$inputs/NativeRetainedScrutineePoly.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeRetainedScrutineePoly.hs

    # 3. Exact PiTA Entry 5 surface spelling with [a] and [Bool] under ListTuplePuns
    #    (both standard and under -XImpredicativeTypes)
    cd "$dir/impred"
    cp "$dir/native/NativeRetainedScrutineePolyProvider.hi" .
    cat >PitaEntry5Surface.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import NativeRetainedScrutineePolyProvider (rev)

appendNilR :: forall a. foreach (xs :: [a]) -> (xs ++ '[]) :~: xs
appendNilR [] = Refl
appendNilR (_ : xs) = case appendNilR xs of Refl -> Refl

withRev :: forall a. foreach (ys :: [a]) -> (rev ys ++ '[]) :~: rev ys
withRev ys = appendNilR (rev ys)

withRevCase :: forall a. foreach (ys :: [a]) -> (rev ys ++ '[]) :~: rev ys
withRevCase ys = case appendNilR (rev ys) of Refl -> Refl

withRevCaseBool :: foreach (ys :: [Bool]) -> (rev ys ++ '[]) :~: rev ys
withRevCaseBool ys = case appendNilR (rev ys) of Refl -> Refl

main :: IO ()
main = do
  let xs :: [Int]
      xs = [1, 2]
      bs :: [Bool]
      bs = [True, False]
  case withRev xs of
    Refl -> case withRevCase xs of
      Refl -> case withRevCaseBool bs of
        Refl -> print True
HS
    "${ghc[@]}" "$opt" -XImpredicativeTypes -fno-code -c PitaEntry5Surface.hs
    "${ghc[@]}" "$opt" -c PitaEntry5Surface.hs
    "${ghc[@]}" "$opt" "$dir/native/NativeRetainedScrutineePolyProvider.o" PitaEntry5Surface.o -o run-pita5
    [[ "$(./run-pita5)" == "True" ]]
done

# 4. GHCi interactive execution check
mkdir -p "$work/ghci"
cd "$work/ghci"
cp "$inputs/NativeRetainedScrutineePolyProvider.hs" \
   "$inputs/NativeRetainedScrutineePoly.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci NativeRetainedScrutineePoly.hs -e 'main' >ghci.out
diff -u <(printf '%s\n' "$expected") ghci.out

# 5. Negative control: uncertified IO/non-equation call in case scrutinee still fails with GHC-83605
cd "$work/-O0/neg"
cp "$work/-O0/native/NativeRetainedScrutineePolyProvider.hi" .
cat >BadUncertifiedScrutinee.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadUncertifiedScrutinee where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (List)
import NativeRetainedScrutineePolyProvider

{-# OPAQUE opaqueId #-}
opaqueId :: List a -> List a
opaqueId xs = foldl (\acc x -> x : acc) [] xs

appendNilR :: forall a. foreach (xs :: List a) -> appList xs [] :~: xs
appendNilR [] = Refl
appendNilR (_ : xs) = case appendNilR xs of Refl -> Refl

bad :: forall a. foreach (ys :: List a) -> Int
bad ys = case appendNilR (opaqueId ys) of
  Refl -> 0
HS
if "${ghc[@]}" -O0 -fno-code -c BadUncertifiedScrutinee.hs >bad.log 2>&1; then
    cat bad.log >&2
    exit 1
fi
grep -Fq '[GHC-83605]' bad.log

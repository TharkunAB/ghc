#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client,nocode}
    cd "$out/$opt/provider"
    cp "$src/NativeBootSigmaExistsProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeBootSigmaExistsProvider.hs
    cp ./*.hi "$src/NativeBootSigmaExists.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c -fno-code -fwrite-interface NativeBootSigmaExists.hs
    "${ghc[@]}" "-$opt" -c NativeBootSigmaExists.hs
    "${ghc[@]}" "-$opt" NativeBootSigmaExists.o ../provider/NativeBootSigmaExistsProvider.o -o run
    ./run

    cd ../nocode
    cp "$src/NativeBootSigmaExistsProvider.hs" "$src/NativeBootSigmaExists.hs" .
    sed -i 's/module Main (main) where/module NativeBootSigmaExists (main) where/' NativeBootSigmaExists.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -c NativeBootSigmaExistsProvider.hs
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -c NativeBootSigmaExists.hs
done

# Ordinary Haskell module (DependentTypes off) can use (/\) and discardEvidence.
mkdir -p "$out/ordinary"
cd "$out/ordinary"
cat >OrdinaryCPair.hs <<'HS'
{-# LANGUAGE ExplicitNamespaces, GADTs, TypeOperators, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Main (main) where
import GHC.Exists (type (/\) (..), discardEvidence)

useEvidence :: (Show a /\ a) -> String
useEvidence (MkCPair x) = show x

main :: IO ()
main = do
  let box :: Show Int /\ Int
      box = MkCPair 55
  if useEvidence box == "55" && discardEvidence box == 55
    then pure ()
    else error "OrdinaryCPair mismatch"
HS
"${ghc[@]}" -O2 OrdinaryCPair.hs -o run_ord
./run_ord

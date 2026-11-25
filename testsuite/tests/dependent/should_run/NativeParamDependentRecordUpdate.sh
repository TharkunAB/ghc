#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client,ordinary}
    cd "$out/$opt/provider"
    cp "$src/NativeParamDependentRecordUpdateProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeParamDependentRecordUpdateProvider.hs
    cp ./*.hi "$src/NativeParamDependentRecordUpdate.hs" ../client/

    cd ../client
    test ! -e NativeParamDependentRecordUpdateProvider.hs
    "${ghc[@]}" "-$opt" -c NativeParamDependentRecordUpdate.hs
    "${ghc[@]}" "-$opt" NativeParamDependentRecordUpdate.o ../provider/NativeParamDependentRecordUpdateProvider.o -o run
    test "$(./run)" = "True"

    # Also verify ordinary source-free client with NoExperimentalDependentTypes and Strict.
    cp ../provider/*.hi ../ordinary/
    cd ../ordinary
    test ! -e NativeParamDependentRecordUpdateProvider.hs
    cat >OrdinaryClient.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, Strict #-}
module Main where
import Control.Exception (ErrorCall, evaluate, try)
import NativeParamDependentRecordUpdateProvider

swapElem :: Columns a -> Columns Bool
swapElem r = r { storedLength = S Z, cells = VCons True VNil }

keepWitness :: Columns a -> Columns a
keepWitness r = r { storedLength = storedLength r }

negateCols :: Columns Bool -> Columns Bool
negateCols r = r { cells = mapVec not (cells r) }

checkForced :: a -> IO Bool
checkForced ~v = either (const True) (const False) <$> (try (evaluate v >> pure ()) :: IO (Either ErrorCall ()))

main :: IO ()
main = do
  let r0 = Columns (S Z) (VCons False VNil)
      r1 = negateCols r0
      r2 = swapElem (Columns (S Z) undefined :: Columns Char)
  b1 <- checkForced (keepWitness (Columns Z undefined :: Columns Int))
  let ~badLen = undefined :: Nat
  b2 <- checkForced (keepWitness (Columns badLen undefined :: Columns Int))
  print (toList (cells r1) == [True] && toList (cells r2) == [True] && not b1 && b2)
HS
    "${ghc[@]}" "-$opt" -c OrdinaryClient.hs
    "${ghc[@]}" "-$opt" OrdinaryClient.o ../provider/NativeParamDependentRecordUpdateProvider.o -o run-ord
    test "$(./run-ord)" = "True"
done

mkdir -p "$out/ghci"
cp "$src/NativeParamDependentRecordUpdateProvider.hs" "$src/NativeParamDependentRecordUpdate.hs" "$out/ghci/"
cd "$out/ghci"
ghci_out=$("$1" "-B$2" --interactive -ignore-dot-ghci -v0 -package-env - -i -i. NativeParamDependentRecordUpdate.hs <<'GHCI'
main
GHCI
)
if ! grep -Fq "True" <<<"$ghci_out"; then
    printf 'unexpected ghci output: %s\n' "$ghci_out" >&2
    exit 1
fi

mkdir -p "$out/neg"
cp "$out/O0/provider/"*.hi "$out/neg/"
cd "$out/neg"

reject() {
    local pattern=$1
    local file=$2
    if "${ghc[@]}" -O0 -c "$file" >rejected.log 2>&1 ||
       ! grep -Eq "$pattern" rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
}

# 1. Changing element type when another field sharing `a` is omitted must fail.
cat >PinnedParamMismatch.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module PinnedParamMismatch where
import NativeParamDependentRecordUpdateProvider

bad :: SameCols Char -> SameCols Bool
bad s = s { firstCells = mapVec (== 'a') (firstCells s) }
HS
reject 'GHC-(39999|83865)' PinnedParamMismatch.hs

# 2. Inconsistent type-changing replacements across fields sharing `a` must fail.
cat >InconsistentParamChange.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module InconsistentParamChange where
import NativeParamDependentRecordUpdateProvider

bad :: SameCols Char -> SameCols Bool
bad s = s { firstCells = mapVec (== 'a') (firstCells s)
          , secondCells = mapVec (\_ -> (0 :: Int)) (secondCells s) }
HS
reject 'GHC-(39999|83865)' InconsistentParamChange.hs

# 3. Updating witness without supplying replacement for parameterized payload must fail with GHC-83605.
cat >MissingParamPayload.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module MissingParamPayload where
import NativeParamDependentRecordUpdateProvider

bad :: Columns a -> Columns a
bad r = r { storedLength = Z }
HS
reject 'GHC-83605' MissingParamPayload.hs
grep -Eq 'requires replacement for field .*cells' rejected.log

# 4. Updating payload from an unrelated record witness must fail.
cat >WrongParamReceiver.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module WrongParamReceiver where
import NativeParamDependentRecordUpdateProvider

bad :: Columns a -> Columns a -> Columns a
bad p q = p { cells = cells q }
HS
reject 'GHC-(05617|83865)' WrongParamReceiver.hs

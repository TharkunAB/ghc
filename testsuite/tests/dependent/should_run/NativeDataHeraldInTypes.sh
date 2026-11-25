#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeDataHeraldInTypes.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

expected='(Succ (Succ (Succ Zero)),Succ (Succ Zero),[True,False],T,Succ (Succ Zero),Succ Zero,True)'

for opt in -O0 -O2; do
    dir="$work/$opt"
    mkdir -p "$dir"/{native,puns,readback,neg}

    # 1. Compile Provider + Client under -XDependentTypes -XNoListTuplePuns and run
    cd "$dir/native"
    cp "$inputs/NativeDataHeraldInTypesProvider.hs" \
       "$inputs/NativeDataHeraldInTypes.hs" .
    "${ghc[@]}" "$opt" -c NativeDataHeraldInTypesProvider.hs
    "${ghc[@]}" "$opt" -c NativeDataHeraldInTypes.hs
    "${ghc[@]}" "$opt" NativeDataHeraldInTypesProvider.o \
        NativeDataHeraldInTypes.o -o run
    actual=$(./run)
    [[ "$actual" == "$expected" ]]

    # 2. Also verify data [] / data [T] / data () / data (T, True) when ListTuplePuns IS enabled
    cd "$dir/puns"
    cp "$dir/native/NativeDataHeraldInTypesProvider.hi" \
       "$dir/native/NativeDataHeraldInTypesProvider.o" .
    cat >ListTuplePunsOn.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Main where
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:)(Refl))
import NativeDataHeraldInTypesProvider (T (..))

pNil :: Proxy (data ([] :: [T])) :~: Proxy ('[] :: [T])
pNil = Refl

pOne :: Proxy (data [T]) :~: Proxy '[ 'T ]
pOne = Refl

pUnit :: Proxy (data ()) :~: Proxy '()
pUnit = Refl

pPair :: Proxy (data (T, True)) :~: Proxy '( 'T, 'True )
pPair = Refl

pPairCon :: Proxy ((data (,)) (data T) (data False)) :~: Proxy '( 'T, 'False )
pPairCon = Refl

main :: IO ()
main = seq pNil $ seq pOne $ seq pUnit $ seq pPair $ seq pPairCon $ print True
HS
    "${ghc[@]}" "$opt" -c ListTuplePunsOn.hs
    "${ghc[@]}" "$opt" NativeDataHeraldInTypesProvider.o ListTuplePunsOn.o -o run-puns
    [[ "$(./run-puns)" == "True" ]]

    # 3. Separate .hi readback check + strict -Wpun-uses -Wunticked-promoted-constructors -Werror check
    cd "$dir/readback"
    cp "$dir/native/NativeDataHeraldInTypesProvider.hi" \
       "$inputs/NativeDataHeraldInTypes.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeDataHeraldInTypes.hs
    cat >CleanDataHeraldNoPunWarnings.hs <<'HS'
{-# LANGUAGE DependentTypes, NoListTuplePuns #-}
{-# OPTIONS_GHC -Wall -Wpun-uses -Wunticked-promoted-constructors -Werror #-}
module CleanDataHeraldNoPunWarnings
  ( boxDataT
  , boxDataList
  , boxDataPair
  , boxPunVar
  ) where
import NativeDataHeraldInTypesProvider (Nat (..), PairBox (..), T (..))

boxDataT :: PairBox (data T)
boxDataT = MkPairBox

boxDataList :: PairBox (data [T])
boxDataList = MkPairBox

boxDataPair :: PairBox (data (T, True))
boxDataPair = MkPairBox

boxPunVar :: foreach (x :: Nat) -> PairBox (data x)
boxPunVar x = helper True
  where
    helper :: forall x. x -> PairBox (data x)
    helper _ = MkPairBox
HS
    "${ghc[@]}" "$opt" -fno-code -c CleanDataHeraldNoPunWarnings.hs
done

# 4. GHCi interactive check
mkdir -p "$work/ghci"
cd "$work/ghci"
cp "$inputs/NativeDataHeraldInTypesProvider.hs" \
   "$inputs/NativeDataHeraldInTypes.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci NativeDataHeraldInTypes.hs -e 'main' >ghci.out
diff -u <(printf '%s\n' "$expected") ghci.out

# 5. Negative controls
cd "$work/-O0/neg"

# 5a. ExplicitNamespaces required for 'data' herald in types and expressions
cat >NoExplicitNamespacesTy.hs <<'HS'
{-# LANGUAGE Haskell2010, DataKinds #-}
module NoExplicitNamespacesTy where
import Data.Proxy (Proxy (Proxy))
bad :: Proxy (data True)
bad = Proxy
HS
if "${ghc[@]}" -O0 -fno-code -c NoExplicitNamespacesTy.hs >bad1.log 2>&1; then
    cat bad1.log >&2
    exit 1
fi
grep -Fq 'ExplicitNamespaces' bad1.log

cat >NoExplicitNamespacesExp.hs <<'HS'
{-# LANGUAGE Haskell2010 #-}
module NoExplicitNamespacesExp where
bad :: Bool
bad = data True
HS
if "${ghc[@]}" -O0 -fno-code -c NoExplicitNamespacesExp.hs >bad2.log 2>&1; then
    cat bad2.log >&2
    exit 1
fi
grep -Fq 'ExplicitNamespaces' bad2.log

# 5b. Proxy T vs Proxy (data T) really are different kinds: equating them fails!
cp "$work/-O0/native/NativeDataHeraldInTypesProvider.hi" .
cat >PunKindMismatch.hs <<'HS'
{-# LANGUAGE DependentTypes, NoListTuplePuns #-}
module PunKindMismatch where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeDataHeraldInTypesProvider (T (..))

bad :: Proxy T :~: Proxy (data T)
bad = Refl
HS
if "${ghc[@]}" -O0 -fno-code -c PunKindMismatch.hs >bad3.log 2>&1; then
    cat bad3.log >&2
    exit 1
fi
grep -Fq '[GHC-83865]' bad3.log

# 5c. Unadorned punned variable in RTA still fails, while (data x) succeeded above!
cat >UnadornedPunRta.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module UnadornedPunRta where
import Data.Proxy (Proxy (Proxy))
import NativeDataHeraldInTypesProvider (Nat (..))

idNatProxy :: forall (n :: Nat) -> Proxy n
idNatProxy _ = Proxy

bad :: foreach (x :: Nat) -> Proxy (data x)
bad x = check (4 :: Int)
  where
    check :: forall x. x -> Proxy (data x)
    check _ = idNatProxy x
HS
if "${ghc[@]}" -O0 -fno-code -c UnadornedPunRta.hs >bad4.log 2>&1; then
    cat bad4.log >&2
    exit 1
fi
grep -Fq '[GHC-09591]' bad4.log

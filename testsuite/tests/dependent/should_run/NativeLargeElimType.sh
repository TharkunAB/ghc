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

# 6. Indexed forall binders and carried GADT/existential arguments (R13_LargeElimIndexedArgs / Adequacy.logVLam).
mkdir -p "$out/indexed-args"/{prov,cli,same,neg}
cd "$out/indexed-args/prov"
cat >IndexedLargeElimProv.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, GADTs, KindSignatures, ListTuplePuns #-}
module IndexedLargeElimProv where

import Data.Kind (Type)

data Nat = Zero | Succ Nat
data Fin (n :: Nat) where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n
data Env (n :: Nat) where
  ENil  :: Env Zero
  ECons :: Clos -> Env n -> Env (Succ n)
data Clos where
  MkClos :: UTerm n -> Env n -> Clos
data Box :: Nat -> Type where
  MkBox :: Box n

data Val = Bot | Arr Val Val | Join Val Val
data Witness (v :: Val) (w :: Val) (e :: Env n) (b :: UTerm (Succ n)) = MkWitness

logVLamBox :: forall (n :: Nat). Val -> Box n -> Type
logVLamBox Bot        _ = type ()
logVLamBox (Arr _ _)  _ = type Int
logVLamBox (Join _ _) _ = type Bool

logVLamAdeq :: forall (n :: Nat). Val -> Env n -> UTerm (Succ n) -> Type
logVLamAdeq Bot        _ _ = type ()
logVLamAdeq (Arr v w)  e b = type (Witness v w e b)
logVLamAdeq (Join _ _) _ _ = type Bool

logVLamClos :: Val -> Clos -> Type
logVLamClos Bot        _ = type ()
logVLamClos (Arr _ _)  _ = type Int
logVLamClos (Join u v) c = type (logVLamClos u c, logVLamClos v c)
HS
cat >"$out/indexed-args/cli/IndexedLargeElimCli.hs" <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module IndexedLargeElimCli where

import Data.Type.Equality ((:~:) (Refl))
import IndexedLargeElimProv

useBoxBot :: forall (n :: Nat) (b :: Box n). logVLamBox 'Bot b
useBoxBot = ()

useAdeqArr :: forall (n :: Nat) (v :: Val) (w :: Val) (e :: Env n) (b :: UTerm ('Succ n)).
              logVLamAdeq ('Arr v w) e b :~: Witness v w e b
useAdeqArr = Refl

useClosBot :: forall (c :: Clos). logVLamClos 'Bot c
useClosBot = ()

useClosJoin :: forall (u :: Val) (v :: Val) (c :: Clos).
               logVLamClos u c -> logVLamClos v c -> logVLamClos ('Join u v) c
useClosJoin x y = (x, y)
HS
cat >"$out/indexed-args/same/SameModIndexedLargeElim.hs" <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, GADTs, KindSignatures, ListTuplePuns, TypeOperators #-}
module SameModIndexedLargeElim where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat
data Box :: Nat -> Type where
  MkBox :: Box n
data Clos where
  MkClos :: Box n -> Clos
data Val = Bot | Arr Val Val | Join Val Val

logVLam :: forall (n :: Nat). Val -> Box n -> Type
logVLam Bot        _ = type ()
logVLam (Arr _ _)  _ = type Int
logVLam (Join _ _) _ = type Bool

logVLamClos :: Val -> Clos -> Type
logVLamClos Bot        _ = type ()
logVLamClos (Arr _ _)  _ = type Int
logVLamClos (Join u v) c = type (logVLamClos u c, logVLamClos v c)

sameBot :: forall (n :: Nat) (b :: Box n). logVLam 'Bot b
sameBot = ()

sameJoinClos :: forall (u :: Val) (v :: Val) (c :: Clos).
                logVLamClos ('Join u v) c :~: (logVLamClos u c, logVLamClos v c)
sameJoinClos = Refl
HS
for opt in O0 O2; do
    cd "$out/indexed-args/prov"
    "${ghc[@]}" "-$opt" -fforce-recomp -c IndexedLargeElimProv.hs
    cp IndexedLargeElimProv.hi "$out/indexed-args/cli/"
    cd "$out/indexed-args/cli"
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -fforce-recomp -c IndexedLargeElimCli.hs
    "${ghc[@]}" "-$opt" -fforce-recomp -c IndexedLargeElimCli.hs
    cd "$out/indexed-args/same"
    "${ghc[@]}" "-$opt" -fno-code -fwrite-interface -fforce-recomp -c SameModIndexedLargeElim.hs
    "${ghc[@]}" "-$opt" -fforce-recomp -c SameModIndexedLargeElim.hs
done
cd "$out/indexed-args/neg"
cat >HiddenClosRecProv.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, GADTs, KindSignatures, ListTuplePuns #-}
module HiddenClosRecProv (Nat(..), Clos, Val(..), logVLamClos) where
import Data.Kind (Type)
data Nat = Zero | Succ Nat
data Clos where MkClos :: Nat -> Clos
data Val = Bot | Join Val Val
logVLamClos :: Val -> Clos -> Type
logVLamClos Bot        _ = type ()
logVLamClos (Join u v) c = type (logVLamClos u c, logVLamClos v c)
HS
if "${ghc[@]}" -O0 -c HiddenClosRecProv.hs >hidx.log 2>&1 ||
   ! grep -Fq '[GHC-70566]' hidx.log; then
    cat hidx.log >&2
    exit 1
fi

{-# LANGUAGE DependentTypes, GADTs, LambdaCase #-}
module ProofWithHoles where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Data.Nat.Dependent (Nat(..), eqSize, plus)
import Data.Nat.Dependent.Properties (eqSizeRefl, plusZero)
import GHC.Sigma (Sigma (Pair))

localDouble :: Nat -> Nat
localDouble Zero     = Zero
localDouble (Succ n) = Succ (Succ (localDouble n))

holeLocal :: localDouble (Succ Zero) :~: Succ (Succ Zero)
holeLocal = _

holeForeach :: foreach (m :: Nat) -> plus m Zero :~: m
holeForeach m = _

curryPair :: forall a (p :: a -> Type) r1. (Sigma a p -> r1) -> foreach (x :: a) -> p x -> r1
curryPair k x px = _

useImportedEqSize :: foreach (n :: Nat) -> eqSize n n :~: True
useImportedEqSize n = eqSizeRefl n

{-# LANGUAGE DependentTypes, LambdaCase #-}
module ProofWithHoles where

import Data.Type.Equality ((:~:) (Refl))
import Data.Nat.Dependent (Nat(..), eqSize, plus)
import Data.Nat.Dependent.Properties (eqSizeRefl, plusZero)

localDouble :: Nat -> Nat
localDouble Zero     = Zero
localDouble (Succ n) = Succ (Succ (localDouble n))

holeLocal :: localDouble (Succ Zero) :~: Succ (Succ Zero)
holeLocal = _

holeForeach :: foreach (m :: Nat) -> plus m Zero :~: m
holeForeach m = _

useImportedEqSize :: foreach (n :: Nat) -> eqSize n n :~: True
useImportedEqSize n = eqSizeRefl n

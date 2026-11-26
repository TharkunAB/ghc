{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module NativeAbsurdForEachTriBad where
import Data.Type.Equality ((:~:) (Refl))
data Nat = Zero | Succ Nat
data Vec a (n :: Nat) where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)
triBad :: foreach (m n p :: Nat) -> Vec a m -> Vec b n -> Vec c p
       -> (m :~: n) -> (n :~: p) -> Vec (a, b, c) m
triBad Zero Zero Zero VNil VNil VNil Refl Refl = VNil

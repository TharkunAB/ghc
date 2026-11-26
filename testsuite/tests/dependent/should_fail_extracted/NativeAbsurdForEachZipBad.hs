{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module NativeAbsurdForEachZipBad where
import Data.Type.Equality ((:~:) (Refl))
data Nat = Zero | Succ Nat
data Vec a (n :: Nat) where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)
zipBad :: foreach (m n :: Nat) -> Vec a m -> Vec b n -> (m :~: n) -> Vec (a, b) m
zipBad Zero Zero VNil VNil Refl = VNil

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import NativeStructuralSizePredicateCount (Count(..), same)

sameRefl :: foreach (n :: Count) -> same n n :~: 'True
sameRefl Z = Refl
sameRefl (S n) = sameRefl n

main :: IO ()
main = case sameRefl (S (S Z)) of
  Refl -> print (same Z (S (error "forced predecessor")))

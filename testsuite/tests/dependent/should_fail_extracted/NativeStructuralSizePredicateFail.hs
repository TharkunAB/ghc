{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
module NativeStructuralSizePredicateFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeStructuralSizePredicateCount (Count(..), same)

wrong :: foreach (n :: Count) -> same 'Z ('S n) :~: 'True
wrong _ = Refl

unknownFirst :: foreach (n :: Count) -> same n 'Z :~: 'False
unknownFirst _ = Refl

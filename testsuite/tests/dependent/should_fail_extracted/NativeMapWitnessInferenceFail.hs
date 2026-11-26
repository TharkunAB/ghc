{-# LANGUAGE AllowAmbiguousTypes, DataKinds, GADTs, NoDependentTypes,
             ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module NativeMapWitnessInferenceFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeMapWitnessLib (NegateAll, First(..), Checked(..))

checkedFirstBad :: Checked xs -> First xs
checkedFirstBad (Checked _ h) = h

rigidBad :: forall xs ys. Checked (NegateAll xs) -> Checked ys
rigidBad w = w

type family Plain a :: Bool where
  Plain Int = 'False

inverse :: forall a b. (Plain a ~ Plain b) => a :~: b
inverse = Refl

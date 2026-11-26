{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
module NativeMapWitnessInferenceFreshFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeMapWitnessLib (NegateAll)

fresh :: NegateAll '[ 'True] :~: '[ 'False]
fresh = Refl

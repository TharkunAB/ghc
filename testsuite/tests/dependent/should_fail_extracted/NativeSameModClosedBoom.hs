{-# LANGUAGE DependentTypes #-}
module NativeSameModClosedBoom where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
explode :: Natural -> Natural
explode _ = error "same-module boom"
bad :: explode 0 :~: 0
bad = Refl

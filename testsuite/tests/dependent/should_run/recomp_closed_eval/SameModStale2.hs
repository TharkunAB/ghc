{-# LANGUAGE DependentTypes #-}
module StaleMod where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
bump :: Natural -> Natural
bump n = fromIntegral (fromEnum n + 2)
proof :: bump 5 :~: 7
proof = Refl

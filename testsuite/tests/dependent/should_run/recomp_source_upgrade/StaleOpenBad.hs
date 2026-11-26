{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeLocalOpenStale where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
size :: [Natural] -> Natural
size xs = fromIntegral (length xs) + 1
proof :: NativeLocalOpenStale.size '[1,2] :~: 2
proof = Refl

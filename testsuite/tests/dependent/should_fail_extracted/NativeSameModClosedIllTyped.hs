{-# LANGUAGE DependentTypes #-}
module NativeSameModClosedIllTyped where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
incNat :: Natural -> Natural
incNat n = fromIntegral (fromEnum n + 1)
bad :: incNat 'True :~: 1
bad = Refl

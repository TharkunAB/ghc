{-# LANGUAGE DataKinds, DependentTypes #-}
module NativeAutoNumUncertified where
import Data.Type.Equality ((:~:)(Refl))
data Bad = BZero | BSucc Bad
instance Num Bad where
  (+) _ _ = BZero
  (*) _ _ = BZero
  (-) _ _ = BZero
  negate _ = BZero
  abs _ = BZero
  signum _ = BZero
  fromInteger _ = BZero
bad :: (3 :: Bad) :~: 'BZero
bad = Refl

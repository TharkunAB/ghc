{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeLiftedCallbackNoInverse where
import Data.Type.Equality ((:~:)(Refl))

noInverse :: foreach (cmp :: Bool -> Bool -> Ordering) -> foreach (x :: Bool)
          -> foreach (y :: Bool) -> foreach (z :: Bool)
          -> cmp x y :~: cmp x z -> y :~: z
noInverse _ _ _ _ Refl = Refl

swapped :: forall a b. foreach (cmp :: a -> a -> b) -> foreach (x :: a) -> foreach (y :: a)
        -> cmp x y :~: cmp y x
swapped _ _ _ = Refl

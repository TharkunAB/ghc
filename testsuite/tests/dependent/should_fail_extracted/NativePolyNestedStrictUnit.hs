{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativePolyNestedStrictUnit where

import Data.Type.Equality ((:~:) (Refl))

unitRTo :: (a, ()) -> a
unitRTo (x, ()) = x

unitRFrom :: a -> (a, ())
unitRFrom x = (x, ())

badRT :: foreach (p :: (a, ())) -> unitRFrom (unitRTo p) :~: p
badRT (_, _) = Refl

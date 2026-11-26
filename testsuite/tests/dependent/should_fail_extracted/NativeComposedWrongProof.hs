{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeComposedWrongProof where
import Data.Type.Equality ((:~:))
import NativeComposedIsoFields (Iso(..))
bad :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
    -> foreach (h :: a -> a) -> foreach (k :: a -> a)
    -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
    -> (foreach (y :: a) -> h (f (g (k y))) :~: y)
    -> Iso a a
bad _ g h k left right = MkIso @a @a to from left right
  where
    to :: a -> a
    to x = h x
    from :: a -> a
    from y = g (k y)

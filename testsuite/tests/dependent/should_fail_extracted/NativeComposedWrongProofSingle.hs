{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeComposedWrongProofSingle where
import Data.Type.Equality ((:~:))
import NativeComposedIsoFields (Iso(..))
badSingle :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
          -> foreach (h :: a -> a)
          -> (foreach (x :: a) -> g (h (f x)) :~: x)
          -> (foreach (y :: a) -> h (f (g y)) :~: y)
          -> Iso a a
badSingle _ g h left right = MkIso @a @a to g left right
  where
    to :: a -> a
    to x = h x

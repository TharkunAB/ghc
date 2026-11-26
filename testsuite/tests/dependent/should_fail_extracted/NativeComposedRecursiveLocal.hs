{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeComposedRecursiveLocal where
import Data.Type.Equality ((:~:))
import NativeComposedIsoFields (Iso(..))
badRec :: forall a. foreach (h :: a -> a) -> foreach (k :: a -> a)
       -> (foreach (x :: a) -> k (h (h x)) :~: x)
       -> (foreach (y :: a) -> h (h (k y)) :~: y)
       -> Iso a a
badRec h k left right = MkIso @a @a to k left right
  where
    to :: a -> a
    to x = h (to x)

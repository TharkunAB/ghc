{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeComposedOrphanRule where
import Data.Type.Equality ((:~:))
import NativeComposedIsoFields (Iso(..))
import NativeComposedOrphanRule_Helper ()
badDirect :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
          -> foreach (h :: b -> c) -> foreach (k :: c -> b)
          -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
          -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
          -> Iso a c
badDirect f g h k left right = MkIso @a @c (h . f) (g . k) left right
badWhere :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
         -> foreach (h :: a -> a)
         -> (foreach (x :: a) -> g (h (f x)) :~: x)
         -> (foreach (y :: a) -> h (f (g y)) :~: y)
         -> Iso a a
badWhere f g h left right = MkIso @a @a to g left right
  where
    to x = h (f x)

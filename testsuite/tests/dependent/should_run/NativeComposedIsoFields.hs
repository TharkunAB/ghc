{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeComposedIsoFields (Iso(..), pack) where

import Data.Type.Equality ((:~:))

data Iso a b where
    MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
          -> (foreach (x :: a) -> from (to x) :~: x)
          -> (foreach (y :: b) -> to (from y) :~: y)
          -> Iso a b

pack :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
     -> foreach (h :: b -> c) -> foreach (k :: c -> b)
     -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
     -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
     -> Iso a c
pack f g h k left right = MkIso @a @c to from left right
  where
    to :: a -> c
    to x = h (f x)
    from :: c -> a
    from y = g (k y)

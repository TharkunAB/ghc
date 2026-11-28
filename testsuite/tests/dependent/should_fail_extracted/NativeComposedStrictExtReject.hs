{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, Strict, TypeApplications, TypeOperators #-}
module NativeComposedStrictExtReject where
import Data.Type.Equality ((:~:))
import NativeComposedIsoFields (Iso(..))
badStrict :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
          -> foreach (h :: a -> a)
          -> (foreach (x :: a) -> g (h (f x)) :~: x)
          -> (foreach (y :: a) -> h (f (g y)) :~: y)
          -> Iso a a
badStrict f g h left right = MkIso @a @a to g left right
  where
    to :: a -> a
    to x = h (f x)

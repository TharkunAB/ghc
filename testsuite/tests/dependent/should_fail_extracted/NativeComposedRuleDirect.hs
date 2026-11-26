{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeComposedRuleDirect where
import Data.Type.Equality ((:~:))
import NativeComposedIsoFields (Iso(..))
badRule :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
        -> foreach (h :: b -> c) -> foreach (k :: c -> b)
        -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
        -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
        -> Iso a c
badRule f g h k left right = MkIso @a @c (h . f) (g . k) left right
{-# RULES "bad compose" forall f g x. (f . g) x = f x #-}

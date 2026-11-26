{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeComposedRuleWhere where
import Data.Type.Equality ((:~:))
import NativeComposedIsoFields (Iso(..))
badRuleWhere :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
             -> foreach (h :: a -> a)
             -> (foreach (x :: a) -> g (h (f x)) :~: x)
             -> (foreach (y :: a) -> h (f (g y)) :~: y)
             -> Iso a a
badRuleWhere f g h left right = MkIso @a @a to g left right
  where
    to x = h (f x)
{-# RULES "bad compose where" forall f g x. (f . g) x = f x #-}

{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeComposedIndependent (independent) where

independent :: (b -> c) -> (a -> b) -> a -> c
independent h f y = to y
  where
    to x = h (f x)

{-# RULES "compose/any" forall f g x. (f . g) x = f x #-}

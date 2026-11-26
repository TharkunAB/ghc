{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeMultiParamNonApart where

import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

ambiguous :: foreach (f :: a -> b) -> foreach (x :: a)
          -> omap f (Right x :: Either e a) :~: Right (f x)
ambiguous _ _ = Refl

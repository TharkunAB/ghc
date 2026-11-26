{-# LANGUAGE DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, ScopedTypeVariables #-}
module NativeMultiParamBodyRejects where

import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

data Dict c where
  Dict :: c => Dict c

masked :: forall e. Dict (LabelledMap e (Either e)) -> ()
masked Dict =
  let inner :: foreach (f :: a -> b) -> foreach (x :: a)
            -> lmap f (Right x :: Either e a) :~: Right (f x)
      inner _ _ = Refl
  in inner id True `seq` ()

badEq :: foreach (n :: MyNat) -> (n == n) :~: False
badEq MZero = Refl
badEq (MSucc _) = Refl

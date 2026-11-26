{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns, MagicHash #-}
module NativeLiftedCallbackExtra (lawAmbiguous, keepPartial) where

import CompareAt (Result)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (TYPE, RuntimeRep)

lawAmbiguous :: forall tag. foreach (f :: Bool -> Result tag) -> foreach (x :: Bool) -> f x :~: f x
lawAmbiguous _ _ = Refl

keepPartial :: forall (r :: RuntimeRep) (b :: TYPE r). foreach (f :: Bool -> b -> Int)
            -> foreach (p :: Bool) -> f p :~: f p
keepPartial _ _ = Refl

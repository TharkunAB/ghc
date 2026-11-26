{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeOrdinaryGadtSourceMixed (mixed) where

import Data.Type.Equality ((:~:)(Refl))
import NativeOrdinaryGadtSourceTree (swapPair)

mixed :: foreach (b :: Bool) -> foreach (p :: (Bool, Ordering))
      -> '(b, swapPair (swapPair p)) :~: '(b, p)
mixed False _ = error "no proof for False"
mixed True (_, _) = Refl

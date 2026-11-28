{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers, TypeFamilies,
             TypeOperators #-}
module NativeOpenMap where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import NativeOpenMapEmptyRule ()
import OpenMapProof ()
import OpenMapSource (OrdinaryPrefix)
import qualified OpenMapWorker as W

type family AtRed (f :: [W.Lamp] %U -> [W.Signal]) where
  AtRed f = f '[ 'W.Red]

prefixProof :: AtRed OrdinaryPrefix :~: '[ 'W.Stop]
prefixProof = Refl

closedSignal :: W.signal 'W.Red :~: 'W.Stop
closedSignal = Refl

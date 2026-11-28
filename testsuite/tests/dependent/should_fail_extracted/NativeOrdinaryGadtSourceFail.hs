{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeOrdinaryGadtSourceFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOrdinaryGadtSourceTree

wrong :: foreach (x :: Tree) -> focus ('Arrow x 'Empty) :~: 'Join x 'Other
wrong _ = Refl

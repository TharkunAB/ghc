{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies, TypeOperators #-}
module NativeUnsaturatedMatchAndSynRejects where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, M, Multiplicity(Many))
import NativePartialSynonym (Drop, Slot)
import NativeUnsaturatedProvider (Callback, NamedMap, Same, Select, WrittenMap)

type ApplyM :: FUN @M 'Many Bool Bool -> Bool
type ApplyM f = f 'True

type BadMatch = ApplyM (Select 'True)

type BadDirectM = NamedMap @Bool @M @Bool (Select 'True) '[ 'True]

type BadWritten = WrittenMap (Callback 'True) '[ 'True]

stuck :: Same (Callback 'True) (Callback 'False) :~: 'False
stuck = Refl

type family Claim a = r | r -> a where
  Claim a = Slot (Drop a)

type Pair e a = (e, a)
type Copy a = Pair a a

badForward :: Slot Copy :~: Slot (Pair Bool)
badForward = Refl

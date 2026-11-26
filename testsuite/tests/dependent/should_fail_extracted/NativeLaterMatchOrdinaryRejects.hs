{-# LANGUAGE DataKinds, GADTs, KindSignatures, Modifiers, NoDependentTypes #-}
{-# LANGUAGE NoLiberalTypeSynonyms, PolyKinds, StandaloneKindSignatures, TypeApplications, TypeFamilies #-}
module NativeLaterMatchOrdinaryRejects where

import AnchorReceiver (WrapOpen)
import Data.Kind (Type)
import GHC.Matchability (M, U)
import LaterMatchability (ForwardBox, KnownListCallback, Map, Recipe)
import PromotedEndo (Endo(Endo))

type Apply f a = f a
type BadApplyOff = Apply KnownListCallback Bool

type Holder :: ((Bool %M -> Bool %M -> Type) %M -> Endo @M @M Bool) -> Type
data Holder f
type BadEndo1 = Holder ('Endo @U)
type BadEndo2 = Holder ('Endo @M @U)

type BadRankOptIn = Recipe Map

type BadBoxArity = ForwardBox (WrapOpen 'True)

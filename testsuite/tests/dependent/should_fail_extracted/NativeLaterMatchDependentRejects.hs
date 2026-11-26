{-# LANGUAGE DataKinds, DependentTypes, KindSignatures, ListTuplePuns, Modifiers #-}
{-# LANGUAGE NoLiberalTypeSynonyms, PolyKinds, RankNTypes, StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions, TypeApplications, TypeFamilies #-}
module NativeLaterMatchDependentRejects where

import AnchorReceiver
import Data.Kind (Type)
import GHC.Matchability (M, Matchability, U)
import LaterMatchability (ApplyFlipped, KnownListCallback, Map, Recipe)

type ApplyM :: (Type -> Type) -> Type -> Type
type ApplyM f a = f a
type BadSignedM = ApplyM KnownListCallback Bool

type ApplyU (f :: Type %U -> Type) a = f a
type BadSignedU = ApplyU Maybe Bool

type BadComp1 = ApplyFlipped @M Maybe Bool
type BadComp2 (m :: Matchability) = ApplyFlipped @m Maybe Bool

type family MOnly (f :: Type %M -> Type) (xs :: [Type]) :: [Type]
type BadRecipe = Recipe MOnly

type WrongMapper = forall (flag :: Bool) {b} {a}. @U (a %M -> b) %U -> [a] %U -> [b]
type Wrong :: WrongMapper -> Type
data Wrong f = Wrong
type BadWrongKind = Wrong Map

type FixedReceiver = Receiver @U (Select 'True) 'Match
type FixedSelect = Receiver (Select @U 'True) 'Match
type WrongAnchor = Receiver @M (Select @M 'True) 'Unmatch

type family Selector (a :: Type) :: Matchability
type Unselected :: forall a. @U Anchor (Selector a)
type family Unselected @a
type BadAmbiguous = Receiver (Select 'True) Unselected

data FixedBox (f :: Type -> Type) = FixedBox (f Bool)
type BadFixedBox = FixedBox (WrapOpen 'True)

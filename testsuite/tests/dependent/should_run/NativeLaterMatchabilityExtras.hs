{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, Modifiers, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeFamilies #-}
{-# LANGUAGE TypeOperators, UndecidableInstances #-}
module NativeLaterMatchabilityExtras
  ( Specialized
  , bothProof
  , hidden
  , hiddenExplicit
  , unrelated
  , unrelatedExplicit
  , sampleRec
  , openRec
  , UsesBoxKind(..)
  , Unanchored
  , PartialEndo
  ) where

import AnchorReceiver
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M, Matchability, U)
import LaterMatchability (FamilyBox, KnownListCallback, Map, Mapper)
import PromotedEndo (Endo(Endo), Pure1)

type Unanchored = Pure1 'Endo
type Holder :: ((Bool %M -> Bool %M -> Type) %M -> Endo @M @M Bool) -> Type
data Holder f
type PartialEndo = Holder ('Endo @M)

type ShortMapper = forall {b} {a}. @U (a %M -> b) %U -> [a] %U -> [b]
type Short :: ShortMapper -> Type
data Short f = Short
type Specialized = Short Map

type family AtBoth (mapper :: Mapper) :: ([Type], [Type]) where
  forall (mapper :: Mapper).
    AtBoth mapper = '(mapper @M Maybe '[Bool], mapper @U KnownListCallback '[Bool])

bothProof :: AtBoth Map :~: '( '[Maybe Bool], '[[Bool]])
bothProof = Refl

type family Forget (m :: Matchability) :: Matchability where
  Forget M = U
  Forget U = U

type HiddenReceiver :: forall (m :: Matchability).
  ((Type %m -> Type) %U -> Type) -> Anchor (Forget m) -> Type
type family HiddenReceiver @m f anchor where
  HiddenReceiver @M f 'Unmatch = f Maybe
  HiddenReceiver @U f 'Unmatch = f Identity

type UnrelatedReceiver :: forall (m :: Matchability).
  ((Type %m -> Type) %U -> Type) -> Bool -> Type
type family UnrelatedReceiver @m f flag where
  UnrelatedReceiver @M f 'True = f Maybe
  UnrelatedReceiver @U f 'True = f Identity

hidden :: HiddenReceiver (Select 'True) 'Unmatch
hidden = True

hiddenExplicit :: HiddenReceiver @M (Select @M 'True) 'Unmatch
hiddenExplicit = Just True

unrelated :: UnrelatedReceiver (Select 'True) 'True
unrelated = True

unrelatedExplicit :: UnrelatedReceiver @M (Select @M 'True) 'True
unrelatedExplicit = Just True

data Recursive f = Recursive (f Bool) (Maybe (Recursive f))

sampleRec :: Recursive Maybe
sampleRec = Recursive (Just True) Nothing

openRec :: Recursive (WrapOpen 'True)
openRec = Recursive [False] Nothing

type OpenCallback = WrapOpen 'True
data UsesBoxKind (x :: FamilyBox OpenCallback) = UsesBoxKind

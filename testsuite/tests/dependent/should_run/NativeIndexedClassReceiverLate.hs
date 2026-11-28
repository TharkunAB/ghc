{-# LANGUAGE DataKinds, DependentTypes, FlexibleInstances, FunctionalDependencies, ListTuplePuns #-}
{-# LANGUAGE Modifiers, MultiParamTypeClasses, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions, TypeFamilies, UndecidableInstances #-}
module NativeIndexedClassReceiverLate
  ( makeReceiver
  , fromKey
  , hiddenContents
  ) where

import Data.Kind (Constraint, Type)
import GHC.Matchability (Matchability, U)
import IndexedMap (Batch(..), FamilyBatch, Pick, familyInput)

makeReceiver :: () -> FamilyBatch
makeReceiver () = familyInput

type Observe :: forall (m :: Matchability). (Type %m -> Type) -> Type
type family Observe @m f where
  Observe @m f = f Bool

hiddenContents :: Batch @m f -> [Observe @m f]
hiddenContents (Batch xs) = xs

type Choose :: forall (m :: Matchability). Type -> (Type %m -> Type) -> Constraint
class Choose @m key f | key -> m f where
  fromKey :: key -> Batch @m f

instance (f ~ Pick 'True) => Choose @U Bool f where
  fromKey _ = Batch [Just True]

{-# LANGUAGE Modifiers, NoDependentTypes, PolyKinds, RankNTypes, StandaloneKindSignatures #-}
module NativeUnsaturatedInvisibleRequiredOff where

import Data.Kind (Type)
import GHC.Exts (U)
import InvisibleFamily (Required)

type Carry :: (forall k -> @U k %U -> Type) -> Type
data Carry f

bad :: Carry Required
bad = undefined

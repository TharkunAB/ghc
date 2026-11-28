{-# LANGUAGE PolyKinds, TypeFamilies #-}
module NoPolyKindsOpenFamMatchabilityA
  ( Rewrapping(..)
  , TransExternal
  ) where

import Data.Kind (Type)

class Rewrapping s where
  type Unwrapped s :: Type

instance Rewrapping (Maybe a) where
  type Unwrapped (Maybe a) = a -> a

type family TransExternal a b
type instance TransExternal Int b = Int -> b

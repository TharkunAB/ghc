{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeAssociatedSumFamilyRejects where

import Data.Kind (Type)
import NativeAssociatedSumProvider (Tagged)

class BadPattern (a :: Type) where
  type Defaulted a
  type Defaulted [b] = some (tag :: Bool) | Tagged tag b

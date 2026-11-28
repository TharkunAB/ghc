{-# LANGUAGE DataKinds, DependentTypes, ExplicitForAll, ListTuplePuns, MultiParamTypeClasses, TypeFamilies #-}
module NativeAssociatedSumDefaultScope where

import Data.Kind (Type)
import NativeAssociatedSumProvider (Tagged)

class BadDefault (a :: Type) where
  type Defaulted a
  type Defaulted b = some (tag :: Bool) | Tagged tag a

class Invalid a b where
  type Missing a

instance forall a b. Invalid [a] b where
  type Missing [a] = some (tag :: Bool) | Tagged tag b

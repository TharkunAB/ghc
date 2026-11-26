{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, PolyKinds, TypeFamilies #-}
module NativeClosedFamilySumScopeRejects where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import NativeClosedFamilySumProvider (Tagged)

type family BadClosed (select :: Bool) (a :: Type) :: Type where
  BadClosed False unused = some (tag :: Bool) | Tagged tag Bool
  BadClosed True a = some (tag :: Bool) | Tagged tag unused

type family OpenFam (a :: Type) :: Type
type instance OpenFam Bool = some (tag :: Bool) | Tagged tag missing
type instance OpenFam Int = some (tag :: Proxy tag) | Char

{-# LANGUAGE NoDependentTypes, NoLiberalTypeSynonyms, PolyKinds, TypeFamilies, UnsaturatedTypeFamilies #-}
module NativeStandaloneUnsaturationProvider
  ( Apply
  , Box(Box)
  , Identity
  , Plain
  , Result
  ) where

import Data.Proxy (Proxy)
import GHC.TypeNats (CmpNat)

type family Identity a where
  Identity a = a

type Apply f a = f a
type Plain a = Maybe a
type Result = Apply Identity Bool

data Box f = Box (f Bool)

_builtinPrefix :: Proxy (CmpNat 1)
_builtinPrefix = undefined

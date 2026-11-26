{-# LANGUAGE DataKinds, NoDependentTypes, NoLiberalTypeSynonyms #-}
{-# LANGUAGE NoUnsaturatedTypeFamilies, TypeApplications, TypeOperators #-}
module NativeUnsaturatedNoExtRejects where

import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, Multiplicity(Many), U)
import GHC.TypeNats (CmpNat)
import NativeStandaloneUnsaturationProvider (Box(Box), Identity)
import NativeUnsaturatedProvider (NamedMap, Select)

type Apply :: FUN @U 'Many Bool Bool -> Bool
type Apply f = f 'True

badRaw :: Apply (Select 'True) :~: 'True
badRaw = Refl

prefix :: Proxy (CmpNat 1)
prefix = Proxy

badDirect :: NamedMap @Bool @U @Bool (Select 'True) '[ 'True] :~: '[Select 'True 'True]
badDirect = Refl

badStandalone :: Box Identity
badStandalone = Box True

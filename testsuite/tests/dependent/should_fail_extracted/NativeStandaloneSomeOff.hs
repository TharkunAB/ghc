{-# LANGUAGE DataKinds, KindSignatures, NoDependentTypes, NoLiberalTypeSynonyms, UnsaturatedTypeFamilies #-}
module NativeStandaloneSomeOff where

import Data.Proxy (Proxy)

type Packet = some (b :: Bool) | Proxy b

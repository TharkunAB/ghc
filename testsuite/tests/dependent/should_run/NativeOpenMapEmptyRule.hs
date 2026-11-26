{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -fomit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenMapEmptyRule (emptyUnderRule) where

import qualified ClosedTableLibrary as C
import Data.Type.Equality ((:~:)(Refl))
import OpenMapSource (NegateAll)

{-# RULES "change/flip" forall x. C.flipBit x = True #-}

emptyUnderRule :: NegateAll '[] :~: '[]
emptyUnderRule = Refl

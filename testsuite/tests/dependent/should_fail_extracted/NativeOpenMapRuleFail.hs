{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenMapRuleFail where

import qualified ClosedTableLibrary as C
import Data.Type.Equality ((:~:)(Refl))
import OpenMapSource (NegateAll)

{-# RULES "change/flip" forall x. C.flipBit x = True #-}

proof :: NegateAll '[ 'True] :~: '[ 'False]
proof = Refl

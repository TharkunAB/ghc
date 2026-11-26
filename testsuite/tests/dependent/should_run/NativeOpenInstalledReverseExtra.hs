{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenInstalledReverseExtra (publicProof, closedOk) where

import Data.Type.Equality ((:~:)(Refl))
import Prelude (Bool(..))
import qualified Prelude as P
import PreludeMapLengthProof (Spine)
import PreludeMapLengthSource (Count)
import PreludeReverseLengthProof (lengthReverse)

publicProof :: Spine xs -> Count (P.reverse xs) :~: Count xs
publicProof = lengthReverse

closed :: P.reverse '[2, 3, 5] :~: '[5, 3, 2]
closed = Refl

nested :: P.max (P.reverse '[]) (P.reverse '[ 'P.Just 'x']) :~: '[ 'P.Just 'x']
nested = Refl

closedOk :: Bool
closedOk = case (closed, nested) of (Refl, Refl) -> True

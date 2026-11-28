{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, KindSignatures,
             PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -O1 -fenable-rewrite-rules #-}
module NativeOpenIndexedAppendRulesFail where

import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

appendEmpty :: forall a (n :: P.Nat) (xs :: P.Vec a n).
               P.appendVec 'P.VNil xs :~: xs
appendEmpty = Refl

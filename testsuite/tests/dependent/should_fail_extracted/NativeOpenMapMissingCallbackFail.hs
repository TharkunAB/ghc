{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, StandaloneKindSignatures #-}
module NativeOpenMapMissingCallbackFail where

import qualified NativeOpenMapForward as F
import qualified OpenMapWorker as W

type NegateAll :: [Bool] -> [Bool]
type NegateAll xs = W.each F.flipBool xs

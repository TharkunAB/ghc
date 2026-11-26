{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
module NativeNestedPackagePatEscape where

import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider
import NativeNestedPackagePatProvider

escape :: NestedResult a -> Vec a k
escape (NestedResult (Pair _ (_, Pair _ (_, _, whole@(VCons _ _))))) = whole
escape _ = error "empty"

{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
module NativeNestedPackagePatPartial where

import NativeGadtRetainedSumProvider
import NativeNestedPackagePatProvider

partial :: (HiddenVec Bool, HiddenVec Bool) -> Bool
partial (VCons _ _, VCons _ _) = True
partial (VNil, VNil) = False

inaccessible :: Sealed a -> Bool
inaccessible (Sealed (Coupled VNil (VCons _ _))) = True
inaccessible _ = False

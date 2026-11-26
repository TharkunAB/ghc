{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedPackageStrictFirst where
import NativeRetainedPackageStrictPair (DPair(..), strictFirst)

secondStrict :: foreach (q :: DPair a p) -> p (strictFirst q)
secondStrict (Pair _ val) = val

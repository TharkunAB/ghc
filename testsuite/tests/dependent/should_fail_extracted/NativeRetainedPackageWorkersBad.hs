{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedPackageWorkersBad where
import NativeRetainedPackageProjRule ()
import qualified PackedLength as Worker
import PackedVecLibrary (PackedVec(..), Vec)

payloadRule :: foreach (p :: PackedVec a) -> Vec a (Worker.packageLength p)
payloadRule (PackVec _ xs) = xs

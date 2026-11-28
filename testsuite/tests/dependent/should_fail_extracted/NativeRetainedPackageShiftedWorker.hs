{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedPackageShiftedWorker where
import NativeRetainedPackageShiftedWitness (shiftedWitness)
import RetainedNat (Retained(..))

projectShifted :: foreach (q :: Retained p) -> p (shiftedWitness q)
projectShifted (Retain _ val) = val

{-# LANGUAGE GADTs, NoDependentTypes #-}
module NativeRetainedPackageShiftedWitness (shiftedWitness) where
import PackedVecLibrary (Nat(..))
import RetainedNat (Retained(..))

shiftedWitness :: Retained p -> Nat
shiftedWitness (Retain n _) = S n

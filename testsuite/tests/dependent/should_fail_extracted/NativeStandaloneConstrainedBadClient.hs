{-# LANGUAGE NoDependentTypes #-}
module NativeStandaloneConstrainedBadClient where
import NativeStandaloneConstrainedFamilyProvider
bad :: ElemTop [Int] -> ()
bad _ = ()

{-# LANGUAGE DependentTypes #-}
module NativeProposal378BadInferredAppLocal where
import NativeProposal378QuantifiersProvider (Nat(..), Vec(..))
localInf :: foreach {k :: Nat}. Vec Char k -> Nat
localInf _ = k
bad :: Nat
bad = localInf @Zero VNil

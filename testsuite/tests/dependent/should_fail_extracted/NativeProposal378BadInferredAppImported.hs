{-# LANGUAGE DependentTypes #-}
module NativeProposal378BadInferredAppImported where
import NativeProposal378QuantifiersProvider (Nat(..), Vec(..), onlyInferred)
bad :: Nat
bad = onlyInferred @Char @Zero VNil

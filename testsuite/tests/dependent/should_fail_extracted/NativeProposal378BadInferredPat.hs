{-# LANGUAGE DependentTypes #-}
module NativeProposal378BadInferredPat where
import NativeProposal378QuantifiersProvider (Nat(..), Vec(..))
badPat :: foreach {k :: Nat}. Vec Char k -> Nat
badPat @n _ = n

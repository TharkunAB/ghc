{-# LANGUAGE DependentTypes #-}
module NativeProposal378ForEachInValExpr where
import NativeProposal378SyntaxAndFlagsProvider (Nat, Vec)
x = (foreach (n :: Nat) -> Vec Int n)

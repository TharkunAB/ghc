{-# LANGUAGE DataKinds, GADTs, ImpredicativeTypes, NoDependentTypes, ScopedTypeVariables #-}
module NativeNamedSplitAppendQL where
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (CheckedSplit, Nat(..), Vec, appendVec, toList)

appendIndependent :: CheckedSplit Bool ('S 'Z) ('S 'Z)
                  -> CheckedSplit Bool ('S 'Z) ('S ('S 'Z)) -> [Bool]
appendIndependent (Pair _ (_, a :: Vec Bool n, _)) (Pair _ (_, b :: Vec Bool m, _)) =
    toList (appendVec a b)

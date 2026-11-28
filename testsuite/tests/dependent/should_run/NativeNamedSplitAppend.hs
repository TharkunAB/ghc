{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, NoImpredicativeTypes, ScopedTypeVariables #-}
module NativeNamedSplitAppend where
import GHC.Sigma (Sigma(Pair))
import NativeNamedSplitOpaque (HiddenVec)
import PackedVecLibrary (CheckedSplit, Nat(..), Vec, appendVec, toList)

appendIndependent :: CheckedSplit Bool ('S 'Z) ('S 'Z)
                  -> CheckedSplit Bool ('S 'Z) ('S ('S 'Z)) -> [Bool]
appendIndependent (Pair _ (_, a :: Vec Bool n, _)) (Pair _ (_, b :: Vec Bool m, _)) =
    toList (appendVec a b)

appendOpaque :: Vec Bool 'Z -> HiddenVec -> [Bool]
appendOpaque left (Pair _ right) = toList (appendVec left right)

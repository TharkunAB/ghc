{-# LANGUAGE AllowAmbiguousTypes, DataKinds, GADTs, ImpredicativeTypes, NoDependentTypes #-}
module NativeNamedSplitIncompletePlan where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import NativeNamedSplitOpaque (Fold)
import PackedVecLibrary (CheckedSplit, Nat(..), Vec, appendVec, toList)

unknown :: Vec Bool n -> Vec Bool m -> Vec Bool (Fold hidden)
unknown _ _ = error "hidden index"

bad :: CheckedSplit Bool ('S 'Z) ('S ('S 'Z)) -> [Bool]
bad (Pair _ (left, right, _)) = toList (unknown left right)

wrongLength :: CheckedSplit Bool ('S 'Z) ('S ('S 'Z)) -> Vec Bool 'Z
wrongLength (Pair _ (left, right, Refl)) = appendVec left right

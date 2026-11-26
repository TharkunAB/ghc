{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoDependentTypes, ScopedTypeVariables #-}
module NativeNamedSplitScope where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import NativeNamedSplitOpaque (Fold, HiddenVec, Opaque, Token)
import PackedVecLibrary (CheckedSplit, Nat(..), Vec, toList, zipVec)

independent :: CheckedSplit Bool ('S 'Z) ('S 'Z) -> CheckedSplit Bool ('S 'Z) ('S 'Z) -> Bool
independent (Pair _ (_, _ :: Vec Bool same, _)) (Pair _ (_, _ :: Vec Bool same, _)) = True

escape :: forall outside. CheckedSplit Bool ('S 'Z) ('S 'Z) -> Vec Bool outside
escape (Pair _ (_, rhs :: Vec Bool suffix, _)) = rhs

lazyProof :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
lazyProof (Pair _ ~(_, _ :: Vec Bool suffix, Refl)) (_ :: Vec Bool ('S suffix)) = True

nonInjective :: Opaque -> Bool
nonInjective (Pair _ (_ :: Token suffix)) = True

joinDistinct :: CheckedSplit Bool ('S 'Z) ('S 'Z)
             -> CheckedSplit Bool ('S 'Z) ('S ('S 'Z)) -> [(Bool, Bool)]
joinDistinct (Pair _ (_, a, _)) (Pair _ (_, b, _)) = toList (zipVec a b)

atNamedIndex :: Proxy n -> Vec Bool (Fold n) -> Bool
atNamedIndex _ _ = True

invert :: forall (other :: Nat). Proxy other -> HiddenVec -> Bool
invert selected (Pair _ cells) = atNamedIndex selected cells

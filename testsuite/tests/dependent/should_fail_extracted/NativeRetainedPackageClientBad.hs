{-# LANGUAGE BangPatterns, DataKinds, GADTs, NoDependentTypes, PolyKinds, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module NativeRetainedPackageClientBad where
import ConservedPartition (Split, partitionVec)
import Data.Type.Equality ((:~:)(Refl))
import DependentPair (DPair(..))
import qualified GHC.Sigma as S
import PackedVecLibrary
import RetainedNat
import StoredVector (ListVector)

data Flag b where
  Yes :: Flag 'True
  No  :: Flag 'False

wrongPair :: DPair Bool Flag
wrongPair = Pair True No

escapePair :: DPair a p -> p w
escapePair (Pair _ val) = val

newtype Box (n :: Nat) = Box { boxContents :: Vec Int n }
escapeBox :: Retained Box -> Vec Int m
escapeBox p = withRetained p (\_ box -> boxContents box)
escapePat :: Retained Box -> Vec Int m
escapePat p = withRetained p (\_ box -> case box of Box xs -> xs)

lazyProof :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
lazyProof (S.Pair Z ~(_, _, Refl)) (_ :: Vec Bool ('S 'Z)) = True
lazyProof _ _ = False

reversed :: Vec Bool total -> CheckedSplit Bool ('S 'Z) total -> Bool
reversed (_ :: Vec Bool ('S 'Z)) (S.Pair Z !(_, _, Refl)) = True
reversed _ _ = False

escapeSplit :: CheckedSplit Bool ('S 'Z) ('S 'Z) -> Vec Bool suffix
escapeSplit (S.Pair _ (_, right, Refl)) = right

keepOnly :: Vec Int n -> Vec Int n
keepOnly xs = case partitionVec even xs of
    S.Pair _ (S.Pair _ (kept, _, Refl)) -> kept

wrongTotal :: Split Int ('S 'Z)
wrongTotal = S.Pair Z (S.Pair Z (VNil, VNil, Refl))

badCut :: (Vec Int ('S ('S 'Z)), Vec Int 'Z)
badCut = splitAtVec (S (S Z)) (VCons (7 :: Int) VNil)

badSuffix :: (Vec Int ('S 'Z), Vec Int ('S ('S 'Z)))
badSuffix = splitAtVec (S Z) (VCons (7 :: Int) (VCons 9 VNil))

badTake :: Vec Int 'Z
badTake = takeVec (S Z) (VCons 7 VNil)

badDrop :: Vec Int 'Z
badDrop = dropVec (S Z) (VCons 7 (VCons 9 VNil))

omitProof :: Vec a cut -> Vec a suffix -> (Add cut suffix :~: total) -> Vec a total
omitProof left right _ = appendVec left right

wrongList :: [a] -> [a] -> ListVector a
wrongList xs ys = S.Pair (sourceLength xs) (sourceVector ys)

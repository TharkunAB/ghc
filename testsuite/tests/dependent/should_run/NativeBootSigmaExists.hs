{-# LANGUAGE DataKinds, DependentTypes #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Werror #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import GHC.Exists
import GHC.Sigma (Sigma(..), first, second)
import GHC.Tuple (Tuple2, Unit)
import NativeBootSigmaExistsProvider
import NativeBootSigmaExistsOrdinary (checkOrdinaryCPair)
import Prelude

bottomNat :: Nat
bottomNat = error "witness forced"

bottomVec :: Vec Char n
bottomVec = error "payload forced"

localDependentZip :: forall a. foreach (p :: PackedVec a) -> Vec (Tuple2 a a) (first p)
localDependentZip p = zipVec (second p) (second p)

lazySecondOf :: ConstInt -> Int
lazySecondOf p = second p

lazyVecPair :: PackedVec Char
lazyVecPair =
  let n0 = bottomNat
  in Pair (S n0) (VCons 'k' (bottomVec :: Vec Char n0))
{-# NOINLINE lazyVecPair #-}

main :: IO Unit
main = do
  let packed :: PackedVec Char
      packed = Pair (S (S Z)) (VCons 'a' (VCons 'b' VNil))
      zipped = vecToList (zipVec (second packed) (second packed))
      localZipped = vecToList (localDependentZip @Char packed)
      viaExportedZip = vecToList (checkDependentZip @Char packed)
      lazyFirst = first (Pair (S Z) (bottomVec :: Vec Char ('S 'Z)) :: PackedVec Char)
      constPair :: ConstInt
      constPair = Pair bottomNat 42
      lazySecond = lazySecondOf constPair
      lazyHead = take 1 (vecToList (second lazyVecPair))
      rendered = map renderShowable showables
      nested = showTwo (MkCPair (MkCPair (7 :: Int)))
      barStr = openBarShowable barShowable
      plainVal = discardEvidence (MkCPair (123 :: Int) :: Show Int /\ Int)
      lazyDiscard = const True (discardEvidence (MkCPair (error "payload unused") :: Show Int /\ Int))
  res <- evaluate
    ( zipped == [('a', 'a'), ('b', 'b')]
    && localZipped == [('a', 'a'), ('b', 'b')]
    && viaExportedZip == [('a', 'a'), ('b', 'b')]
    && lazyFirst == S Z
    && lazySecond == 42
    && lazyHead == "k"
    && rendered == ["42", "True", "\"ok\""]
    && nested == (True, "7")
    && barStr == "99"
    && plainVal == 123
    && lazyDiscard
    && checkOrdinaryCPair
    )
  if not res
    then error "NativeBootSigmaExists observation mismatch"
    else pure ()
  -- Verify strict constructor match on bottom package still raises the exception.
  caught <- try (evaluate (discardEvidence (error "bottom cpair" :: Show Int /\ Int))) :: IO (Either ErrorCall Int)
  case caught of
    Left _  -> pure ()
    Right _ -> error "expected bottom cpair exception"

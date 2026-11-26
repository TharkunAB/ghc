{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}

module Main (main) where

import Data.Type.Equality ( (:~:) (Refl), gcastWith, sym )
import Dependent.ObsEq
  ( ObsConfig
  , castEq
  , checkObsEq
  , defaultObsConfig
  , obsEq
  , setApproxDepth
  , setMaxSize
  , setPartialPct
  , setTests
  , setTimeoutUs
  , totalObsConfig
  , usingEq
  )
import ObsProvider ( Nat (..), natToInt, plus, stepUnfoldAdd, whileAdd )

-- 1. Non-structural local unfold (lazy-equivalent to plus on partial spines),
--  tail-recursive accumulator (total-domain equivalent), Collatz, and McCarthy 91.
localUnfoldAdd :: Nat -> Nat -> Nat
localUnfoldAdd m n = step (Just m) n
  where
    step Nothing acc = acc
    step (Just Zero) acc = step Nothing acc
    step (Just (Succ k)) acc = Succ (step (Just k) acc)

accPlus :: Nat -> Nat -> Nat
accPlus Zero acc = acc
accPlus (Succ m) acc = accPlus m (Succ acc)

collatz :: Nat -> Nat
collatz Zero = Succ Zero
collatz (Succ Zero) = Succ Zero
collatz n
  | isEven n = collatz (half n)
  | otherwise = collatz (Succ (triple n))
  where
    isEven Zero = True
    isEven (Succ Zero) = False
    isEven (Succ (Succ k)) = isEven k

    half Zero = Zero
    half (Succ Zero) = Zero
    half (Succ (Succ k)) = Succ (half k)

    triple Zero = Zero
    triple (Succ k) = Succ (Succ (Succ (triple k)))

mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

mc91Spec :: Int -> Int
mc91Spec n
  | n > 100 = n - 10
  | otherwise = 91

cfgTotal200 :: ObsConfig
cfgTotal200 = setTests 200 (setMaxSize 16 (setApproxDepth 10 (setTimeoutUs 20000 totalObsConfig)))

-- Observational proofs (with partial-pct = 25 active by default!):
localUnfoldLaw :: foreach (m :: Nat) (n :: Nat) -> localUnfoldAdd m n :~: plus m n
localUnfoldLaw _m _n = obsEq

importedUnfoldLaw :: foreach (m :: Nat) (n :: Nat) -> stepUnfoldAdd m n :~: plus m n
importedUnfoldLaw _m _n = obsEq

mc91Law :: foreach (n :: Int) -> mc91 n :~: mc91Spec n
mc91Law _n = obsEq

-- Total-domain observational proofs via checkObsEq:
accPlusLaw :: foreach (m :: Nat) (n :: Nat) -> accPlus m n :~: plus m n
accPlusLaw _m _n = checkObsEq cfgTotal200

importedWhileLaw :: foreach (m :: Nat) (n :: Nat) -> whileAdd m n :~: plus m n
importedWhileLaw _m _n = checkObsEq (setPartialPct 0 (setTests 200 defaultObsConfig))

collatzLaw :: foreach (n :: Nat) -> collatz n :~: Succ Zero
collatzLaw _n = checkObsEq cfgTotal200

collatzLawInline :: foreach (n :: Nat) -> collatz n :~: Succ Zero
collatzLawInline _n = checkObsEq (setPartialPct 0 (setTests 120 defaultObsConfig))

-- Runtime indexed transport via castEq, usingEq, gcastWith, and localUnfoldLaw:
data Vec a (n :: Nat) where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

vSum :: Vec Int n -> Int
vSum VNil = 0
vSum (VCons x xs) = x + vSum xs

toUnfoldVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (localUnfoldAdd m n)
toUnfoldVec _m _n v = castEq obsEq v

fromUnfoldVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (localUnfoldAdd m n) -> Vec Int (plus m n)
fromUnfoldVec m n v = case sym (localUnfoldLaw m n) of
  Refl -> v

roundtripUnfoldVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (plus m n)
roundtripUnfoldVec m n v = fromUnfoldVec m n (toUnfoldVec m n v)

toAccVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (accPlus m n)
toAccVec m n v = gcastWith (checkObsEq cfgTotal200 :: plus m n :~: accPlus m n) v

fromAccVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (accPlus m n) -> Vec Int (plus m n)
fromAccVec m n v = usingEq (accPlusLaw m n) v

roundtripAccVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (plus m n)
roundtripAccVec m n v = fromAccVec m n (toAccVec m n v)

runIntChecks :: Int -> IO ()
runIntChecks x =
  case mc91Law x of
    Refl -> pure ()

main :: IO ()
main = do
  let baseUnfold :: Vec Int (Succ (Succ (Succ Zero)))
      baseUnfold = VCons 7 (VCons 8 (VCons 9 VNil))
      baseAcc :: Vec Int (Succ (Succ (Succ Zero)))
      baseAcc = VCons 3 (VCons 4 (VCons 5 VNil))
      two = Succ (Succ Zero)
      one = Succ Zero

  if vSum (roundtripUnfoldVec (Succ (Succ Zero)) (Succ Zero) baseUnfold) /= 24
    || vSum (roundtripAccVec (Succ (Succ Zero)) (Succ Zero) baseAcc) /= 12
    || natToInt (plus two one) /= 3
    then error "NativeObsEqPlugin: transport mismatch"
    else pure ()

  runIntChecks 42

  case (importedUnfoldLaw two one, importedWhileLaw two one, collatzLaw two, collatzLawInline one) of
    (Refl, Refl, Refl, Refl) -> pure ()

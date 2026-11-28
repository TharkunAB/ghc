{-# LANGUAGE DependentTypes #-}

-- | Spectral runtime benchmark comparing Dependent Haskell indexed state-window
-- Fibonacci recurrence against a paired plain-Haskell implementation.
--
-- Default invocation runs the dependent kernel for testsuite stats collection.
-- Passing "plain" runs the paired non-dependent kernel on identical inputs;
-- passing "verify" asserts both kernels produce the exact same checksum.
module Main (main) where

import Data.Kind (Type)
import GHC.Exists (Exists, data Pack, withExists)
import GHC.Tuple (Unit)
import System.Environment (getArgs)

-- | Peano natural numbers.
data Nat = Zero | Succ Nat

-- | Bounded positions in a length-indexed vector.
type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

-- | Length-indexed strict Int vector.
type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil  :: Vec a Zero
  VCons :: !a -> !(Vec a n) -> Vec a (Succ n)

-- | Unindexed plain-Haskell twin vector.
data PlainVec a
  = PNil
  | PCons !a !(PlainVec a)

fibMod :: Int
fibMod = 1000000007

-- | Safe Fin-indexed lookup on Vec.
lookupVec :: Fin n -> Vec a n -> a
lookupVec FZ (VCons x _) = x
lookupVec (FS i) (VCons _ xs) = lookupVec i xs

-- | Advance a 4-state Fibonacci recurrence window while preserving length 4.
type N4 = Succ (Succ (Succ (Succ Zero)))

stepDepWindow :: Vec Int N4 -> Vec Int N4
stepDepWindow (VCons a (VCons b (VCons c (VCons d VNil)))) =
  let next = (a + b + c + d) `rem` fibMod
  in VCons b (VCons c (VCons d (VCons next VNil)))

iterDepWindow :: Int -> Vec Int N4 -> Vec Int N4
iterDepWindow 0 w = w
iterDepWindow k w = iterDepWindow (k - 1) (stepDepWindow w)

buildDepPrefix :: Int -> Exists (Vec Int)
buildDepPrefix n = go n 1 1
  where
    go :: Int -> Int -> Int -> Exists (Vec Int)
    go 0 _ _ = Pack (type Zero) VNil
    go k a b =
      let c = (a + b) `rem` fibMod
      in withExists (go (k - 1) b c)
           (\len rest -> (Pack (type (Succ len)) (VCons a rest) :: Exists (Vec Int)))

sumDepVec :: Vec Int n -> Int
sumDepVec = go 0
  where
    go :: Int -> Vec Int m -> Int
    go !acc VNil = acc
    go !acc (VCons x xs) = go ((acc + x) `rem` fibMod) xs

{-# NOINLINE depFibStep #-}
depFibStep :: Int -> Int
depFibStep seed =
  let s = seed `rem` 97
      w0 :: Vec Int N4
      w0 = VCons (s + 1) (VCons (s + 2) (VCons (s + 3) (VCons (s + 5) VNil)))
      wFinal = iterDepWindow 32 w0
      tap = (lookupVec FZ wFinal + lookupVec (FS (FS (FS FZ))) wFinal) `rem` fibMod
      pref = withExists (buildDepPrefix 24) (\_ v -> sumDepVec v)
  in (tap + pref) `rem` fibMod

{-# NOINLINE depFibBench #-}
depFibBench :: Int -> Int
depFibBench iters = go 0 iters
  where
    go !acc 0 = acc
    go !acc i = go ((acc + depFibStep i) `rem` fibMod) (i - 1)

-- Paired plain-Haskell implementation

lookupPlain :: Int -> PlainVec a -> a
lookupPlain 0 (PCons x _) = x
lookupPlain k (PCons _ xs) = lookupPlain (k - 1) xs
lookupPlain _ PNil = error "lookupPlain: out of bounds"

stepPlainWindow :: PlainVec Int -> PlainVec Int
stepPlainWindow (PCons a (PCons b (PCons c (PCons d PNil)))) =
  let next = (a + b + c + d) `rem` fibMod
  in PCons b (PCons c (PCons d (PCons next PNil)))
stepPlainWindow _ = error "stepPlainWindow: invalid window length"

iterPlainWindow :: Int -> PlainVec Int -> PlainVec Int
iterPlainWindow 0 w = w
iterPlainWindow k w = iterPlainWindow (k - 1) (stepPlainWindow w)

buildPlainPrefix :: Int -> PlainVec Int
buildPlainPrefix n = go n 1 1
  where
    go 0 _ _ = PNil
    go k a b =
      let c = (a + b) `rem` fibMod
      in PCons a (go (k - 1) b c)

sumPlainVec :: PlainVec Int -> Int
sumPlainVec = go 0
  where
    go !acc PNil = acc
    go !acc (PCons x xs) = go ((acc + x) `rem` fibMod) xs

{-# NOINLINE plainFibStep #-}
plainFibStep :: Int -> Int
plainFibStep seed =
  let s = seed `rem` 97
      w0 = PCons (s + 1) (PCons (s + 2) (PCons (s + 3) (PCons (s + 5) PNil)))
      wFinal = iterPlainWindow 32 w0
      tap = (lookupPlain 0 wFinal + lookupPlain 3 wFinal) `rem` fibMod
      pref = sumPlainVec (buildPlainPrefix 24)
  in (tap + pref) `rem` fibMod

{-# NOINLINE plainFibBench #-}
plainFibBench :: Int -> Int
plainFibBench iters = go 0 iters
  where
    go !acc 0 = acc
    go !acc i = go ((acc + plainFibStep i) `rem` fibMod) (i - 1)

main :: IO Unit
main = do
  args <- getArgs
  let n = 10000
  case args of
    ["plain"] -> print (plainFibBench n)
    ["verify"] -> do
      let d = depFibBench n
          p = plainFibBench n
      if d == p
        then print d
        else error ("Mismatch: dep=" ++ show d ++ " plain=" ++ show p)
    _ -> print (depFibBench n)

{-# LANGUAGE DependentTypes #-}

-- | Spectral runtime benchmark comparing Dependent Haskell length-indexed
-- vector append, split, reversal, and inner product against a paired
-- plain-Haskell implementation.
module Main (main) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Tuple (Unit)
import System.Environment (getArgs)

-- | Peano natural numbers.
data Nat = Zero | Succ Nat

-- | Strict pair type avoiding tuple pun ambiguity under NoListTuplePuns.
data Pair2 a b = MkPair2 !a !b

-- | Length-indexed strict vector.
type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil  :: Vec a Zero
  VCons :: !a -> !(Vec a n) -> Vec a (Succ n)

-- | Unindexed plain-Haskell twin vector.
data PlainVec a
  = PNil
  | PCons !a !(PlainVec a)

vecMod :: Int
vecMod = 1000000007

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

congSucc :: a :~: b -> Succ a :~: Succ b
congSucc Refl = Refl

plusZeroR :: foreach (n :: Nat) -> plus n Zero :~: n
plusZeroR Zero = Refl
plusZeroR (Succ n) = congSucc (plusZeroR n)

plusSuccR :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSuccR Zero _ = Refl
plusSuccR (Succ m) n = congSucc (plusSuccR m n)

castVec :: m :~: n -> Vec a m -> Vec a n
castVec Refl xs = xs

intToNat :: Int -> Nat
intToNat n
  | n <= 0 = Zero
  | otherwise = Succ (intToNat (n - 1))

buildDepVec :: foreach (n :: Nat) -> Int -> Int -> Vec Int n
buildDepVec Zero _ _ = VNil
buildDepVec (Succ n) cur step =
  let next = (cur + step) `rem` 1021
  in VCons cur (buildDepVec n next step)

appendDepVec :: Vec a m -> Vec a n -> Vec a (plus m n)
appendDepVec VNil ys = ys
appendDepVec (VCons x xs) ys = VCons x (appendDepVec xs ys)

splitDepVec :: forall a n. foreach (m :: Nat) -> Vec a (plus m n) -> Pair2 (Vec a m) (Vec a n)
splitDepVec Zero xs = MkPair2 VNil xs
splitDepVec (Succ m) (VCons x xs) =
  case splitDepVec @a @n m xs of
    MkPair2 pre suf -> MkPair2 (VCons x pre) suf

zipAddDepVec :: Vec Int n -> Vec Int n -> Vec Int n
zipAddDepVec VNil VNil = VNil
zipAddDepVec (VCons x xs) (VCons y ys) =
  VCons ((x * 5 + y * 3 + 1) `rem` vecMod) (zipAddDepVec xs ys)

revAccDepVec :: forall a. foreach (m :: Nat) -> foreach (n :: Nat) -> Vec a m -> Vec a n -> Vec a (plus m n)
revAccDepVec Zero _ VNil acc = acc
revAccDepVec (Succ m) n (VCons x xs) acc =
  case plusSuccR m n of
    Refl -> revAccDepVec @a m (Succ n) xs (VCons x acc)

reverseDepVec :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
reverseDepVec n xs =
  case plusZeroR n of
    Refl -> revAccDepVec @a n Zero xs VNil

dotDepVec :: Vec Int n -> Vec Int n -> Int
dotDepVec = go 0
  where
    go :: Int -> Vec Int m -> Vec Int m -> Int
    go !acc VNil VNil = acc
    go !acc (VCons x xs) (VCons y ys) = go ((acc + x * y) `rem` vecMod) xs ys

{-# NOINLINE depVecStep #-}
depVecStep :: foreach (len :: Nat) -> Int -> Int
depVecStep len seed =
  let a :: Vec Int len
      a = buildDepVec len (seed `rem` 251) 7
      b :: Vec Int len
      b = buildDepVec len ((seed * 3) `rem` 251) 11
      ab :: Vec Int (plus len len)
      ab = appendDepVec a b
      splitRes :: Pair2 (Vec Int len) (Vec Int len)
      splitRes = splitDepVec @Int @len len ab
  in case splitRes of
    MkPair2 left right ->
      let revRight :: Vec Int len
          revRight = reverseDepVec @Int len right
          mixed :: Vec Int len
          mixed = zipAddDepVec left revRight
      in dotDepVec mixed a

{-# NOINLINE depVecBench #-}
depVecBench :: Int -> Int
depVecBench iters =
  let n32 = intToNat 32
      go !acc 0 = acc
      go !acc i = go ((acc + depVecStep n32 i) `rem` vecMod) (i - 1)
  in go 0 iters

-- Paired plain-Haskell implementation

buildPlainVec :: Int -> Int -> Int -> PlainVec Int
buildPlainVec 0 _ _ = PNil
buildPlainVec k cur step =
  let next = (cur + step) `rem` 1021
  in PCons cur (buildPlainVec (k - 1) next step)

appendPlainVec :: PlainVec a -> PlainVec a -> PlainVec a
appendPlainVec PNil ys = ys
appendPlainVec (PCons x xs) ys = PCons x (appendPlainVec xs ys)

splitPlainVec :: Int -> PlainVec a -> Pair2 (PlainVec a) (PlainVec a)
splitPlainVec 0 xs = MkPair2 PNil xs
splitPlainVec k (PCons x xs) =
  case splitPlainVec (k - 1) xs of
    MkPair2 pre suf -> MkPair2 (PCons x pre) suf
splitPlainVec _ PNil = error "splitPlainVec: short vector"

zipAddPlainVec :: PlainVec Int -> PlainVec Int -> PlainVec Int
zipAddPlainVec PNil PNil = PNil
zipAddPlainVec (PCons x xs) (PCons y ys) =
  PCons ((x * 5 + y * 3 + 1) `rem` vecMod) (zipAddPlainVec xs ys)
zipAddPlainVec _ _ = error "zipAddPlainVec: length mismatch"

reversePlainVec :: PlainVec a -> PlainVec a
reversePlainVec = go PNil
  where
    go !acc PNil = acc
    go !acc (PCons x xs) = go (PCons x acc) xs

dotPlainVec :: PlainVec Int -> PlainVec Int -> Int
dotPlainVec = go 0
  where
    go !acc PNil PNil = acc
    go !acc (PCons x xs) (PCons y ys) = go ((acc + x * y) `rem` vecMod) xs ys
    go _ _ _ = error "dotPlainVec: length mismatch"

{-# NOINLINE plainVecStep #-}
plainVecStep :: Int -> Int -> Int
plainVecStep len seed =
  let a = buildPlainVec len (seed `rem` 251) 7
      b = buildPlainVec len ((seed * 3) `rem` 251) 11
      ab = appendPlainVec a b
      MkPair2 left right = splitPlainVec len ab
      revRight = reversePlainVec right
      mixed = zipAddPlainVec left revRight
  in dotPlainVec mixed a

{-# NOINLINE plainVecBench #-}
plainVecBench :: Int -> Int
plainVecBench iters = go 0 iters
  where
    go !acc 0 = acc
    go !acc i = go ((acc + plainVecStep 32 i) `rem` vecMod) (i - 1)

main :: IO Unit
main = do
  args <- getArgs
  let n = 4000
  case args of
    ["plain"] -> print (plainVecBench n)
    ["verify"] -> do
      let d = depVecBench n
          p = plainVecBench n
      if d == p
        then print d
        else error ("Mismatch: dep=" ++ show d ++ " plain=" ++ show p)
    _ -> print (depVecBench n)

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs #-}
-- | Parameterized dependent record definitions for record update testing.
module NativeParamDependentRecordUpdateProvider
  ( Nat (..)
  , Vec (..)
  , natToInt
  , toList
  , mapVec
  , zipVec
  , Columns (..)
  , PairCols (..)
  , SameCols (..)
  ) where

data Nat = Z | S Nat
  deriving (Eq, Show)

data Vec a (n :: Nat) where
  VNil  :: Vec a 'Z
  VCons :: a -> Vec a n -> Vec a ('S n)

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S n) = 1 + natToInt n

toList :: Vec a n -> [a]
toList VNil         = []
toList (VCons x xs) = x : toList xs

mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec _ VNil         = VNil
mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)

zipVec :: Vec a n -> Vec b n -> Vec (a, b) n
zipVec VNil         VNil         = VNil
zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)

data Columns a = Columns
  { storedLength :: Nat
  , cells        :: Vec a storedLength
  }

data PairCols a b = PairCols
  { pairLength :: Nat
  , leftCells  :: Vec a pairLength
  , rightCells :: Vec b pairLength
  }

data SameCols a = SameCols
  { sameLength  :: Nat
  , firstCells  :: Vec a sameLength
  , secondCells :: Vec a sameLength
  }

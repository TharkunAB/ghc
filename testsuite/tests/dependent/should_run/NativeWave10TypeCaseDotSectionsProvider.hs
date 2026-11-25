{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeOperators #-}
-- | Provider declarations for runtime Type case, dependent dot, T2T sections,
-- and superclass selectors.
module NativeWave10TypeCaseDotSectionsProvider
  ( Nat (..)
  , plus
  , (+)
  , natToInt
  , Vec (..)
  , vReplicate
  , mapVec
  , zipVec
  , toList
  , Columns (..)
  , ColBox (..)
  , Choose (..)
  , SubChoose (..)
  , SubSubChoose (..)
  , AltSubChoose (..)
  , Tagged (..)
  , tagBool
  , chooseTagged
  ) where

import Prelude hiding ((+))
import qualified Prelude as P

data Nat = Z | S Nat
  deriving (Eq, Show)

plus :: Nat -> Nat -> Nat
plus Z     n = n
plus (S m) n = S (plus m n)

infixl 6 +
(+) :: Nat -> Nat -> Nat
Z   + n = n
S m + n = S (m + n)

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S n) = 1 P.+ natToInt n

data Vec a (n :: Nat) where
  VNil  :: Vec a 'Z
  VCons :: a -> Vec a n -> Vec a ('S n)

vReplicate :: foreach (n :: Nat) -> a -> Vec a n
vReplicate Z     _ = VNil
vReplicate (S k) x = VCons x (vReplicate k x)

mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec _ VNil         = VNil
mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)

zipVec :: Vec a n -> Vec b n -> Vec (a, b) n
zipVec VNil         VNil         = VNil
zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)

toList :: Vec a n -> [a]
toList VNil         = []
toList (VCons x xs) = x : toList xs

data Columns a = Columns
  { storedLength :: Nat
  , cells        :: Vec a storedLength
  }

data ColBox a = ColBox
  { boxTag    :: Int
  , innerCols :: Columns a
  }

class Choose a where
  choose :: a -> Bool

class Choose a => SubChoose a where
  subLabel :: a -> Int

class SubChoose a => SubSubChoose a where
  subSubLabel :: a -> Int

class Choose a => AltSubChoose a where
  altLabel :: a -> Int

instance Choose Bool where
  choose b = b

instance SubChoose Bool where
  subLabel b = if b then 1 else 0

instance SubSubChoose Bool where
  subSubLabel b = if b then 10 else 20

data Tagged (b :: Bool) where
  Tagged :: Bool -> Tagged b

tagBool :: foreach (b :: Bool) -> Tagged b
tagBool b = Tagged @b b

chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)

{-# LANGUAGE DataKinds, DependentTypes #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Werror #-}
module NativeAutoNumLiteralsProvider
  ( Peano(..)
  , plusP
  , timesP
  , VecP(..)
  , replicateP
  , appendP
  , vecPToList
  , ThreeTruesVec
  , sameModPlusProof
  , sameModZeroProof
  ) where

import Control.Exception (ArithException(Underflow), throw)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (List)
import Prelude

-- | A user-defined Peano natural with no FromNatural imports or instances.
data Peano = PZero | PSucc Peano
  deriving (Eq, Show)

-- | Add two Peano naturals.
plusP :: Peano -> Peano -> Peano
plusP PZero m = m
plusP (PSucc n) m = PSucc (plusP n m)

-- | Multiply two Peano naturals.
timesP :: Peano -> Peano -> Peano
timesP PZero _ = PZero
timesP (PSucc n) m = plusP m (timesP n m)

instance Num Peano where
  (+) = plusP
  (*) = timesP
  PZero - PZero = PZero
  PSucc n - PZero = PSucc n
  PSucc n - PSucc m = n - m
  PZero - PSucc _ = throw Underflow
  negate PZero = PZero
  negate (PSucc _) = throw Underflow
  abs n = n
  signum PZero = PZero
  signum (PSucc _) = PSucc PZero
  fromInteger n
    | n < 0     = throw Underflow
    | n == 0    = PZero
    | otherwise = PSucc (fromInteger (n - 1))

-- | A vector indexed by Peano.
data VecP a (n :: Peano) where
  VPNil  :: VecP a 'PZero
  VPCons :: a -> VecP a n -> VecP a ('PSucc n)

-- | Replicate an element n times at runtime and at the type level.
replicateP :: foreach (n :: Peano) -> a -> VecP a n
replicateP PZero _ = VPNil
replicateP (PSucc n) x = VPCons x (replicateP n x)

-- | Concatenate two Peano-indexed vectors.
appendP :: VecP a m -> VecP a n -> VecP a (plusP m n)
appendP VPNil ys = ys
appendP (VPCons x xs) ys = VPCons x (appendP xs ys)

-- | Convert a Peano-indexed vector to a list.
vecPToList :: VecP a n -> List a
vecPToList VPNil = []
vecPToList (VPCons x xs) = x : vecPToList xs

-- | Same-module type synonym in a subsequent TyClGroup using a promoted numeral.
type ThreeTruesVec = VecP Bool 3

-- | Same-module type-level numeral promotion using only instance Num Peano.
sameModPlusProof :: plusP 2 3 :~: 5
sameModPlusProof = Refl

-- | Same-module zero numeral promotion.
sameModZeroProof :: (0 :: Peano) :~: 'PZero
sameModZeroProof = Refl

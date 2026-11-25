{-# LANGUAGE DataKinds, DependentTypes #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Werror #-}
module NativeBootSigmaExistsProvider
  ( Nat(..)
  , Vec(..)
  , PackedVec
  , headVec
  , zipVec
  , vecToList
  , ConstInt
  , showables
  , renderShowable
  , TwoConstraints
  , showTwo
  , barShowable
  , openBarShowable
  , checkDependentZip
  ) where

import GHC.Exists
import GHC.Sigma (first, second)
import GHC.Tuple (Tuple2)
import GHC.Types (List)
import Prelude (Bool(..), Eq(..), Int, Show(..), String)

-- | A Peano natural number.
data Nat = Z | S Nat
  deriving (Eq, Show)

-- | A length-indexed vector.
data Vec a (n :: Nat) where
  VNil  :: Vec a 'Z
  VCons :: a -> Vec a n -> Vec a ('S n)

-- | A vector paired with its runtime length.
type PackedVec a = some (n :: Nat) | Vec a n

-- | Return the first element of a nonempty vector.
headVec :: Vec a ('S n) -> a
headVec (VCons x _) = x

-- | Pair corresponding elements of two vectors of equal length.
zipVec :: Vec a n -> Vec b n -> Vec (Tuple2 a b) n
zipVec VNil VNil = VNil
zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)

-- | Convert a vector to an ordinary list.
vecToList :: Vec a n -> List a
vecToList VNil = []
vecToList (VCons x xs) = x : vecToList xs

-- | A dependent pair whose payload ignores its witness.
type ConstInt = some (n :: Nat) | Int

-- | Heterogeneous values paired with Show dictionaries.
showables :: List (exists a. Show a /\ a)
showables = [MkCPair (42 :: Int), MkCPair True, MkCPair ("ok" :: String)]

-- | Format one constrained existential package.
renderShowable :: (exists a. Show a /\ a) -> String
renderShowable (MkCPair x) = show x

-- | Right-nested constraint pairing using infixr 2 fixity.
type TwoConstraints a = Eq a /\ Show a /\ a

-- | Open nested constraint evidence.
showTwo :: (exists a. TwoConstraints a) -> Tuple2 Bool String
showTwo (MkCPair (MkCPair x)) = (x == x, show x)

-- | Explicit bar existential package carrying Show evidence.
barShowable :: exists a | Show a /\ a
barShowable = Pack (type Int) (MkCPair 99)

-- | Eliminate an explicit bar existential package carrying Show evidence.
openBarShowable :: (exists a | Show a /\ a) -> String
openBarShowable pkg = case pkg of
  Pack (type _) (MkCPair x) -> show x

-- | Project and zip the payload of a retained Sigma pair at its own witness.
checkDependentZip :: foreach (p :: PackedVec a) -> Vec (Tuple2 a a) (first p)
checkDependentZip p = zipVec (second p) (second p)

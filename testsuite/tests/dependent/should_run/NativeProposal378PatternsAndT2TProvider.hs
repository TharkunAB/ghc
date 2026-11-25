{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, PolyKinds #-}
{-# LANGUAGE RankNTypes, RequiredTypeArguments, ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions, TypeApplications, TypeFamilies #-}
module NativeProposal378PatternsAndT2TProvider
  ( Nat (..)
  , plus
  , Vec (..)
  , vecToList
  , T (..)
  , ShowVec (..)
  , InvisVec (..)
  , mkInvisTrue
  , matchImportedT
  , matchImportedShow
  , matchImportedInvis
  , importedSomeVec
  ) where

import Data.Kind (Type)
import GHC.Sigma (Sigma (Pair))

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

plus :: Nat -> Nat -> Nat
plus Zero     m = m
plus (Succ k) m = Succ (plus k m)

data Vec (a :: Type) (n :: Nat) where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

vecToList :: Vec a n -> [a]
vecToList VNil         = []
vecToList (VCons x xs) = x : vecToList xs

-- Deliverable 3b: Retained GADT constructors mixing forall, constraints, and
-- visible/invisible foreach fields across .hi interfaces.
data T where
  MkT :: forall (a :: Type) -> foreach (n :: Nat) -> Vec a n -> T

data ShowVec where
  MkShow :: forall a. Show a => foreach (n :: Nat) -> Vec a n -> ShowVec

data InvisVec a where
  MkInvis      :: foreach (n :: Nat). Vec a n -> InvisVec a
  MkInvisBrace :: forall {a}. foreach (n :: Nat). Vec a n -> InvisVec a
  MkInvisInf   :: foreach {n :: Nat}. Vec a n -> InvisVec a

mkInvisTrue :: InvisVec Bool
mkInvisTrue = MkInvis (VCons True (VCons False VNil))

matchImportedT :: T -> Nat
matchImportedT (MkT (type _) n _) = n

matchImportedShow :: ShowVec -> String
matchImportedShow (MkShow _ xs) = show (vecToList xs)

matchImportedInvis :: InvisVec a -> (Nat, [a])
matchImportedInvis (MkInvis @_ @n xs)   = (n, vecToList xs)
matchImportedInvis (MkInvisBrace @n xs) = (n, vecToList xs)
matchImportedInvis (MkInvisInf xs)      = (Zero, vecToList xs)

-- Deliverable P1.12.2: Uncaptured inline `some` exported across .hi.
importedSomeVec :: a -> (some (n :: Nat) | Vec a n)
importedSomeVec x = Pair (Succ Zero) (VCons x VNil)

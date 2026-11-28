{-# LANGUAGE DependentTypes #-}
module NativeRetainedScrutineePolyProvider
  ( Nat (..)
  , Vec (..)
  , rev
  , appList
  , mapFlip
  ) where

import Data.Kind (Type)
import GHC.Types (List)

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

data Vec (a :: Type) (n :: Nat) where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

appList :: List a -> List a -> List a
appList [] ys = ys
appList (x : xs) ys = x : appList xs ys

rev :: List a -> List a
rev [] = []
rev (x : xs) = appList (rev xs) [x]

mapFlip :: List Bool -> List Bool
mapFlip [] = []
mapFlip (b : bs) = not b : mapFlip bs

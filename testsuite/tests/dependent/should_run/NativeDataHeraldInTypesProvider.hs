{-# LANGUAGE DependentTypes, NoListTuplePuns #-}
module NativeDataHeraldInTypesProvider
  ( Nat (..)
  , Vec (..)
  , T (..)
  , PairBox (..)
  , (:+:) (..)
  , type (+++)
  , (+++)
  , plus
  ) where

import Data.Kind (Type)
import GHC.Types (List)

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

data Vec (a :: Type) (n :: Nat) where
  VNil  :: Vec a (data Zero)
  VCons :: a -> Vec a n -> Vec a (data (Succ n))

-- Punned type and data constructor pair:
data T = T
  deriving (Eq, Show)

-- Punned infix type and data constructor pair:
data a :+: b = a :+: b
  deriving (Eq, Show)

data PairBox (x :: k) = MkPairBox

-- Punned type synonym operator vs promoted term operator:
type a +++ b = Either a b

infixr 5 +++
(+++) :: List a -> List a -> List a
[] +++ ys = ys
(x : xs) +++ ys = x : (xs +++ ys)

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

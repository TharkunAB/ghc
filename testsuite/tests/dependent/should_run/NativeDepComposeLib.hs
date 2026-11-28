{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}

module NativeDepComposeLib
  ( Nat (..)
  , plus
  , minus
  , Vec (..)
  , toListVec
  , appendVec
  , swapPair
  , fmapMaybe
  , shunt
  , reverseShunt
  , (.)
  , ($)
  , cong
  ) where

import Control.Exception (ArithException (Underflow), throw)
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Matchability (Matchability)
import GHC.TypeNats.Experimental (FromNatural)
import qualified GHC.TypeNats as TN
import Prelude (Eq (..), Maybe (..), Num (..), Ord (..), Show, otherwise)

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

minus :: Nat -> Nat -> Nat
minus Zero     _        = Zero
minus (Succ m) Zero     = Succ m
minus (Succ m) (Succ n) = minus m n

type family ToPeano (n :: TN.Nat) :: Nat where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n TN.- 1))

type instance FromNatural Nat n = ToPeano n

instance Num Nat where
  (+) = plus
  m * n = go m
    where
      go Zero     = Zero
      go (Succ k) = plus n (go k)
  n - Zero        = n
  Zero - Succ _   = throw Underflow
  Succ n - Succ m = n - m
  abs n           = n
  signum Zero     = Zero
  signum (Succ _) = Succ Zero
  fromInteger n
    | n < 0     = throw Underflow
    | n == 0    = Zero
    | otherwise = Succ (fromInteger (n - 1))

data Vec a (n :: Nat) where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

deriving instance Eq a => Eq (Vec a n)

toListVec :: Vec a n -> [a]
toListVec VNil         = []
toListVec (VCons x xs) = x : toListVec xs

appendVec :: Vec a n -> Vec a m -> Vec a (plus n m)
appendVec VNil         ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

swapPair :: forall a b. (a, b) -> (b, a)
swapPair (x, y) = (y, x)

fmapMaybe :: forall a b. (a -> b) -> Maybe a -> Maybe b
fmapMaybe _ Nothing  = Nothing
fmapMaybe f (Just x) = Just (f x)

shunt :: forall a. [a] -> [a] -> [a]
shunt []       acc = acc
shunt (x : xs) acc = shunt xs (x : acc)

reverseShunt :: forall a. [a] -> [a]
reverseShunt xs = shunt xs []

infixr 9 .

(.) :: forall {a} {b} {m :: Matchability} (c :: b %m -> Type).
       (foreach (y :: b) -> c y)
    -> foreach (f :: a -> b)
    -> foreach (x :: a)
    -> c (f x)
(g . f) x = g (f x)

infixr 0 $

($) :: forall {a} {m :: Matchability} (b :: a %m -> Type).
       (foreach (x :: a) -> b x)
    -> foreach (x :: a)
    -> b x
f $ x = f x

cong
  :: forall {a} {b} {m :: Matchability}.
     forall (f :: a %m -> b)
  -> forall (x :: a) (y :: a).
     x :~: y -> f x :~: f y
cong _ Refl = Refl

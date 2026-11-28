{-# LANGUAGE GHC2024, DataKinds, DependentTypes, EmptyCase, ExplicitNamespaces, LambdaCase, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror -fwrite-source-equations #-}
module HaskLibCore
  ( Nat (..), plus, mul, natToInteger
  , Fin (..), mkFin, finToInt, oppositeFin
  , Vec (..), appendVec, fromListExact, toListVec, packListVec
  , Dec (..), Not, decToBool, DecEq (..)
  , Sigma (..), first, second, withSigma
  , data Pack, type (/\) (..), discardEvidence, withEvidence
  , Inspect (..), inspect
  , Iso (..), swap, swapInvolutive
  , cong, cong2, begin, stepEq, (===), qed
  , (.), ($)
  ) where

import Control.Exception (ArithException (Underflow), throw)
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl), sym, trans)
import Data.Void (Void)
import GHC.Exists (data Pack, type (/\) (..), discardEvidence)
import GHC.Matchability (Matchability, U)
import GHC.Sigma (Sigma (..), first, second)
import qualified GHC.TypeNats as TN
import GHC.TypeNats.Experimental (FromNatural)
import Prelude
  ( Bool (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show
  , otherwise, (&&)
  )
import qualified Prelude as P

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

mul :: Nat -> Nat -> Nat
mul Zero     _ = Zero
mul (Succ m) n = plus n (mul m n)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 P.+ natToInteger n

type family ToPeano (n :: TN.Nat) :: Nat where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n TN.- 1))

type instance FromNatural Nat n = ToPeano n

instance Num Nat where
  (+) = plus
  (*) = mul
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

data Fin (n :: Nat) where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

deriving instance Eq (Fin n)

finToInt :: Fin n -> Int
finToInt FZ     = 0
finToInt (FS i) = 1 P.+ finToInt i

mkFin :: foreach (n :: Nat) -> Int -> Maybe (Fin n)
mkFin n position
  | position < 0 = Nothing
  | otherwise    = go n position
  where
    go :: foreach (m :: Nat) -> Int -> Maybe (Fin m)
    go Zero     _ = Nothing
    go (Succ m) k =
      if k == 0
        then Just FZ
        else case go m (k P.- 1) of
          Nothing      -> Nothing
          Just smaller -> Just (FS smaller)

weakenFin :: forall (n :: Nat). Fin n -> Fin (Succ n)
weakenFin FZ     = FZ
weakenFin (FS i) = FS (weakenFin i)

lastFin :: foreach (n :: Nat) -> Fin (Succ n)
lastFin Zero     = FZ
lastFin (Succ n) = FS (lastFin n)

oppositeFin :: foreach (n :: Nat) -> Fin n -> Fin n
oppositeFin Zero     = \case {}
oppositeFin (Succ n) = \case
  FZ   -> lastFin n
  FS i -> weakenFin (oppositeFin n i)

data Vec a (n :: Nat) where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

deriving instance Eq a => Eq (Vec a n)

toListVec :: Vec a n -> [a]
toListVec VNil         = []
toListVec (VCons x xs) = x : toListVec xs

appendVec :: Vec a m -> Vec a n -> Vec a (plus m n)
appendVec VNil         ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n)
fromListExact Zero     []       = Just VNil
fromListExact Zero     (_ : _)  = Nothing
fromListExact (Succ _) []       = Nothing
fromListExact (Succ n) (x : xs) = case fromListExact n xs of
  Nothing -> Nothing
  Just vs -> Just (VCons x vs)

type VecAt :: Type -> Nat %U -> Type
type VecAt a = \n -> Vec a n

packListVec :: forall a. [a] -> Sigma Nat (VecAt a)
packListVec []       = Pair @Nat @(VecAt a) Zero VNil
packListVec (x : xs) = case packListVec xs of
  Pair n vs -> Pair @Nat @(VecAt a) (Succ n) (VCons x vs)

withSigma :: forall {a} (b :: a %U -> Type) r. Sigma a b -> (foreach (x :: a) -> b x -> r) -> r
withSigma (Pair x y) k = k x y

withEvidence :: forall c a r. (c /\ a) -> (c => a -> r) -> r
withEvidence (MkCPair x) k = k x

type Not (p :: Type) = p -> Void

data Dec (p :: Type) = Yes p | No (Not p)

decToBool :: Dec p -> Bool
decToBool (Yes _) = True
decToBool (No _)  = False

cong :: forall {a} {b} {m :: Matchability}. forall (f :: a %m -> b) -> forall (x :: a) (y :: a). x :~: y -> f x :~: f y
cong _ Refl = Refl

cong2
  :: forall {a} {b} {c} {m1 :: Matchability} {m2 :: Matchability}.
     forall (f :: a %m1 -> b %m2 -> c) -> forall (x1 :: a) (x2 :: a) (y1 :: b) (y2 :: b).
     x1 :~: x2 -> y1 :~: y2 -> f x1 y1 :~: f x2 y2
cong2 _ Refl Refl = Refl

class DecEq a where
  decEq :: foreach (x :: a) -> foreach (y :: a) -> Dec (x :~: y)

instance DecEq Bool where
  decEq False False = Yes Refl
  decEq False True  = No (\case {})
  decEq True  False = No (\case {})
  decEq True  True  = Yes Refl

instance DecEq Nat where
  decEq Zero     Zero     = Yes Refl
  decEq Zero     (Succ _) = No (\case {})
  decEq (Succ _) Zero     = No (\case {})
  decEq (Succ m) (Succ n) = case decEq m n of
    Yes eq  -> Yes (cong Succ eq)
    No notE -> No (\case Refl -> notE Refl)

instance DecEq a => DecEq (Vec a n) where
  decEq VNil         VNil         = Yes Refl
  decEq (VCons x xs) (VCons y ys) = case decEq x y of
    No notH  -> No (\case Refl -> notH Refl)
    Yes Refl -> case decEq xs ys of
      No notT  -> No (\case Refl -> notT Refl)
      Yes Refl -> Yes Refl

data Inspect (lhs :: b) where
  Reveal :: foreach (y :: b) -> (lhs :~: y) -> Inspect lhs

inspect :: forall {a} {b}. foreach (f :: a -> b) -> foreach (x :: a) -> Inspect (f x)
inspect f x = Reveal (f x) Refl

data Iso a b where
  MkIso
    :: foreach (to :: a -> b)
    -> foreach (from :: b -> a)
    -> (foreach (x :: a) -> from (to x) :~: x)
    -> (foreach (y :: b) -> to (from y) :~: y)
    -> Iso a b

swap :: forall a b. (a, b) -> (b, a)
swap (x, y) = (y, x)

swapInvolutive :: forall a b. foreach (p :: (a, b)) -> swap (swap p) :~: p
swapInvolutive (_, _) = Refl

begin :: forall {k} (x :: k) (y :: k). x :~: y -> x :~: y
begin eq = eq

stepEq :: forall {k} (y :: k) {x :: k} {z :: k}. x :~: y -> y :~: z -> x :~: z
stepEq = trans

infixr 2 ===
(===) :: forall {k} {x :: k} {y :: k} {z :: k}. x :~: y -> y :~: z -> x :~: z
(===) = trans

qed :: forall {k} (x :: k). x :~: x
qed = Refl

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

_unusedSym :: a :~: b -> b :~: a
_unusedSym = sym

_unusedAnd :: Bool -> Bool -> Bool
_unusedAnd = (&&)

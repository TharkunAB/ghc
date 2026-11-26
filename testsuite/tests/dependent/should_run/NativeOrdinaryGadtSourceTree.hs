{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeOrdinaryGadtSourceTree
  ( Tree(..), focus, known, pairRoundTrip, swapPair, later ) where

import Data.Type.Equality ((:~:)(Refl))

data Tree where
  Empty, Other :: Tree
  Arrow, Join  :: Tree -> Tree -> Tree

focus :: Tree -> Tree
focus Empty = Empty
focus Other = Other
focus (Arrow x _) = x
focus (Join l r) = Join (focus l) (focus r)

known :: focus ('Join ('Arrow 'Empty 'Other) 'Other) :~: 'Join 'Empty 'Other
known = Refl

swapSum :: Either a b -> Either b a
swapSum (Left x) = Right x
swapSum (Right y) = Left y

sumRoundTrip :: foreach (s :: Either a b) -> swapSum (swapSum s) :~: s
sumRoundTrip (Left _) = Refl
sumRoundTrip (Right _) = Refl

swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y, x)

pairRoundTrip :: foreach (p :: (a, b)) -> swapPair (swapPair p) :~: p
pairRoundTrip (_, _) = Refl

pairRoundTripCase :: foreach (p :: (a, b)) -> swapPair (swapPair p) :~: p
pairRoundTripCase p = case p of
  (_, _) -> Refl

first :: (a, b) -> a
first (x, _) = x

firstLaw :: forall a b. first '(a, b) :~: a
firstLaw = Refl

later :: Bool -> Bool -> foreach (t :: Tree) -> (Bool, t :~: t)
later False _ _ = (False, Refl)
later True flag (Join _ _) | flag = (False, Refl)
later True _ (Join _ _) = (True, Refl)
later _ _ _ = (True, Refl)

laterCallback :: foreach (f :: Tree -> Bool) -> foreach (t :: Tree) -> f t :~: f t
laterCallback _ (Join _ _) = Refl
laterCallback _ _ = Refl

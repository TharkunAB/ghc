{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
module NativeSameModNonStructuralOpen where

import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (Matchability)

data Nat = Zero | Succ Nat

data Color = Red | Green | Blue | Yellow

cong ::
  forall {a} {b} {m :: Matchability}.
  forall (f :: a %m -> b) ->
  forall (x :: a) (y :: a).
  x :~: y ->
  f x :~: f y
cong _ Refl = Refl

-- 1. Accumulator where-helper used in open foreach types without plugins.
accPlus :: Nat -> Nat -> Nat
accPlus m n = go m n
  where
    go Zero acc = acc
    go (Succ k) acc = go k (Succ acc)

accPlusRefl :: foreach (m n :: Nat) -> accPlus m n :~: accPlus m n
accPlusRefl _ _ = Refl

accPlusCong ::
  foreach (m n :: Nat) ->
  forall (k :: Nat).
  accPlus m n :~: k ->
  Succ (accPlus m n) :~: Succ k
accPlusCong _ _ eq = cong Succ eq

accPlusSubst ::
  forall (m1 m2 :: Nat).
  foreach (n :: Nat) ->
  m1 :~: m2 ->
  accPlus m1 n :~: accPlus m2 n
accPlusSubst _ Refl = Refl

-- 2. Coalgebraic unfold where-helper with Maybe stepping.
unfoldPlus :: Nat -> Nat -> Nat
unfoldPlus m n = go (m, n)
  where
    step (Zero, Zero) = Nothing
    step (Zero, Succ y) = Just (Zero, y)
    step (Succ x, y) = Just (x, y)

    go s = case step s of
      Nothing -> Zero
      Just s' -> Succ (go s')

unfoldPlusRefl :: foreach (m n :: Nat) -> unfoldPlus m n :~: unfoldPlus m n
unfoldPlusRefl _ _ = Refl

-- 3. Direct self-recursive function with literal pattern scrutiny and DEFAULT.
charDepth :: Char -> Nat
charDepth 'a' = Zero
charDepth 'b' = Succ (charDepth 'a')
charDepth _ = Succ Zero

charDepthRefl :: foreach (c :: Char) -> charDepth c :~: charDepth c
charDepthRefl _ = Refl

-- 4. Function calling a guarded local helper used in a same-module GADT and foreach type.
eqColor :: Color -> Color -> Bool
eqColor Red Red = True
eqColor Green Green = True
eqColor Blue Blue = True
eqColor Yellow Yellow = True
eqColor _ _ = False

pickColors :: Int -> [Color]
pickColors n = go 0
  where
    go v
      | v >= n = []
      | otherwise = Red : go (v + 1)

validColoring :: [Int] -> Bool
validColoring steps = case pickColors (length steps) of
  [] -> True
  c : _ -> eqColor c Red

data ColoringCert (steps :: [Int]) where
  MkColoringCert ::
    !(validColoring steps :~: True) ->
    ColoringCert steps

validColoringRefl :: foreach (steps :: [Int]) -> validColoring steps :~: validColoring steps
validColoringRefl _ = Refl

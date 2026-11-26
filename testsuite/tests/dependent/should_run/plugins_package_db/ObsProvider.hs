{-# LANGUAGE DependentTypes #-}
module ObsProvider (Nat(..), plus, natToInt, stepUnfoldAdd, whileAdd) where

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

stepUnfoldAdd :: Nat -> Nat -> Nat
stepUnfoldAdd m n = step (Just m) n
  where
    step Nothing acc = acc
    step (Just Zero) acc = step Nothing acc
    step (Just (Succ k)) acc = Succ (step (Just k) acc)

whileAdd :: Nat -> Nat -> Nat
whileAdd Zero acc = acc
whileAdd (Succ m) acc = whileAdd m (Succ acc)

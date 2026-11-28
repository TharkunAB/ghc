{-# LANGUAGE DependentTypes #-}
module NativeHoleLaterSignedFit where

import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat

data StepAt (n :: Nat) (a :: Nat) where
  SAZ :: StepAt Zero a

stepAtUnique :: StepAt n a -> StepAt n b -> a :~: b
stepAtUnique = undefined

data Step (m :: Nat) (a :: Nat) where
  SVar :: StepAt n a -> Step n a

stepUnique :: Step m a -> Step m b -> a :~: b
stepUnique (SVar p1) (SVar p2) =
  (_ :: forall n' a' b'. StepAt n' a' -> StepAt n' b' -> a' :~: b') p1 p2

useStep :: Step m a -> Step m b -> Bool
useStep p q = case stepUnique p q of Refl -> True

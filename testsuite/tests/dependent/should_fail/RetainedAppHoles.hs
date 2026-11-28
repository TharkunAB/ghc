{-# LANGUAGE DependentTypes #-}
module RetainedAppHoles where

import Data.Type.Equality ((:~:) (Refl), trans)
import GHC.Exts (Matchability)

data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

plusIdentityR :: foreach (m :: Nat) -> plus m Zero :~: m
plusIdentityR Zero     = Refl
plusIdentityR (Succ m) = cong Succ (plusIdentityR m)

plusSuc :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSuc Zero     _ = Refl
plusSuc (Succ m) n = cong Succ (plusSuc m n)

plusCommOk :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
plusCommOk m Zero     = plusIdentityR m
plusCommOk m (Succ k) = trans (plusSuc m k) (cong Succ (plusCommOk m k))

-- 1. Two holes directly under trans at a retained equality goal.
plusCommStep1 :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
plusCommStep1 m Zero     = plusIdentityR m
plusCommStep1 m (Succ k) = trans _ _

-- 2. Hole inside cong Succ _ after filling the first trans step.
plusCommStep2 :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
plusCommStep2 m Zero     = plusIdentityR m
plusCommStep2 m (Succ k) = trans (plusSuc m k) (cong Succ _)

-- 2b. Hole in first trans step with cong Succ _ in second step.
plusCommStep2b :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
plusCommStep2b m Zero     = plusIdentityR m
plusCommStep2b m (Succ k) = trans _ (cong Succ _)

-- 3. Holes in visible foreach argument positions of plusComm.
plusComm :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
plusComm m Zero     = plusIdentityR m
plusComm m (Succ k) = trans (plusSuc m k) (cong Succ (plusComm _ _))

-- 4. Named and type-annotated holes in retained foreach argument positions.
namedAndSignedHoles :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
namedAndSignedHoles m n = plusCommOk _first (_ :: Nat)

-- 5. Constructor application wrapping a hole in a retained foreach argument.
conRetainedHole :: foreach (m :: Nat) -> plus (Succ m) Zero :~: Succ m
conRetainedHole m = plusIdentityR (Succ _)

-- 6. Explicit invisible retained hole @{_}.
invisId :: foreach (n :: Nat). plus n Zero :~: n
invisId @n = plusIdentityR n

useInvisHole :: foreach (m :: Nat) -> plus m Zero :~: m
useInvisHole m = invisId @{_}

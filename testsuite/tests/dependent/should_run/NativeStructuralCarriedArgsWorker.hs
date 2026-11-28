{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
module NativeStructuralCarriedArgsWorker
  ( Term(..), Label(..), decorate, decorateLast, carryLast
  , Nat(..), timesWild, timesNamed, pow ) where

import Data.Type.Equality ((:~:)(Refl))

data Term = Empty | Atom Label | Next Term
data Label = Here | There Label
data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

timesWild :: Nat -> Nat -> Nat
timesWild Zero _ = Zero
timesWild (Succ m) n = plus n (timesWild m n)

timesNamed :: Nat -> Nat -> Nat
timesNamed Zero _ = Zero
timesNamed (Succ m) n = plus n (timesNamed m n)

pow :: Nat -> Nat -> Nat
pow _ Zero = Succ Zero
pow m (Succ n) = timesNamed m (pow m n)

decorate :: Term -> Label -> Label -> Term
decorate Empty _ _ = Empty
decorate (Atom _) _ right = Next (Atom right)
decorate (Next term) left right = Next (decorate term left right)

decorateLast :: Label -> Term -> Term
decorateLast label Empty = Atom label
decorateLast _ (Atom _) = Empty
decorateLast label (Next term) = Next (decorateLast label term)

carryLast :: Label -> Label -> Label
carryLast l Here = l
carryLast l (There r) = There (carryLast l r)

localLast :: foreach (l :: Label) -> foreach (r :: Label)
          -> carryLast l ('There r) :~: 'There (carryLast l r)
localLast _ _ = Refl

{-# LANGUAGE GHC2024, DataKinds, DependentTypes, ListTuplePuns, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror -fno-enable-rewrite-rules #-}
module HaskLibProps
  ( plusZero, plusSucc, plusComm, plusAssoc, mulZero, mulSucc, mulDistribR, mulAssoc
  ) where

import Data.Type.Equality ((:~:) (Refl), sym, trans)
import HaskLibCore (Nat (..), cong, mul, plus)
import Prelude ()

plusZero :: foreach (n :: Nat) -> plus n Zero :~: n
plusZero Zero     = Refl
plusZero (Succ n) = cong Succ (plusZero n)

plusSucc :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSucc Zero     _ = Refl
plusSucc (Succ m) n = cong Succ (plusSucc m n)

plusComm :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
plusComm Zero     n = sym (plusZero n)
plusComm (Succ m) n = trans (cong Succ (plusComm m n)) (sym (plusSucc n m))

plusAssoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
          -> plus (plus m n) p :~: plus m (plus n p)
plusAssoc Zero     _ _ = Refl
plusAssoc (Succ m) n p = cong Succ (plusAssoc m n p)

mulZero :: foreach (n :: Nat) -> mul n Zero :~: Zero
mulZero Zero     = Refl
mulZero (Succ n) = mulZero n

mulSucc :: foreach (m :: Nat) -> foreach (n :: Nat) -> mul m (Succ n) :~: plus m (mul m n)
mulSucc Zero     _ = Refl
mulSucc (Succ m) n =
  cong Succ
    (trans (cong (plus n) (mulSucc m n))
      (trans (sym (plusAssoc n m (mul m n)))
        (trans (cong (\k -> plus k (mul m n)) (plusComm n m))
               (plusAssoc m n (mul m n)))))

mulDistribR :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
            -> mul (plus m n) p :~: plus (mul m p) (mul n p)
mulDistribR Zero     _ _ = Refl
mulDistribR (Succ m) n p =
  trans (cong (plus p) (mulDistribR m n p)) (sym (plusAssoc p (mul m p) (mul n p)))

mulAssoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
         -> mul (mul m n) p :~: mul m (mul n p)
mulAssoc Zero     _ _ = Refl
mulAssoc (Succ m) n p =
  trans (mulDistribR n (mul m n) p) (cong (\k -> plus (mul n p) k) (mulAssoc m n p))

{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations -fno-enable-rewrite-rules #-}
module DependentHoleFits where

import Data.Type.Equality ((:~:) (Refl), trans)
import GHC.Exts (Matchability)
import qualified GHC.Internal.List as InternalList
import GHC.List (List, foldr)
import Prelude hiding (foldr)

data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

congSucc :: m :~: n -> Succ m :~: Succ n
congSucc Refl = Refl

plusZero :: foreach (m :: Nat) -> plus m Zero :~: m
plusZero Zero     = Refl
plusZero (Succ m) = congSucc (plusZero m)

data LE (m :: Nat) (n :: Nat) where
  LZ :: LE Zero n
  LS :: LE m n -> LE (Succ m) (Succ n)

-- 1. Base case suggests Refl; recursive step suggests cong Succ (plusAssoc m n p)
--    and congSucc (plusAssoc m n p), plus subgoal-splitting refinement fits.
plusAssoc :: foreach (m n p :: Nat) -> plus (plus m n) p :~: plus m (plus n p)
plusAssoc Zero     _ _ = _
plusAssoc (Succ m) n p = _

-- 2. Unsplit equation suggests pattern-matching on 'm' to reduce 'plus'.
plusCommUnsplit :: foreach (m n :: Nat) -> plus m n :~: plus n m
plusCommUnsplit m n = _

-- 3. Direct foreach lemma instantiation suggests plusZero m.
usePlusZero :: foreach (m :: Nat) -> plus m Zero :~: m
usePlusZero m = _

-- 4. GADT proof holes inside foreach equations suggest LZ and LS h.
leZero :: foreach (n :: Nat) -> LE Zero n
leZero _ = _

leSucc :: foreach (m n :: Nat) -> LE m n -> LE (Succ m) (Succ n)
leSucc m n h = _

-- 5. Polymorphic foreach law instantiation and unrenamed signature binders.
data MyList a = Nil | Cons a (MyList a)

data SameLen (xs :: MyList a) (ys :: MyList b) where
  SameNil  :: SameLen Nil Nil
  SameCons :: SameLen xs ys -> SameLen (Cons x xs) (Cons y ys)

sameLenRefl :: forall a. foreach (xs :: MyList a) -> SameLen xs xs
sameLenRefl Nil         = SameNil
sameLenRefl (Cons _ xs) = SameCons (sameLenRefl xs)

useSameLen :: forall a b. foreach (xs :: MyList a) -> foreach (ys :: MyList b) -> SameLen xs xs
useSameLen xs _ = _

-- 6. Higher-order foldr goal does not loop when checking Refl as a hole fit.
foldrPlusGoal :: foreach (x :: Nat) -> foreach (xs :: List Nat) -> foreach (z :: Nat)
              -> foldr plus z (x : xs) :~: plus (foldr plus Zero (x : xs)) z
foldrPlusGoal x xs z = _

-- 7. Point-free foreach holes suggest matching foreach functions (even when
--    one side reduces reverse to reverseOnto) and do not suggest pattern-
--    matching on unbound DepFunTy binders.
size :: forall a. List a -> Nat
size []       = Zero
size (_ : xs) = Succ (size xs)

plusSucc :: foreach (m n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSucc Zero     _ = Refl
plusSucc (Succ m) n = congSucc (plusSucc m n)

lengthReverseOnto :: forall a. foreach (xs :: List a) -> foreach (acc :: List a)
                  -> size (InternalList.reverseOnto xs acc) :~: plus (size xs) (size acc)
lengthReverseOnto []       _   = Refl
lengthReverseOnto (x : xs) acc =
  case lengthReverseOnto xs (x : acc) of
    Refl -> plusSucc (size xs) (size acc)

lengthReverse :: forall a. foreach (xs :: List a) -> size (reverse xs) :~: size xs
lengthReverse xs =
  case lengthReverseOnto xs [] of
    Refl -> plusZero (size xs)

lengthRev :: forall a. foreach (xs :: List a) -> size (reverse xs) :~: size xs
lengthRev = _

reverseIdPointfree :: forall a. foreach (xs :: List a) -> reverse xs :~: xs
reverseIdPointfree = _

-- 8. Transitivity step refinement fit on an inductive equality goal (PLFH).
add2 :: Nat -> Nat -> Nat
add2 Zero     n = n
add2 (Succ m) n = Succ (add2 m n)

add2Zero :: foreach (m :: Nat) -> add2 m Zero :~: m
add2Zero Zero     = Refl
add2Zero (Succ m) = congSucc (add2Zero m)

add2Succ :: foreach (m n :: Nat) -> add2 m (Succ n) :~: Succ (add2 m n)
add2Succ Zero     _ = Refl
add2Succ (Succ m) n = congSucc (add2Succ m n)

add2Comm :: foreach (m k :: Nat) -> add2 m k :~: add2 k m
add2Comm m Zero     = add2Zero m
add2Comm m (Succ k) = _

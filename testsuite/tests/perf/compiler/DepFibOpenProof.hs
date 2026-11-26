{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DependentTypes #-}

-- | Compile-time benchmark for symbolic open reduction and inductive proofs.
-- See Note [Symbolic Fibonacci open-reduction benchmark].
module DepFibOpenProof where

import Data.Type.Equality ((:~:) (Refl), sym, trans)
import GHC.Matchability (Matchability)
import GHC.Tuple (Tuple2)

{- Note [Symbolic Fibonacci open-reduction benchmark]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DepFibOpenProof exercises SourceTable equation publication, OpenWhnf symbolic
reduction, and the typechecker equality solver on recursive Peano arithmetic.
Every theorem quantifies over symbolic runtime binders with foreach so that
typechecking reduces open terms under pattern refinements rather than closed
ground literals:
  * Semiring laws on Nat (plusAssoc, plusComm, multComm, multDistRight,
    multDistLeft, multAssoc).
  * Coupled pair recurrence fibPairSpec and linear-time fibFast equivalence.
  * Prefix-sum law sumFibIdentity.
  * Fibonacci addition law fibAddition (d'Ocagne identity).
  * Mutual even/odd Cassini identities (cassiniEven, cassiniOdd).
-}

-- | Peano natural numbers.
data Nat
  = Zero
  | Succ Nat

-- | Structural addition on the left operand.
plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

-- | Structural multiplication on the left operand.
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = plus n (mult m n)

-- | Square a Peano natural number with two structural clauses matching mult.
sq :: Nat -> Nat
sq Zero = Zero
sq (Succ n) = plus (Succ n) (mult n (Succ n))

-- | Double a Peano natural number.
double :: Nat -> Nat
double Zero = Zero
double (Succ n) = Succ (Succ (double n))

-- | Advance a consecutive Fibonacci pair (F_n, F_{n+1}) to (F_{n+1}, F_{n+2}).
stepFib :: Tuple2 Nat Nat -> Tuple2 Nat Nat
stepFib (a, b) = (b, plus a b)

-- | Compute the pair (F_n, F_{n+1}) in linear time by structural recursion.
fibPair :: Nat -> Tuple2 Nat Nat
fibPair Zero = (Zero, Succ Zero)
fibPair (Succ n) = stepFib (fibPair n)

-- | Fibonacci number extracted from fibPair.
fib :: Nat -> Nat
fib Zero = Zero
fib (Succ n) = snd (fibPair n)

-- | Alternate linear-time Fibonacci projection via fst . stepFib.
fibFast :: Nat -> Nat
fibFast Zero = Zero
fibFast (Succ n) = fst (stepFib (fibPair n))

-- | Prefix sum fib 1 + fib 2 + ... + fib n.
sumFib :: Nat -> Nat
sumFib Zero = Zero
sumFib (Succ n) = plus (sumFib n) (fib (Succ n))

-- | Apply a unary matchable or unmatchable function across an equality.
cong
  :: forall {a} {b} {m :: Matchability}.
     forall (f :: a %m -> b)
  -> forall (x :: a) (y :: a).
     x :~: y -> f x :~: f y
cong _ Refl = Refl

-- | Apply a binary matchable or unmatchable function across two equalities.
cong2
  :: forall {a} {b} {c} {m1 :: Matchability} {m2 :: Matchability}.
     forall (f :: a %m1 -> b %m2 -> c)
  -> forall (x1 :: a) (y1 :: a) (x2 :: b) (y2 :: b).
     x1 :~: y1 -> x2 :~: y2 -> f x1 x2 :~: f y1 y2
cong2 _ Refl Refl = Refl

-- | Lift an equality into the second component of a Tuple2 at a fixed first component.
congPairR
  :: forall (x :: Nat) -> forall (y1 :: Nat) (y2 :: Nat).
     y1 :~: y2 -> (x, y1) :~: (x, y2)
congPairR _ Refl = Refl

-- | Monomorphic congruence for fst on Tuple2 Nat Nat.
congFstNat :: forall (p :: Tuple2 Nat Nat) (q :: Tuple2 Nat Nat). p :~: q -> fst p :~: fst q
congFstNat Refl = Refl

-- | Monomorphic congruence for snd on Tuple2 Nat Nat.
congSndNat :: forall (p :: Tuple2 Nat Nat) (q :: Tuple2 Nat Nat). p :~: q -> snd p :~: snd q
congSndNat Refl = Refl

-- Semiring laws on Nat

plusZeroRight :: foreach (n :: Nat) -> plus n Zero :~: n
plusZeroRight Zero = Refl
plusZeroRight (Succ k) = cong Succ (plusZeroRight k)

plusSuccRight :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSuccRight Zero _ = Refl
plusSuccRight (Succ k) n = cong Succ (plusSuccRight k n)

plusAssoc
  :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat)
  -> plus (plus a b) c :~: plus a (plus b c)
plusAssoc Zero _ _ = Refl
plusAssoc (Succ k) b c = cong Succ (plusAssoc k b c)

plusComm :: foreach (a :: Nat) -> foreach (b :: Nat) -> plus a b :~: plus b a
plusComm a Zero = plusZeroRight a
plusComm a (Succ k) = trans (plusSuccRight a k) (cong Succ (plusComm a k))

plusFourSwap
  :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> foreach (d :: Nat)
  -> plus (plus a b) (plus c d) :~: plus (plus a c) (plus b d)
plusFourSwap a b c d =
  trans (plusAssoc a b (plus c d))
    (trans (cong (plus a) (sym (plusAssoc b c d)))
      (trans (cong (plus a) (cong (\u -> plus u d) (plusComm b c)))
        (trans (cong (plus a) (plusAssoc c b d))
          (sym (plusAssoc a c (plus b d))))))

multZeroRight :: foreach (n :: Nat) -> mult n Zero :~: Zero
multZeroRight Zero = Refl
multZeroRight (Succ k) = multZeroRight k

multSuccRight :: foreach (m :: Nat) -> foreach (n :: Nat) -> mult m (Succ n) :~: plus m (mult m n)
multSuccRight Zero _ = Refl
multSuccRight (Succ k) n =
  cong Succ
    (trans (cong (plus n) (multSuccRight k n))
      (trans (sym (plusAssoc n k (mult k n)))
        (trans (cong (\u -> plus u (mult k n)) (plusComm n k))
          (plusAssoc k n (mult k n)))))

multComm :: foreach (a :: Nat) -> foreach (b :: Nat) -> mult a b :~: mult b a
multComm a Zero = multZeroRight a
multComm a (Succ k) = trans (multSuccRight a k) (cong (plus a) (multComm a k))

multDistRight
  :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat)
  -> mult (plus a b) c :~: plus (mult a c) (mult b c)
multDistRight Zero _ _ = Refl
multDistRight (Succ k) b c =
  trans (cong (plus c) (multDistRight k b c))
    (sym (plusAssoc c (mult k c) (mult b c)))

multDistLeft
  :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat)
  -> mult a (plus b c) :~: plus (mult a b) (mult a c)
multDistLeft a b c =
  trans (multComm a (plus b c))
    (trans (multDistRight b c a)
      (cong2 plus (multComm b a) (multComm c a)))

multAssoc
  :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat)
  -> mult (mult a b) c :~: mult a (mult b c)
multAssoc Zero _ _ = Refl
multAssoc (Succ k) b c =
  trans (multDistRight b (mult k b) c)
    (cong (plus (mult b c)) (multAssoc k b c))

sqSpec :: foreach (n :: Nat) -> sq n :~: mult n n
sqSpec Zero = Refl
sqSpec (Succ _) = Refl

doublePlus :: foreach (n :: Nat) -> double n :~: plus n n
doublePlus Zero = Refl
doublePlus (Succ k) =
  cong Succ (trans (cong Succ (doublePlus k)) (sym (plusSuccRight k k)))

-- Fibonacci structural recurrence and linear-pair equivalence

fibPairSpec :: foreach (n :: Nat) -> fibPair n :~: (fib n, fib (Succ n))
fibPairSpec Zero = Refl
fibPairSpec (Succ k) =
  trans
    (cong stepFib (fibPairSpec k))
    (congPairR (fib (Succ k)) (sym (congSndNat (cong stepFib (fibPairSpec k)))))

fibSuccSucc :: foreach (n :: Nat) -> fib (Succ (Succ n)) :~: plus (fib n) (fib (Succ n))
fibSuccSucc n = congSndNat (cong stepFib (fibPairSpec n))

fibFastCorrect :: foreach (n :: Nat) -> fibFast n :~: fib n
fibFastCorrect Zero = Refl
fibFastCorrect (Succ k) = congFstNat (cong stepFib (fibPairSpec k))

-- Prefix-sum identity: sumFib n + 1 = fib (n + 2)

sumFibIdentity :: foreach (n :: Nat) -> plus (sumFib n) (Succ Zero) :~: fib (Succ (Succ n))
sumFibIdentity Zero = Refl
sumFibIdentity (Succ k) =
  trans (plusAssoc (sumFib k) (fib (Succ k)) (Succ Zero))
    (trans (cong (plus (sumFib k)) (plusComm (fib (Succ k)) (Succ Zero)))
      (trans (sym (plusAssoc (sumFib k) (Succ Zero) (fib (Succ k))))
        (trans (cong (\u -> plus u (fib (Succ k))) (sumFibIdentity k))
          (trans (plusComm (fib (Succ (Succ k))) (fib (Succ k)))
            (sym (fibSuccSucc (Succ k)))))))

-- Addition law for Fibonacci numbers (d'Ocagne identity):
-- fib (m + Succ n) = fib m * fib n + fib (Succ m) * fib (Succ n)

fibAddition
  :: foreach (m :: Nat) -> foreach (n :: Nat)
  -> fib (plus m (Succ n)) :~: plus (mult (fib m) (fib n)) (mult (fib (Succ m)) (fib (Succ n)))
fibAddition Zero n = sym (plusZeroRight (fib (Succ n)))
fibAddition (Succ Zero) n =
  trans (fibSuccSucc n)
    (sym (cong2 plus (plusZeroRight (fib n)) (plusZeroRight (fib (Succ n)))))
fibAddition (Succ (Succ k)) n =
  trans (fibSuccSucc (plus k (Succ n)))
    (trans
      (trans (cong2 plus (fibAddition k n) (fibAddition (Succ k) n))
        (plusFourSwap
          (mult (fib k) (fib n))
          (mult (fib (Succ k)) (fib (Succ n)))
          (mult (fib (Succ k)) (fib n))
          (mult (fib (Succ (Succ k))) (fib (Succ n)))))
      (cong2 plus
        (trans (sym (multDistRight (fib k) (fib (Succ k)) (fib n)))
          (cong (\u -> mult u (fib n)) (sym (fibSuccSucc k))))
        (trans (sym (multDistRight (fib (Succ k)) (fib (Succ (Succ k))) (fib (Succ n))))
          (cong (\u -> mult u (fib (Succ n))) (sym (fibSuccSucc (Succ k)))))))

-- Mutual even/odd Cassini identities:
--   cassiniEven k : fib (2k + 2) * fib (2k) + 1 = sq (fib (2k + 1))
--   cassiniOdd  k : fib (2k + 3) * fib (2k + 1) = sq (fib (2k + 2)) + 1

cassiniEven
  :: foreach (k :: Nat)
  -> plus (mult (fib (Succ (Succ (double k)))) (fib (double k))) (Succ Zero)
       :~: sq (fib (Succ (double k)))
cassiniEven Zero = Refl
cassiniEven (Succ k) =
  trans
    (cong (\u -> plus (mult u (fib (Succ (Succ (double k))))) (Succ Zero))
      (fibSuccSucc (Succ (Succ (double k)))))
    (trans
      (cong (\u -> plus u (Succ Zero))
        (multDistRight
          (fib (Succ (Succ (double k))))
          (fib (Succ (Succ (Succ (double k)))))
          (fib (Succ (Succ (double k))))))
      (trans
        (cong (\u -> plus (plus u (mult (fib (Succ (Succ (Succ (double k))))) (fib (Succ (Succ (double k)))))) (Succ Zero))
          (sym (sqSpec (fib (Succ (Succ (double k)))))))
        (trans
          (trans
            (plusAssoc
              (sq (fib (Succ (Succ (double k)))))
              (mult (fib (Succ (Succ (Succ (double k))))) (fib (Succ (Succ (double k)))))
              (Succ Zero))
            (trans
              (cong (plus (sq (fib (Succ (Succ (double k))))))
                (plusComm
                  (mult (fib (Succ (Succ (Succ (double k))))) (fib (Succ (Succ (double k)))))
                  (Succ Zero)))
              (sym
                (plusAssoc
                  (sq (fib (Succ (Succ (double k)))))
                  (Succ Zero)
                  (mult (fib (Succ (Succ (Succ (double k))))) (fib (Succ (Succ (double k)))))))))
          (trans
            (cong (\u -> plus u (mult (fib (Succ (Succ (Succ (double k))))) (fib (Succ (Succ (double k))))))
              (sym (cassiniOdd k)))
            (trans
              (sym
                (multDistLeft
                  (fib (Succ (Succ (Succ (double k)))))
                  (fib (Succ (double k)))
                  (fib (Succ (Succ (double k))))))
              (trans
                (cong (mult (fib (Succ (Succ (Succ (double k))))))
                  (sym (fibSuccSucc (Succ (double k)))))
                (sym (sqSpec (fib (Succ (Succ (Succ (double k)))))))))))))

cassiniOdd
  :: foreach (k :: Nat)
  -> mult (fib (Succ (Succ (Succ (double k))))) (fib (Succ (double k)))
       :~: plus (sq (fib (Succ (Succ (double k))))) (Succ Zero)
cassiniOdd k =
  trans
    (cong (\u -> mult u (fib (Succ (double k)))) (fibSuccSucc (Succ (double k))))
    (trans
      (multDistRight
        (fib (Succ (double k)))
        (fib (Succ (Succ (double k))))
        (fib (Succ (double k))))
      (trans
        (cong (\u -> plus u (mult (fib (Succ (Succ (double k)))) (fib (Succ (double k)))))
          (sym (sqSpec (fib (Succ (double k))))))
        (trans
          (cong (\u -> plus u (mult (fib (Succ (Succ (double k)))) (fib (Succ (double k)))))
            (sym (cassiniEven k)))
          (trans
            (trans
              (plusAssoc
                (mult (fib (Succ (Succ (double k)))) (fib (double k)))
                (Succ Zero)
                (mult (fib (Succ (Succ (double k)))) (fib (Succ (double k)))))
              (trans
                (cong (plus (mult (fib (Succ (Succ (double k)))) (fib (double k))))
                  (plusComm
                    (Succ Zero)
                    (mult (fib (Succ (Succ (double k)))) (fib (Succ (double k))))))
                (sym
                  (plusAssoc
                    (mult (fib (Succ (Succ (double k)))) (fib (double k)))
                    (mult (fib (Succ (Succ (double k)))) (fib (Succ (double k))))
                    (Succ Zero)))))
            (cong (\u -> plus u (Succ Zero))
              (trans
                (sym
                  (multDistLeft
                    (fib (Succ (Succ (double k))))
                    (fib (double k))
                    (fib (Succ (double k)))))
                (trans
                  (cong (mult (fib (Succ (Succ (double k))))) (sym (fibSuccSucc (double k))))
                  (sym (sqSpec (fib (Succ (Succ (double k)))))))))))))

-- | Symbolic specialization combining fibFastCorrect and fibAddition.
fibFastAddition
  :: foreach (m :: Nat) -> foreach (n :: Nat)
  -> fibFast (plus m (Succ n))
       :~: plus (mult (fibFast m) (fibFast n)) (mult (fibFast (Succ m)) (fibFast (Succ n)))
fibFastAddition m n =
  trans (fibFastCorrect (plus m (Succ n)))
    (trans (fibAddition m n)
      (sym
        (cong2 plus
          (cong2 mult (fibFastCorrect m) (fibFastCorrect n))
          (cong2 mult (fibFastCorrect (Succ m)) (fibFastCorrect (Succ n))))))

{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-missing-signatures -fno-enable-rewrite-rules #-}

module Main (main, SizedVec (..)) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl), (:~~:) (HRefl), sym, trans)
import NativeDepComposeLib
import GHC.Matchability (Matchability, U)
import GHC.Sigma (Sigma (..), first, second)
import NativeDepComposeH2010 (checkH2010)
import NativeDepComposeImpredicative (checkImpredicative)
import NativeDepComposeSymUnsatOk ()
import Prelude (Bool (..), Char, Int, IO, Maybe (..), fail, id, pure, (==), (&&), (++))

type VecAt a = \n -> Vec a n

mapSigma
  :: forall {a} {b} (p :: a %U -> Type) (q :: b %U -> Type).
     foreach (f :: a -> b)
  -> (foreach (x :: a) -> p x -> q (f x))
  -> Sigma a p
  -> Sigma b q
mapSigma f g (Pair x px) = Pair (f x) (g x px)

zeros :: foreach (n :: Nat) -> Vec Nat n
zeros Zero     = VNil
zeros (Succ n) = VCons Zero (zeros n)

plusZero :: foreach (n :: Nat) -> plus n Zero :~: n
plusZero Zero     = Refl
plusZero (Succ n) = cong Succ (plusZero n)

minusDiag :: foreach (n :: Nat) -> minus n n :~: Zero
minusDiag Zero     = Refl
minusDiag (Succ n) = minusDiag n

swapInv :: forall a b. foreach (p :: (a, b)) -> swapPair (swapPair p) :~: p
swapInv (_, _) = Refl

-- 1. Overloaded numeral in retained function argument
zerosPlus1 :: foreach (n :: Nat) -> Vec Nat (plus 1 n)
zerosPlus1 = (.) zeros (plus 1)

-- 2. Compound codomain motive inference + explicit inline @(\y -> ...)
plusIdPlusOne :: foreach (n :: Nat) -> plus (plus (Succ Zero) n) Zero :~: plus (Succ Zero) n
plusIdPlusOne = plusZero . plus (Succ Zero)

plusIdExplicitLam :: foreach (n :: Nat) -> plus (plus (Succ Zero) n) Zero :~: plus (Succ Zero) n
plusIdExplicitLam = (.) @(\y -> plus y Zero :~: y) plusZero (plus (Succ Zero))

-- 3. Constructor as retained function argument + cong (Succ . plus m)
plusIdSucc :: foreach (n :: Nat) -> plus (Succ n) Zero :~: Succ n
plusIdSucc = plusZero . Succ

congSuccPlus :: foreach (m :: Nat) -> forall (x :: Nat) (y :: Nat).
                x :~: y -> Succ (plus m x) :~: Succ (plus m y)
congSuccPlus m eq = cong (Succ . plus m) eq

-- 4. Same-module signed/unsigned functions and inline lambda
addOneSigned :: Nat -> Nat
addOneSigned n = Succ n

addOneUnsigned n = Succ n

plusIdSigned :: foreach (n :: Nat) -> plus (Succ n) Zero :~: Succ n
plusIdSigned = plusZero . addOneSigned

plusIdUnsigned :: foreach (n :: Nat) -> plus (Succ n) Zero :~: Succ n
plusIdUnsigned = plusZero . addOneUnsigned

plusIdLambda :: foreach (n :: Nat) -> plus (Succ n) Zero :~: Succ n
plusIdLambda = plusZero . (\n -> Succ n)

-- 5. Same-module dependent record selector in type signature + record dot
data SizedVec a = SizedVec
  { vecLen   :: Nat
  , vecItems :: Vec a vecLen
  }

extractItems :: foreach (v :: SizedVec a) -> Vec a (vecLen v)
extractItems (SizedVec _ xs) = xs

-- 6. Local where composition, inline composition, and type-level (.)
data Iso a b where
  MkIso
    :: foreach (to :: a -> b)
    -> foreach (from :: b -> a)
    -> (foreach (x :: a) -> from (to x) :~: x)
    -> (foreach (y :: b) -> to (from y) :~: y)
    -> Iso a b

composeLeft
  :: foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> (foreach (x :: a) -> g (f x) :~: x)
  -> (foreach (y :: b) -> k (h y) :~: y)
  -> foreach (x :: a) -> g (k (h (f x))) :~: x
composeLeft f g _ _ gf kh x = trans (cong g (kh (f x))) (gf x)

isoTransWhere :: forall a b c. Iso a b -> Iso b c -> Iso a c
isoTransWhere (MkIso f g gf fg) (MkIso h k kh hk) =
  MkIso @a @c to from (composeLeft f g h k gf kh) (composeLeft k h g f hk fg)
  where
    to   = h . f
    from = g . k

isoTransInline :: forall a b c. Iso a b -> Iso b c -> Iso a c
isoTransInline (MkIso f g gf fg) (MkIso h k kh hk) =
  MkIso @a @c (h . f) (g . k) (composeLeft f g h k gf kh) (composeLeft k h g f hk fg)

fmapMaybeCompose
  :: forall a b c.
     foreach (g :: b -> c)
  -> foreach (f :: a -> b)
  -> foreach (mx :: Maybe a)
  -> fmapMaybe (g . f) mx :~: fmapMaybe g (fmapMaybe f mx)
fmapMaybeCompose _ _ Nothing  = Refl
fmapMaybeCompose _ _ (Just _) = Refl

-- 7. Nested reverse wrapper and nested GADT projection in types
appendNilR :: forall a. foreach (xs :: [a]) -> (xs ++ '[]) :~: xs
appendNilR []       = Refl
appendNilR (x : xs) = cong (x :) (appendNilR xs)

appendAssoc :: forall a. foreach (xs :: [a]) -> foreach (ys :: [a]) -> foreach (zs :: [a])
            -> ((xs ++ ys) ++ zs) :~: (xs ++ (ys ++ zs))
appendAssoc []       _  _  = Refl
appendAssoc (x : xs) ys zs = cong (x :) (appendAssoc xs ys zs)

shuntAppend :: forall a. foreach (xs :: [a]) -> foreach (ys :: [a])
            -> shunt xs ys :~: (reverseShunt xs ++ ys)
shuntAppend []       _  = Refl
shuntAppend (x : xs) ys = case shuntAppend xs [x] of
  Refl -> trans (shuntAppend xs (x : ys)) (sym (appendAssoc (reverseShunt xs) [x] ys))

shuntInvAux :: forall a. foreach (xs :: [a]) -> foreach (ys :: [a])
            -> shunt (shunt xs ys) '[] :~: (shunt ys '[] ++ xs)
shuntInvAux []       ys = sym (appendNilR (shunt ys []))
shuntInvAux (x : xs) ys = case shuntAppend ys [x] of
  Refl -> case shuntInvAux xs (x : ys) of
    Refl -> case appendAssoc (shunt ys []) [x] xs of
      Refl -> Refl

reverseShuntInv :: forall a. foreach (xs :: [a]) -> reverseShunt (reverseShunt xs) :~: xs
reverseShuntInv xs = shuntInvAux xs []

appendVecAssoc
  :: forall a {m :: Nat} {n :: Nat} {p :: Nat}.
     foreach (xs :: Vec a m) -> foreach (ys :: Vec a n) -> foreach (zs :: Vec a p)
  -> appendVec (appendVec xs ys) zs :~~: appendVec xs (appendVec ys zs)
appendVecAssoc VNil          _  _  = HRefl
appendVecAssoc (VCons _x xs) ys zs = case appendVecAssoc @a xs ys zs of HRefl -> HRefl

depAbstract
  :: forall a b {mh :: Matchability} {mk :: Matchability}
            (h :: b %mh -> a) (k :: a %mk -> b).
     (foreach (y :: b) -> k (h y) :~: y)
  -> foreach (f :: a -> b)
  -> foreach (x :: a)
  -> k (h (f x)) :~: f x
depAbstract kh f x = (kh . f) x

main :: IO ()
main = do
  let two   = Succ (Succ Zero)
      three = Succ two
      lit2  = 2 :: Nat
      i1    = 1 :: Int
      v12   = VCons two (VCons three VNil)
      sv    = SizedVec (Succ (Succ Zero)) v12
      iso1  = MkIso @(Int, Bool) @(Bool, Int) swapPair swapPair swapInv swapInv
      iso2  = MkIso @(Bool, Int) @(Int, Bool) swapPair swapPair swapInv swapInv
      isoOk = case (isoTransWhere iso1 iso2, isoTransInline iso1 iso2) of
        (MkIso t1 f1 gf1 fg1, MkIso t2 f2 gf2 fg2) ->
             t1 (i1, True) == (i1, True)
          && f1 (i1, True) == (i1, True)
          && t2 (i1, True) == (i1, True)
          && f2 (i1, True) == (i1, True)
          && (case (gf1 (i1, True), fg1 (i1, True), gf2 (i1, True), fg2 (i1, True)) of
                (Refl, Refl, Refl, Refl) -> True)
      ok = checkH2010
        && checkImpredicative
        && toListVec (zerosPlus1 lit2) == [Zero, Zero, Zero]
        && (case (plusIdPlusOne two, plusIdExplicitLam two, plusIdSucc two, minusDiag three) of (Refl, Refl, Refl, Refl) -> True)
        && (case (congSuccPlus two (Refl :: Zero :~: Zero), plusIdSigned two, plusIdUnsigned two, plusIdLambda two) of
              (Refl, Refl, Refl, Refl) -> True)
        && toListVec (extractItems sv) == [two, three]
        && toListVec sv.vecItems == [two, three]
        && isoOk
        && (case (fmapMaybeCompose Succ (plus two) (Just three), reverseShuntInv [two, three], depAbstract @Nat @Nat @id @id (\_ -> Refl) Succ two) of
              (Refl, Refl, Refl) -> True)
        && (case appendVecAssoc v12 v12 v12 of HRefl -> True)
        && (let s1 :: Sigma Nat (VecAt Char)
                s1 = Pair (Succ (Succ Zero)) (VCons 'a' (VCons 'a' VNil))
                s2 :: Sigma Nat (VecAt Char)
                s2 = mapSigma @(VecAt Char) @(VecAt Char) Succ (\_ v -> VCons 'z' v) s1
            in first s2 == three && toListVec (second s2) == "zaa")
  if ok then pure () else fail "NativeDependentCompose failed"

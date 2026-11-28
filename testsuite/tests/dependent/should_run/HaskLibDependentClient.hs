{-# LANGUAGE GHC2024, AllowAmbiguousTypes, DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall -Werror -fno-enable-rewrite-rules #-}
module HaskLibDependentClient (checkDependentSuite, SizedVec (..)) where

import Data.Type.Equality ((:~:) (Refl))
import HaskLibCore
import qualified HaskLibProps as NP
import Prelude (Bool (..), Char, Int, Show (..), not, (==), (&&))

zeros :: foreach (n :: Nat) -> Vec Nat n
zeros Zero     = VNil
zeros (Succ n) = VCons Zero (zeros n)

zerosPlus1 :: foreach (n :: Nat) -> Vec Nat (plus 1 n)
zerosPlus1 = (.) zeros (plus 1)

plusIdPlusOne :: foreach (n :: Nat) -> plus (plus (Succ Zero) n) Zero :~: plus (Succ Zero) n
plusIdPlusOne = NP.plusZero . plus (Succ Zero)

plusIdExplicitLam :: foreach (n :: Nat) -> plus (plus (Succ Zero) n) Zero :~: plus (Succ Zero) n
plusIdExplicitLam = (.) @(\y -> plus y Zero :~: y) NP.plusZero (plus (Succ Zero))

plusIdSucc :: foreach (n :: Nat) -> plus (Succ n) Zero :~: Succ n
plusIdSucc = NP.plusZero . Succ

congSuccPlus :: foreach (m :: Nat) -> forall (x :: Nat) (y :: Nat). x :~: y -> Succ (plus m x) :~: Succ (plus m y)
congSuccPlus m eq = cong (Succ . plus m) eq

data SizedVec a = SizedVec
  { vecLen   :: Nat
  , vecItems :: Vec a vecLen
  }

extractSizedItems :: foreach (v :: SizedVec a) -> Vec a (vecLen v)
extractSizedItems (SizedVec _ xs) = xs

stepReasoning :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus n m)
stepReasoning m n =
  begin
    (stepEq @(Succ (plus m n)) (NP.plusSucc m n)
      (stepEq @(Succ (plus n m)) (cong Succ (NP.plusComm m n))
        (qed @(Succ (plus n m)))))

chainReasoning :: foreach (m :: Nat) -> plus (plus m Zero) Zero :~: m
chainReasoning m = NP.plusZero (plus m Zero) === NP.plusZero m

eqZeroNat :: Nat -> Bool
eqZeroNat Zero     = True
eqZeroNat (Succ _) = False

classifyZero :: foreach (m :: Nat) -> Bool
classifyZero m = case inspect eqZeroNat m of
  Reveal True  Refl -> True
  Reveal False Refl -> False

checkDependentSuite :: Bool
checkDependentSuite =
  let two    = Succ (Succ Zero)
      three  = Succ two
      litTwo = 2 :: Nat
      v12    = VCons two (VCons three VNil)
      sv     = SizedVec two v12
      cPair :: Show Int /\ Int
      cPair  = MkCPair 42
      exPkg :: exists (n :: Nat) | Vec Char n
      exPkg  = Pack (type (Succ (Succ Zero))) (VCons 'a' (VCons 'b' VNil))
      exOk   = case exPkg of Pack (type _) vs -> toListVec vs == "ab"
      i1      = 1 :: Int
      isoSwap = MkIso @(Int, Bool) @(Bool, Int) swap swap swapInvolutive swapInvolutive
      isoOk   = case isoSwap of MkIso t f gf fg -> case (gf (i1, True), fg (True, i1)) of (Refl, Refl) -> f (t (i1, True)) == (i1, True)
  in toListVec (zerosPlus1 litTwo) == [Zero, Zero, Zero]
     && (case (plusIdPlusOne two, plusIdExplicitLam two, plusIdSucc two, congSuccPlus two (Refl :: Zero :~: Zero)) of
           (Refl, Refl, Refl, Refl) -> True)
     && (case (stepReasoning two three, chainReasoning two, NP.plusZero $ two) of (Refl, Refl, Refl) -> True)
     && toListVec (extractSizedItems sv) == [two, three]
     && toListVec sv.vecItems == [two, three]
     && classifyZero Zero && not (classifyZero two)
     && decToBool (decEq v12 v12) && not (decToBool (decEq v12 (VCons three (VCons two VNil))))
     && finToInt (oppositeFin 3 (FZ :: Fin 3)) == 2
     && exOk && isoOk
     && discardEvidence cPair == 42
     && withEvidence cPair show == "42"

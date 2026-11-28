{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

module NativeDepComposeImpredicative (checkImpredicative) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl), gcastWith, type (~))
import NativeDepComposeLib (Nat (..), cong, plus)
import Prelude (Bool (..), const, (&&))

rewrite :: forall {k} (a :: k) (b :: k) r. (a :~: b) -> ((a ~ b) => r) -> r
rewrite = gcastWith

data Even :: Nat -> Type where
  EvenZ :: Even Zero
  EvenS :: Odd n -> Even (Succ n)

data Odd :: Nat -> Type where
  OddS :: Even n -> Odd (Succ n)

plusSucc :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSucc Zero     _ = Refl
plusSucc (Succ m) n = cong Succ (plusSucc m n)

evenDouble :: foreach (k :: Nat) -> Even (plus k k)
evenDouble Zero     = EvenZ
evenDouble (Succ k) = rewrite (plusSucc k k) (EvenS (OddS (evenDouble k)))

evenWitnessOk :: Even n -> Bool
evenWitnessOk EvenZ               = True
evenWitnessOk (EvenS (OddS rest)) = evenWitnessOk rest

data EvenWitness (n :: Nat) where
  MkEvenWitness :: foreach (k :: Nat) -> plus k k :~: n -> EvenWitness n

evenWitnessSuc2 :: EvenWitness n -> EvenWitness (Succ (Succ n))
evenWitnessSuc2 (MkEvenWitness k Refl) =
  MkEvenWitness (Succ k) (cong Succ (plusSucc k k))

checkImpredicative :: Bool
checkImpredicative =
  let two = Succ (Succ Zero)
      w2  = evenWitnessSuc2 (MkEvenWitness Zero Refl)
      wOk = case w2 of MkEvenWitness _ Refl -> True
  in evenWitnessOk (evenDouble two)
       && wOk
       && const True (plusSucc two two)
       && const True (cong Succ (plusSucc two two))

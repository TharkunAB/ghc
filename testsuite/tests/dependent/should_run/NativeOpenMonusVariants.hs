{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
module NativeOpenMonusVariants (monusMixed, monusLocal, monusLocalZero) where

import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthSize (Nat(..))

monusMixed :: Nat -> Nat -> Nat
monusMixed m        Zero     = m
monusMixed Zero     (Succ _) = Zero
monusMixed (Succ m) (Succ n) = monusMixed m n

reflexivity :: foreach (n :: Nat) -> n :~: n
reflexivity _ = Refl

monusLocal :: Nat -> Nat -> Nat
monusLocal m        Zero     = m
monusLocal Zero     (Succ _) = Zero
monusLocal (Succ m) (Succ n) = monusLocal m n

monusLocalZero :: foreach (n :: Nat) -> monusLocal n 'Zero :~: n
monusLocalZero n = case reflexivity n of Refl -> Refl

{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenMatchabilityCongNat (Nat(..), add, addRight) where

data Nat = Z | S Nat

add :: Nat -> Nat -> Nat
add Z n = n
add (S m) n = S (add m n)

addRight :: Nat -> Nat -> Nat
addRight m Z = m
addRight m (S n) = S (addRight m n)

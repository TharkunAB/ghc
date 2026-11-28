{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module StepSource (stepNat) where
import PackedVecLibrary (Nat(..))

{-# NOINLINE stepNat #-}
stepNat :: Nat -> Nat
stepNat Z = S Z
stepNat (S n) = n

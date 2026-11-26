{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             GADTs, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenZipRowsRuleFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenZipRowsRuleSpoof ()
import NativeOpenZipRowsVariants (countRight)
import PackedVecLibrary (Nat(..))
import ZipLengthSource (zipLength)

{-# RULES "change zip" forall xs ys. zipLength xs ys = S Z #-}

emptyLeft :: forall (ys :: [Bool]). zipLength ('[] :: [Char]) ys :~: 'Z
emptyLeft = Refl

spoofStep :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
             countRight (x ': xs) (y ': ys) :~: 'S (countRight (x ': xs) ys)
spoofStep = Refl

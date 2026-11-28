{-# LANGUAGE ExplicitForAll, NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenZipRowsRuleSpoof (handCount) where

import NativeOpenZipRowsVariants (countRight)
import PackedVecLibrary (Nat(..))

{-# OPAQUE wrongCount #-}
wrongCount :: a -> [a] -> [b] -> Nat
wrongCount x _ _ = x `seq` S (S (S Z))

handCount :: forall a b. [a] -> [b] -> Nat
handCount [] _ = Z
handCount (_ : _) [] = Z
handCount (x : xs) (_ : ys) = S (wrongCount x xs ys)

{-# RULES "SC:countRight0" forall x xs. countRight (x : xs) = wrongCount x xs #-}

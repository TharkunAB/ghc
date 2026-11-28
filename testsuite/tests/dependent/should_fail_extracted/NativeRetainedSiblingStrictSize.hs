{-# LANGUAGE BangPatterns, DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
module NativeRetainedSiblingStrictSize where

import PackedVecLibrary (Nat, Vec)
import SiblingColumns (Columns(..))

forceLetters :: Columns -> Nat
forceLetters (Columns n !_ _) = n

strictSizeProof :: foreach (p :: Columns) -> Vec Char (forceLetters p)
strictSizeProof (Columns _ xs _) = xs

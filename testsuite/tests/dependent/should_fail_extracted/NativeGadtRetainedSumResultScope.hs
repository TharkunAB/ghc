{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, RankNTypes #-}
module NativeGadtRetainedSumResultScope where

import NativeGadtRetainedSumProvider

data BadScope (a :: Nat) where
  BadScope :: forall (a :: Nat). foreach (n :: Nat)
           -> (foreach (n :: Nat) -> (some (m :: Nat) | Vec Char m))
           -> BadScope m

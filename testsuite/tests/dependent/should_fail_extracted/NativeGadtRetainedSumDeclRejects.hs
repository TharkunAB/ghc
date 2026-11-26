{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, RankNTypes #-}
module NativeGadtRetainedSumDeclRejects where

import NativeGadtRetainedSumProvider

data RepeatedMatrix a (rows :: Nat) (columns :: Nat) where
  RepeatedMatrix :: foreach (rows :: Nat) (columns :: Nat)
                 -> Vec (Vec a columns) rows -> RepeatedMatrix a rows rows

data RepeatedBlock a (shape :: BlockShape) where
  RepeatedBlock :: foreach (mark :: Bool) (depth :: Nat) (width :: Nat)
                -> Vec a width -> Vec a depth -> RepeatedBlock a ('Block width mark width)

data HiddenKind where
  HiddenKind :: foreach (n :: Nat)
             -> (foreach (n :: FlagAt n) -> (some (m :: Nat) | (KindTag n, Vec Char m)))
             -> HiddenKind

data UnmatchedErased a (n :: Nat) where
  UnmatchedErased :: foreach (n :: Nat) -> Vec b n -> UnmatchedErased a n

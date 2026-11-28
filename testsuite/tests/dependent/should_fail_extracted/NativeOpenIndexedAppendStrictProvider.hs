{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoDependentTypes, TypeFamilies #-}
{-# OPTIONS_GHC -O1 -fwrite-source-equations #-}
module NativeOpenIndexedAppendStrictProvider
  ( Nat(..), Add, Vec(..), appendStrictNil, appendStrictHead, appendChangeCarried ) where

data Nat = Z | S Nat

type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add 'Z m = m
  Add ('S n) m = 'S (Add n m)

data Vec a (n :: Nat) where
  VNil :: Vec a 'Z
  VCons :: a -> Vec a n -> Vec a ('S n)

appendStrictNil :: Vec a n -> Vec a m -> Vec a (Add n m)
appendStrictNil VNil ys = case ys of
  VNil -> VNil
  VCons hd rest -> hd `seq` VCons hd rest
appendStrictNil (VCons x xs) ys = VCons x (appendStrictNil xs ys)

appendStrictHead :: Vec a n -> Vec a m -> Vec a (Add n m)
appendStrictHead VNil ys = ys
appendStrictHead (VCons x xs) ys = x `seq` VCons x (appendStrictHead xs ys)

appendChangeCarried :: Vec a n -> Vec a m -> Vec a (Add n m)
appendChangeCarried VNil ys = ys
appendChangeCarried (VCons x xs) ys =
  VCons x (appendChangeCarried xs (error "changed right operand" `asTypeOf` ys))

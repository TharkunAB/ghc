{-# LANGUAGE DataKinds, EmptyCase, ExplicitNamespaces, ExperimentalDependentTypes, GADTs, KindSignatures, PolyKinds #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeRetainedPolyAndInlineGadtProvider
  ( Nat(..)
  , Fin(..)
  , UTerm(..)
  , UCtx(..)
  , Chain(..)
  , plug
  ) where

data Nat = Zero | Succ Nat

data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

data UCtx (m :: Nat) (n :: Nat) where
  CHole :: UCtx n n
  CLam  :: UCtx m ('Succ n) -> UCtx m n
  CAppL :: UCtx m n -> UTerm n -> UCtx m n
  CAppR :: UTerm n -> UCtx m n -> UCtx m n

data Chain (m :: Nat) (n :: Nat) where
  CRefl :: Chain n n
  CStep :: Chain m p -> Chain p n -> Chain m n

plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole       t = t
plug (CLam c)    t = ULam (plug c t)
plug (CAppL c r) t = UApp (plug c t) r
plug (CAppR l c) t = UApp l (plug c t)

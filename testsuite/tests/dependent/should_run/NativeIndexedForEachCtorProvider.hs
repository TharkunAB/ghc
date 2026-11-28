{-# LANGUAGE DataKinds, EmptyCase, DependentTypes, ListTuplePuns, GADTs, KindSignatures #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, StandaloneKindSignatures, TypeApplications, TypeOperators #-}
module NativeIndexedForEachCtorProvider
  ( Nat(..), Fin(..), UTerm(..), BetaStep(..), BetaSteps(..)
  , EvalStep(..), Wrap(..), SomeTerm(..), WrapSome(..)
  , RemoveFin(..), CmpFin(..), cmpFinFZ
  , LamSteps, VarSteps
  , packLam, packVar, mkEvalStep
  ) where

import Data.Kind (Type)
import GHC.Sigma (Sigma(..))

data Nat = Zero | Succ Nat

data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

type BetaStep :: forall (n :: Nat). UTerm n -> UTerm n -> Type
data BetaStep t t' where
  BId :: BetaStep t t

type BetaSteps :: forall (n :: Nat). UTerm n -> UTerm n -> Type
data BetaSteps t t' where
  BRefl :: BetaSteps t t
  BCons :: BetaStep s t -> BetaSteps t u -> BetaSteps s u

type EvalStep :: forall (k :: Nat). UTerm k -> Type
data EvalStep m where
  MkStep :: foreach (n :: UTerm k) -> BetaSteps m n -> EvalStep m

type Wrap :: forall (n :: Nat). UTerm n -> Type
data Wrap t where
  MkWrap :: foreach (t :: UTerm n) -> Wrap t

data SomeTerm where
  MkSome :: UTerm n -> SomeTerm

type WrapSome :: SomeTerm -> Type
data WrapSome s where
  MkWrapSome :: foreach (s :: SomeTerm) -> WrapSome s

type LamSteps :: forall (n :: Nat) -> UTerm ('Succ n) -> Type
type LamSteps n b' = some (t' :: UTerm n) | BetaSteps ('ULam b') t'

type VarSteps :: Type
type VarSteps = some (t' :: UTerm ('Succ ('Succ 'Zero))) | BetaSteps ('UVar ('FS 'FZ)) t'

{-# NOINLINE packLam #-}
packLam :: forall (n :: Nat). foreach (b' :: UTerm ('Succ n)) -> LamSteps n b'
packLam b' = Pair (ULam b') BRefl

{-# NOINLINE packVar #-}
packVar :: VarSteps
packVar = Pair (UVar (FS FZ)) BRefl

{-# NOINLINE mkEvalStep #-}
mkEvalStep :: forall (k :: Nat) (m :: UTerm k).
              foreach (n :: UTerm k) -> BetaSteps m n -> EvalStep m
mkEvalStep n steps = MkStep n steps

type RemoveFin :: forall (n :: Nat). Fin ('Succ n) -> Fin ('Succ n) -> Fin n -> Type
data RemoveFin c x y where
  RemGt :: forall {n} (y :: Fin n). RemoveFin 'FZ ('FS y) y
  RemLt :: forall {n} (c :: Fin ('Succ n)). RemoveFin ('FS c) 'FZ 'FZ
  RemSS :: RemoveFin c x y -> RemoveFin ('FS c) ('FS x) ('FS y)

type CmpFin :: forall (n :: Nat). Fin ('Succ n) -> Fin ('Succ n) -> Type
data CmpFin c x where
  CmpEq  :: CmpFin c c
  CmpNeq :: foreach (y :: Fin n) -> RemoveFin c x y -> CmpFin c x

cmpFinFZ :: forall (m :: Nat).
            foreach (x :: Fin ('Succ ('Succ m))) -> CmpFin ('FS ('FZ @m)) x
cmpFinFZ x = case x of
  FZ    -> CmpNeq FZ (RemLt @FZ)
  FS x' -> case x' of
    FZ   -> CmpEq
    FS y -> CmpNeq (FS y) (RemSS (RemGt @y))

{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ExplicitNamespaces, GADTs, ListTuplePuns, TypeFamilies #-}
-- | Provider module for mutually recursive datatypes and Type-valued large eliminations.
module NativeMutualLargeElimDataProvider
  ( Val (..)
  , HaltsV (..)
  , LogArr (..)
  , logVLam
  , botProof
  , Nat (..)
  , Fin (..)
  , UTerm (..)
  , Env (..)
  , Clos (..)
  , EvalBig (..)
  , AboveFun (..)
  , HaltsIx (..)
  , LogEIx
  , LogArrIx (..)
  , logVLamIx
  , fundamentalApp
  ) where

import Data.Kind (Type)

-- 1. Minimal LargeElimMutualData shape.
data Val = Bot | Arr Val Val | Join Val Val

type HaltsV :: Val -> Type
data HaltsV v where
  MkHaltsV :: logVLam v -> HaltsV v

newtype LogArr (v :: Val) (w :: Val) =
  MkLogArr { runLogArr :: HaltsV v -> HaltsV w }

logVLam :: Val -> Type
logVLam Bot        = type ()
logVLam (Arr v w)  = type (LogArr v w)
logVLam (Join u v) = type (logVLam u, logVLam v)

botProof :: logVLam Bot
botProof = ()

-- 2. Indexed PLFA Adequacy shape (forall n. Value -> Env n -> UTerm (Succ n) -> Type).
data Nat = Zero | Succ Nat

data Fin (n :: Nat) where
  FZ :: forall (n :: Nat). Fin (Succ n)
  FS :: forall (n :: Nat). Fin n -> Fin (Succ n)

data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

data Clos where
  MkClos :: UTerm n -> Env n -> Clos

data Env (n :: Nat) where
  ENil  :: Env Zero
  ECons :: Clos -> Env n -> Env (Succ n)

type EvalBig :: forall (n :: Nat). Env n -> UTerm n -> Clos -> Type
data EvalBig e m c where
  ELam :: EvalBig e (ULam b) (MkClos (ULam b) e)
  EApp :: EvalBig e l (MkClos (ULam b) d)
       -> EvalBig (ECons (MkClos m e) d) b c
       -> EvalBig e (UApp l m) c

data AboveFun (v :: Val) where
  AboveArr   :: AboveFun (Arr v w)
  AboveJoinL :: AboveFun u -> AboveFun (Join u v)
  AboveJoinR :: AboveFun v -> AboveFun (Join u v)

type HaltsIx :: forall (n :: Nat). Env n -> UTerm n -> Val -> Type
data HaltsIx e m v where
  MkHaltsIx :: EvalBig e m (MkClos (ULam b) d) -> logVLamIx v d b -> HaltsIx e m v

type LogEIx :: forall (k :: Nat). Val -> Env k -> UTerm k -> Type
type LogEIx v e m = AboveFun v -> HaltsIx e m v

newtype LogArrIx (v :: Val) (w :: Val) (e :: Env n) (b :: UTerm (Succ n)) =
  MkLogArrIx { runLogArrIx :: forall (k :: Nat) (d :: Env k) (m :: UTerm k).
                              LogEIx v d m -> AboveFun w -> HaltsIx (ECons (MkClos m d) e) b w }

logVLamIx :: forall (n :: Nat). Val -> Env n -> UTerm (Succ n) -> Type
logVLamIx Bot        _ _ = type ()
logVLamIx (Arr v w)  e b = type (LogArrIx v w e b)
logVLamIx (Join u v) e b = type (logVLamIx u e b, logVLamIx v e b)

fundamentalApp :: forall (n :: Nat) (e :: Env n) (l :: UTerm n) (m :: UTerm n) (v :: Val) (w :: Val).
                  LogEIx (Arr v w) e l
               -> LogEIx v e m
               -> LogEIx w e (UApp l m)
fundamentalApp leFun leArg af =
  case leFun AboveArr of
    MkHaltsIx evL lvL ->
      case runLogArrIx lvL leArg af of
        MkHaltsIx evB lvB -> MkHaltsIx (EApp evL evB) lvB

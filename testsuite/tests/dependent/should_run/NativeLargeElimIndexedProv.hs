{-# LANGUAGE DataKinds, ExplicitNamespaces, DependentTypes, GADTs, KindSignatures, ListTuplePuns #-}
module NativeLargeElimIndexedProv where

import Data.Kind (Type)

data Nat = Zero | Succ Nat
data Fin (n :: Nat) where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n
data Env (n :: Nat) where
  ENil  :: Env Zero
  ECons :: Clos -> Env n -> Env (Succ n)
data Clos where
  MkClos :: UTerm n -> Env n -> Clos
data Box :: Nat -> Type where
  MkBox :: Box n

data Val = Bot | Arr Val Val | Join Val Val
data Witness (v :: Val) (w :: Val) (e :: Env n) (b :: UTerm (Succ n)) = MkWitness

logVLamBox :: forall (n :: Nat). Val -> Box n -> Type
logVLamBox Bot        _ = type ()
logVLamBox (Arr _ _)  _ = type Int
logVLamBox (Join _ _) _ = type Bool

logVLamAdeq :: forall (n :: Nat). Val -> Env n -> UTerm (Succ n) -> Type
logVLamAdeq Bot        _ _ = type ()
logVLamAdeq (Arr v w)  e b = type (Witness v w e b)
logVLamAdeq (Join _ _) _ _ = type Bool

logVLamClos :: Val -> Clos -> Type
logVLamClos Bot        _ = type ()
logVLamClos (Arr _ _)  _ = type Int
logVLamClos (Join u v) c = type (logVLamClos u c, logVLamClos v c)

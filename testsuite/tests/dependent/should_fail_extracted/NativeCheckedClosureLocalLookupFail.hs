{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             StandaloneKindSignatures, TypeOperators #-}
module NativeCheckedClosureLocalLookupFail
  ( Nat(..), Fin(..), Env, lookupHidden, EvalHidden(..) ) where

import Data.Kind (Type)

data Nat = Zero | Succ Nat

data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data Env (n :: Nat) where
  ENil  :: Env 'Zero
  ECons :: Bool -> Env n -> Env ('Succ n)

lookupHidden :: Env n -> Fin n -> Bool
lookupHidden (ECons x _) FZ     = x
lookupHidden (ECons _ e) (FS i) = lookupHidden e i

type EvalHidden :: forall (n :: Nat). Env n -> Fin n -> Bool -> Type
data EvalHidden e x b where
  MkEvalHidden :: EvalHidden e x (lookupHidden e x)

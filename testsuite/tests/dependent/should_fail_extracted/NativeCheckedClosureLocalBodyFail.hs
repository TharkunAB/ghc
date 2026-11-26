{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             StandaloneKindSignatures #-}
module NativeCheckedClosureLocalBodyFail where

import Data.Kind (Type)

data Nat = Zero | Succ Nat
data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)
data Env (n :: Nat) where
  ENil  :: Env 'Zero
  ECons :: Bool -> Env n -> Env ('Succ n)

badWrongBody :: Env n -> Fin n -> Bool
badWrongBody (ECons x _) FZ     = x
badWrongBody (ECons x _) (FS _) = x

type EvalWrong :: forall (n :: Nat). Env n -> Fin n -> Bool -> Type
data EvalWrong e x b where
  MkEvalWrong :: EvalWrong e x (badWrongBody e x)

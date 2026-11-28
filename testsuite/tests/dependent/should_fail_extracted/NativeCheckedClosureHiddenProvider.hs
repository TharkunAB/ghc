{-# LANGUAGE DataKinds, DependentTypes, GADTs, LambdaCase, PolyKinds, RankNTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations -fomit-interface-pragmas #-}
module NativeCheckedClosureHiddenProvider
  ( Sort(..), Scope(..), Slot(..), Expr(..), substSingle
  , Env, Fin(..), lookupHidden ) where

data Sort = Nat | Arrow Sort Sort
data Scope = Empty | Push Scope Sort
data Slot (g :: Scope) (a :: Sort) where
  New :: Slot (Push g a) a
  Old :: Slot g a -> Slot (Push g b) a
data Expr (g :: Scope) (a :: Sort) where
  At   :: Slot g a -> Expr g a
  Bind :: Expr (Push g a) b -> Expr g (Arrow a b)
  Lit  :: Expr g Nat

extendName :: (forall a. Slot g a -> Slot d a) -> (forall s. Slot (Push g b) s -> Slot (Push d b) s)
extendName _ New = New
extendName f (Old x) = Old (f x)

renameExpr :: (forall a. Slot g a -> Slot d a) -> Expr g s -> Expr d s
renameExpr f (At x) = At (f x)
renameExpr f (Bind n) = Bind (renameExpr (extendName f) n)
renameExpr _ Lit = Lit

extendExpr :: (forall a. Slot g a -> Expr d a) -> (forall s. Slot (Push g b) s -> Expr (Push d b) s)
extendExpr _ New = At New
extendExpr f (Old x) = renameExpr Old (f x)

walk :: (forall a. Slot g a -> Expr d a) -> Expr g s -> Expr d s
walk f (At x) = f x
walk f (Bind n) = Bind (walk (extendExpr f) n)
walk _ Lit = Lit

substSingle :: Expr (Push g a) b -> Expr g a -> Expr g b
substSingle n w = walk (\case { New -> w; Old x -> At x }) n

data Env (g :: Scope) where
  ENil  :: Env Empty
  ECons :: Bool -> Env g -> Env (Push g a)

data Fin (g :: Scope) where
  FZ :: Fin (Push g a)
  FS :: Fin g -> Fin (Push g a)

lookupHidden :: Env g -> Fin g -> Bool
lookupHidden (ECons x _) FZ = x
lookupHidden (ECons _ e) (FS i) = lookupHidden e i

{-# LANGUAGE GADTs, RankNTypes #-}
-- Check interface serialization of SourceTable, CheckedClosureSourceTable,
-- FinalCoreSourceTable, and -fno-write-source-equations suppression.
module DepIfaceSerialization
  ( Nat(..), flipBit, monus
  , Sort(..), Scope(..), Slot(..), Expr(..)
  , extendName, renameExpr, extendExpr, walk, replace
  ) where

data Nat = Zero | Succ Nat

flipBit :: Bool -> Bool
flipBit False = True
flipBit True  = False

monus :: Nat -> Nat -> Nat
monus m        Zero     = m
monus Zero     (Succ _) = Zero
monus (Succ m) (Succ n) = monus m n

data Sort = Base | Arrow Sort Sort
data Scope = Empty | Push Scope Sort

data Slot (g :: Scope) (a :: Sort) where
  New :: Slot (Push g a) a
  Old :: Slot g a -> Slot (Push g b) a

data Expr (g :: Scope) (a :: Sort) where
  At   :: Slot g a -> Expr g a
  Bind :: Expr (Push g a) b -> Expr g (Arrow a b)
  Call :: Expr g (Arrow a b) -> Expr g a -> Expr g b

extendName :: (forall a. Slot g a -> Slot d a)
           -> (forall s. Slot (Push g b) s -> Slot (Push d b) s)
extendName _ New     = New
extendName f (Old x) = Old (f x)

renameExpr :: (forall a. Slot g a -> Slot d a) -> Expr g s -> Expr d s
renameExpr f (At x)     = At (f x)
renameExpr f (Bind n)   = Bind (renameExpr (extendName f) n)
renameExpr f (Call l m) = Call (renameExpr f l) (renameExpr f m)

extendExpr :: (forall a. Slot g a -> Expr d a)
           -> (forall s. Slot (Push g b) s -> Expr (Push d b) s)
extendExpr _ New     = At New
extendExpr f (Old x) = renameExpr Old (f x)

walk :: (forall a. Slot g a -> Expr d a) -> Expr g s -> Expr d s
walk f (At x)     = f x
walk f (Bind n)   = Bind (walk (extendExpr f) n)
walk f (Call l m) = Call (walk f l) (walk f m)

replace :: Expr (Push g a) b -> Expr g a -> Expr g b
replace n w = walk (\s -> case s of { New -> w; Old x -> At x }) n

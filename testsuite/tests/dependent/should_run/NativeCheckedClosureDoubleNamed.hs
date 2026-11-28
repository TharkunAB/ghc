{-# LANGUAGE DataKinds, DependentTypes, GADTs, LambdaCase, PolyKinds, RankNTypes #-}
module NativeCheckedClosureDoubleNamed where

data Sort = Nat | FlagSort
data Scope = Empty | Push Scope Sort

data Slot (g :: Scope) (a :: Sort) where
  New :: Slot (Push g a) a
  Old :: Slot g a -> Slot (Push g b) a

data Expr (g :: Scope) (a :: Sort) where
  At :: Slot g a -> Expr g a
  Lit :: Expr g Nat
  Incr :: Expr g Nat -> Expr g Nat
  Flag :: Expr g FlagSort
  Both :: Expr g Nat -> Expr g FlagSort -> Expr g Nat
  Bind :: Expr (Push g Nat) Nat -> Expr g Nat
  BindBoth :: Expr (Push (Push g Nat) FlagSort) Nat -> Expr g Nat

extendName :: (forall a. Slot g a -> Slot d a)
           -> (forall s. Slot (Push g b) s -> Slot (Push d b) s)
extendName _ New = New
extendName f (Old x) = Old (f x)

renameExpr :: (forall a. Slot g a -> Slot d a) -> Expr g s -> Expr d s
renameExpr f (At x) = At (f x)
renameExpr _ Lit = Lit
renameExpr f (Incr n) = Incr (renameExpr f n)
renameExpr _ Flag = Flag
renameExpr f (Both n b) = Both (renameExpr f n) (renameExpr f b)
renameExpr f (Bind n) = Bind (renameExpr (extendName f) n)
renameExpr f (BindBoth n) = BindBoth (renameExpr (extendName (extendName f)) n)

{-# NOINLINE weakenSlot #-}
weakenSlot :: forall g b a. Slot g a -> Slot (Push g b) a
weakenSlot x = Old x

extendExpr :: (forall a. Slot g a -> Expr d a)
           -> (forall s. Slot (Push g b) s -> Expr (Push d b) s)
extendExpr _ New = At New
extendExpr f (Old x) = renameExpr weakenSlot (f x)

walk :: (forall a. Slot g a -> Expr d a) -> Expr g s -> Expr d s
walk f (At x) = f x
walk _ Lit = Lit
walk f (Incr n) = Incr (walk f n)
walk _ Flag = Flag
walk f (Both n b) = Both (walk f n) (walk f b)
walk f (Bind n) = Bind (walk (extendExpr f) n)
walk f (BindBoth n) = BindBoth (walk (extendExpr (extendExpr f)) n)

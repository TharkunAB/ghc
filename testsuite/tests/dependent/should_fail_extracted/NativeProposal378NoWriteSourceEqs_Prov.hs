{-# OPTIONS_GHC -fno-write-source-equations #-}
{-# LANGUAGE DependentTypes #-}
-- Strictly single-pragma module testing Proposal #378 -XDependentTypes bundle,
-- default-on source equations, and multi-binder forall/foreach/some/exists telescopes.
module NativeProposal378NoWriteSourceEqs_Prov
  ( Nat(..), Vec(..), Sort(..), Scope(..), Slot(..), Expr(..)
  , Pair2(..), Triple3(..), Proxy(..)
  , plus, extendName, renameExpr, extendExpr, walk, replace
  , PairVecs, ThreeVecs, PipeSomeVecs, ExistsVec, ExistsBarVec
  ) where

import Data.Kind (Type)

data Pair2 a b = MkPair2 a b
data Triple3 a b c = MkTriple3 a b c

type Nat :: Type
data Nat = Zero | Succ Nat deriving (Eq, Show)

type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

-- Closure-capturing substitution published automatically without -fwrite-source-equations:
data Sort = SNat | SArr Sort Sort
data Scope = Empty | Push Scope Sort

data Slot (g :: Scope) (a :: Sort) where
  New :: Slot (Push g a) a
  Old :: Slot g a -> Slot (Push g b) a

data Expr (g :: Scope) (a :: Sort) where
  At   :: Slot g a -> Expr g a
  Bind :: Expr (Push g a) b -> Expr g (SArr a b)
  Lit  :: Expr g SNat

extendName :: (forall a. Slot g a -> Slot d a)
           -> (forall s. Slot (Push g b) s -> Slot (Push d b) s)
extendName _ New = New
extendName f (Old x) = Old (f x)

renameExpr :: (forall a. Slot g a -> Slot d a) -> Expr g s -> Expr d s
renameExpr f (At x) = At (f x)
renameExpr f (Bind n) = Bind (renameExpr (extendName f) n)
renameExpr _ Lit = Lit

extendExpr :: (forall a. Slot g a -> Expr d a)
           -> (forall s. Slot (Push g b) s -> Expr (Push d b) s)
extendExpr _ New = At New
extendExpr f (Old x) = renameExpr Old (f x)

walk :: (forall a. Slot g a -> Expr d a) -> Expr g s -> Expr d s
walk f (At x) = f x
walk f (Bind n) = Bind (walk (extendExpr f) n)
walk _ Lit = Lit

replace :: Expr (Push g a) b -> Expr g a -> Expr g b
replace n w = walk (\x -> case x of { New -> w; Old y -> At y }) n

-- Multi-binder some and exists telescopes (both '.' and '|') with NoListTuplePuns active:
data Proxy (x :: k) = MkProxy

type PairVecs a = some (m n :: Nat). Pair2 (Vec a m) (Vec a n)
type ThreeVecs a = some (m :: Nat) (n p :: Nat). Triple3 (Vec a m) (Vec a n) (Vec a p)
type PipeSomeVecs a = some (m :: Nat) (n :: Nat) | Pair2 (Vec a m) (Vec a n)
type ExistsVec = exists {k} (x :: k) (m n :: Nat). Triple3 (Proxy x) (Vec Int m) (Vec Int n)
type ExistsBarVec a = exists (m n :: Nat) | Pair2 (Vec a m) (Vec a n)

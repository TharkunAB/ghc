{-# LANGUAGE DependentTypes #-}
module NativeProposal378QuantifiersProvider
  ( Nat(..)
  , plus
  , Vec(..)
  , toListVec
  , quickLength
  , vReplicateImplicit
  , vReplicateExplicit
  , skipInferred
  , onlyInferred
  , unannotId
  , unannotLen
  , unannotPlus
  , unannotInferred
  , wildcardType
  , the
  , ex5
  , ex5Annot
  , ex5Maybe
  , idDep
  , implType
  , Ty(..)
  , Term(..)
  , Step(..)
  , Prog(..)
  , liftApp1
  , EqP(..)
  , Leibniz(..)
  , leibnizToEqP
  , Ctx(..)
  , FinCtx(..)
  , STerm(..)
  ) where

import Data.Kind (Type)
import GHC.Matchability (U)
import GHC.Tuple (Tuple2)
import GHC.Types (List)

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

data Vec a (n :: Nat) where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

toListVec :: Vec a n -> List a
toListVec VNil = []
toListVec (VCons x xs) = x : toListVec xs

-- Proposal #378 §4.8: @pat visibility override for SpecifiedSpec invisible foreach
quickLength :: forall a. foreach (n :: Nat). Vec a n -> Nat
quickLength @_ @n _ = n

vReplicateImplicit :: forall {a}. foreach (n :: Nat). a -> Vec a n
vReplicateImplicit @Zero _ = VNil
vReplicateImplicit @(Succ k) x = VCons x (vReplicateImplicit @k x)

vReplicateExplicit :: forall a. foreach (n :: Nat). a -> Vec a n
vReplicateExplicit @_ @Zero _ = VNil
vReplicateExplicit @a @(Succ k) x = VCons x (vReplicateExplicit @a @k x)

-- Proposal #378 §4.13: {m :: Nat} has InferredSpec and is skipped by @pat / @arg
skipInferred :: forall a. foreach {m :: Nat}. foreach (n :: Nat). Vec a m -> Vec a n -> Tuple2 Nat Nat
skipInferred @_ @n _ _ = (m, n)

onlyInferred :: forall a. foreach {k :: Nat}. Vec a k -> Nat
onlyInferred _ = k

-- Proposal #378 §4.4 & §4.11: unannotated foreach binders and wildcard (_ :: t1) binders
unannotId :: foreach a -> a -> a
unannotId _ x = x

unannotLen :: forall a. foreach n -> foreach (_ :: Vec a n) -> Nat
unannotLen n _ = n

unannotPlus :: forall a. foreach m n. Vec a m -> Vec a n -> Nat
unannotPlus @_ @m @n _ _ = plus m n

unannotInferred :: forall a. foreach {n}. Vec a n -> Nat
unannotInferred _ = n

wildcardType :: foreach _ -> Int -> Int
wildcardType _ x = x + 1

-- Proposal #378 §4.9: foreach (a :: Type) -> scoping and unadorned type arguments
the :: forall a -> a -> a
the _ x = x

ex5 :: foreach (a :: Type) -> a -> a
ex5 a x = the a (x :: a)

ex5Annot :: foreach (a :: Type) -> a -> a
ex5Annot a (x :: a) = the a x

ex5Maybe :: foreach (a :: Type) -> a -> Maybe a
ex5Maybe a x = ex5 (Maybe a) (Just x)

idDep :: foreach (a :: Type) -> foreach (x :: a) -> a
idDep _ x = x

implType :: foreach (a :: Type). a -> Tuple2 a (List Char)
implType @b (x :: b) = (the b x, show b)

-- Existential GADT retained operand
data Ty = TUnit | TArr Ty Ty
  deriving (Eq, Show)

data Term :: Ty -> Type where
  Unit :: Term TUnit
  App  :: Term (TArr a b) -> Term a -> Term b

data Step :: forall a. Term a -> Term a -> Type where
  StepUnit :: Step Unit Unit
  XiApp1   :: Step l l' -> Step (App l m) (App l' m)

data Prog :: forall a. Term a -> Type where
  PStep :: foreach (n :: Term a) -> Step m n -> Prog m

liftApp1 :: foreach (l :: Term (TArr a b)) (m :: Term a) -> Prog l -> Prog (App l m)
liftApp1 _ m (PStep l' s) = PStep (App l' m) (XiApp1 s)

-- Matchability forgetfulness in Leibniz equality
data EqP (x :: a) :: a -> Type where
  ReflP :: EqP x x

newtype Leibniz a (x :: a) (y :: a)
  = Leibniz (forall (p :: a %U -> Type). p x -> p y)

leibnizToEqP :: forall a (x :: a) (y :: a). Leibniz a x y -> EqP x y
leibnizToEqP (Leibniz f) = f @(EqP x) ReflP

-- Higher-rank renaming callback kind matchability inference
data Ctx = Empty | Snoc Ctx Ty

data FinCtx (g :: Ctx) (t :: Ty) where
  FZ :: FinCtx (Snoc g t) t
  FS :: FinCtx g t -> FinCtx (Snoc g s) t

data STerm (g :: Ctx) (t :: Ty) where
  SVar :: FinCtx g t -> STerm g t
  SLam :: STerm (Snoc g a) b -> STerm g (TArr a b)

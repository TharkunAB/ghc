{-# OPTIONS_GHC -DHIDE_SINGLE_SUBST #-}
{-# LANGUAGE AllowAmbiguousTypes, CPP, DependentTypes, LambdaCase #-}
module NativeSameModHideNamedSingleSubst
#ifdef HIDE_SINGLE_SUBST
  ( Ty(..), Ctx(..), Lookup(..), Term(..), weakenLookup, ext, rename, exts, subst, substSingle, Step(..) )
#endif
  where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (U)
import GHC.Types (Matchability)

data Ty = TNat | TArr Ty Ty
data Ctx = CEmpty | CExt Ctx Ty
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a
data Term (g :: Ctx) (a :: Ty) where
  Var  :: Lookup g a -> Term g a
  Lam  :: Term (CExt g a) b -> Term g (TArr a b)
  App  :: Term g (TArr a b) -> Term g a -> Term g b
  Zero :: Term g TNat

weakenLookup :: forall d b a. Lookup d a -> Lookup (CExt d b) a
weakenLookup x = S x

ext :: (forall a. Lookup g a -> Lookup d a) -> (forall a. Lookup (CExt g b) a -> Lookup (CExt d b) a)
ext _ Z     = Z
ext r (S x) = S (r x)

rename :: (forall a. Lookup g a -> Lookup d a) -> Term g b -> Term d b
rename r (Var x)   = Var (r x)
rename r (Lam n)   = Lam (rename (ext r) n)
rename r (App l m) = App (rename r l) (rename r m)
rename _ Zero      = Zero

exts :: forall g d b. (forall a. Lookup g a -> Term d a) -> (forall a. Lookup (CExt g b) a -> Term (CExt d b) a)
exts _ Z     = Var Z
exts s (S x) = rename weakenLookup (s x)

subst :: (forall a. Lookup g a -> Term d a) -> Term g b -> Term d b
subst s (Var x)   = s x
subst s (Lam n)   = Lam (subst (exts s) n)
subst s (App l m) = App (subst s l) (subst s m)
subst _ Zero      = Zero

singleSubst :: forall g a. Term g a -> (forall b. Lookup (CExt g a) b -> Term g b)
singleSubst w Z     = w
singleSubst _ (S x) = Var x

substSingle :: Term (CExt g a) b -> Term g a -> Term g b
substSingle n w = subst (singleSubst w) n

#ifdef RULE_SINGLE_SUBST
{-# RULES "hijackSingleSubst" forall w. singleSubst w Z = Zero #-}
#endif

type Step :: forall (g :: Ctx) (a :: Ty). Term g a -> Term g a -> Type
data Step m n where
  BetaLam :: forall g a b (body :: Term (CExt g a) b) (v :: Term g a).
             Step (App (Lam body) v) (substSingle body v)

sameModTypedEq :: forall g a b (n :: Term (CExt g a) b) (v :: Term g a).
                  substSingle n v :~: subst (singleSubst v) n
sameModTypedEq = Refl

#ifndef HIDE_SINGLE_SUBST
data Nat = NZero | NSucc Nat
data Fin (n :: Nat) where
  FZ :: Fin (NSucc n)
  FS :: Fin n -> Fin (NSucc n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (NSucc n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

extU :: (Fin m -> Fin n) -> Fin (NSucc m) -> Fin (NSucc n)
extU _   FZ     = FZ
extU rho (FS x) = FS (rho x)

renameU :: (Fin m -> Fin n) -> UTerm m -> UTerm n
renameU rho (UVar x)   = UVar (rho x)
renameU rho (ULam b)   = ULam (renameU (extU rho) b)
renameU rho (UApp l m) = UApp (renameU rho l) (renameU rho m)

extsU :: (Fin m -> UTerm n) -> Fin (NSucc m) -> UTerm (NSucc n)
extsU _     FZ     = UVar FZ
extsU sigma (FS x) = renameU shift (sigma x)

substU :: (Fin m -> UTerm n) -> UTerm m -> UTerm n
substU sigma (UVar x)   = sigma x
substU sigma (ULam b)   = ULam (substU (extsU sigma) b)
substU sigma (UApp l m) = UApp (substU sigma l) (substU sigma m)

substSingleInlineU :: UTerm (NSucc n) -> UTerm n -> UTerm n
substSingleInlineU b w = substU (\case { FZ -> w; FS x -> UVar x }) b

singleSubstU :: UTerm n -> Fin (NSucc n) -> UTerm n
singleSubstU w FZ     = w
singleSubstU _ (FS x) = UVar x

substSingleNamedU :: UTerm (NSucc n) -> UTerm n -> UTerm n
substSingleNamedU b w = substU (singleSubstU w) b

type Rename m n = Fin m -> Fin n
type Subst m n = Fin m -> UTerm n

ids :: Subst m m
ids = UVar

shift :: Rename m (NSucc m)
shift = FS

myShift :: Rename m (NSucc m)
myShift = FS

ren :: Rename m n -> Subst m n
ren r x = UVar (r x)

consSubst :: UTerm n -> Subst m n -> Subst (NSucc m) n
consSubst t _ FZ     = t
consSubst _ s (FS x) = s x

seqSubst :: Subst m n -> Subst n k -> Subst m k
seqSubst p q x = substU q (p x)

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

sameIds :: forall m (x :: Fin m). substU (ids @m) (UVar x) :~: UVar x
sameIds = Refl

sameShiftSig :: forall m (x :: Fin m). renameU (shift @m) (UVar x) :~: UVar (FS x)
sameShiftSig = Refl

sameShiftCong :: forall m n (p :: Fin m %U -> UTerm n) (q :: Fin m %U -> UTerm n) (x :: Fin m).
                 (p x :~: q x) -> extsU p (FS x) :~: extsU q (FS x)
sameShiftCong eq = cong (renameU shift) eq

t2tOnlyCong :: forall n (x :: Fin n). UVar (FS x) :~: UVar (FS x)
t2tOnlyCong = cong (renameU myShift) (Refl :: UVar x :~: UVar x)

sameRen :: forall m n (r :: Fin m %U -> Fin n) (x :: Fin m).
           substU (ren r) (UVar x) :~: UVar (r x)
sameRen = Refl

sameConsZ :: forall m n (t :: UTerm n) (s :: Fin m %U -> UTerm n).
             consSubst t s FZ :~: t
sameConsZ = Refl

sameSeq :: forall m n k (p :: Fin m %U -> UTerm n) (q :: Fin n %U -> UTerm k) (x :: Fin m).
           substU (seqSubst p q) (UVar x) :~: substU q (p x)
sameSeq = Refl

sameNamedU :: forall n (b :: UTerm (NSucc n)) (w :: UTerm n).
              substSingleNamedU b w :~: substU (singleSubstU w) b
sameNamedU = Refl

sameInlineUZ :: forall n (w :: UTerm n). substSingleInlineU (UVar FZ) w :~: w
sameInlineUZ = Refl
#endif

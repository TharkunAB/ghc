{-# LANGUAGE DataKinds, DependentTypes, GADTs, LambdaCase, ListTuplePuns, PolyKinds, RankNTypes, TemplateHaskell #-}
module NativeSameModulePrunedSplice (Nat, UCtx, UTerm, Ty, Ctx, Lookup, Term, plug, substSingle, ok) where

data Nat = Zero | Succ Nat
data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
data UCtx (m :: Nat) (n :: Nat) where
  CHole :: UCtx n n
  CLam  :: UCtx m ('Succ n) -> UCtx m n

plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole    t = t
plug (CLam c) t = ULam (plug c t)

data Ty = TNat | TArr Ty Ty
data Ctx = CEmpty | CExt Ctx Ty
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a
data Term (g :: Ctx) (a :: Ty) where
  Var :: Lookup g a -> Term g a
  Lam :: Term (CExt g a) b -> Term g (TArr a b)

ext :: (forall a. Lookup g a -> Lookup d a) -> Lookup (CExt g b) a -> Lookup (CExt d b) a
ext _ Z     = Z
ext r (S x) = S (r x)

rename :: (forall a. Lookup g a -> Lookup d a) -> Term g a -> Term d a
rename r (Var x) = Var (r x)
rename r (Lam n) = Lam (rename (ext r) n)

exts :: (forall a. Lookup g a -> Term d a) -> Lookup (CExt g b) a -> Term (CExt d b) a
exts _ Z     = Var Z
exts s (S x) = rename S (s x)

subst :: (forall a. Lookup g a -> Term d a) -> Term g a -> Term d a
subst s (Var x) = s x
subst s (Lam n) = Lam (subst (exts s) n)

substSingle :: Term (CExt g b) a -> Term g b -> Term g a
substSingle m v = subst (\case { Z -> v; S x -> Var x }) m

$(pure [])

ok :: Int
ok = 1

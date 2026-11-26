{-# LANGUAGE DataKinds, DependentTypes, GADTs, LambdaCase, ListTuplePuns, PolyKinds, RankNTypes, TypeOperators #-}
module NativeSameModOrphanBootRule (Ty(..), Ctx(..), Lookup(..), Term(..), ext, rename, exts, subst, substSingle, law) where
import Data.Type.Equality ((:~:) (Refl))
import NativeSameModOrphanBootRule_Orphan ()
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
{-# NOINLINE substSingle #-}
substSingle :: Term (CExt g b) a -> Term g b -> Term g a
substSingle m v = subst (\case { Z -> v; S x -> Var x }) m
law :: forall g a (v :: Term g a). substSingle (Var Z) v :~: v
law = Refl

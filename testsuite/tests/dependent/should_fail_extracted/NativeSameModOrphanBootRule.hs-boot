{-# LANGUAGE DataKinds, GADTs, PolyKinds #-}
module NativeSameModOrphanBootRule (Ty(..), Ctx(..), Lookup(..), Term(..), substSingle) where
data Ty = TNat | TArr Ty Ty
data Ctx = CEmpty | CExt Ctx Ty
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a
data Term (g :: Ctx) (a :: Ty) where
  Var :: Lookup g a -> Term g a
  Lam :: Term (CExt g a) b -> Term g (TArr a b)
substSingle :: Term (CExt g b) a -> Term g b -> Term g a

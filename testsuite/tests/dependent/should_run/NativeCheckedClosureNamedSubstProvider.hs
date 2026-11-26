{-# LANGUAGE DataKinds, DependentTypes #-}
module NativeCheckedClosureNamedSubstProvider where

data Ty = TNat | TArr Ty Ty | TList Ty
data Ctx = CEmpty | CExt Ctx Ty

data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a

data Term (g :: Ctx) (a :: Ty) where
  Var      :: Lookup g a -> Term g a
  Lam      :: Term (CExt g a) b -> Term g (TArr a b)
  App      :: Term g (TArr a b) -> Term g a -> Term g b
  Zero     :: Term g TNat
  Nil      :: Term g (TList a)
  Cons     :: Term g a -> Term g (TList a) -> Term g (TList a)
  CaseList :: Term g (TList a) -> Term g b -> Term (CExt (CExt g a) (TList a)) b -> Term g b

weakenLookup :: forall d b a. Lookup d a -> Lookup (CExt d b) a
weakenLookup x = S x

ext :: (forall a. Lookup g a -> Lookup d a)
    -> (forall a. Lookup (CExt g b) a -> Lookup (CExt d b) a)
ext _ Z     = Z
ext r (S x) = S (r x)

rename :: (forall a. Lookup g a -> Lookup d a) -> Term g b -> Term d b
rename r (Var x)          = Var (r x)
rename r (Lam n)          = Lam (rename (ext r) n)
rename r (App l m)        = App (rename r l) (rename r m)
rename _ Zero             = Zero
rename _ Nil              = Nil
rename r (Cons x xs)      = Cons (rename r x) (rename r xs)
rename r (CaseList l n c) = CaseList (rename r l) (rename r n) (rename (ext (ext r)) c)

exts :: forall g d b.
        (forall a. Lookup g a -> Term d a)
     -> (forall a. Lookup (CExt g b) a -> Term (CExt d b) a)
exts _ Z     = Var Z
exts s (S x) = rename weakenLookup (s x)

subst :: (forall a. Lookup g a -> Term d a) -> Term g b -> Term d b
subst s (Var x)          = s x
subst s (Lam n)          = Lam (subst (exts s) n)
subst s (App l m)        = App (subst s l) (subst s m)
subst _ Zero             = Zero
subst _ Nil              = Nil
subst s (Cons x xs)      = Cons (subst s x) (subst s xs)
subst s (CaseList l n c) = CaseList (subst s l) (subst s n) (subst (exts (exts s)) c)

singleSubst :: forall g a. Term g a -> (forall b. Lookup (CExt g a) b -> Term g b)
singleSubst w Z     = w
singleSubst _ (S x) = Var x

substSingle :: Term (CExt g a) b -> Term g a -> Term g b
substSingle n w = subst (singleSubst w) n

substSinglePerm :: Term g a -> Term (CExt g a) b -> Term g b
substSinglePerm w n = subst (singleSubst w) n

doubleSubst :: forall g a b. Term g a -> Term g b -> (forall c. Lookup (CExt (CExt g a) b) c -> Term g c)
doubleSubst _  vs Z         = vs
doubleSubst v  _  (S Z)     = v
doubleSubst _  _  (S (S x)) = Var x

substDouble :: Term (CExt (CExt g a) b) c -> Term g a -> Term g b -> Term g c
substDouble m v vs = subst (doubleSubst v vs) m

doubleSubstSwapped :: forall g a b. Term g b -> Term g a -> (forall c. Lookup (CExt (CExt g a) b) c -> Term g c)
doubleSubstSwapped vs _  Z         = vs
doubleSubstSwapped _  v  (S Z)     = v
doubleSubstSwapped _  _  (S (S x)) = Var x

substDoubleSwapped :: Term g b -> Term g a -> Term (CExt (CExt g a) b) c -> Term g c
substDoubleSwapped vs v m = subst (doubleSubstSwapped vs v) m

idsT :: forall g a. Lookup g a -> Term g a
idsT = Var

shiftT :: forall g b a. Lookup g a -> Lookup (CExt g b) a
shiftT = S

renT :: forall g d. (forall a. Lookup g a -> Lookup d a) -> (forall a. Lookup g a -> Term d a)
renT r x = Var (r x)

consSubstT :: forall g d b. Term d b -> (forall a. Lookup g a -> Term d a)
           -> (forall a. Lookup (CExt g b) a -> Term d a)
consSubstT t _ Z     = t
consSubstT _ s (S x) = s x

seqSubstT :: forall g d e. (forall a. Lookup g a -> Term d a)
          -> (forall a. Lookup d a -> Term e a)
          -> (forall a. Lookup g a -> Term e a)
seqSubstT p q x = subst q (p x)

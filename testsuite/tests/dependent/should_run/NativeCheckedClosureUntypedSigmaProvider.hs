{-# LANGUAGE DataKinds, DependentTypes, LambdaCase #-}
module NativeCheckedClosureUntypedSigmaProvider where

data Nat = Zero | Succ Nat

data Fin (n :: Nat) where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

shiftFin :: Fin n -> Fin (Succ n)
shiftFin x = FS x

extU :: (Fin m -> Fin n) -> Fin (Succ m) -> Fin (Succ n)
extU _   FZ     = FZ
extU rho (FS x) = FS (rho x)

renameU :: (Fin m -> Fin n) -> UTerm m -> UTerm n
renameU rho (UVar x)   = UVar (rho x)
renameU rho (ULam b)   = ULam (renameU (extU rho) b)
renameU rho (UApp l m) = UApp (renameU rho l) (renameU rho m)

extsU :: (Fin m -> UTerm n) -> Fin (Succ m) -> UTerm (Succ n)
extsU _     FZ     = UVar FZ
extsU sigma (FS x) = renameU shiftFin (sigma x)

substU :: (Fin m -> UTerm n) -> UTerm m -> UTerm n
substU sigma (UVar x)   = sigma x
substU sigma (ULam b)   = ULam (substU (extsU sigma) b)
substU sigma (UApp l m) = UApp (substU sigma l) (substU sigma m)

substSingleU :: UTerm (Succ n) -> UTerm n -> UTerm n
substSingleU b w = substU (\case { FZ -> w; FS x -> UVar x }) b

type Rename m n = Fin m -> Fin n
type Subst m n = Fin m -> UTerm n

ids :: Subst m m
ids = UVar

shift :: Rename m (Succ m)
shift = FS

ren :: Rename m n -> Subst m n
ren r x = UVar (r x)

consSubst :: UTerm n -> Subst m n -> Subst (Succ m) n
consSubst t _ FZ     = t
consSubst _ s (FS x) = s x

seqSubst :: Subst m n -> Subst n k -> Subst m k
seqSubst p q x = substU q (p x)

singleSubstU :: UTerm n -> Fin (Succ n) -> UTerm n
singleSubstU w FZ     = w
singleSubstU _ (FS x) = UVar x

substSingleNamedU :: UTerm (Succ n) -> UTerm n -> UTerm n
substSingleNamedU m w = substU (singleSubstU w) m

{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, EmptyCase, DependentTypes, ListTuplePuns, GADTs #-}
{-# LANGUAGE LambdaCase, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications, TypeOperators #-}
module NativeSameModuleClosurePlugProvider
#if defined(HIDE_UCTX_CON)
  (Nat(..), Fin(..), UTerm(..), UCtx(CHole, CAppL, CAppR), SCtx(..),
   plug, plugS, plugLamEq, extU, renameU, extsU, substU, substSingleU,
   Ty(..), Ctx(..), Lookup(..), Term(..), ext, rename, weakenLookup, exts, subst, substSingle, substDouble, Value(..), Step(..), betaShift)
#elif defined(HIDE_TERM_CON)
  (Nat(..), Fin(..), UTerm(..), UCtx(..), SCtx(..),
   plug, plugS, extU, renameU, extsU, substU, substSingleU,
   Ty(..), Ctx(..), Lookup(..), Term(Var, Lam), ext, rename, weakenLookup, exts, subst, substSingle, substDouble, Step(..), betaShift)
#elif defined(HIDE_LOOKUP_CON)
  (Nat(..), Fin(..), UTerm(..), UCtx(..), SCtx(..),
   plug, plugS, extU, renameU, extsU, substU, substSingleU,
   Ty(..), Ctx(..), Lookup(Z), Term(..), ext, rename, weakenLookup, exts, subst, substSingle, substDouble, Step(..), betaShift)
#elif defined(HIDE_PLUG)
  (Nat(..), Fin(..), UTerm(..), UCtx(..), SCtx(..),
   plugS, plugLamEq, extU, renameU, extsU, substU, substSingleU,
   Ty(..), Ctx(..), Lookup(..), Term(..), ext, rename, weakenLookup, exts, subst, substSingle, substDouble, Value(..), Step(..), betaShift)
#elif defined(HIDE_SUBST_SINGLE)
  (Nat(..), Fin(..), UTerm(..), UCtx(..), SCtx(..),
   plug, plugS, extU, renameU, extsU, substU, substSingleU,
   Ty(..), Ctx(..), Lookup(..), Term(..), ext, rename, weakenLookup, exts, subst, substDouble, Value(..), Step(..), betaShift)
#elif defined(HIDE_EXTS)
  (Nat(..), Fin(..), UTerm(..), UCtx(..), SCtx(..),
   plug, plugS, extU, renameU, extsU, substU, substSingleU,
   Ty(..), Ctx(..), Lookup(..), Term(..), ext, rename, weakenLookup, subst, substSingle, substDouble, Value(..), Step(..), betaShift)
#endif
where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

data UCtx (m :: Nat) (n :: Nat) where
  CHole :: UCtx n n
  CLam  :: UCtx m ('Succ n) -> UCtx m n
  CAppL :: UCtx m n -> UTerm n -> UCtx m n
  CAppR :: UTerm n -> UCtx m n -> UCtx m n

data SCtx (m :: Nat) (n :: Nat) where
  SHole :: SCtx n n
  SLam  :: SCtx ('Succ m) ('Succ n) -> SCtx ('Succ m) n
  SAppL :: SCtx m n -> UTerm n -> SCtx m n
  SAppR :: UTerm n -> SCtx m n -> SCtx m n

#ifdef REVERSED_ORDER
plugLamEq :: forall m n (c :: UCtx m ('Succ n)) (t :: UTerm m).
             plug (CLam c) t :~: ULam (plug c t)
plugLamEq = Refl

plugSLamEq :: forall m n (c :: SCtx ('Succ m) ('Succ n)) (t :: UTerm ('Succ m)).
              plugS (SLam c) t :~: ULam (plugS c t)
plugSLamEq = Refl
#endif

plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole       t =
#ifdef STRICT_PLUG
  t `seq` t
#else
  t
#endif
plug (CLam c)    t =
#ifdef WRONG_PLUG
  ULam (plug c (UApp t t))
#else
  ULam (plug c t)
#endif
plug (CAppL c r) t = UApp (plug c t) r
plug (CAppR l c) t = UApp l (plug c t)

#ifdef SPECIALISE_PLUG
{-# SPECIALISE plug :: UCtx 'Zero 'Zero -> UTerm 'Zero -> UTerm 'Zero #-}
#endif

#ifdef LOCAL_RULE_PLUG
{-# NOINLINE plug #-}
{-# RULES "bad/plug" forall c t. plug c t = t #-}
#endif

plugS :: SCtx m n -> UTerm m -> UTerm n
plugS SHole       t = t
plugS (SLam c)    t = ULam (plugS c t)
plugS (SAppL c r) t = UApp (plugS c t) r
plugS (SAppR l c) t = UApp l (plugS c t)

#ifndef REVERSED_ORDER
plugLamEq :: forall m n (c :: UCtx m ('Succ n)) (t :: UTerm m).
             plug (CLam c) t :~: ULam (plug c t)
plugLamEq = Refl

plugSLamEq :: forall m n (c :: SCtx ('Succ m) ('Succ n)) (t :: UTerm ('Succ m)).
              plugS (SLam c) t :~: ULam (plugS c t)
plugSLamEq = Refl
#endif

-- Untyped scope-indexed substitution in the same module
extU :: (Fin m -> Fin n) -> Fin ('Succ m) -> Fin ('Succ n)
extU _   FZ     = FZ
extU rho (FS x) = FS (rho x)

renameU :: (Fin m -> Fin n) -> UTerm m -> UTerm n
renameU rho (UVar x)   = UVar (rho x)
renameU rho (ULam b)   = ULam (renameU (extU rho) b)
renameU rho (UApp l m) = UApp (renameU rho l) (renameU rho m)

extsU :: (Fin m -> UTerm n) -> Fin ('Succ m) -> UTerm ('Succ n)
extsU _     FZ     = UVar FZ
extsU sigma (FS x) = renameU FS (sigma x)

substU :: (Fin m -> UTerm n) -> UTerm m -> UTerm n
substU sigma (UVar x)   = sigma x
substU sigma (ULam b)   = ULam (substU (extsU sigma) b)
substU sigma (UApp l m) = UApp (substU sigma l) (substU sigma m)

substSingleU :: UTerm ('Succ n) -> UTerm n -> UTerm n
substSingleU b w = substU (\case
  FZ -> w
  FS x -> UVar x) b

renameULamEq :: forall m n (r :: Fin m -> Fin n) (b :: UTerm ('Succ m)).
                renameU r (ULam b) :~: ULam (renameU (extU r) b)
renameULamEq = Refl

betaUntypedEq :: forall n (x :: Fin n).
                 substSingleU (ULam (UVar (FS FZ))) (UVar x) :~: ULam (UVar (FS x))
betaUntypedEq = Refl

-- Intrinsically-typed STLC with single & double substitution in the same module
data Ty = TNat | TArr Ty Ty | TList Ty
  deriving (Eq, Show)

data Ctx = CEmpty | CExt Ctx Ty
  deriving (Eq, Show)

data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a

data Term (g :: Ctx) (a :: Ty) where
  Var     :: Lookup g a -> Term g a
  Lam     :: Term (CExt g a) b -> Term g (TArr a b)
  App     :: Term g (TArr a b) -> Term g a -> Term g b
  TZero   :: Term g TNat
  TNil    :: Term g (TList a)
  TCons   :: Term g a -> Term g (TList a) -> Term g (TList a)
  CaseLst :: Term g (TList a) -> Term g b -> Term (CExt (CExt g a) (TList a)) b -> Term g b

#ifdef REVERSED_ORDER
type Value :: forall (g :: Ctx) (a :: Ty). Term g a -> Type
data Value m where
  VLam  :: Value (Lam n)
  VZero :: Value TZero
  VNil  :: Value TNil
  VCons :: Value v -> Value vs -> Value (TCons v vs)

type Step :: forall (g :: Ctx) (a :: Ty). Term g a -> Term g a -> Type
data Step m n where
  BetaLam :: forall g a b (n :: Term (CExt g a) b) (w :: Term g a).
             Value w -> Step (App (Lam n) w) (substSingle n w)
  BetaLst :: forall g a b (v :: Term g a) (vs :: Term g (TList a))
                    (n :: Term g b) (c :: Term (CExt (CExt g a) (TList a)) b).
             Value v -> Value vs -> Step (CaseLst (TCons v vs) n c) (substDouble c v vs)
#endif

#ifdef PRENEX_EXT
ext :: (forall a. Lookup g a -> Lookup d a) -> Lookup (CExt g b) a -> Lookup (CExt d b) a
#else
ext :: (forall a. Lookup g a -> Lookup d a)
    -> (forall a. Lookup (CExt g b) a -> Lookup (CExt d b) a)
#endif
ext _ Z     = Z
ext r (S x) = S (r x)

rename :: (forall a. Lookup g a -> Lookup d a) -> Term g b -> Term d b
rename r (Var x)          = Var (r x)
rename r (Lam n)          = Lam (rename (ext r) n)
rename r (App l m)        = App (rename r l) (rename r m)
rename _ TZero            = TZero
rename _ TNil             = TNil
rename r (TCons x xs)     = TCons (rename r x) (rename r xs)
rename r (CaseLst xs n c) = CaseLst (rename r xs) (rename r n) (rename (ext (ext r)) c)

weakenLookup :: forall d b a. Lookup d a -> Lookup (CExt d b) a
weakenLookup x = S x

#ifdef PRENEX_EXT
exts :: (forall a. Lookup g a -> Term d a) -> Lookup (CExt g b) a -> Term (CExt d b) a
#else
exts :: (forall a. Lookup g a -> Term d a)
     -> (forall a. Lookup (CExt g b) a -> Term (CExt d b) a)
#endif
exts _ Z     = Var Z
#ifdef DIRECT_CON_WEAKEN
exts s (S x) = rename S (s x)
#else
exts s (S x) = rename weakenLookup (s x)
#endif

#ifdef LOCAL_RULE_HELPER
{-# NOINLINE exts #-}
{-# RULES "bad/exts" forall s. exts s Z = TZero #-}
#endif

subst :: (forall a. Lookup g a -> Term d a) -> Term g b -> Term d b
subst s (Var x)          = s x
subst s (Lam n)          = Lam (subst (exts s) n)
subst s (App l m)        = App (subst s l) (subst s m)
subst _ TZero            = TZero
subst _ TNil             = TNil
subst s (TCons x xs)     = TCons (subst s x) (subst s xs)
subst s (CaseLst xs n c) = CaseLst (subst s xs) (subst s n) (subst (exts (exts s)) c)

substSingle :: forall g b a. Term (CExt g b) a -> Term g b -> Term g a
substSingle m v = subst go m
  where
    go :: Lookup (CExt g b) x -> Term g x
    go Z     = v
#ifdef STRICT_SUBST
    go (S x) = v `seq` Var x
#elif defined(WRONG_SUBST)
    go (S _) = v
#else
    go (S x) = Var x
#endif

#ifdef SPECIALISE_SUBST
{-# SPECIALISE substSingle :: Term (CExt CEmpty TNat) TNat -> Term CEmpty TNat -> Term CEmpty TNat #-}
#endif

#ifdef LOCAL_RULE_SUBST
{-# NOINLINE substSingle #-}
{-# RULES "bad/substSingle" forall m v. substSingle m v = v #-}
#endif

substDouble :: Term (CExt (CExt g a) b) c -> Term g a -> Term g b -> Term g c
substDouble n v w = subst (\case
  Z       -> w
  S Z     -> v
  S (S x) -> Var x) n

#ifndef REVERSED_ORDER
type Value :: forall (g :: Ctx) (a :: Ty). Term g a -> Type
data Value m where
  VLam  :: Value (Lam n)
  VZero :: Value TZero
  VNil  :: Value TNil
  VCons :: Value v -> Value vs -> Value (TCons v vs)

type Step :: forall (g :: Ctx) (a :: Ty). Term g a -> Term g a -> Type
data Step m n where
  BetaLam :: forall g a b (n :: Term (CExt g a) b) (w :: Term g a).
             Value w -> Step (App (Lam n) w) (substSingle n w)
  BetaLst :: forall g a b (v :: Term g a) (vs :: Term g (TList a))
                    (n :: Term g b) (c :: Term (CExt (CExt g a) (TList a)) b).
             Value v -> Value vs -> Step (CaseLst (TCons v vs) n c) (substDouble c v vs)
#endif

betaShift :: Step @(CExt CEmpty TNat) @(TArr TNat (TArr TNat TNat))
               (App (Lam (Lam (Var (S Z)))) (Lam (Var (S Z))))
#ifdef BAD_CAPTURE_TARGET
               (Lam (Lam (Var (S Z))))
#else
               (Lam (Lam (Var (S (S Z)))))
#endif
betaShift = BetaLam VLam

betaDouble :: Step @CEmpty @TNat
                (CaseLst (TCons TZero TNil) TZero (Var (S Z)))
                TZero
betaDouble = BetaLst VZero VNil

betaId :: forall g a (v :: Term g a).
          substSingle (Var Z) v :~: v
betaId = Refl

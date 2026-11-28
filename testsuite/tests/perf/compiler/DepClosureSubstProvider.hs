{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE LambdaCase #-}

-- | Intrinsically-typed and scope-indexed De Bruijn calculus provider.
-- See Note [Checked source closures for substitution].
module DepClosureSubstProvider
  ( Nat (..)
  , Fin (..)
  , Ty (..)
  , Ctx (..)
  , Var (..)
  , Tm (..)
  , UTm (..)
  , UCtx (..)
  , Tag (..)
  , MarkedVar (..)
  , FoundVar (..)
  , extFin
  , renameUTm
  , extsUTm
  , substUTm
  , substSingleU
  , plugUCtx
  , extVar
  , renameTm
  , extsTm
  , substTm
  , substSingle
  , substPair
  , findMarkedVar
  , markFoundVar
  ) where

{- Note [Checked source closures for substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DepClosureSubstProvider exports capture-avoiding simultaneous renaming and
substitution over a 16-constructor intrinsically-typed De Bruijn term GADT,
together with scope-indexed untyped contexts and indexed variable lookup.
Compiling this module publishes CheckedClosureSourceTable entries for
substSingle, substPair, and substSingleU in its interface file.
-}

-- | Peano context lengths for untyped De Bruijn scopes.
data Nat
  = Zero
  | Succ Nat

-- | Bounded De Bruijn indices.
data Fin (n :: Nat) where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

-- | Object-language types.
data Ty
  = TUnit
  | TBool
  | TNat
  | TPair Ty Ty
  | TSum Ty Ty
  | TArr Ty Ty

-- | Typing contexts.
data Ctx
  = Empty
  | Snoc Ctx Ty

-- | Typed De Bruijn variables.
data Var (g :: Ctx) (a :: Ty) where
  VZ :: Var (Snoc g a) a
  VS :: Var g a -> Var (Snoc g b) a

-- | 16-constructor intrinsically-typed lambda calculus with products, sums,
-- naturals, let-binding, and general recursion.
data Tm (g :: Ctx) (a :: Ty) where
  V        :: Var g a -> Tm g a
  UnitVal  :: Tm g TUnit
  TrueVal  :: Tm g TBool
  FalseVal :: Tm g TBool
  ZeroVal  :: Tm g TNat
  SuccVal  :: Tm g TNat -> Tm g TNat
  IfVal    :: Tm g TBool -> Tm g a -> Tm g a -> Tm g a
  NatFold  :: Tm g TNat -> Tm g a -> Tm (Snoc g TNat) a -> Tm g a
  PairVal  :: Tm g a -> Tm g b -> Tm g (TPair a b)
  FstVal   :: Tm g (TPair a b) -> Tm g a
  SndVal   :: Tm g (TPair a b) -> Tm g b
  InlVal   :: Tm g a -> Tm g (TSum a b)
  InrVal   :: Tm g b -> Tm g (TSum a b)
  CaseSum  :: Tm g (TSum a b) -> Tm (Snoc g a) c -> Tm (Snoc g b) c -> Tm g c
  Lam      :: Tm (Snoc g a) b -> Tm g (TArr a b)
  App      :: Tm g (TArr a b) -> Tm g a -> Tm g b

-- | Scope-indexed untyped lambda terms.
data UTm (n :: Nat) where
  UVar  :: Fin n -> UTm n
  ULam  :: UTm (Succ n) -> UTm n
  UApp  :: UTm n -> UTm n -> UTm n
  UPair :: UTm n -> UTm n -> UTm n

-- | One-hole scope-indexed untyped evaluation contexts.
data UCtx (inner :: Nat) (outer :: Nat) where
  UHole  :: UCtx n n
  UCLam  :: UCtx inner (Succ outer) -> UCtx inner outer
  UCAppL :: UCtx inner outer -> UTm outer -> UCtx inner outer
  UCAppR :: UTm outer -> UCtx inner outer -> UCtx inner outer

-- | Extend an untyped variable renaming under one binder.
extFin :: (Fin m -> Fin n) -> Fin (Succ m) -> Fin (Succ n)
extFin _ FZ = FZ
extFin rho (FS x) = FS (rho x)

-- | Rename variables across an untyped term.
renameUTm :: (Fin m -> Fin n) -> UTm m -> UTm n
renameUTm rho (UVar x) = UVar (rho x)
renameUTm rho (ULam b) = ULam (renameUTm (extFin rho) b)
renameUTm rho (UApp f x) = UApp (renameUTm rho f) (renameUTm rho x)
renameUTm rho (UPair l r) = UPair (renameUTm rho l) (renameUTm rho r)

-- | Extend an untyped simultaneous substitution under one binder.
extsUTm :: (Fin m -> UTm n) -> Fin (Succ m) -> UTm (Succ n)
extsUTm _ FZ = UVar FZ
extsUTm sigma (FS x) = renameUTm FS (sigma x)

-- | Apply an untyped simultaneous substitution.
substUTm :: (Fin m -> UTm n) -> UTm m -> UTm n
substUTm sigma (UVar x) = sigma x
substUTm sigma (ULam b) = ULam (substUTm (extsUTm sigma) b)
substUTm sigma (UApp f x) = UApp (substUTm sigma f) (substUTm sigma x)
substUTm sigma (UPair l r) = UPair (substUTm sigma l) (substUTm sigma r)

-- | Substitute one untyped term for the innermost variable.
substSingleU :: UTm (Succ n) -> UTm n -> UTm n
substSingleU body arg = substUTm (\case { FZ -> arg; FS x -> UVar x }) body

-- | Plug a term into a scope-indexed untyped context.
plugUCtx :: UCtx inner outer -> UTm inner -> UTm outer
plugUCtx UHole t = t
plugUCtx (UCLam c) t = ULam (plugUCtx c t)
plugUCtx (UCAppL c r) t = UApp (plugUCtx c t) r
plugUCtx (UCAppR l c) t = UApp l (plugUCtx c t)

-- | Extend a typed variable renaming under one binder.
extVar
  :: (forall x. Var g x -> Var d x)
  -> (forall x. Var (Snoc g b) x -> Var (Snoc d b) x)
extVar _ VZ = VZ
extVar rho (VS v) = VS (rho v)

-- | Rename variables across a 16-constructor typed term.
renameTm :: (forall x. Var g x -> Var d x) -> Tm g a -> Tm d a
renameTm rho (V v) = V (rho v)
renameTm _ UnitVal = UnitVal
renameTm _ TrueVal = TrueVal
renameTm _ FalseVal = FalseVal
renameTm _ ZeroVal = ZeroVal
renameTm rho (SuccVal n) = SuccVal (renameTm rho n)
renameTm rho (IfVal c t e) = IfVal (renameTm rho c) (renameTm rho t) (renameTm rho e)
renameTm rho (NatFold n z s) =
  NatFold (renameTm rho n) (renameTm rho z) (renameTm (extVar rho) s)
renameTm rho (PairVal l r) = PairVal (renameTm rho l) (renameTm rho r)
renameTm rho (FstVal p) = FstVal (renameTm rho p)
renameTm rho (SndVal p) = SndVal (renameTm rho p)
renameTm rho (InlVal l) = InlVal (renameTm rho l)
renameTm rho (InrVal r) = InrVal (renameTm rho r)
renameTm rho (CaseSum sc l r) =
  CaseSum (renameTm rho sc) (renameTm (extVar rho) l) (renameTm (extVar rho) r)
renameTm rho (Lam b) = Lam (renameTm (extVar rho) b)
renameTm rho (App f x) = App (renameTm rho f) (renameTm rho x)

-- | Extend a typed simultaneous substitution under one binder.
extsTm
  :: (forall x. Var g x -> Tm d x)
  -> (forall x. Var (Snoc g b) x -> Tm (Snoc d b) x)
extsTm _ VZ = V VZ
extsTm sigma (VS v) = renameTm VS (sigma v)

-- | Apply a typed simultaneous substitution across a 16-constructor term.
substTm :: (forall x. Var g x -> Tm d x) -> Tm g a -> Tm d a
substTm sigma (V v) = sigma v
substTm _ UnitVal = UnitVal
substTm _ TrueVal = TrueVal
substTm _ FalseVal = FalseVal
substTm _ ZeroVal = ZeroVal
substTm sigma (SuccVal n) = SuccVal (substTm sigma n)
substTm sigma (IfVal c t e) = IfVal (substTm sigma c) (substTm sigma t) (substTm sigma e)
substTm sigma (NatFold n z s) =
  NatFold (substTm sigma n) (substTm sigma z) (substTm (extsTm sigma) s)
substTm sigma (PairVal l r) = PairVal (substTm sigma l) (substTm sigma r)
substTm sigma (FstVal p) = FstVal (substTm sigma p)
substTm sigma (SndVal p) = SndVal (substTm sigma p)
substTm sigma (InlVal l) = InlVal (substTm sigma l)
substTm sigma (InrVal r) = InrVal (substTm sigma r)
substTm sigma (CaseSum sc l r) =
  CaseSum (substTm sigma sc) (substTm (extsTm sigma) l) (substTm (extsTm sigma) r)
substTm sigma (Lam b) = Lam (substTm (extsTm sigma) b)
substTm sigma (App f x) = App (substTm sigma f) (substTm sigma x)

-- | Substitute one typed term for the innermost binder.
substSingle :: Tm (Snoc g a) b -> Tm g a -> Tm g b
substSingle body arg = substTm (\case { VZ -> arg; VS v -> V v }) body

-- | Substitute two typed terms for the two innermost binders simultaneously.
substPair :: Tm (Snoc (Snoc g a) b) c -> Tm g a -> Tm g b -> Tm g c
substPair body firstArg secondArg =
  substTm (\case { VZ -> secondArg; VS VZ -> firstArg; VS (VS v) -> V v }) body

-- | Auxiliary annotation carried by indexed variable lookups.
data Tag (a :: Ty) = MkTag Bool

-- | Context-indexed variable paired with an annotation.
data MarkedVar (g :: Ctx) where
  MkMarkedVar :: Var g a -> Tag a -> MarkedVar g

-- | Position-indexed witness locating an annotated variable in a context.
data FoundVar (g :: Ctx) where
  FoundHead :: Tag a -> FoundVar (Snoc g a)
  FoundTail :: FoundVar g -> FoundVar (Snoc g a)

-- | Convert a MarkedVar into a structural FoundVar witness.
findMarkedVar :: MarkedVar g -> FoundVar g
findMarkedVar (MkMarkedVar VZ tag) = FoundHead tag
findMarkedVar (MkMarkedVar (VS next) tag) = FoundTail (findMarkedVar (MkMarkedVar next tag))

-- | Reconstruct a MarkedVar from a FoundVar witness.
markFoundVar :: FoundVar g -> MarkedVar g
markFoundVar (FoundHead tag) = MkMarkedVar VZ tag
markFoundVar (FoundTail rest) = case markFoundVar rest of
  MkMarkedVar v tag -> MkMarkedVar (VS v) tag

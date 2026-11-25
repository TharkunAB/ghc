{-# LANGUAGE CPP, DependentTypes, ListTuplePuns #-}
-- | Provider for casted certified source-call operands in retained fields (R03, R05, R11).
module NativeRetainedCastSourceCallProvider
  ( Nat (..)
  , Ty (..)
  , Ctx (..)
  , Lookup (..)
  , Term (..)
  , ext
  , rename
  , exts
  , subst
  , substSingle
  , Value (..)
  , Step (..)
  , Steps (..)
  , Progress (..)
  , EvalResult (..)
  , inlineBetaStep
  , progress
  , eval
  ) where

import Data.Kind (Type)

-- | Peano fuel for bounded evaluation.
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- | Object-language simple types.
data Ty = TNat | TArr Ty Ty

-- | De Bruijn typing contexts.
data Ctx = CEmpty | CExt Ctx Ty

-- | Intrinsically typed context membership witness.
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a

-- | Intrinsically typed lambda terms.
data Term (g :: Ctx) (a :: Ty) where
  Var :: Lookup g a -> Term g a
  Lam :: Term (CExt g a) b -> Term g (TArr a b)
  App :: Term g (TArr a b) -> Term g a -> Term g b

-- | Extend a renaming under one binder.
ext :: (forall a. Lookup g a -> Lookup d a) -> Lookup (CExt g b) a -> Lookup (CExt d b) a
ext _ Z     = Z
ext r (S x) = S (r x)

-- | Apply a renaming across a term.
rename :: (forall a. Lookup g a -> Lookup d a) -> Term g a -> Term d a
rename r (Var x)   = Var (r x)
rename r (Lam n)   = Lam (rename (ext r) n)
rename r (App l m) = App (rename r l) (rename r m)

-- | Extend a substitution under one binder.
exts :: (forall a. Lookup g a -> Term d a) -> Lookup (CExt g b) a -> Term (CExt d b) a
exts _ Z     = Var Z
exts s (S x) = rename S (s x)

-- | Apply a parallel substitution across a term.
subst :: (forall a. Lookup g a -> Term d a) -> Term g a -> Term d a
subst s (Var x)   = s x
subst s (Lam n)   = Lam (subst (exts s) n)
subst s (App l m) = App (subst s l) (subst s m)

-- | Substitute one closed or open term for variable zero.
substSingle :: forall g b a. Term (CExt g b) a -> Term g b -> Term g a
substSingle m v = subst go m
  where
    go :: Lookup (CExt g b) x -> Term g x
    go Z     = v
    go (S x) = Var x

-- | Value witness for lambda abstractions.
type Value :: forall (g :: Ctx) (a :: Ty). Term g a -> Type
data Value m where
  VLam :: Value (Lam n)

-- | Call-by-value small-step reduction witness.
type Step :: forall (g :: Ctx) (a :: Ty). Term g a -> Term g a -> Type
data Step m n where
  XiApp1  :: Step l l' -> Step (App l m) (App l' m)
  XiApp2  :: Value l -> Step m m' -> Step (App l m) (App l m')
  BetaLam :: forall g a b (n :: Term (CExt g a) b) (w :: Term g a).
             Value w -> Step (App (Lam n) w) (substSingle n w)

-- | Reflexive-transitive small-step sequence.
type Steps :: forall (g :: Ctx) (a :: Ty). Term g a -> Term g a -> Type
data Steps m n where
  SRefl :: Steps m m
  SCons :: Step l m -> Steps m n -> Steps l n

-- | Progress decision with a retained target term in 'PStep'.
type Progress :: forall (a :: Ty). Term CEmpty a -> Type
data Progress m where
  PStep :: foreach (n :: Term CEmpty a) -> Step m n -> Progress m
  PDone :: Value m -> Progress m

-- | Exported inlineable beta step for cross-module client unfolding checks.
{-# INLINE inlineBetaStep #-}
inlineBetaStep :: forall a b.
                  foreach (body :: Term (CExt CEmpty a) b)
               -> foreach (m :: Term CEmpty a)
               -> Value m
               -> Progress (App (Lam body) m)
inlineBetaStep body m w = PStep (substSingle body m) (BetaLam w)

-- Unexported helper: Gentle and -O2 inline this into 'appBeta', casting 'm'
-- inside the certified 'substSingle' retained operand.
betaStep :: forall a b.
            foreach (body :: Term (CExt CEmpty a) b)
         -> foreach (m :: Term CEmpty a)
         -> Value m
         -> Progress (App (Lam body) m)
betaStep body m w = PStep (substSingle body m) (BetaLam w)

appBeta :: forall a b.
           foreach (l :: Term CEmpty (TArr a b))
        -> foreach (m :: Term CEmpty a)
        -> Value l
        -> Value m
        -> Progress (App l m)
appBeta l = case l of
#ifdef DIRECT_INLINE
  Lam body -> \m _ w -> PStep (substSingle body m) (BetaLam w)
#else
  Lam body -> \m _ w -> betaStep body m w
#endif
  Var x    -> \_ _ _ -> case x of {}
  App _ _  -> \_ v _ -> case v of {}

-- | Compute progress for any closed well-typed term.
progress :: forall (a :: Ty). foreach (m :: Term CEmpty a) -> Progress m
progress t = case t of
  Var x   -> case x of {}
  Lam _   -> PDone VLam
  App l m -> case progress l of
    PStep l' s -> PStep (App l' m) (XiApp1 s)
    PDone v    -> case progress m of
      PStep m' s -> PStep (App l m') (XiApp2 v s)
      PDone w    -> appBeta l m v w

-- | Gas-bounded evaluation result carrying the retained final term and trace.
type EvalResult :: forall (a :: Ty). Term CEmpty a -> Type
data EvalResult m where
  EvalSteps :: foreach (n :: Term CEmpty a) -> Steps m n -> Maybe (Value n) -> EvalResult m

-- | Six-line intrinsically typed evaluator over retained 'PStep'.
eval :: forall (a :: Ty). Nat -> foreach (m :: Term CEmpty a) -> EvalResult m
eval gas m = case gas of
  Zero   -> EvalSteps m SRefl Nothing
  Succ k -> case progress m of
    PDone v   -> EvalSteps m SRefl (Just v)
    PStep n s -> case eval k n of
      EvalSteps p ss mv -> EvalSteps p (SCons s ss) mv

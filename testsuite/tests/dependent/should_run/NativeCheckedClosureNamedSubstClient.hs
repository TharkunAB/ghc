{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes #-}
module NativeCheckedClosureNamedSubstClient where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (U)
import qualified NativeCheckedClosureNamedSubstProvider as M

type Step :: forall (g :: M.Ctx) (a :: M.Ty). M.Term g a -> M.Term g a -> Type
data Step m n where
  BetaLam :: forall g a b (body :: M.Term (M.CExt g a) b) (v :: M.Term g a).
             Step (M.App (M.Lam body) v) (M.substSingle body v)

eqVarZ :: forall g a (v :: M.Term g a). M.substSingle (M.Var M.Z) v :~: v
eqVarZ = Refl

eqLamShift :: forall g a c (v :: M.Term g a).
              M.substSingle (M.Lam (M.Var (M.S M.Z)) :: M.Term (M.CExt g a) (M.TArr c a)) v
                :~: M.Lam (M.rename M.weakenLookup v)
eqLamShift = Refl

eqAbstractSingle :: forall g a b (n :: M.Term (M.CExt g a) b) (v :: M.Term g a).
                    M.substSingle n v :~: M.subst (M.singleSubst v) n
eqAbstractSingle = Refl

eqAbstractSinglePerm :: forall g a b (n :: M.Term (M.CExt g a) b) (v :: M.Term g a).
                        M.substSinglePerm v n :~: M.subst (M.singleSubst v) n
eqAbstractSinglePerm = Refl

eqAbstractDouble :: forall g a b c (m :: M.Term (M.CExt (M.CExt g a) b) c) (v :: M.Term g a) (vs :: M.Term g b).
                    M.substDouble m v vs :~: M.subst (M.doubleSubst v vs) m
eqAbstractDouble = Refl

eqAbstractDoubleSwapped :: forall g a b c (m :: M.Term (M.CExt (M.CExt g a) b) c) (v :: M.Term g a) (vs :: M.Term g b).
                           M.substDoubleSwapped vs v m :~: M.subst (M.doubleSubstSwapped vs v) m
eqAbstractDoubleSwapped = Refl

type Renaming g d = forall a. M.Lookup g a %U -> M.Lookup d a
type SubstFn g d = forall a. M.Lookup g a %U -> M.Term d a

eqIdsTVar :: forall g a (x :: M.Lookup g a). M.subst M.idsT (M.Var x) :~: M.Var x
eqIdsTVar = Refl

eqRenTVar :: forall g d a (r :: Renaming g d) (x :: M.Lookup g a).
             M.subst (M.renT r) (M.Var x) :~: M.Var (r x)
eqRenTVar = Refl

eqConsTZ :: forall g d b (t :: M.Term d b) (s :: SubstFn g d).
            M.consSubstT t s M.Z :~: t
eqConsTZ = Refl

eqSeqTVar :: forall g d e a (p :: SubstFn g d) (q :: SubstFn d e) (x :: M.Lookup g a).
             M.subst (M.seqSubstT p q) (M.Var x) :~: M.subst q (p x)
eqSeqTVar = Refl

type SimRel :: forall g a. M.Term g a -> M.Term g a -> Type
data SimRel m n where
  SimVar  :: SimRel (M.Var x) (M.Var x)
  SimLam  :: SimRel m n -> SimRel (M.Lam m) (M.Lam n)
  SimApp  :: SimRel l l' -> SimRel m m' -> SimRel (M.App l m) (M.App l' m')
  SimZero :: SimRel M.Zero M.Zero

type Compatible :: forall g d. SubstFn g d -> SubstFn g d -> Type
type Compatible @g @d p q = forall a. foreach (x :: M.Lookup g a) -> SimRel (p x) (q x)

singleCompat :: forall g a (v :: M.Term g a) (v' :: M.Term g a).
                SimRel v v' -> Compatible (M.singleSubst v) (M.singleSubst v')
singleCompat sv index = case index of
  M.Z   -> sv
  M.S y -> SimVar @y

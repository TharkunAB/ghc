{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes #-}
module NativeCheckedClosureUntypedSigmaClient where

import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (U)
import GHC.Types (Matchability)
import qualified NativeCheckedClosureUntypedSigmaProvider as U

type SubstFn m n = U.Fin m %U -> U.UTerm n

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

subApp :: forall m n (s :: SubstFn m n) (l :: U.UTerm m) (r :: U.UTerm m).
          U.substU s (U.UApp l r) :~: U.UApp (U.substU s l) (U.substU s r)
subApp = Refl

subLam :: forall m n (s :: SubstFn m n) (b :: U.UTerm (U.Succ m)).
          U.substU s (U.ULam b) :~: U.ULam (U.substU (U.extsU s) b)
subLam = Refl

extsFZ :: forall m n (s1 :: SubstFn m n) (s2 :: SubstFn m n).
          U.extsU s1 U.FZ :~: U.extsU s2 U.FZ
extsFZ = Refl

stepFS :: forall m n (s1 :: SubstFn m n) (s2 :: SubstFn m n) (x :: U.Fin m).
          (s1 x :~: s2 x) -> U.extsU s1 (U.FS x) :~: U.extsU s2 (U.FS x)
stepFS eq = cong (U.renameU U.shiftFin) eq

bareIdsVar :: forall m (x :: U.Fin m). U.substU U.ids (U.UVar x) :~: U.UVar x
bareIdsVar = Refl

bareShiftVar :: forall m (x :: U.Fin m). U.renameU U.shift (U.UVar x) :~: U.UVar (U.FS x)
bareShiftVar = Refl

consIdsZ :: forall n (w :: U.UTerm n). U.substU (U.consSubst w U.ids) (U.UVar U.FZ) :~: w
consIdsZ = Refl

seqVar :: forall m n k (p :: SubstFn m n) (q :: SubstFn n k) (x :: U.Fin m).
          U.substU (U.seqSubst p q) (U.UVar x) :~: U.substU q (p x)
seqVar = Refl

namedVsSubstU :: forall n (m :: U.UTerm (U.Succ n)) (w :: U.UTerm n).
                 U.substSingleNamedU m w :~: U.substU (U.singleSubstU w) m
namedVsSubstU = Refl

inlineShiftEq :: forall n (w :: U.UTerm n).
                 U.substSingleU (U.ULam (U.UVar (U.FS U.FZ)) :: U.UTerm (U.Succ n)) w
                   :~: U.ULam (U.renameU U.shiftFin w)
inlineShiftEq = Refl

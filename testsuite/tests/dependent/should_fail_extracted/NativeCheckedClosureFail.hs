{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             GADTs, Modifiers, PolyKinds, RankNTypes, ScopedTypeVariables,
             StandaloneKindSignatures, TypeApplications, TypeOperators #-}
module NativeCheckedClosureFail where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeCheckedClosureProvider as P
import qualified NativeCheckedClosureDoubleProvider as DP
import qualified NativeCheckedClosureDoubleNamed as DN
import qualified NativeCheckedLookupProvider as LP
import qualified NativeCheckedPlugSubstUProvider as UP

type Step :: forall g a. P.Expr g a -> P.Expr g a -> Type
data Step m n where
  Beta :: forall g a b (n :: P.Expr (P.Push g a) b) (w :: P.Expr g a).
          Step (P.Call (P.Bind n) w) (P.replace n w)

type NatScope = P.Push P.Empty P.Nat
type Fn = P.Arrow P.Nat P.Nat
type Inserted = P.Bind (P.At (P.Old P.New)) :: P.Expr NatScope Fn
type Body = P.Bind (P.At (P.Old P.New)) :: P.Expr (P.Push NatScope Fn) (P.Arrow P.Nat Fn)

wrongCapture :: Step (P.Call (P.Bind Body) Inserted)
                     (P.Bind (P.Bind (P.At P.New)))
wrongCapture = Beta @NatScope @Fn @(P.Arrow P.Nat Fn) @Body @Inserted

wrongStamp :: forall g a b (next :: P.Slot g a) (stamp :: P.Stamp a) (other :: P.Stamp a).
              P.findMarked ('P.Marked ('P.Old next) stamp :: P.Marked (P.Push g b))
                :~: 'P.InTail (P.findMarked ('P.Marked next other :: P.Marked g))
wrongStamp = Refl

wrongDuplicate :: forall g a (stamp :: P.Stamp a).
                  P.findMarked
                    (P.markFound ('P.InTail ('P.AtHead stamp) :: P.Found (P.Push (P.Push g a) a)))
                  :~: 'P.AtHead stamp
wrongDuplicate = Refl

type Outer = DP.Push DP.Empty DP.Nat
type BodyScope = DP.Push (DP.Push Outer DP.Nat) DP.FlagSort
type DUnder = DP.BindBoth
    (DP.Both (DP.At (DP.Old (DP.Old (DP.Old DP.New)))) (DP.At (DP.Old (DP.Old DP.New))))
      :: DP.Expr BodyScope DP.Nat

wrongDoubleCapture :: DP.replaceBoth DUnder (DP.At DP.New :: DP.Expr Outer DP.Nat) DP.Flag
                   :~: DP.BindBoth (DP.Both (DP.At (DP.Old DP.New)) DP.Flag)
wrongDoubleCapture = Refl

type Renaming g d = forall a. DP.Slot g a %U -> DP.Slot d a
type SubstFn g d = forall a. DN.Slot g a %U -> DN.Expr d a

wrongRenaming :: forall g d a (r :: Renaming g d) (s :: Renaming g d) (x :: DP.Slot g a).
                 DP.renameExpr r (DP.At x) :~: DP.At (s x)
wrongRenaming = Refl

wrongSubst :: forall g d a (s :: SubstFn g d) (t :: SubstFn g d) (x :: DN.Slot g a).
              DN.walk s (DN.At x) :~: t x
wrongSubst = Refl

wrongLookupIdx :: Proxy x -> Proxy y -> LP.lookupEnv ('LP.Cons x ('LP.Cons y 'LP.Nil)) ('LP.FS 'LP.FZ) :~: x
wrongLookupIdx _ _ = Refl

wrongHole :: forall (n :: UP.Nat) (m :: UP.UTerm n). UP.plug UP.CHole m :~: UP.ULam (UP.UVar UP.FZ)
wrongHole = Refl

type BetaStep :: forall (n :: UP.Nat). UP.UTerm n -> UP.UTerm n -> Type
data BetaStep t t' where
  BetaU :: forall (n :: UP.Nat) (b :: UP.UTerm ('UP.Succ n)) (w :: UP.UTerm n).
           BetaStep (UP.UApp (UP.ULam b) w) (UP.substSingleU b w)

wrongCaptureU :: BetaStep
                   (UP.UApp (UP.ULam (UP.ULam (UP.UVar (UP.FS UP.FZ)))) (UP.UVar UP.FZ) :: UP.UTerm ('UP.Succ 'UP.Zero))
                   (UP.ULam (UP.UVar UP.FZ))
wrongCaptureU = BetaU

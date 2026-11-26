{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             EmptyCase, GADTs, LambdaCase, Modifiers, PolyKinds, RankNTypes,
             ScopedTypeVariables, StandaloneKindSignatures, TypeAbstractions,
             TypeApplications, TypeOperators #-}
module NativeCheckedClosureCases
  ( caseBinder, underBoth, runCheckedLookup, runLocalLookup, runPlugSubstU ) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Kind (Type)
import Data.List (isPrefixOf)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeCheckedClosureProvider as P
import qualified NativeCheckedClosureDoubleProvider as DP
import qualified NativeCheckedClosureDoubleNamed as DN
import qualified NativeCheckedLookupProvider as LP
import qualified NativeLocalLookup as LL
import qualified NativeCheckedPlugSubstUProvider as UP

type Step :: forall g a. P.Expr g a -> P.Expr g a -> Type
data Step m n where
  Beta :: forall g a b (n :: P.Expr (P.Push g a) b) (w :: P.Expr g a).
          Step (P.Call (P.Bind n) w) (P.replace n w)
  Unroll :: forall g a (n :: P.Expr (P.Push g a) a).
            Step (P.Loop n) (P.replace n (P.Loop n))

type NatScope = P.Push P.Empty P.Nat
type Fn = P.Arrow P.Nat P.Nat
type Inserted = P.Bind (P.At (P.Old P.New)) :: P.Expr NatScope Fn
type Body = P.Bind (P.At (P.Old P.New)) :: P.Expr (P.Push NatScope Fn) (P.Arrow P.Nat Fn)

captureAvoided :: Step (P.Call (P.Bind Body) Inserted)
                       (P.Bind (P.Bind (P.At (P.Old (P.Old P.New)))))
captureAvoided = Beta @NatScope @Fn @(P.Arrow P.Nat Fn) @Body @Inserted

caseBinder :: P.replace
                (P.Test P.Lit P.Lit (P.At P.New)
                  :: P.Expr (P.Push P.Empty P.Nat) P.Nat)
                P.Lit :~: P.Test P.Lit P.Lit (P.At P.New)
caseBinder = Refl

openTail :: forall g a b (next :: P.Slot g a) (stamp :: P.Stamp a).
            P.findMarked ('P.Marked ('P.Old next) stamp :: P.Marked (P.Push g b))
              :~: 'P.InTail (P.findMarked ('P.Marked next stamp :: P.Marked g))
openTail = Refl

encodeTail :: forall g a b (next :: P.Found g) (member :: P.Slot g a) (stamp :: P.Stamp a).
              P.markFound next :~: 'P.Marked member stamp ->
              P.markFound ('P.InTail next :: P.Found (P.Push g b)) :~: 'P.Marked ('P.Old member) stamp
encodeTail Refl = Refl

duplicateRoundTrip :: forall g a (stamp :: P.Stamp a).
                      P.findMarked
                        (P.markFound ('P.InTail ('P.AtHead stamp) :: P.Found (P.Push (P.Push g a) a)))
                      :~: 'P.InTail ('P.AtHead stamp)
duplicateRoundTrip = Refl

type Outer = DP.Push DP.Empty DP.Nat
type BodyScope = DP.Push (DP.Push Outer DP.Nat) DP.FlagSort
type DBody = DP.Both (DP.At (DP.Old DP.New)) (DP.At DP.New) :: DP.Expr BodyScope DP.Nat
type DUnder = DP.BindBoth
    (DP.Both (DP.At (DP.Old (DP.Old (DP.Old DP.New)))) (DP.At (DP.Old (DP.Old DP.New))))
      :: DP.Expr BodyScope DP.Nat

twoReplacements :: DP.replaceBoth DBody (DP.Lit :: DP.Expr Outer DP.Nat) DP.Flag
                 :~: DP.Both DP.Lit DP.Flag
twoReplacements = Refl

underBoth :: DP.replaceBoth DUnder (DP.At DP.New :: DP.Expr Outer DP.Nat) DP.Flag
          :~: DP.BindBoth (DP.Both (DP.At (DP.Old (DP.Old DP.New))) DP.Flag)
underBoth = Refl

type Renaming g d = forall a. DP.Slot g a %U -> DP.Slot d a
type SubstFn g d = forall a. DN.Slot g a %U -> DN.Expr d a

renameVariable :: forall g d a (r :: Renaming g d) (x :: DP.Slot g a).
                  DP.renameExpr r (DP.At x) :~: DP.At (r x)
renameVariable = Refl

renameBinders :: forall g d (r :: Renaming g d)
                        (n :: DP.Expr (DP.Push (DP.Push g DP.Nat) DP.FlagSort) DP.Nat).
                 DP.renameExpr r (DP.BindBoth n)
                 :~: DP.BindBoth (DP.renameExpr (DP.extendName (DP.extendName r)) n)
renameBinders = Refl

walkVariable :: forall g d a (s :: SubstFn g d) (x :: DN.Slot g a).
                DN.walk s (DN.At x) :~: s x
walkVariable = Refl

walkBinders :: forall g d (s :: SubstFn g d)
                      (n :: DN.Expr (DN.Push (DN.Push g DN.Nat) DN.FlagSort) DN.Nat).
               DN.walk s (DN.BindBoth n)
               :~: DN.BindBoth (DN.walk (DN.extendExpr (DN.extendExpr s)) n)
walkBinders = Refl

type Found :: forall (n :: LP.Nat). LP.Env n -> LP.Fin n -> Bool -> Type
data Found e i v where
  AtZero :: Found ('LP.Cons x e) 'LP.FZ x
  AtNext :: Found e i x -> Found ('LP.Cons y e) ('LP.FS i) x

lookupEvidence :: Found e i x -> LP.lookupEnv e i :~: x
lookupEvidence AtZero = Refl
lookupEvidence (AtNext p) = lookupEvidence p

runCheckedLookup :: IO (Bool, Bool, Bool)
runCheckedLookup = do
  let left = LP.lookupEnv (LP.Cons True (error "tail" :: LP.Env 'LP.Z)) LP.FZ
      next = LP.lookupEnv (LP.Cons (error "head") (LP.Cons True LP.Nil)) (LP.FS LP.FZ)
  order <- try (evaluate (LP.lookupEnv (error "environment" :: LP.Env ('LP.S 'LP.Z)) (error "position")))
  let leftFirst = either (isPrefixOf "environment" . displayException) (const False)
                  (order :: Either ErrorCall Bool)
  pure (case lookupEvidence (AtZero :: Found ('LP.Cons 'True 'LP.Nil) 'LP.FZ 'True) of Refl -> left, next, leftFirst)

type LFound :: forall (n :: LL.Nat). LL.Env n -> LL.Fin n -> LL.Clos -> Type
data LFound e x c where
  ELZ :: LFound ('LL.ECons c e) 'LL.FZ c
  ELS :: LFound e x c -> LFound ('LL.ECons other e) ('LL.FS x) c

lookupAgrees :: LFound e x c -> LL.lookupEnv e x :~: c
lookupAgrees ELZ     = Refl
lookupAgrees (ELS p) = lookupAgrees p

outerStep :: LL.EvalBig ('LL.ECons ('LL.MkClos ('LL.ULam ('LL.ULam ('LL.UVar 'LL.FZ))) 'LL.ENil)
                         ('LL.ECons ('LL.MkClos ('LL.ULam ('LL.UVar 'LL.FZ)) 'LL.ENil) 'LL.ENil))
                       ('LL.UVar ('LL.FS 'LL.FZ))
                       ('LL.MkClos ('LL.ULam ('LL.UVar 'LL.FZ)) 'LL.ENil)
outerStep = LL.EVar LL.ELam

runLocalLookup :: (Bool, Bool, Bool, Bool)
runLocalLookup =
  let c0 = LL.MkClos (LL.ULam (LL.UVar LL.FZ)) LL.ENil
      env = LL.ECons (error "unused head closure") (LL.ECons c0 (error "unused tail env"))
      hit = case LL.lookupEnv env (LL.FS LL.FZ) of
              LL.MkClos (LL.ULam (LL.UVar LL.FZ)) LL.ENil -> True
              _ -> False
      eqOk = case lookupAgrees (ELS ELZ :: LFound ('LL.ECons ('LL.MkClos ('LL.ULam ('LL.ULam ('LL.UVar 'LL.FZ))) 'LL.ENil)
                                                   ('LL.ECons ('LL.MkClos ('LL.ULam ('LL.UVar 'LL.FZ)) 'LL.ENil) 'LL.ENil))
                                                 ('LL.FS 'LL.FZ)
                                                 ('LL.MkClos ('LL.ULam ('LL.UVar 'LL.FZ)) 'LL.ENil)) of
               Refl -> True
      kOk = case LL.evalBigIsLam LL.evalKIdOmega of LL.MkIsLamClos -> True
      stepOk = case LL.evalDeterm outerStep outerStep of Refl -> True
  in (hit, eqOk, kOk, stepOk)

congLam :: a :~: b -> UP.ULam a :~: UP.ULam b
congLam Refl = Refl

plugLamEq :: forall (m :: UP.Nat) (n :: UP.Nat) (t :: UP.UTerm m).
             foreach (c :: UP.UCtx m ('UP.Succ n)) ->
             UP.plug (UP.CLam c) t :~: UP.ULam (UP.plug c t)
plugLamEq _ = Refl

plugCong :: forall (m :: UP.Nat) (n :: UP.Nat) (t :: UP.UTerm m) (t' :: UP.UTerm m).
            foreach (c :: UP.UCtx m n) -> t :~: t' -> UP.plug c t :~: UP.plug c t'
plugCong c eq = case c of
  UP.CHole      -> eq
  UP.CLam c'    -> congLam (plugCong c' eq)
  UP.CAppL c' _ -> case plugCong c' eq of Refl -> Refl
  UP.CAppR _ c' -> case plugCong c' eq of Refl -> Refl

localExtU :: (UP.Fin m -> UP.Fin n) -> UP.Fin ('UP.Succ m) -> UP.Fin ('UP.Succ n)
localExtU _   UP.FZ     = UP.FZ
localExtU rho (UP.FS x) = UP.FS (rho x)

localRenameU :: (UP.Fin m -> UP.Fin n) -> UP.UTerm m -> UP.UTerm n
localRenameU rho (UP.UVar x)   = UP.UVar (rho x)
localRenameU rho (UP.ULam b)   = UP.ULam (localRenameU (localExtU rho) b)
localRenameU rho (UP.UApp l m) = UP.UApp (localRenameU rho l) (localRenameU rho m)

localExtsU :: (UP.Fin m -> UP.UTerm n) -> UP.Fin ('UP.Succ m) -> UP.UTerm ('UP.Succ n)
localExtsU _     UP.FZ     = UP.UVar UP.FZ
localExtsU sigma (UP.FS x) = localRenameU UP.FS (sigma x)

localSubstU :: (UP.Fin m -> UP.UTerm n) -> UP.UTerm m -> UP.UTerm n
localSubstU sigma (UP.UVar x)   = sigma x
localSubstU sigma (UP.ULam b)   = UP.ULam (localSubstU (localExtsU sigma) b)
localSubstU sigma (UP.UApp l m) = UP.UApp (localSubstU sigma l) (localSubstU sigma m)

localSubstSingleU :: UP.UTerm ('UP.Succ n) -> UP.UTerm n -> UP.UTerm n
localSubstSingleU b w = localSubstU (\case { UP.FZ -> w; UP.FS x -> UP.UVar x }) b

localSubstId :: forall (n :: UP.Nat) (w :: UP.UTerm n). localSubstSingleU ('UP.UVar 'UP.FZ) w :~: w
localSubstId = Refl

runCong :: UP.UCtx ('UP.Succ 'UP.Zero) 'UP.Zero -> Bool
runCong ctx =
  case plugCong @('UP.Succ 'UP.Zero) @'UP.Zero @(UP.UVar UP.FZ) @(UP.UVar UP.FZ) ctx Refl of
    Refl -> True

runPlugSubstU :: String
runPlugSubstU =
  let r1 = plugLamEq @('UP.Succ 'UP.Zero) @'UP.Zero @(UP.UVar UP.FZ) UP.CHole
      r2 = runCong (UP.CLam (UP.CAppL (UP.CAppR (error "unused left") UP.CHole) (error "unused right")))
      r3 = localSubstId @('UP.Succ 'UP.Zero) @(UP.UVar UP.FZ)
      _keep = (captureAvoided, openTail, encodeTail, duplicateRoundTrip, twoReplacements,
               renameVariable, renameBinders, walkVariable, walkBinders)
  in case UP.plug (UP.CAppL UP.CHole (error "unused right")) (error "unused hole" :: UP.UTerm 'UP.Zero) of
       UP.UApp _ _ | r2 -> show (r1, r3)
       _                -> error "unreachable"

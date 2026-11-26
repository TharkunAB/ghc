{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DependentTypes #-}

-- | Client benchmark for CheckedClosureSourceTable interface readback and
-- capture-avoiding symbolic beta reduction.
module DepClosureSubst where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import qualified DepClosureSubstProvider as P

-- | Small-step operational semantics indexed by typed source and target terms.
type Step :: forall (g :: P.Ctx) (a :: P.Ty). P.Tm g a -> P.Tm g a -> Type
data Step m n where
  BetaLam
    :: forall g a b (body :: P.Tm (P.Snoc g a) b) (arg :: P.Tm g a).
       Step (P.App (P.Lam body) arg) (P.substSingle body arg)
  BetaIfTrue
    :: forall g a (t :: P.Tm g a) (e :: P.Tm g a).
       Step (P.IfVal P.TrueVal t e) t
  BetaIfFalse
    :: forall g a (t :: P.Tm g a) (e :: P.Tm g a).
       Step (P.IfVal P.FalseVal t e) e
  BetaFst
    :: forall g a b (l :: P.Tm g a) (r :: P.Tm g b).
       Step (P.FstVal (P.PairVal l r)) l
  BetaSnd
    :: forall g a b (l :: P.Tm g a) (r :: P.Tm g b).
       Step (P.SndVal (P.PairVal l r)) r
  BetaCaseInl
    :: forall g a b c (v :: P.Tm g a) (l :: P.Tm (P.Snoc g a) c) (r :: P.Tm (P.Snoc g b) c).
       Step (P.CaseSum (P.InlVal v) l r) (P.substSingle l v)
  BetaCaseInr
    :: forall g a b c (v :: P.Tm g b) (l :: P.Tm (P.Snoc g a) c) (r :: P.Tm (P.Snoc g b) c).
       Step (P.CaseSum (P.InrVal v) l r) (P.substSingle r v)
  BetaNatZero
    :: forall g a (z :: P.Tm g a) (s :: P.Tm (P.Snoc g P.TNat) a).
       Step (P.NatFold P.ZeroVal z s) z
  BetaNatSucc
    :: forall g a (n :: P.Tm g P.TNat) (z :: P.Tm g a) (s :: P.Tm (P.Snoc g P.TNat) a).
       Step (P.NatFold (P.SuccVal n) z s) (P.substSingle s n)
  CongSucc
    :: Step m n -> Step (P.SuccVal m) (P.SuccVal n)
  CongIf
    :: Step c c' -> Step (P.IfVal c t e) (P.IfVal c' t e)
  CongAppL
    :: Step f f' -> Step (P.App f x) (P.App f' x)
  CongAppR
    :: Step x x' -> Step (P.App f x) (P.App f x')
  CongPairL
    :: Step l l' -> Step (P.PairVal l r) (P.PairVal l' r)
  CongPairR
    :: Step r r' -> Step (P.PairVal l r) (P.PairVal l r')

-- | Reflexive-transitive multi-step reduction.
type Steps :: forall (g :: P.Ctx) (a :: P.Ty). P.Tm g a -> P.Tm g a -> Type
data Steps m n where
  StepsRefl :: Steps m m
  StepsCons :: Step l m -> Steps m n -> Steps l n

type G1 = P.Snoc P.Empty P.TNat
type G2 = P.Snoc G1 (P.TArr P.TNat P.TNat)
type FnNat = P.TArr P.TNat P.TNat

-- | Open argument referencing the outer context variable so capture avoidance
-- must shift under inner Lam, CaseSum, and NatFold binders.
type OpenArg = P.Lam (P.SuccVal (P.V (P.VS P.VZ))) :: P.Tm G1 FnNat

-- | Nested lambda body binding two inner variables around a reference to the
-- substituted binder.
type CaptureBody =
  P.Lam (P.App (P.V (P.VS P.VZ)) (P.SuccVal (P.V P.VZ)))
    :: P.Tm (P.Snoc G1 FnNat) FnNat

type ShiftedOpenArg =
  P.Lam (P.SuccVal (P.V (P.VS (P.VS P.VZ))))
    :: P.Tm (P.Snoc G1 P.TNat) FnNat

-- | Capture-avoiding beta reduction under an inner lambda binder.
stepCaptureAvoided
  :: Step (P.App (P.Lam CaptureBody) OpenArg)
          (P.Lam (P.App ShiftedOpenArg (P.SuccVal (P.V P.VZ))))
stepCaptureAvoided = BetaLam @G1 @FnNat @FnNat @CaptureBody @OpenArg

-- | Capture-avoiding substitution across CaseSum branches and NatFold step binders.
type RichBody =
  P.CaseSum
    (P.InlVal (P.App (P.V P.VZ) P.ZeroVal) :: P.Tm (P.Snoc G1 FnNat) (P.TSum P.TNat P.TBool))
    (P.NatFold (P.V P.VZ) (P.App (P.V (P.VS P.VZ)) (P.V (P.VS (P.VS P.VZ)))) (P.SuccVal (P.V P.VZ)))
    (P.IfVal (P.V P.VZ) P.ZeroVal (P.SuccVal P.ZeroVal))
    :: P.Tm (P.Snoc G1 FnNat) P.TNat

richSubstCheck
  :: P.substSingle RichBody OpenArg
       :~: P.CaseSum
             (P.InlVal (P.App OpenArg P.ZeroVal))
             (P.NatFold (P.V P.VZ) (P.App ShiftedOpenArg (P.V (P.VS P.VZ))) (P.SuccVal (P.V P.VZ)))
             (P.IfVal (P.V P.VZ) P.ZeroVal (P.SuccVal P.ZeroVal))
richSubstCheck = Refl

-- | Simultaneous two-variable substitution across pairs, projections, and lambdas.
type PairBody =
  P.PairVal
    (P.App (P.V P.VZ) (P.V (P.VS P.VZ)))
    (P.Lam (P.App (P.V (P.VS P.VZ)) (P.SuccVal (P.V P.VZ))))
    :: P.Tm (P.Snoc (P.Snoc G1 P.TNat) FnNat) (P.TPair P.TNat FnNat)

pairSubstCheck
  :: P.substPair PairBody (P.SuccVal (P.V P.VZ)) OpenArg
       :~: P.PairVal
             (P.App OpenArg (P.SuccVal (P.V P.VZ)))
             (P.Lam (P.App ShiftedOpenArg (P.SuccVal (P.V P.VZ))))
pairSubstCheck = Refl

-- | Multi-step reduction trace combining beta, CaseSum, Pair projections, and NatFold.
type PipelineTerm =
  P.CaseSum
    (P.InlVal (P.FstVal (P.PairVal (P.SuccVal P.ZeroVal) P.TrueVal)) :: P.Tm G1 (P.TSum P.TNat P.TBool))
    (P.App (P.Lam (P.NatFold (P.V P.VZ) (P.SuccVal P.ZeroVal) (P.SuccVal (P.SuccVal (P.V P.VZ))))) (P.V P.VZ))
    P.ZeroVal

pipelineTrace :: Steps PipelineTerm (P.SuccVal (P.SuccVal P.ZeroVal))
pipelineTrace =
  StepsCons
    (BetaCaseInl @G1 @P.TNat @P.TBool @P.TNat
      @(P.FstVal (P.PairVal (P.SuccVal P.ZeroVal) P.TrueVal))
      @(P.App (P.Lam (P.NatFold (P.V P.VZ) (P.SuccVal P.ZeroVal) (P.SuccVal (P.SuccVal (P.V P.VZ))))) (P.V P.VZ))
      @P.ZeroVal)
    (StepsCons
      (CongAppR BetaFst)
      (StepsCons
        (BetaLam @G1 @P.TNat @P.TNat
          @(P.NatFold (P.V P.VZ) (P.SuccVal P.ZeroVal) (P.SuccVal (P.SuccVal (P.V P.VZ))))
          @(P.SuccVal P.ZeroVal))
        (StepsCons
          (BetaNatSucc @G1 @P.TNat @P.ZeroVal @(P.SuccVal P.ZeroVal) @(P.SuccVal (P.SuccVal (P.V P.VZ))))
          StepsRefl)))

-- | Untyped one-hole context plug composed with capture-avoiding substitution.
untypedContextSubst
  :: P.substSingleU
       (P.plugUCtx
          (P.UCLam (P.UCAppL P.UHole (P.UVar P.FZ)))
          (P.UPair (P.UVar P.FZ) (P.UVar (P.FS P.FZ))
            :: P.UTm (P.Succ (P.Succ (P.Succ P.Zero)))))
       (P.ULam (P.UVar (P.FS P.FZ)))
     :~: P.ULam
           (P.UApp
              (P.UPair (P.UVar P.FZ) (P.ULam (P.UVar (P.FS (P.FS P.FZ)))))
              (P.UVar P.FZ))
untypedContextSubst = Refl

-- | Symbolic open equation check on context-indexed variable lookup.
symbolicMarkedTail
  :: forall g a b (v :: P.Var g a) (tag :: P.Tag a).
     P.findMarkedVar (P.MkMarkedVar (P.VS v) tag :: P.MarkedVar (P.Snoc g b))
       :~: P.FoundTail (P.findMarkedVar (P.MkMarkedVar v tag))
symbolicMarkedTail = Refl

-- | Symbolic open equation check on context-indexed variable reconstruction.
symbolicMarkRoundtrip
  :: forall g a (tag :: P.Tag a).
     P.markFoundVar (P.FoundHead tag :: P.FoundVar (P.Snoc g a))
       :~: P.MkMarkedVar P.VZ tag
symbolicMarkRoundtrip = Refl

-- | Term-level wrapper exercising specialiser interaction on imported closures.
evalStepTerm :: P.Tm g a -> P.Tm (P.Snoc g a) b -> P.Tm g b
evalStepTerm arg body = P.substSingle body arg

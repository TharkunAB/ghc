{-# LANGUAGE DependentTypes #-}
module Main (main) where

import Data.Kind (Type)
import qualified GHC.Tuple as Tup
import GHC.Types (List)
import Numeric.Natural (Natural)
import NativeProposal378QuantifiersProvider

natImplicit :: foreach (n :: Natural). Natural
natImplicit @n = n

withComputed :: foreach (m :: Nat) (n :: Nat) -> List Char
withComputed m n = toListVec (vReplicateImplicit @(plus m n) 'k')

lamInvis :: foreach (n :: Nat). Vec Char n -> Nat
lamInvis = \ @n _ -> n

forwardEx5 :: foreach (b :: Type) -> b -> b
forwardEx5 b y = ex5 b y

forwardIdDep :: foreach (b :: Type) -> foreach (y :: b) -> b
forwardIdDep b y = idDep b y

checkR04 :: Int
checkR04 =
  let idFn :: Term (TArr TUnit TUnit)
      idFn = error "unused"
      step0 :: Prog Unit
      step0 = PStep Unit StepUnit
  in case step0 of
       PStep _ StepUnit ->
         let _ = liftApp1 idFn Unit
         in 1

checkR23 :: List Char
checkR23 =
  case leibnizToEqP (Leibniz (\px -> px) :: Leibniz Bool True True) of
    ReflP -> "refl"

extRen :: forall g d s.
          (forall x. FinCtx g x -> FinCtx d x)
       -> (forall x. FinCtx (Snoc g s) x -> FinCtx (Snoc d s) x)
extRen _ FZ     = FZ
extRen r (FS v) = FS (r v)

sRename :: forall g d t.
           (forall x. FinCtx g x -> FinCtx d x)
        -> STerm g t -> STerm d t
sRename r (SVar v) = SVar (r v)
sRename r (SLam m) = SLam (sRename (extRen r) m)

data SimRel :: forall (g :: Ctx) (t :: Ty). STerm g t -> STerm g t -> Type where
  SimVar :: SimRel (SVar v) (SVar v)
  SimLam :: SimRel n n' -> SimRel (SLam n) (SLam n')

simRename
  :: forall (g :: Ctx) (d :: Ctx) (t :: Ty)
            (r :: forall x. FinCtx g x -> FinCtx d x)
            (m :: STerm g t) (m' :: STerm g t).
     SimRel m m' -> SimRel (sRename r m) (sRename r m')
simRename SimVar     = SimVar
simRename (SimLam s) = SimLam (simRename s)

simStep
  :: forall (g :: Ctx) (d :: Ctx) (t :: Ty)
            (r :: forall x. FinCtx g x -> FinCtx d x)
            (m :: STerm g t).
     SimRel m m -> Tup.Tuple2 (SimRel (sRename r m) (sRename r m)) Int
simStep s =
  case simRename @g @d @t @r s of
    SimVar       -> (SimVar, 0)
    SimLam inner -> (SimLam inner, 1)

checkR16 :: Int
checkR16 =
  let sim0 :: SimRel @(Snoc Empty TUnit) @(TArr TUnit TUnit)
                     (SLam (SVar (FS FZ))) (SLam (SVar (FS FZ)))
      sim0 = SimLam SimVar
  in seq simStep (case sim0 of SimLam SimVar -> 1)

main :: IO Tup.Unit
main = do
  let v2 = vReplicateImplicit @(Succ (Succ Zero)) 'a'
      v1Legacy = vReplicateImplicit @{Succ Zero} 'b'
      vExp = vReplicateExplicit @Char @(Succ (Succ Zero)) 'c'
      q1 = quickLength @_ @(Succ (Succ Zero)) v2
      q2 = quickLength @Char @_ v1Legacy
      sk = skipInferred @Char @(Succ (Succ Zero)) v1Legacy v2
      oi = onlyInferred v2
      l1 = lamInvis v2
      l2 = lamInvis @(Succ Zero) v1Legacy
      localLam :: foreach (n :: Nat). Vec Char n -> Nat
      localLam = \ @n _ -> n
      l3 = localLam v2
      n7 = natImplicit @7
      comp = withComputed (Succ Zero) (Succ (Succ Zero))
      u1 = unannotId Int 99
      u2 = unannotLen (Succ (Succ Zero)) v2
      u3 = unannotPlus @Char @(Succ Zero) @(Succ (Succ Zero)) v1Legacy v2
      u4 = unannotInferred v2
      w1 = wildcardType Bool 41
      e1 = ex5 Int 42
      e2 = ex5 (Maybe Char) (Just 'z')
      e3 = ex5 (type Int) 7
      e4 = ex5Annot Int 19
      e5 = ex5Maybe Char 'm'
      d1 = idDep Nat (Succ Zero)
      it1 = implType @Int 12
      fw1 = forwardEx5 Bool True
      fw2 = forwardIdDep Nat Zero
  print
    ( (toListVec v2, toListVec v1Legacy, toListVec vExp, q1, q2, sk, oi, l1, l2, l3, n7, comp)
    , (u1, u2, u3, u4, w1)
    , (e1, e2, e3, e4, e5, d1, it1, fw1, fw2)
    , (checkR04, checkR23, checkR16)
    )

{-# LANGUAGE DependentTypes, ListTuplePuns #-}
-- | Client for casted certified source-call operands across interfaces (R03, R05, R11).
module Main (main) where

import NativeRetainedCastSourceCallProvider

-- | Identity combinator.
idTerm :: Term CEmpty (TArr a a)
idTerm = Lam (Var Z)

-- | Constant combinator.
constTerm :: Term CEmpty (TArr a (TArr b a))
constTerm = Lam (Lam (Var (S Z)))

-- | Closed two-step redex.
sample :: Term CEmpty (TArr TNat TNat)
sample = App (App constTerm idTerm) idTerm

-- | Exercise exported INLINE unfolding with a GADT-cast source-call operand in client Core.
clientAppBeta :: forall a b.
                 foreach (l :: Term CEmpty (TArr a b))
              -> foreach (m :: Term CEmpty a)
              -> Value l
              -> Value m
              -> Progress (App l m)
clientAppBeta l = case l of
  Lam body -> \m _ w -> inlineBetaStep body m w
  Var x    -> \_ _ _ -> case x of {}
  App _ _  -> \_ v _ -> case v of {}

-- | Verify that stepping the left function in an application leaves an unused right operand lazy.
lazyRightStep :: Bool
lazyRightStep =
  let partial :: Term CEmpty (TArr TNat TNat)
      partial = App (App constTerm idTerm) (error "lazy right operand forced")
   in case progress partial of
        PStep _ (XiApp1 _) -> True
        _                  -> False

-- | Run all checks and print the tuple summary.
main :: IO ()
main = do
  let okFull = case eval (Succ (Succ (Succ Zero))) sample of
        EvalSteps _ (SCons _ (SCons _ SRefl)) (Just VLam) -> True
        _ -> False
      okOne = case eval (Succ Zero) sample of
        EvalSteps _ (SCons _ SRefl) Nothing -> True
        _ -> False
      lPoly :: Term CEmpty (TArr (TArr TNat TNat) (TArr TNat TNat))
      lPoly = idTerm
      idNat :: Term CEmpty (TArr TNat TNat)
      idNat = idTerm
      okClient = case (progress lPoly, progress idNat) of
        (PDone v, PDone w) -> case clientAppBeta lPoly idNat v w of
          PStep t (BetaLam VLam) -> case progress t of
            PDone VLam -> True
            _          -> False
          _ -> False
        _ -> False
  print (okFull, okOne, okClient, lazyRightStep)

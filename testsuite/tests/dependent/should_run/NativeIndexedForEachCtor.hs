{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
module Main where

import GHC.Sigma (Sigma(..))
import NativeIndexedForEachCtorProvider

lazyWitness :: EvalStep m -> Bool
lazyWitness (MkStep _ _) = True

checkPackLam :: foreach (b' :: UTerm ('Succ 'Zero)) -> LamSteps 'Zero b' -> Bool
checkPackLam _ (Pair t steps) = case t of
  ULam _ -> case steps of BRefl -> True; _ -> False
  _ -> False

checkPackVar :: VarSteps -> Bool
checkPackVar (Pair t steps) = case t of
  UVar x -> case x of
    FS y -> case y of
      FZ -> case steps of BRefl -> True; _ -> False
      _ -> False
    _ -> False
  _ -> False

mkLazyLamWrap :: foreach (b :: UTerm ('Succ 'Zero)) -> Wrap ('ULam b)
mkLazyLamWrap b = MkWrap (ULam b)

checkLazyWrap :: Wrap t -> Bool
checkLazyWrap (MkWrap _) = True

checkLazySome :: WrapSome s -> Bool
checkLazySome (MkWrapSome _) = True

mkLazyLamSigma :: foreach (b :: UTerm ('Succ 'Zero)) -> LamSteps 'Zero b
mkLazyLamSigma b = Pair (ULam b) BRefl

main :: IO ()
main = do
  let lamArg :: UTerm ('Succ 'Zero)
      lamArg = UVar FZ
      s1 :: LamSteps 'Zero ('UVar 'FZ)
      s1 = packLam (UVar FZ)
      s2 = packVar
      unusedU :: UTerm 'Zero
      unusedU = error "unused-witness"
      e1 = mkEvalStep unusedU (error "unused-steps")
      e2 = MkStep (ULam (UVar FZ)) BRefl
      unusedLamBody :: UTerm ('Succ 'Zero)
      unusedLamBody = error "boom-lam-field"
      unusedW :: UTerm 'Zero
      unusedW = error "unused-wrap"
      unusedSome :: SomeTerm
      unusedSome = error "unused-some"
  print ( checkPackLam lamArg (packLam lamArg)
        , checkPackLam (UVar FZ) s1
        , checkPackVar s2
        , lazyWitness e1
        , case e2 of
            MkStep n steps -> case n of
              ULam _ -> case steps of BRefl -> True; _ -> False
              _ -> False
        , case mkLazyLamWrap unusedLamBody of
            MkWrap t -> case t of ULam _ -> True; _ -> False
        , checkLazyWrap (MkWrap unusedW)
        , checkLazySome (MkWrapSome unusedSome)
        , case mkLazyLamSigma unusedLamBody of
            Pair t _ -> case t of ULam _ -> True; _ -> False
        , case cmpFinFZ @('Succ 'Zero) (FS (FS FZ)) of
            CmpNeq y r -> case y of
              FS z -> case z of
                FZ -> case r of RemSS RemGt -> True
                _ -> False
              _ -> False
        )

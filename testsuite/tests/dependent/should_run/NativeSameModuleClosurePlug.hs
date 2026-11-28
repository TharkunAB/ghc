{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, PolyKinds, TypeApplications, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:) (Refl))
import qualified NativeSameModuleClosurePlugProvider as P
import qualified NativeSameModuleClosurePlugRev as PR
import qualified NativeSameModuleClosurePlugPrenex as PP
import qualified NativeSameModulePrunedSplice as PS
import qualified NativeSameModuleNamedAndSigma as NS

checkStep :: P.Step m n -> String
checkStep (P.BetaLam _)   = "lam"
checkStep (P.BetaLst _ _) = "lst"

checkStepR :: PR.Step m n -> String
checkStepR (PR.BetaLam _)   = "lam"
checkStepR (PR.BetaLst _ _) = "lst"

checkStepP :: PP.Step m n -> String
checkStepP (PP.BetaLam _)   = "lam"
checkStepP (PP.BetaLst _ _) = "lst"

main :: IO ()
main = do
  PS.ok `seq` NS.sameInlineUZ @(NS.NZero) @(NS.ULam (NS.UVar NS.FZ)) `seq` pure ()
  let p1 = P.plugLamEq @('P.Succ 'P.Zero) @'P.Zero @'P.CHole @('P.UVar 'P.FZ)
      p2 = P.betaId @'P.CEmpty @('P.TArr 'P.TNat 'P.TNat) @('P.Lam ('P.Var 'P.Z))
      p3 = P.betaUntypedEq @('P.Succ 'P.Zero) @'P.FZ
      r1 = PR.plugLamEq @('PR.Succ 'PR.Zero) @'PR.Zero @'PR.CHole @('PR.UVar 'PR.FZ)
      q1 = PP.plugLamEq @('PP.Succ 'PP.Zero) @'PP.Zero @'PP.CHole @('PP.UVar 'PP.FZ)
  case (p1, p2, p3, r1, q1) of
    (Refl, Refl, Refl, Refl, Refl) ->
      if (checkStepR PR.betaShift, checkStepR PR.betaDouble) == ("lam", "lst")
         && (checkStepP PP.betaShift, checkStepP PP.betaDouble) == ("lam", "lst")
        then print (checkStep P.betaShift, checkStep P.betaDouble)
        else error "Variant mismatch"

{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
module Main where
import Data.Functor.Const (Const(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import NativePackageOperand ()
import NativeRetainedPackageClients
import NativeRetainedPackageProviders (captured, sumWitness)
import PackedVecLibrary
import RetainedNat
import RetainedProjection (project)

main :: IO ()
main = do
  print (withRetained (constantPackage (error "unused witness") (7 :: Int))
                  (\_ box -> getConst box),
              withRetained (constantPackage Z (error "unused payload" :: Int))
                  (\n _ -> natToInt n))
  let q = constantPackage (error "unused projected witness") (7 :: Int)
  print (getConst (project q))
  print (withRetained q (\_ box -> case box of Const x -> x))
  case captured of
    Pair True Refl -> pure ()
    Pair False _ -> fail "unexpected captured witness"
  case sumWitness (S Z) (S (S Z)) of
    Pair k Refl | natToInt k == 3 -> pure ()
    _ -> fail "unexpected sum witness"
  let m = error "unused first operand" :: Nat
      n = error "unused second operand" :: Nat
  case sumWitness m n of
    Pair _ _ -> pure ()
  runGeneral
  runRuntimeProjection
  runQuantified
  runLinked
  runListVector
  runDefaultO1

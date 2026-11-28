{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, NoImpredicativeTypes #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import NativeAppUnderRetainedEqualityImpred (runImpred)
import Provider

isApplication :: Typed 'Empty -> Bool
isApplication (Pack _ (App Id Zero)) = True
isApplication _ = False

main :: IO ()
main = do
  let direct = packApp Empty Nat Nat Nat Refl Id Zero
      inferred = inferApp Empty Nat Nat Nat Refl Id Zero
  print (isApplication direct, isApplication inferred)
  print runImpred

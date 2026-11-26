{-# LANGUAGE DataKinds, GADTs, ImpredicativeTypes, NoDependentTypes #-}
module NativeAppUnderRetainedEqualityImpred (runImpred) where
import Data.Type.Equality ((:~:)(Refl))
import Provider

isApplication :: Typed 'Empty -> Bool
isApplication (Pack _ (App Id Zero)) = True
isApplication _ = False

runImpred :: (Bool, Bool)
runImpred =
  let direct = packApp Empty Nat Nat Nat Refl Id Zero
      inferred = inferApp Empty Nat Nat Nat Refl Id Zero
  in (isApplication direct, isApplication inferred)

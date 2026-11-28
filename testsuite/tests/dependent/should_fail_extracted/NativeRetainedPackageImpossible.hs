{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedPackageImpossible where
import Data.Type.Equality ((:~~:)(HRefl))
import PackedVecLibrary (Nat(..), Vec(..))

impossible :: foreach (xs :: Vec Bool ('S n)) -> Maybe (xs :~~: ('VNil :: Vec Bool 'Z)) -> Bool
impossible _ proof = case proof of
    Just HRefl -> True
    Nothing -> False

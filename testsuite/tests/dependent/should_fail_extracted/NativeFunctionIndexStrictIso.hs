{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeFunctionIndexStrictIso where
import Data.Type.Equality ((:~:)(Refl))
import NativeFunctionIndexIsoIdentity (strictIdentity)

strict :: foreach (x :: a) -> strictIdentity (strictIdentity x) :~: x
strict _ = Refl

{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies, TypeOperators #-}
module NativeDataFamilyRetainedWrongIdentity where
import Data.Type.Equality ((:~:)(Refl))
import TupleProvider
wrong :: foreach (x :: Tuple Bool Bool) -> foreach (y :: Tuple Bool Bool) -> x :~: y
wrong _ _ = Refl

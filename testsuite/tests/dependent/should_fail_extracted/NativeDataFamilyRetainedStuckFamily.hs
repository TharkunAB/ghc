{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies, TypeOperators #-}
module NativeDataFamilyRetainedStuckFamily where
import Data.Type.Equality ((:~:)(Refl))
type family Reduce a
unresolved :: foreach (t :: Reduce Bool) -> t :~: t
unresolved _ = Refl

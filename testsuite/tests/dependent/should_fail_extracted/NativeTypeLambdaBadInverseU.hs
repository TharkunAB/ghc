{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativeTypeLambdaBadInverseU where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
badInverseU :: forall (p :: Type %U -> Type) a b. (p a ~ p b) => a :~: b
badInverseU = Refl

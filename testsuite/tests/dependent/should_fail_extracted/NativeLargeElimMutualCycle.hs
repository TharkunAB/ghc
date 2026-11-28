{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns, TypeOperators #-}
module NativeLargeElimMutualCycle where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
data Val = Bot | Arr Val Val
f :: Val -> Type
f Bot       = type ()
f (Arr a _) = type (g a)
g :: Val -> Type
g Bot       = type ()
g (Arr a _) = type (f a)
bad :: f 'Bot :~: ()
bad = Refl

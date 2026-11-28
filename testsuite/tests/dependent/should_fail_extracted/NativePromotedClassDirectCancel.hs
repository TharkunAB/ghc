{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativePromotedClassDirectCancel where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
control :: forall a b (f :: a %U -> b) (x :: a) (y :: a). Proxy f -> Prelude.fmap f ('Just x) :~: Prelude.fmap f ('Just y) -> x :~: y
control _ Refl = Refl

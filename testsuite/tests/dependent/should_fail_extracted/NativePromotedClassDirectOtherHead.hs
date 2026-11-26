{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativePromotedClassDirectOtherHead where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
control :: forall a b (f :: a %U -> b) (g :: a %U -> b) (x :: a). Proxy f -> Proxy g -> Proxy x -> Prelude.fmap f ('Just x) :~: 'Just (g x)
control _ _ _ = Refl

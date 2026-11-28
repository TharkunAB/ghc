{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module NativePromotedClassDirectGiven where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
control :: forall a b (f :: a %U -> b) (x :: a). Functor Maybe => Proxy f -> Proxy x -> Prelude.fmap f ('Nothing :: Maybe a) :~: ('Nothing :: Maybe b)
control _ _ = Refl

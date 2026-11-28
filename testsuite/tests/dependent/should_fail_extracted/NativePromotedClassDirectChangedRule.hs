{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativePromotedClassDirectChangedRule where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
{-# RULES "map-bool-nothing" forall f. Prelude.fmap @Maybe f (Nothing @Bool) = Just (f True) #-}
control :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x -> Prelude.fmap f ('Nothing :: Maybe a) :~: ('Nothing :: Maybe b)
control _ _ = Refl

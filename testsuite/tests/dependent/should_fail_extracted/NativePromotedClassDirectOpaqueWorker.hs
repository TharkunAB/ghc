{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativePromotedClassDirectOpaqueWorker where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import NativePromotedClassDirectOpaqueWorker_Prov (Alternate(..))
control :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x -> alternate f ('Just x) :~: ('Nothing :: Maybe b)
control _ _ = Refl

{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativePromotedClassAltControl (altControl) where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import NativePromotedClassAltProv (Alternate(..))

altControl :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x
           -> alternate f ('Just x) :~: ('Nothing :: Maybe b)
altControl _ _ = Refl

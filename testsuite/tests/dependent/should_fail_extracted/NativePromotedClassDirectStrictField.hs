{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativePromotedClassDirectStrictField where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import DirectMappingProvider (Alternate(..))
control :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x -> strictMap f ('Just x) :~: 'Just (f x)
control _ _ = Refl

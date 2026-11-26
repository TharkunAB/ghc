{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativePromotedClassMappingGiven where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import MappingWorker (MapLike(..))
present :: forall a b (f :: a %U -> b) (x :: a). MapLike Maybe => Proxy f -> Proxy x -> mapLike f ('Just x) :~: 'Just (f x)
present _ _ = Refl

{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers, PolyKinds, TypeOperators #-}
module NativePromotedClassMappingWrongTag where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import MappingTags (Tagged(..))
import MappingWorker (MapLike(..))
tag :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x -> mapLike f ('Old x) :~: 'Old (f x)
tag _ _ = Refl

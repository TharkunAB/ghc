{-# LANGUAGE DependentTypes, NoListTuplePuns #-}
module NativeDataHeraldPunKindMismatch where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeDataHeraldInTypesProvider (T (..))
bad :: Proxy T :~: Proxy (data T)
bad = Refl

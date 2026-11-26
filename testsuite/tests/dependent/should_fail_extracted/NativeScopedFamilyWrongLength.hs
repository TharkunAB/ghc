{-# LANGUAGE DataKinds, NoDependentTypes #-}
module NativeScopedFamilyWrongLength where
import Data.Proxy
import ScopedFamilyMethodProvider
wrong :: Collects c => c -> Proxy 6
wrong x = case measuredEntry x 5 of (_, lengthProxy) -> lengthProxy
wrongProject :: Collects c => c -> Proxy 6
wrongProject x = snd (id (measuredEntry x 5))

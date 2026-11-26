{-# LANGUAGE NoDependentTypes, TypeFamilies #-}
module NativeScopedDictCombinedMissing where
import Data.Proxy
import NativeScopedDictExportProvider
bad :: Proxy (Element Int)
bad = Proxy

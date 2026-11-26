{-# LANGUAGE Haskell2010, NoDependentTypes, TypeFamilies #-}
module NativeScopedFamilyMissingOwner where
import Data.Proxy
import ScopedFamilyMethodProvider
missing :: Proxy (Element Double)
missing = Proxy

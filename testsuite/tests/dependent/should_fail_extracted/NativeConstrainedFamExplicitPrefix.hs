{-# LANGUAGE Modifiers, NoDependentTypes, TypeFamilies #-}
module NativeConstrainedFamExplicitPrefix where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Matchability (U)
import NativeConstrainedFamilyProvider
data Hold (f :: Type %U -> Type)
bad :: Proxy c -> Proxy (Hold (Explicit (Key Bool)))
bad _ = Proxy

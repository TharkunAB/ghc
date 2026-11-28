{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeCaseFieldCompoundKind where
import Data.Kind (Type)
import Data.Proxy (Proxy)

data Tag k l = Tag

data Compound (k :: Type) (l :: Type) (a :: Tag k l) = Compound
  (forall (k :: Type) (l :: Type) (b :: Bool). Proxy b ->
    (case b of { True -> Proxy a; False -> Proxy '(k, l) }))

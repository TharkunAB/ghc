{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeTypeCaseFieldFamilyKind where
import Data.Kind (Type)
import Data.Proxy (Proxy)

type family PayloadKind (k :: Type) (l :: Type) :: Type
data Compound (k :: Type) (l :: Type) (a :: PayloadKind k l) = Compound
  (forall (k :: Type) (l :: Type) (b :: Bool). Proxy b ->
    (case b of { True -> Proxy a; False -> Proxy '(k, l) }))

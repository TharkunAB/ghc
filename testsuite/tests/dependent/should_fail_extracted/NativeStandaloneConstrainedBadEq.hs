{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module NativeStandaloneConstrainedBadEq where
import Data.Kind (Type)
import NativeStandaloneConstrainedFamilyProvider (Base, ElemTop)
type MissingPremise :: Base c => c -> Type
type family MissingPremise c where
  MissingPremise c = ElemTop c

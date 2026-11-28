{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers, TypeFamilies #-}
module NativeConstrainedFamUnsatElem where
import Data.Kind (Type)
import GHC.Matchability (U)
import NativeConstrainedFamilyProvider (Collects(..))
data Uses (f :: Type %U -> Type) = Uses
partial :: Uses Elem
partial = Uses

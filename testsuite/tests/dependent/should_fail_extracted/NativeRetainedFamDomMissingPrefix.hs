{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers, TypeFamilies #-}
module NativeRetainedFamDomMissingPrefix where
import ConstrainedElements (KnownIndex(..))
import GHC.Matchability (U)
data Uses (f :: Maybe Int %U -> Maybe Int) = Uses
prefix :: Uses Echo
prefix = Uses

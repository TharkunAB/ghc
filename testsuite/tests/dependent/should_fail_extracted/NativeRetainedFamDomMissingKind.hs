{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeFamilies, TypeOperators #-}
module NativeRetainedFamDomMissingKind where
import ConstrainedElements (KnownIndex(..))
import Data.Type.Equality ((:~:)(Refl))
missingKind :: Echo ('Nothing :: Maybe Int) :~: 'Nothing
missingKind = Refl

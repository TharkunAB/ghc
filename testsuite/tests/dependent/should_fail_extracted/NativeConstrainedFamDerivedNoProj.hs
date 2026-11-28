{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving, NoDependentTypes, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module NativeConstrainedFamDerivedNoProj where
import NativeConstrainedFamilyProvider (Allowed, ListBacking, ListSource)
newtype Wrapped a = Wrapped [a]
type instance ListBacking (Wrapped a) = ()
deriving newtype instance Allowed a => ListSource (Wrapped a)
newtype ViaList a = ViaList [a]
type instance ListBacking (ViaList a) = ()
deriving via [a] instance Allowed a => ListSource (ViaList a)
newtype Inferred a = Inferred [a]
    deriving newtype ListSource
type instance ListBacking (Inferred a) = ()

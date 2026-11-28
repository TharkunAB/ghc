{-# LANGUAGE NoDependentTypes, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module NativeConstrainedFamMissingEq where
import NativeConstrainedFamilyProvider (Collects)
instance Collects Bool

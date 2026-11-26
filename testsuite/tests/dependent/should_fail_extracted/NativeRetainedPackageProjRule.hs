{-# LANGUAGE NoDependentTypes #-}
module NativeRetainedPackageProjRule () where
import PackedLength (packageLength)
import PackedVecLibrary (Nat(..))
{-# RULES "packageLength/zero" forall p. packageLength p = Z #-}

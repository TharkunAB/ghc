{-# LANGUAGE NoDependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module NativeStandaloneConstrainedDisabled where
import Data.Kind (Type)
class C a
type F :: C a => a -> Type
type family F a

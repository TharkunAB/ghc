{-# LANGUAGE DependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module NativeImportedFamilyCycleFail where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import ImportedFamilyProvider (Payload, hidden)

type instance Payload Int = NativeImportedFamilyCycleFail.keep Bool

keep :: forall a. a -> a
keep x = const x (hidden Proxy)

type family SigFam a
type instance SigFam Int = NativeImportedFamilyCycleFail.keepSig Bool

keepSig :: forall a. InputSig a -> a
keepSig x = x

type InputSig :: SigFam Int -> Type
type family InputSig a where InputSig _ = Bool

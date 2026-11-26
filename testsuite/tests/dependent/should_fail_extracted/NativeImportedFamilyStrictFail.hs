{-# LANGUAGE DependentTypes, TypeFamilies #-}
module NativeImportedFamilyStrictFail where

import ImportedFamilyProvider (Payload, importedStrict)

type family LocalPayload a
type instance LocalPayload Word = NativeImportedFamilyStrictFail.direct Bool

type family Input a where
  Input a = a

direct :: forall a. Input a -> a
direct x = seq x x

type instance Payload Int = NativeImportedFamilyStrictFail.keep Bool Char

keep :: forall a b. a -> b -> b
keep x y = importedStrict x y

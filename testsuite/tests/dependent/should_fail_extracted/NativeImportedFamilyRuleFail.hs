{-# LANGUAGE DependentTypes, TypeApplications, TypeFamilies #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeImportedFamilyRuleFail where

import Data.Kind (Type)
import ImportedFamilyProvider (Payload, importedFirst)

type family LocalPayload a
type instance LocalPayload Word = NativeImportedFamilyRuleFail.direct Bool

type family Input a where
  Input a = a

direct :: forall a. Input a -> a
direct x = x

{-# RULES "dependent/direct" forall (x :: Input Type). direct @Type x = x #-}

type instance Payload Int = NativeImportedFamilyRuleFail.keep Bool Char

keep :: forall a. a -> a -> a
keep x y = importedFirst x y

{-# RULES "imported-family/change" forall x y. importedFirst x y = y `seq` x #-}

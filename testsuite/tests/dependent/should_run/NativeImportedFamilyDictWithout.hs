{-# LANGUAGE NoDependentTypes, TypeFamilies #-}
module NativeImportedFamilyDictWithout (dictWithout) where

import Data.Proxy (Proxy(..))
import ImportedFamilyProvider (hidden)

keep :: a -> a
keep x = const x (hidden Proxy)

choose :: a -> a -> a
choose x y = if hidden Proxy then x else y

dictWithout :: (Bool, Bool)
dictWithout = (keep False, choose True False)

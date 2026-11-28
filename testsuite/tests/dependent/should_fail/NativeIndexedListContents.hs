{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeOperators #-}
module NativeIndexedListContents where

import IndexedPrefix (CheckedList, negatedCons)

wrongHead :: foreach (b :: Bool) -> foreach (bs :: [Bool]) -> CheckedList (b ': bs)
wrongHead b bs = negatedCons b bs

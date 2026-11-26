{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeClosedFamilySumCategoryReject where

import NativeClosedFamilySumProvider (Tagged)

data family DataOpen a
type instance DataOpen Bool = some (tag :: Bool) | Tagged tag Bool

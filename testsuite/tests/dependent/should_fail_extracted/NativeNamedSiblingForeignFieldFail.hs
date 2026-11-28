{-# LANGUAGE NoDependentTypes, NoDisambiguateRecordFields #-}
module NativeNamedSiblingForeignFieldFail where

import NamedSiblingColumns
import PackedVecLibrary (Nat(..))

data Other = Other { foreignField :: Bool }

wrong :: NamedColumns
wrong = NamedColumns { storedLength = Z, foreignField = True }

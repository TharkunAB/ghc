{-# LANGUAGE NoDependentTypes #-}
module NativeNamedSiblingMissingWerror where

import NamedSiblingColumns
import PackedVecLibrary (Nat(..))

missingBoth :: NamedColumns
missingBoth = NamedColumns { storedLength = S Z }

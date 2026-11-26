{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers #-}
module NativeOpenInstalledDropWhileStrictFail where

import GHC.Exts (U)
import NativeOpenSpanShapes (strictSpan)

type StrictSplit :: (a %U -> Bool) -> [a] -> ([a], [a])
type StrictSplit p xs = strictSpan p xs

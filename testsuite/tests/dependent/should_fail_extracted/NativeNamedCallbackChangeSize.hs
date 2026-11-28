{-# LANGUAGE GADTs, NoDependentTypes #-}
module NativeNamedCallbackChangeSize where

import Data.Type.Equality ((:~:)(Refl))
import NativeNamedCallbackMeasuredZip (measuredZip)
import qualified PreludeMapLengthSize as A
import NativeNamedCallbackChangeSizeRule ()

check :: [Bool] -> [Int] -> Bool
check xs ys = case measuredZip A.size xs ys Refl of Refl -> True

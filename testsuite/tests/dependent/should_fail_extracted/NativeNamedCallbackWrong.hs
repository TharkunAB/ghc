{-# LANGUAGE GADTs, NoDependentTypes #-}
module NativeNamedCallbackWrong where

import Data.Type.Equality ((:~:)(Refl))
import NativeNamedCallbackMeasuredZip (measuredZip)
import NativeNamedCallbackOtherSizes (otherSize)
import qualified PreludeMapLengthSize as A

uncheckedSize :: [a] -> A.Size
uncheckedSize _ = A.Zero

wrongFn :: [Bool] -> [Int] -> Bool
wrongFn xs ys = case measuredZip otherSize xs ys Refl of Refl -> True

uncheckedFn :: [Bool] -> [Int] -> Bool
uncheckedFn xs ys = case measuredZip uncheckedSize xs ys Refl of Refl -> True

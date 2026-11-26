{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeNamedCallbackMeasuredZip (measuredZip, runMeasure) where

import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthSize (Size, minSize, size)
import PreludeZipRuntime (lengthZip)

measuredZip :: forall a b. foreach (measure :: [a] -> Size)
            -> foreach (xs :: [a]) -> foreach (ys :: [b])
            -> size xs :~: measure xs
            -> size (zip xs ys) :~: minSize (measure xs) (size ys)
measuredZip _ xs ys known =
    case lengthZip xs ys of Refl -> case known of Refl -> Refl

runMeasure :: forall a. foreach (measure :: [a] -> Size) -> foreach (xs :: [a])
           -> (Size, measure xs :~: measure xs)
runMeasure measure xs = (measure xs, Refl)

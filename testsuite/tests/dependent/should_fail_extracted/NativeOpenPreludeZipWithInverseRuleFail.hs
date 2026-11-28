{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenPreludeZipWithInverseRuleFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenPreludeZipWithBadInverseRules ()
import NativeOpenPreludeZipWithFoldProvider (zipProbe)
import PreludeMapLengthSize (minSize, size)

badInverse :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
           -> size (zipProbe xs ys) :~: minSize (size xs) (size ys)
badInverse [] _ = Refl
badInverse (_ : _) [] = Refl
badInverse (_ : xs) (_ : ys) = case badInverse xs ys of Refl -> Refl

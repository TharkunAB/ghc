{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenPreludeZipWithRuleFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenPreludeZipWithBadFold (zipStrictSeed)
import NativeOpenPreludeZipWithBadFoldRules ()
import PreludeMapLengthSize (minSize, size)

badStrict :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
          -> size (zipStrictSeed xs ys) :~: minSize (size xs) (size ys)
badStrict [] _ = Refl
badStrict (_ : _) [] = Refl
badStrict (_ : xs) (_ : ys) = case badStrict xs ys of Refl -> Refl

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers, RankNTypes,
             RequiredTypeArguments, TypeApplications #-}
module NativeOpenFilterCutoffFail where

import Data.Kind (Type)
import GHC.Exts (U)
import PreludeFilterCutoff (decideRow)
import PreludeFilterLengthProof (AtMost)
import PreludeFilterRuntime (BooleanResult, negationResult, runtimeFilterBoundFor)
import PreludeFilterSource (FilterWith, Negation)
import PreludeMapLengthSource (Count)

wrongTarget :: foreach (f :: [Bool] -> Bool) -> foreach (g :: [Bool] -> Bool)
            -> foreach (rows :: [[Bool]]) -> AtMost (Count (FilterWith f rows)) (Count rows)
wrongTarget _ g rows = runtimeFilterBoundFor @[Bool] (type g) (decideRow g) rows

wrongDecider :: foreach (f :: [Bool] -> Bool) -> foreach (g :: [Bool] -> Bool)
             -> foreach (rows :: [[Bool]]) -> AtMost (Count (FilterWith f rows)) (Count rows)
wrongDecider f g rows = runtimeFilterBoundFor @[Bool] (type f) (decideRow g) rows

choose :: forall (a :: Type) b. forall (p :: a %U -> Bool) -> Show b
       => (foreach (x :: a) -> BooleanResult (p x)) -> Int
choose (type _) _ = 0

ambiguousResult :: Int
ambiguousResult = choose @Bool (type Negation) negationResult

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules #-}
module NativeLocalListAccumulatorRevRuleFail where

import Data.Type.Equality ((:~:)(Refl))

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

reverseStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
            -> rev (x ': xs) :~: (rev xs ++ '[x])
reverseStep _ _ = Refl

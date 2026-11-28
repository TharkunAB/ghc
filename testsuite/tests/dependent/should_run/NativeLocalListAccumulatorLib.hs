{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeLocalListAccumulatorLib (shunt, step, rev, reverseStep) where

import Data.Type.Equality ((:~:)(Refl))

shunt :: [a] -> [a] -> [a]
shunt [] ys = ys
shunt (x : xs) ys = shunt xs (x : ys)

step :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
     -> shunt (x ': xs) ys :~: shunt xs (x ': ys)
step _ _ _ = Refl

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

reverseStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
            -> rev (x ': xs) :~: (rev xs ++ '[x])
reverseStep _ _ = Refl

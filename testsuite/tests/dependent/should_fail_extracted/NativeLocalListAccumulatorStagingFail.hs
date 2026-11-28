{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeLocalListAccumulatorStagingFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeLocalListAccumulatorStrict ()

shuntPartial :: [a] -> [a] -> [a]
shuntPartial (x : xs) ys = shuntPartial xs (x : ys)

stepPartial :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
            -> shuntPartial (x ': xs) ys :~: shuntPartial xs (x ': ys)
stepPartial _ _ _ = Refl

shuntLoop :: [a] -> [a] -> [a]
shuntLoop [] ys = ys
shuntLoop (x : xs) ys = shuntLoop (x : xs) (x : ys)

stepLoop :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
         -> shuntLoop (x ': xs) ys :~: shuntLoop xs (x ': ys)
stepLoop _ _ _ = Refl

shuntForced :: [a] -> [a] -> [a]
shuntForced [] ys = ys
shuntForced (x : xs) ys = seq ys (shuntForced xs (x : ys))

stepForced :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
           -> shuntForced (x ': xs) ys :~: shuntForced xs (x ': ys)
stepForced _ _ _ = Refl

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, Strict, TypeOperators #-}
module NativeLocalListAccumulatorStrict (shuntStrict, badStrict) where

import Data.Type.Equality ((:~:)(Refl))

shuntStrict :: [a] -> [a] -> [a]
shuntStrict [] ys = ys
shuntStrict (x : xs) ys = shuntStrict xs (x : ys)

badStrict :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
          -> shuntStrict (x ': xs) ys :~: shuntStrict xs (x ': ys)
badStrict _ _ _ = Refl

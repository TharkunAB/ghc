{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeRevExplicitListO2Shunt (shunt, shuntAppend) where

import Data.Type.Equality ((:~:)(Refl))

shunt :: [a] -> [a] -> [a]
shunt []       ys = ys
shunt (x : xs) ys = shunt xs (x : ys)

shuntAppend :: foreach (xs :: [a]) -> foreach (ys :: [a]) -> foreach (zs :: [a])
            -> shunt (xs ++ ys) zs :~: shunt ys (shunt xs zs)
shuntAppend []       _  _  = Refl
shuntAppend (x : xs) ys zs = shuntAppend xs ys (x : zs)

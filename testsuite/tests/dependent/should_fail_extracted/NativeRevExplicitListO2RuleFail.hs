{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeRevExplicitListO2RuleFail where

import Data.Type.Equality ((:~:)(Refl), gcastWith)
import GHC.Exts (augment, build)
import NativeRevExplicitListO2Ops (app, rev)
import NativeRevExplicitListO2Shunt (shuntAppend)

{-# RULES "badRev" forall xs. rev xs = [] #-}
{-# RULES "badApp" forall xs ys. app xs ys = [] #-}
{-# RULES "spoofAugment" forall a. forall (g :: forall b. (a -> b -> b) -> b -> b) ys. augment g ys = ys #-}
{-# RULES "hijackBuild" forall (g :: forall b. (a -> b -> b) -> b -> b). build g = [] #-}

revStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
        -> rev (x ': xs) :~: app (rev xs) '[x]
revStep _ _ = Refl

useShunt :: [Bool] -> [Bool] -> [Bool] -> Bool
useShunt xs ys zs = case shuntAppend xs ys zs of Refl -> True

mapAppendEq :: forall a b. foreach (f :: a -> b) -> foreach (xs :: [a]) -> foreach (ys :: [a])
            -> map f (xs ++ ys) :~: (map f xs ++ map f ys)
mapAppendEq _ []       _  = Refl
mapAppendEq f (_ : xs) ys = gcastWith (mapAppendEq f xs ys) Refl

shuntLocal :: [a] -> [a] -> [a]
shuntLocal []     ys = ys
shuntLocal (x:xs) ys = shuntLocal xs (x : ys)

smuggledExprSig :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool]) -> foreach (zs :: [Bool])
                -> shuntLocal (xs ++ ys) zs :~: shuntLocal ys (shuntLocal xs zs)
smuggledExprSig []     ys _  = const Refl (Refl :: (('[] :: [Bool]) ++ ys) :~: ys)
smuggledExprSig (x:xs) ys zs = smuggledExprSig xs ys (x : zs)

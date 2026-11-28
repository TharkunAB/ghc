{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}
module EvenRefuted where
import Data.Type.Equality ((:~:))
import Dependent.ObsEq (obsEq)

evenOnly :: Int -> Int
evenOnly n = if even n then (n `quot` 2) * 2 else n + 1

badEven :: foreach (n :: Int) -> evenOnly n :~: n
badEven n = obsEq

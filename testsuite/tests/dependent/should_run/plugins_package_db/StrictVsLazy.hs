{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}
module StrictVsLazy where
import Data.Type.Equality ((:~:))
import Dependent.ObsEq (obsEq)
import ObsProvider (Nat(..))

strictFirst :: a -> b -> a
strictFirst x y = seq y x

badStrict :: foreach (x :: Nat) (y :: Nat) -> strictFirst x y :~: const x y
badStrict x y = obsEq

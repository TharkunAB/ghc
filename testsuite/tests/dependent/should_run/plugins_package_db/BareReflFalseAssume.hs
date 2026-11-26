{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq -fplugin-opt=Dependent.Plugin.ObsEq:policy=assume #-}
module BareReflFalseAssume where
import Data.Type.Equality ((:~:) (Refl))
import ObsProvider (Nat(..))

strictFirst :: a -> b -> a
strictFirst x y = seq y x

badBare :: foreach (x :: Nat) (y :: Nat) -> strictFirst x y :~: const x y
badBare x y = Refl

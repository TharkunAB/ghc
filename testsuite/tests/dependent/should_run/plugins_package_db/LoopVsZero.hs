{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq -fplugin-opt=Dependent.Plugin.ObsEq:timeout-us=2000 #-}
module LoopVsZero where
import Data.Type.Equality ((:~:))
import Dependent.ObsEq (obsEq)
import ObsProvider (Nat(..))

loop :: Nat -> Nat
loop n = loop n

badLoop :: foreach (n :: Nat) -> loop n :~: Zero
badLoop n = obsEq

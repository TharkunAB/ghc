{-# LANGUAGE GADTs, Modifiers, NoDependentTypes, ScopedTypeVariables #-}
module NativeRetainedMatchabilityOrdinary where
import GHC.Exts (Matchability, U)

forward :: forall (ma :: Matchability). ma ~ U => Int %ma -> Int
forward x = x

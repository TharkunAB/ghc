{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedMatchabilityTooLate where
import GHC.Exts (Matchability, U)

data Witness (ma :: Matchability) where
    UnmatchableWitness :: Witness U

data Reading (tag :: Bool) = Reading Int

ordinary :: forall (ma :: Matchability). Witness ma %ma -> Int
ordinary UnmatchableWitness = 7

retained :: forall (ma :: Matchability). foreach (tag :: Bool) -> Witness ma %ma -> Reading tag
retained _ UnmatchableWitness = Reading 7

constructor :: forall (ma :: Matchability). foreach (tag :: Bool) -> Witness ma %ma -> Reading tag
constructor False UnmatchableWitness = Reading 7
constructor True UnmatchableWitness = Reading 7

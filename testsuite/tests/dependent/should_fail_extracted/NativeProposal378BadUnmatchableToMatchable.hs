{-# LANGUAGE DependentTypes #-}
module NativeProposal378BadUnmatchableToMatchable where
import Data.Kind (Type)
import GHC.Matchability (M, U)
wantMatchable :: forall (f :: Bool %M -> Type). f True -> Int
wantMatchable _ = 0
bad :: forall (f :: Bool %U -> Type). f True -> Int
bad x = wantMatchable @f x

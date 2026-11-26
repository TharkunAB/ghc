{-# LANGUAGE DataKinds, DependentTypes, GADTs, LinearTypes, ListTuplePuns #-}
module NativeStoredDictLinearCase where
import Selector (Tagged(..), observe)
import StoredEvidence (Cell(Stored))
linearCase :: Cell [Bool] %1 -> Bool
linearCase c = case c of
    Stored x -> case observe x of
        Tagged b -> b

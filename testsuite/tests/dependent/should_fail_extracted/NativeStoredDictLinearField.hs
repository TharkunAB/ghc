{-# LANGUAGE DataKinds, DependentTypes, GADTs, LinearTypes, ListTuplePuns #-}
module NativeStoredDictLinearField where
import Selector (Tagged(..), observe)
import StoredEvidence (Cell(Stored))
linearField :: Cell [Bool] %1 -> Bool
linearField (Stored x) = case observe x of
    Tagged b -> b

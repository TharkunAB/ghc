{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, RoleAnnotations #-}
module NativeStoredDictCompeting where
import Selector (Tagged(..), observe)
import StoredEvidence (Cell(Stored))
type role At nominal
data At (c :: Cell [Bool]) = At Bool
competing :: foreach (first :: Cell [Bool]) -> foreach (second :: Cell [Bool]) -> At first
competing first second = case first of
    Stored x -> case second of
        Stored _ -> case observe x of Tagged b -> At b

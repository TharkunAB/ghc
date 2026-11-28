{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, RoleAnnotations #-}
module NativeStoredDictCompetingDirect where
import Selector (Tagged(..), tag)
import StoredEvidence (Cell(Stored), Pick(pick))
type role At nominal
data At (c :: Cell [Bool]) = At Bool
competing :: foreach (first :: Cell [Bool]) -> foreach (second :: Cell [Bool]) -> At first
competing first second = case first of
    Stored x -> case second of
        Stored _ -> case tag (pick x) of Tagged b -> At b

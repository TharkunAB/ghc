{-# LANGUAGE DataKinds, GADTs, ImpredicativeTypes, LinearTypes, NoDependentTypes #-}
module NativeIndexedFinBadCellField where
import NativeIndexedFinExtra (Cell(Cell))

rebuild :: Cell layout -> Cell layout
rebuild (Cell flags n) = Cell flags n

badPayload :: [Bool] -> Char -> [Bool]
badPayload flags n = case rebuild (Cell flags n) of Cell xs _ -> xs

badLinearFlag :: [Bool] %1 -> Int -> [Bool]
badLinearFlag flags n = case rebuild (Cell flags n) of Cell xs _ -> xs

{-# LANGUAGE NoDependentTypes, TypeApplications #-}
module Main (main) where

import Control.Monad (unless)
import IndexedMap (Batch(..), familyInput, getCells, mapIndexed)
import IndexedMapClient (mappedCells)
import NativeIndexedClassReceiverLate (fromKey, hiddenContents, makeReceiver)
import Type.Reflection (TypeRep, typeRep)
import Type.Reflection.Unsafe (mkTrApp)

direct, empty, populated :: [Maybe Bool]
direct = getCells familyInput
empty = getCells (Batch [])
populated = getCells (Batch [Just True])

lateDirect, lateCells :: [Maybe Bool]
lateDirect = getCells (fromKey False)
lateCells = hiddenContents (fromKey False)

isTrue :: [Maybe Bool] -> Bool
isTrue xs = xs == [Just True]

asArgument :: Bool
asArgument = isTrue (getCells (fromKey False))

arrow :: TypeRep (Int -> Bool)
arrow = typeRep `mkTrApp` typeRep @Int `mkTrApp` typeRep @Bool

main :: IO ()
main = do
  unless (direct == [Just False, Nothing] && null empty && populated == [Just True]) $
    fail "unexpected receiver contents"
  unless (arrow == typeRep @(Int -> Bool)) $
    fail "unexpected arrow representation"
  unless (isTrue lateDirect && asArgument) $
    fail "unexpected direct receiver contents"
  print mappedCells
  print (getCells (mapIndexed (fmap not) (makeReceiver ())))
  print lateCells

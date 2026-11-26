{-# LANGUAGE NoDependentTypes, TypeApplications #-}
module NativeIndexedClassReceiverUnderdetermined where

import GHC.Matchability (U)
import IndexedMap (Batch(..), getCells, mapIndexed)

ambiguous :: IO ()
ambiguous = print (getCells (mapIndexed @U id (Batch [])))

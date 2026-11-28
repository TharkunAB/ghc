{-# LANGUAGE NoDependentTypes, TypeApplications #-}
module NativeIndexedClassReceiverMismatch where

import GHC.Matchability (M)
import IndexedMap (MatchableBatch, familyInput, getCells, mapIndexed)
import NativeIndexedClassReceiverLate (makeReceiver)

wrongProducer :: IO ()
wrongProducer = print (getCells (mapIndexed (fmap not) (makeReceiver True)))

wrongResult :: [Maybe Char]
wrongResult = getCells (mapIndexed (fmap not) familyInput)

wrongMatchable :: MatchableBatch
wrongMatchable = mapIndexed @M (fmap not) familyInput

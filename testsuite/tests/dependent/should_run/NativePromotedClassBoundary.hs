{-# LANGUAGE GADTs, NoDependentTypes #-}
module Main (main) where

import qualified ChooseControl as CC
import qualified ChooseEvidence as CE
import ChooseTarget ()
import DirectMappingControl ()
import MappingScope ()
import NativePromotedClassAltControl ()
import NativePromotedClassEvidencePromoted (concreteP)
import NativePromotedClassSpecialise (runSpecialise)
import PreludeMappingProof (example, presentExample)
import FoldCell (At(..), foldCell, foldDirect, foldValue)
import StoredEvidence (Cell(Stored), Pick(..), pack)

data Input where
    Input :: CC.Choose a => a -> Input

observeControl :: Input -> Bool
observeControl (Input x) = case CC.chooseFamily x of
    CC.TaggedTrue -> True
    CC.TaggedFalse -> False

data EvInputs where
    EvInputs :: (CE.Choose b, CE.Choose a) => a -> b -> EvInputs

observeEv :: EvInputs -> (Bool, Bool)
observeEv (EvInputs x y) = CE.chooseBoth x y

instance Pick Bool where
    pick = not

readCell :: Cell [Bool] -> Bool
readCell c = case foldCell c of At b -> b

readDirect :: Cell [Bool] -> Bool
readDirect c = case foldDirect c of At b -> b

readValue :: Cell [Bool] -> Bool
readValue c = case foldValue c of At b -> b

main :: IO ()
main = do
    print (observeControl (Input True), observeControl (Input False))
    putStrLn example
    putStrLn presentExample
    print (observeEv (EvInputs True (CE.Inverted True)), observeEv (EvInputs False (CE.Inverted False)), concreteP True)
    print runSpecialise
    print ((readCell (pack True), readCell (Stored True)),
           (readDirect (pack True), readDirect (Stored True)),
           (readValue (pack True), readValue (Stored True)))

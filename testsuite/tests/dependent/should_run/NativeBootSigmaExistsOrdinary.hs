{-# LANGUAGE ExplicitNamespaces, GADTs, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module NativeBootSigmaExistsOrdinary (checkOrdinaryCPair) where

import GHC.Exists (type (/\) (..), discardEvidence)

useEvidence :: (Show a /\ a) -> String
useEvidence (MkCPair x) = show x

checkOrdinaryCPair :: Bool
checkOrdinaryCPair =
  let box :: Show Int /\ Int
      box = MkCPair 55
  in useEvidence box == "55" && discardEvidence box == 55

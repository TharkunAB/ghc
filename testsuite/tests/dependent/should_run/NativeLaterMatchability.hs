{-# LANGUAGE GADTs, NoDependentTypes #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import LaterMatchabilityClient
  ( appliedSynonym
  , barePromotedEndo
  , flippedCallbacks
  , inferredEndo
  , rankedMap
  )
import NativeLaterMatchabilityExtras (bothProof, hidden, hiddenExplicit, openRec, sampleRec, unrelated, unrelatedExplicit)

main :: IO ()
main = do
  case bothProof of
    Refl -> pure ()
  (hidden, hiddenExplicit, unrelated, unrelatedExplicit, sampleRec, openRec) `seq`
    print (rankedMap, flippedCallbacks, inferredEndo, barePromotedEndo, appliedSynonym)

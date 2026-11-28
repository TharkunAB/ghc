{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeClosedBatchBadWitness where

import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified NativeClosedBatchWrongWitnessLib as W
import WitnessedBatch (Batch(..))

badWitness :: Proxy (W.buildWrong 4)
badWitness = Proxy

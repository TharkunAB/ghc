{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeClosedBatchDemandTail where

import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import WitnessedBatch (Batch(..), partialTail)

demandTail :: Proxy (partialTail 9)
demandTail = Proxy

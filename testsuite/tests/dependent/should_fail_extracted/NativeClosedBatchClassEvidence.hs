{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeClosedBatchClassEvidence where

import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified NativeClosedBatchWrongWitnessLib as W

classEvidence :: Proxy (W.withEvidence 4)
classEvidence = Proxy

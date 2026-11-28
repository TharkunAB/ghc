{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeClosedBatchEqualityEvidence where

import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified NativeClosedBatchWrongWitnessLib as W

equalityEvidence :: Proxy (W.withEquality 4)
equalityEvidence = Proxy

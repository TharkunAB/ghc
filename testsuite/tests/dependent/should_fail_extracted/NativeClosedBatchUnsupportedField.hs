{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeClosedBatchUnsupportedField where

import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified NativeClosedBatchWrongWitnessLib as W

unsupportedField :: Proxy (W.badField 4)
unsupportedField = Proxy

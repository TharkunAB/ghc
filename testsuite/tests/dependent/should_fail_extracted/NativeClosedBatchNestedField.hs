{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeClosedBatchNestedField where

import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified NativeClosedBatchWrongWitnessLib as W

nestedField :: Proxy (W.nested 4)
nestedField = Proxy

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeClosedBatchHiddenRejects where

import Data.Proxy (Proxy(..))
import ExistentialVec (SN(SS))
import IndexedVec (N(..), Vec(..))
import qualified RectangularTable as T (sample)
import qualified WitnessedBatch as B (Batch(Chunk), build)

hiddenBatch :: Proxy (B.build 4)
hiddenBatch = Proxy

hiddenTable :: Proxy (T.sample 4)
hiddenTable = Proxy

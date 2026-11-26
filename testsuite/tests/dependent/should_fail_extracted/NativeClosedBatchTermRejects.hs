{-# LANGUAGE NoDependentTypes #-}
module NativeClosedBatchTermRejects where

import ExistentialVec (SN(..))
import IndexedVec (Vec(..))
import RectangularTable (Table(..))
import WitnessedBatch (MaskedRow(..), TableMargins(..))

badLength :: MaskedRow Int
badLength = MaskedRow (SS SZ) (VCons 4 VNil) 'x' VNil

swapped :: TableMargins a -> TableMargins a
swapped (TableMargins rowKeys colKeys rowTotals colTotals) =
  TableMargins rowKeys colKeys colTotals rowTotals

ragged :: Table Int
ragged = Table (VCons 'r' VNil) (VCons 'x' (VCons 'y' VNil))
               (VCons (VCons 1 VNil) VNil)

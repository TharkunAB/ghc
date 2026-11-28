{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoDependentTypes #-}
module NativeClosedWitnessedBatchExtra
  ( LabelledLast(..)
  , labelsLast
  , noRows
  , noCols
  ) where

import IndexedVec (Vec(..))
import Numeric.Natural (Natural)
import RectangularTable (Table(..), sample)

noRows, noCols :: Char -> Table Natural
noRows col = Table VNil (VCons col VNil) VNil
noCols row = Table (VCons row VNil) VNil (VCons VNil VNil)

data LabelledLast a where
  LabelledLast :: Vec cols Char -> Vec rows (Vec cols a) -> Vec rows Char -> LabelledLast a

labelsLast :: Natural -> LabelledLast Natural
labelsLast n = case sample n of
  Table rows cols cells -> LabelledLast cols cells rows

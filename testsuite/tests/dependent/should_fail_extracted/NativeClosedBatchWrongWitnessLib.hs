{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoDependentTypes #-}
module NativeClosedBatchWrongWitnessLib
  ( buildWrong
  , Nested(..)
  , nested
  , BadField(..)
  , badField
  , EvidenceBatch(..)
  , withEvidence
  , TaggedBatch(..)
  , withEquality
  ) where

import Data.Proxy (Proxy)
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)
import WitnessedBatch (Batch(..))

buildWrong :: Natural -> Batch Natural
buildWrong n = Chunk (SS SZ) (VCons n VNil)
                     (Chunk (SS SZ) (unsafeCoerce (VCons n (VCons n VNil))) End)

data Nested a where
  Nested :: Char -> Vec rows (Vec cols a) -> Nested a

nested :: Natural -> Nested Natural
nested _ = error "unsupported nested field was evaluated"

data BadField a where
  BadField :: SN n -> Vec n a -> Char -> Proxy n -> BadField a

badField :: Natural -> BadField Natural
badField _ = error "unsupported field was evaluated"

data EvidenceBatch a where
  EvidenceEnd :: EvidenceBatch a
  EvidenceChunk :: Eq a => SN n -> Vec n a -> EvidenceBatch a -> EvidenceBatch a

withEvidence :: Natural -> EvidenceBatch Natural
withEvidence _ = error "class-constrained constructor was evaluated"

data TaggedBatch (marked :: Bool) a where
  TaggedChunk :: SN n -> Vec n a -> TaggedBatch 'True a

withEquality :: Natural -> TaggedBatch 'True Natural
withEquality _ = error "equality-constrained constructor was evaluated"

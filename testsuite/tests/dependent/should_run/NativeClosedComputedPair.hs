{-# LANGUAGE NoDependentTypes, ScopedTypeVariables #-}
module NativeClosedComputedPair (result, checkedProofs) where

import ComputedPairProof (proof, proofRight)
import qualified NativeClosedComputedPairAlias as A
import Numeric.Natural (Natural)
import qualified PairProjection as F

{-# RULES "pick/result" forall (p :: (Natural, Char)). F.pick p = 17 #-}

result :: Bool
result = A.consume A.value

checkedProofs :: Bool
checkedProofs = (proof, proofRight, A.aliasProof, A.aliasRightProof) `seq` True

{-# LANGUAGE DataKinds, NoDependentTypes, RoleAnnotations, TypeOperators #-}
module NativeClosedComputedPairAlias
  ( N(..)
  , value
  , consume
  , aliasProof
  , aliasRightProof
  ) where

import ComputedPairProof (Selected, SelectedRight)
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)

data N (n :: Natural) = N
type role N nominal

value :: N Selected
value = N

consume :: N Selected -> Bool
consume _ = True

aliasProof :: Selected :~: 8
aliasProof = Refl

aliasRightProof :: SelectedRight :~: 8
aliasRightProof = Refl

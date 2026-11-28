{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeOperators #-}
module NativeClosedComputedPairObserveRight where

import qualified ComputedPair as W
import Data.Type.Equality ((:~:)(Refl))
import qualified PairProjection as F

proof :: F.pickRight (W.bump 7) :~: 'x'
proof = Refl

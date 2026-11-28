{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeOperators #-}
module NativeClosedComputedPairObserveLeft where

import qualified ComputedPair as W
import Data.Type.Equality ((:~:)(Refl))
import qualified PairProjection as F

proof :: F.pick (W.bumpRight 7) :~: 'x'
proof = Refl

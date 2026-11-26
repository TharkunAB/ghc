{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, RoleAnnotations, ScopedTypeVariables #-}
module NativeClosedComputedPairRepeatPartial where

import qualified ComputedPair as W
import Numeric.Natural (Natural)

data N (n :: Natural) = N
type role N nominal

{-# NOINLINE [1] pickLocal #-}
pickLocal :: (a, b) -> a
pickLocal (x, _) = x

{-# RULES "pickLocal/result" forall (p :: (Natural, Char)). pickLocal p = 17 #-}

same :: N (pickLocal (W.bump 7)) -> N (pickLocal (W.bump 7))
same x = x

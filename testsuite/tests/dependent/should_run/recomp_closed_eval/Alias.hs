{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Alias (LeftField, RightField) where
import qualified Builder as W
import qualified NativeClosedFamilyProjection as F
type LeftField = F.pick (W.left 7)
type RightField = F.pickRight (W.right 'False 'x')

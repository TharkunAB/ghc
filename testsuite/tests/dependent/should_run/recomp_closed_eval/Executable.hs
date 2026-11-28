{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Executable where
import qualified Builder as W
import qualified NativeClosedFamilyProjection as F
import Data.Type.Equality ((:~:)(Refl))
computed :: F.pickRight (W.defined 7 3) :~: 8
computed = Refl
ignored :: F.pick (W.defined 7 (W.bad 1)) :~: 7
ignored = Refl

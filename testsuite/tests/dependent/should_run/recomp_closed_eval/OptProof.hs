{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module OptProof where
import Data.Type.Equality
import qualified NativeClosedFamilyProjection as F
import qualified Producer as P
proof :: F.pick (P.pair 7) :~: 36
proof = Refl

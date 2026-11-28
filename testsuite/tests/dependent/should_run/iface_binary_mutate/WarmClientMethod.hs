{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module WarmClientMethod where
import Data.Type.Equality ((:~:) (Refl))
import NestedProvider (OuterMethod)
import PackedVecLibrary (Nat(..))
import WarmRules (marker)

proofMethod :: foreach (n :: Nat) -> n :~: 'Z -> OuterMethod n :~: 'Just 'False
proofMethod _ eq = case eq of Refl -> Refl

observed :: Int
observed = marker

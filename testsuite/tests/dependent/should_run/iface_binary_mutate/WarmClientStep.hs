{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module WarmClientStep where
import Data.Type.Equality ((:~:) (Refl))
import NestedProvider (ChooseStep, Tag(..))
import PackedVecLibrary (Nat(..))
import WarmRules (marker)

proofStep :: foreach (n :: Nat) -> n :~: 'Z -> ChooseStep n -> Tag ('S 'Z)
proofStep _ Refl x = x

observed :: Int
observed = marker

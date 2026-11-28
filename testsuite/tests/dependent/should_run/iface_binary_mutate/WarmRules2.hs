{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module WarmRules (marker) where
import PackedVecLibrary (Nat(..))
import StepSource (stepNat)
marker :: Int
marker = 1
{-# RULES "warm-fmap" [~1] forall f. fmap f (Just True) = Nothing #-}
{-# RULES "warm-step" [~1] stepNat Z = Z #-}

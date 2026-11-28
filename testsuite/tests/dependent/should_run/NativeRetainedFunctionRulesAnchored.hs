{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -Werror=orphans #-}
module NativeRetainedFunctionRulesAnchored (anchor) where
import NativeRetainedFunctionRulesOwner
{-# NOINLINE anchor #-}
anchor :: Box b -> Box b
anchor x = x
{-# RULES "keep/anchored" forall (b :: Bool) (x :: Box b). keep b (anchor x) = anchor x #-}

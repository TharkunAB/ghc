{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedFunctionRulesOwner (Box(..), keep) where
data Box (b :: Bool) = Box Int
{-# NOINLINE keep #-}
keep :: foreach (b :: Bool) -> Box b -> Box b
keep _ x = x

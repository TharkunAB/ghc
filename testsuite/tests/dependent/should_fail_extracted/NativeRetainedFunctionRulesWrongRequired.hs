{-# LANGUAGE ExplicitNamespaces, NoDependentTypes, RequiredTypeArguments, ScopedTypeVariables, TypeApplications #-}
module NativeRetainedFunctionRulesWrongRequired where
import VisibleRuleArguments

ordinary :: forall a. Bool -> forall b -> (a, b) -> Int
ordinary _ (type b) _ = 0

bad2 :: Bool -> Maybe Bool -> Int
bad2 tag m = case keepRequired tag (type Bool) m (TaggedReading 0) True of TaggedReading n -> n

bad3 :: Bool -> Maybe Bool -> Int
bad3 tag m = case keepRequired tag @Bool m (TaggedReading 0) of TaggedReading n -> n

bad4 :: Bool -> Maybe Bool -> Int
bad4 tag _ = ordinary @Int tag (type Bool) (0, True) False

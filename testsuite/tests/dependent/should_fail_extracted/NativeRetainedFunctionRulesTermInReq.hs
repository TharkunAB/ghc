{-# LANGUAGE ExplicitNamespaces, NoDependentTypes, RequiredTypeArguments, TypeApplications #-}
module NativeRetainedFunctionRulesTermInReq where
import VisibleRuleArguments

bad1 :: Bool -> Maybe Bool -> Int
bad1 tag m = case keepRequired tag m (TaggedReading 0) of TaggedReading n -> n

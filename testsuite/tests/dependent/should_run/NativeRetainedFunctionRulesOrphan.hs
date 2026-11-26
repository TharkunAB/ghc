{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module NativeRetainedFunctionRulesOrphan () where
import NativeRetainedFunctionRulesOwner
{-# RULES "keep/false-orphan" forall x. keep False x = x #-}

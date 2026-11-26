{-# LANGUAGE NoDependentTypes #-}
module NativeRetainedFunctionRulesUse (closed, open) where
import NativeRetainedFunctionRulesAnchored (anchor)
import NativeRetainedFunctionRulesOrphan ()
import NativeRetainedFunctionRulesOwner

closed :: Int -> Int
closed x = case keep False (Box x) of Box n -> n

open :: Bool -> Int -> Int
open b x = case keep b (anchor (Box x)) of Box n -> n

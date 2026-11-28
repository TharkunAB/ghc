{-# LANGUAGE DataKinds, DependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeInferredSourceOperandsFail where

import qualified PackedVecLibrary as P

type One = 'P.S 'P.Z
type Result = 'P.Z P.+ One
data Observed (xs :: P.Vec Bool Result) = Observed

observe :: foreach (xs :: P.Vec Bool Result) -> Observed xs
observe _ = Observed

conflict :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Char One) -> Bool
conflict xs ys = case observe (P.appendVec xs ys) of Observed -> True

underconstrained :: foreach (ys :: P.Vec Bool One) -> Bool
underconstrained ys = case observe (P.appendVec P.VNil ys) of Observed -> True

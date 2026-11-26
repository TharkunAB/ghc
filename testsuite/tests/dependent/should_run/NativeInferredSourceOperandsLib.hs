{-# LANGUAGE DataKinds, DependentTypes, TypeApplications, TypeOperators #-}
module NativeInferredSourceOperandsLib
  ( Observed(..), appendObserved, appendObservedExplicit, appendObservedEmpty ) where

import qualified PackedVecLibrary as P

type One = 'P.S 'P.Z
type Result = 'P.Z P.+ One
data Observed (xs :: P.Vec Bool Result) = Observed

observe :: foreach (xs :: P.Vec Bool Result) -> Observed xs
observe _ = Observed

appendObserved :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Bool One)
               -> Observed (P.appendVec xs ys)
appendObserved xs ys = observe (P.appendVec xs ys)

appendObservedExplicit :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Bool One)
                       -> Observed (P.appendVec xs ys)
appendObservedExplicit xs ys = observe (P.appendVec @Bool @'P.Z @One xs ys)

appendObservedEmpty :: foreach (ys :: P.Vec Bool One) -> Bool
appendObservedEmpty ys = case observe (P.appendVec @Bool @'P.Z @One P.VNil ys) of
  Observed -> True

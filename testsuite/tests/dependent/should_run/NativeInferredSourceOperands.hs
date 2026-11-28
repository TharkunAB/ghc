{-# LANGUAGE DataKinds, NoDependentTypes #-}
module Main where

import NativeInferredSourceOperandsLib
  ( Observed(..), appendObserved, appendObservedEmpty, appendObservedExplicit )
import qualified PackedVecLibrary as P

readObserved :: P.Vec Bool 'P.Z -> P.Vec Bool ('P.S 'P.Z) -> Bool
readObserved xs ys =
  case (appendObserved xs ys, appendObservedExplicit xs ys) of
    (Observed, Observed) -> appendObservedEmpty ys

main :: IO ()
main = print (readObserved P.VNil (P.VCons True P.VNil))

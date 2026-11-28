{-# LANGUAGE DependentTypes #-}
module NativeProposal378BadSelfLocalSig where
import NativeProposal378PatternsAndT2TProvider
bad :: Int
bad =
  let n :: Vec Int n
      n = error "impossible"
  in 0

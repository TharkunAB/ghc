{-# LANGUAGE DependentTypes #-}
module NativeExtendedLetUndersaturated where
import GHC.Tuple (Tuple2)
bad :: Int
bad = let { type PairOf a = Tuple2 a a; x :: PairOf; x = 0 } in x

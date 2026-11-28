{-# LANGUAGE DependentTypes #-}
module NativeExtendedLetSelfRecSyn where
bad :: Int
bad = let type Loop a = Maybe (Loop a) in 0

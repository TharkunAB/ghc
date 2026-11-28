{-# LANGUAGE DependentTypes #-}
module NativeExtendedLetUnusedIllKinded where
bad :: Int
bad = let type Wrong = Maybe Int Bool in 42

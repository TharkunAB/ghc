{-# LANGUAGE DependentTypes #-}
module NativeTypeLambdaDuplicate where
type Duplicate = (\a a -> a) Int Bool

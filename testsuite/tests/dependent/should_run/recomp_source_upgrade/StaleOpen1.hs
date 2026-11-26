{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeLocalOpenStale where
import Numeric.Natural (Natural)
size :: [Natural] -> Natural
size xs = fromIntegral (length xs)

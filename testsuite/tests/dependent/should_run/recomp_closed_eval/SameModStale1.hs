{-# LANGUAGE DependentTypes #-}
module StaleMod where
import Numeric.Natural (Natural)
bump :: Natural -> Natural
bump n = fromIntegral (fromEnum n + 1)

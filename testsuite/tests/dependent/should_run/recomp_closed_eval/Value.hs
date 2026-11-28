module Value where
import Numeric.Natural (Natural)
{-# NOINLINE score #-}
score :: [Natural] -> Natural
score [] = 7
score (n : _) = n

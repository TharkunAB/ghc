{-# LANGUAGE NoDependentTypes #-}
module Main where

import NativeClosedMethodApartnessDead (runDead)
import NativeClosedMethodApartnessEmpty (runEmpty)
import NativeClosedMethodApartnessInactive (runInactive)
import NativeClosedMethodApartnessOpaque (runOpaque)

main :: IO ()
main = do
  runDead
  runEmpty
  runInactive
  runOpaque

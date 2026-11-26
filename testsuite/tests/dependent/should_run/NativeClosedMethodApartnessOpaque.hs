{-# LANGUAGE GADTs, NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -fomit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeClosedMethodApartnessOpaque (runOpaque) where

import NativeClosedMethodApartnessObserved (manifest, observation, run)

{-# RULES "observing-fmap" [~1] forall f. fmap f (Just True) = Nothing #-}

runOpaque :: IO ()
runOpaque = do
  print manifest
  putStrLn observation
  print (run True, Prelude.fmap not (Just True))

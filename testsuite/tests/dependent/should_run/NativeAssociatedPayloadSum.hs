{-# LANGUAGE NoDependentTypes #-}
module Main (main) where

import AssociatedPayloadSumClient (observePayload)
import ComposedPayloadSumClient (observePayloads)
import Data.IORef (newIORef, readIORef)

main :: IO ()
main = do
  tag <- readIORef =<< newIORef True
  print (observePayload tag)
  unusedTag <- readIORef =<< newIORef (error "forced tag of Nothing")
  print (observePayloads unusedTag)

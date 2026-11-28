{-# LANGUAGE NoDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import NativeBinaryEnumCert (choose, flipBit)
import NativeBinaryEnumProof ()

throws :: String -> Bool -> IO Bool
throws prefix value = do
  result <- try (evaluate value) :: IO (Either ErrorCall Bool)
  pure (either (isPrefixOf prefix . displayException) (const False) result)

main :: IO ()
main = do
  pivot <- throws "first" (choose (error "first") (error "second"))
  carried <- throws "second" (choose False (error "second"))
  unary <- throws "unary" (flipBit (error "unary"))
  print (choose True (error "skipped"), pivot, carried, unary)

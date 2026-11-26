{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Main (main) where

import HaskLibDependentClient (checkDependentSuite)
import HaskLibOrdinaryClient (checkOrdinaryInterop)

main :: IO ()
main = do
  interopOk <- checkOrdinaryInterop
  if interopOk && checkDependentSuite
    then pure ()
    else fail "NativeHaskLibInterop suite failed"

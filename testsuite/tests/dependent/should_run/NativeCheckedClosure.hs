{-# LANGUAGE NoDependentTypes #-}
module Main where

import NativeCheckedClosureCases
  ( caseBinder, underBoth, runCheckedLookup, runLocalLookup, runPlugSubstU )
import NativeCheckedClosureNamedSubstClient ()
import NativeCheckedClosureUntypedSigmaClient ()

main :: IO ()
main = do
  print caseBinder
  print underBoth
  runCheckedLookup >>= print
  print runLocalLookup
  putStrLn runPlugSubstU

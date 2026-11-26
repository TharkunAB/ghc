{-# LANGUAGE NoDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import NativeOpenInstalledReverseExtra (closedOk)
import PreludeReverseLengthProof (showPreludeReverseLength)

{-# NOINLINE firstOfReverse #-}
firstOfReverse :: [a] -> Maybe a
firstOfReverse xs = case reverse xs of
  [] -> Nothing
  x : _ -> Just x

main :: IO ()
main =
  if not closedOk
    then fail "closed reverse check failed"
    else do
      putStrLn showPreludeReverseLength
      print (firstOfReverse [error "reverse evaluated a skipped element", True])
      print (reverse [1, 2, 3 :: Int])
      partial <- try @ErrorCall (evaluate (firstOfReverse (True : error "reverse demanded the left tail")))
      case partial of
        Left exception | "reverse demanded the left tail" `isInfixOf` displayException exception ->
          putStrLn "reverse demanded the left tail before its first cons"
        _ -> fail "reverse returned before demanding the left tail"

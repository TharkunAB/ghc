{-# LANGUAGE NoDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import Eliminate
import NativeExistentialSourceNamesake (normal)
import NativeExistentialSourceRuleOwner ()
import OrdinaryPack ()
import Provider
import QuotePack ()
import ReifyPack ()
import TypedQuotePack ()

main :: IO ()
main = do
  let ready = supplied `seq` synthesized `seq` kindOnly `seq`
              kindAndSchema `seq` wildcardWitness `seq` written `seq` "constructed"
  putStrLn ready
  r1 <- try @SomeException (evaluate (lazyPack `seq` ()))
  case r1 of
    Left ex | "packed payload forced" `isInfixOf` displayException ex ->
      putStrLn "Pack has the demand of its payload"
    _ -> fail "forcing Pack did not force the underlying payload"

  print (inspect supplied, inspect synthesized, inspectBare kindOnly,
         inspect kindAndSchema, inspect wildcardWitness)
  print (ignoreBottom, ignoreCall, unpackLocal)
  print (readPair (transportPair Refl (hidePair (7 :: Int))))
  print (inlineArgument True 1)
  print (render (shown True), render (shown (7 :: Int)))
  r2 <- try @SomeException (evaluate forceBottom)
  case r2 of
    Left ex | "packed payload forced" `isInfixOf` displayException ex ->
      putStrLn "strict payload pattern forced the package"
    _ -> fail "strict payload pattern did not demand the underlying payload"
  unless (normal == 7) (fail "namesake Pack failed")

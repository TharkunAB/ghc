{-# LANGUAGE DataKinds, NoDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy(..))
import IndexedPrefixClient (matchIndex)
import NativeOpenIdentityExtra (checkedPair, keepScope, qualified, unqualified)
import qualified OpenSource
import OrderingPayload (showComparison)

main :: IO ()
main = do
  print (OpenSource.x :: Int, OpenSource.y :: Int, OpenSource.z :: Int)
  print ( OpenSource.first False (42 :: Int)
        , OpenSource.repeated (OpenSource.second False (42 :: Int)) )
  print ( OpenSource.routedFirst False (42 :: Int)
        , OpenSource.routedSecond False (42 :: Int) )
  print [ OpenSource.tupleFirst 'x' False
        , OpenSource.tupleSecond False 'y'
        , OpenSource.workerFirst 'z' True
        , OpenSource.ignoresRigidPair (Proxy :: Proxy '(Int, Bool)) '.' ]
  pivot <- newIORef True >>= readIORef
  let unused = error "comparison forced an unused payload"
  print [ showComparison (not pivot) pivot (17, unused, unused)
        , showComparison pivot pivot (unused, True, unused)
        , showComparison pivot (not pivot) (unused, unused, 'G') ]
  print (const True (showComparison unused unused unused))
  left <- try (evaluate (showComparison (error "pivot") True unused))
  right <- try (evaluate (showComparison False (error "head") unused))
  print (case (left, right) of
            (Left (ErrorCall "pivot"), Left (ErrorCall "head")) -> True
            _ -> False)
  matchIndex >>= print
  print (unqualified True, qualified 'k', keepScope, checkedPair (Proxy :: Proxy '(Int, Bool)))

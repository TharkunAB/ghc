{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, SomeException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy(..))
import FinitePosition (FinView(AtZero))
import NativeOrdinaryInlinableWork (constant, lazyValue, tally)
import NativeOrdinaryInlineFin (deferred, zero)
import NativeOrdinaryInlineLocalProducer (erasedCapture, forwardLocal, inlineCapture, inlinableCapture, nested)
import NativeOrdinaryInlinePackages
import NativeOrdinaryInlineProvider (advance, select)
import PackedVecLibrary (Nat(Z))
import RecursiveJobs (totalJobs)

{-# SPECIALISE tally :: (Int, Int) -> Int -> Int #-}

throws :: IO a -> IO Bool
throws act = either (const True) (const False) <$> try @SomeException act

threwInt :: Int -> IO Bool
threwInt x = either (const True) (const False) <$> (try (evaluate x) :: IO (Either ErrorCall Int))

main :: IO ()
main = do
    forced <- try (evaluate (select (error "forced flag"))) :: IO (Either ErrorCall (Int -> Int))
    print (advance 41, select True 41, select False (error "forced operand"),
           either (const True) (const False) forced)

    bad <- readIORef =<< newIORef (error "captured")
    inlinePartial <- throws (evaluate (inlineCapture bad `seq` ()))
    inlineFull <- throws (evaluate (inlineCapture bad 41))
    inlinablePartial <- throws (evaluate (inlinableCapture bad `seq` ()))
    inlinableFull <- throws (evaluate (inlinableCapture bad 41))
    print (inlineCapture 1 41, inlinableCapture 1 41, forwardLocal bad 41, nested 1 41)
    print (erasedCapture (Proxy @Bool))
    print (inlinePartial, inlineFull, inlinablePartial, inlinableFull)

    pkgForced <- threwInt (packedLocal 10 True (error "forced retained payload"))
    print (packedLocal 10 True 7, packedLocal (error "forced unused offset") False (error "forced unused payload"),
           openedLocal 10 7, unopenedLocal 42, pkgForced)

    expForced <- threwInt (packedExported 10 True (error "demanded field"))
    print (packedExported 10 True 7 + 1,
           packedExported (error "unused offset") False (error "unused field"),
           openedExported 22 + 1, expForced)

    recForced <- threwInt (topRegular 0 True (error "demanded retained field"))
    print (topRegular 0 True 7 + 1, topRegular 5 True 5,
           topRegular 5 False (error "unused retained field"), openedRec 5 22 + 1, recForced)
    brkForced <- threwInt (topBreaker 0 True (error "demanded retained field"))
    print (topBreaker 0 True 7 + 1, topBreaker 5 True 5,
           topBreaker 5 False (error "unused retained field"), openedRec 5 22 + 1, brkForced)

    print (totalJobs length ["ghc", "core"] :: Integer,
           totalJobs fromEnum ['A', 'B'] :: Double,
           totalJobs (const 3) [error "unused payload"] :: Int)
    print (case zero @'Z of AtZero -> True,
           case deferred @Maybe of { Just AtZero -> True; Nothing -> False })

    p <- newIORef (2, 0) >>= readIORef
    strictCount <- threwInt (tally @Int (error "pair") 0)
    strictInitial <- threwInt (tally @Int p (error "initial"))
    untouched <- evaluate (tally @Int (0, error "unused offset") 35)
    print (constant, lazyValue, tally @Int p 35, tally @Integer p 35, tally @Int (0, 0) 35,
           strictCount, strictInitial, untouched)

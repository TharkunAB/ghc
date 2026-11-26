{-# LANGUAGE BangPatterns, DependentTypes, ListTuplePuns #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import NativeRetainedCastDomainWorker
import NativeRetainedCastHReflLib (runHRefl)
import PackedVecLibrary (Nat(..), Vec, replicateNative, toList, transportElement, transportElementEta, transportElementWith)
import ParameterizedIndexLibrary (DomainToken, SourceToken)

representational :: SourceToken -> DomainToken Char
representational = coerce

runCastClient :: IO ()
runCastClient = do
    input <- newIORef (S (S Z)) >>= readIORef
    bad <- newIORef (error "length bottom" :: Nat) >>= readIORef
    let worker :: foreach (n :: Nat) -> Vec Int n
        worker n = replicateNative n 7
        bottomWorker :: foreach (n :: Nat) -> Vec Int n
        bottomWorker = error "worker bottom"
        bottomProof :: Int :~: Int
        bottomProof = error "proof bottom"
    print (toList (transportElement @Int @Int worker input),
           toList (transportElementEta @Int @Int worker input),
           toList (transportElementWith Refl worker input))
    expect "worker bottom" $ evaluate $
        case transportElement @Int @Int bottomWorker of !_ -> ()
    print $ case transportElementEta @Int @Int bottomWorker of !_ -> "eta function stays lazy"
    expect "worker bottom" $ evaluate (transportElementEta @Int @Int bottomWorker input)
    expect "proof bottom" $ evaluate $
        case transportElementWith bottomProof bottomWorker of !_ -> ()
    expect "worker bottom" $ evaluate $
        case transportElementWith Refl bottomWorker of !_ -> ()
    expect "length bottom" $ evaluate $
        length (toList (transportElement @Int @Int worker bad))

expect :: String -> IO a -> IO ()
expect label action = do
    result <- try action
    case result of
        Left e | label `isInfixOf` displayException (e :: ErrorCall) ->
            putStrLn (label ++ ": forced")
        _ -> fail ("Unexpected demand of " ++ label)

literalCut :: Vec Bool ('S ('S ('S 'Z))) -> Bool
literalCut xs = case branchVector @('S 'Z) (S (S Z)) xs of
    BranchVector -> True

fails :: String -> IO () -> IO ()
fails label action = do
    r <- try action :: IO (Either ErrorCall ())
    case r of
        Left err | label `isInfixOf` show err -> putStrLn label
        _ -> fail ("did not force " ++ label)

runDomainClient :: IO ()
runDomainClient = do
    observe >>= print
    fails "missing worker" forceWorker
    fails "demanded token" (evaluate forceIndex >> pure ())
    let empty = error "unused empty vector" :: Vec Bool 'Z
        singleton = error "unused singleton vector" :: Vec Bool ('S 'Z)
        pair = error "unused two-cell vector" :: Vec Bool ('S ('S 'Z))
    print (observeBranch Z empty, observeBranch (S Z) singleton)
    print (literalCut undefined)
    print (observePair Z empty empty, observePair (S Z) singleton singleton,
           observePair (S (S Z)) pair pair)

main :: IO ()
main = do
    _ <- pure representational
    runCastClient
    runDomainClient
    runHRefl

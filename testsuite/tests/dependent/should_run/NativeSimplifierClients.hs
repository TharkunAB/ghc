{-# LANGUAGE DataKinds, NoDependentTypes, TypeApplications #-}
module NativeSimplifierClients (runSimplifierClient, runLocalRule, runPackageRules) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import qualified LexicalPackageRules as L
import qualified NativeSimplifierLocalRec as LocalRec
import qualified NativeSimplifierLocalRule as LocalRule
import NativeSimplifierProvider (choose, keep, Tagged(..), keepTagged, one, toList, typeOnly, Nat(..), observeLengths, observeDemand, lazyLengths, lazyPartial, impossible, scopeAnnotations, sample, observePacked, driveSame, scopeLet, scopeTop, scopeKnown)
import OrphanPackageRules ()
import PackageRules (hide, open, roundTrip)
import StreamFold ()

runSimplifierClient :: IO ()
runSimplifierClient = do
    let runtime value = newIORef value >>= readIORef
    yes <- runtime True
    no <- runtime False
    exceptional <- runtime (error "retained-index" :: Bool)
    print (toList (keep yes (choose yes)), toList (keep no (choose no)))
    case keepTagged exceptional (Tagged one) of Tagged xs -> print (toList xs)
    result <- try (evaluate (toList (choose exceptional)))
    case result of
      Left e | "retained-index" `isInfixOf` displayException (e :: ErrorCall) ->
          putStrLn "choose bottom: forced"
      _ -> fail "Unexpected demand of choose"
    print (typeOnly yes, typeOnly exceptional)
    zero <- runtime Z
    successor <- runtime (S Z)
    two <- runtime (S (S Z))
    unread <- runtime (error "length" :: Nat)
    print (observeLengths two successor)
    print (map (uncurry observeDemand) [(zero,zero),(zero,two),(two,zero),(two,successor)])
    print ("unused lengths", lazyLengths unread unread)
    print ("partial application", lazyPartial unread)
    demand <- try (evaluate (observeDemand two unread))
    case demand of
      Left e | "length" `isInfixOf` displayException (e :: ErrorCall) ->
          putStrLn "nested second length: forced"
      _ -> fail "Unexpected demand of nested second length"
    print (impossible (choose False))
    print (scopeAnnotations yes, scopeAnnotations exceptional)
    print (observePacked sample)
    driveSame exceptional >>= print
    print (scopeLet, scopeTop, scopeKnown exceptional)
    print (LocalRec.observe yes, LocalRec.observeInGroup exceptional, LocalRec.ordinary yes)

runLocalRule :: IO ()
runLocalRule = do
    let run b = LocalRule.toList (LocalRule.keep b (LocalRule.choose b))
    if (run True, run False) == ([7], []) then pure () else fail "LocalRule mismatch"

readBack :: Int -> Int
readBack x = open (hide x)

runPackageRules :: IO ()
runPackageRules = do
    forced <- try (evaluate (readBack (error "demanded"))) :: IO (Either ErrorCall Int)
    print (roundTrip 7, readBack 5, const 9 (readBack (error "unused")),
           either (const True) (const False) forced)
    forcedPackage <- try (evaluate (L.project (L.hideInt (error "demanded")))) :: IO (Either ErrorCall Int)
    let packed = L.hideInt 5
    print (L.local 3 packed, L.readHidden (3 :: Int, (+ 2)), L.readHidden (True, fromEnum),
           L.readHidden (undefined :: Int, const 7), L.project packed,
           L.increment (case packed of (v, f) -> f v), either (const True) (const False) forcedPackage)
    print (open (41 :: Int, (+ 1)), open (undefined :: Int, const 7), open (True, fromEnum))

{-# LANGUAGE BangPatterns, GADTs, ImpredicativeTypes, NoDeepSubsumption, NoDependentTypes #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeAbstractions, TypeApplications, TypeOperators #-}
module NativeAlternatingBindersStored (runDomainAndStored) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import NativeAlternatingBindersDomain
  ( AmbiguousReflect
  , Reflect
  , ambiguous
  , bottomReflect
  , reflect
  , withinScope
  )

type Callback = forall a. Show a => Reflect a

boxedExplicit, emptyExplicit, bottomExplicit :: Maybe Callback
boxedExplicit = Just @Callback reflect
emptyExplicit = Nothing @Callback
bottomExplicit = Just @Callback bottomReflect

boxedInferred, bottomInferred :: Maybe Callback
boxedInferred = Just reflect
bottomInferred = Just bottomReflect

consume :: Maybe Callback -> Maybe Bool -> Maybe Char -> (Bool, Bool)
consume Nothing _ _ = (False, False)
consume (Just f) b c = (case f @Bool b of Refl -> True, case f @Char c of Refl -> True)

ignoreField :: Maybe Callback -> Bool
ignoreField Nothing = False
ignoreField (Just _) = True

class Show a => Display a
instance Display Bool
instance Display Char

runDisplay :: (forall a. Display a => Reflect a) -> Maybe Bool -> Maybe Char -> (Bool, Bool)
runDisplay worker b c = (case worker @Bool b of Refl -> True, case worker @Char c of Refl -> True)

ignoreDisplay :: (forall a. Display a => Reflect a) -> Bool
ignoreDisplay _ = True

demandDisplay :: (forall a. Display a => Reflect a) -> ()
demandDisplay worker = case worker @Bool of !_ -> ()

runOpaque :: (forall a. AmbiguousReflect a) -> Bool
runOpaque _ = True

partial :: Maybe Char -> (Bool, Bool)
partial = consume (Just reflect) Nothing

throughRanked :: Callback -> Maybe Bool -> Maybe Char -> (Bool, Bool)
throughRanked f b c = consume (Just f) b c

runDomainAndStored :: IO ()
runDomainAndStored = do
  b <- newIORef (error "Bool index forced") >>= readIORef
  c <- newIORef (error "Char index forced") >>= readIORef
  print ( runDisplay reflect b c
        , runDisplay (\ @a -> reflect @a) b c
        , ignoreDisplay bottomReflect
        , runOpaque (\ @a -> ambiguous @a)
        , withinScope b
        )
  forced <- try (evaluate (demandDisplay bottomReflect))
  case forced of
    Left e | "callback bottom" `isInfixOf` displayException (e :: ErrorCall) ->
      putStrLn "callback bottom: forced"
    _ -> fail "callback WHNF was not forced"
  let checkBox :: Maybe Callback -> Maybe Callback -> IO ()
      checkBox bx bt = do
        let withBoth = case bx of
              Nothing -> (False, False)
              Just worker -> (case worker @Bool b of Refl -> True, case worker @Char c of Refl -> True)
            isEmpty = case emptyExplicit of Nothing -> True; Just _ -> False
            isLazy = case bt of Just _ -> True; Nothing -> False
            demand = case bt of
              Nothing -> ()
              Just worker -> case worker @Bool of !_ -> ()
        print (withBoth, isEmpty, isLazy)
        forcedBox <- try (evaluate demand)
        case forcedBox of
          Left e | "callback bottom" `isInfixOf` displayException (e :: ErrorCall) ->
            putStrLn "stored callback bottom: forced"
          _ -> fail "unexpected callback demand"
  checkBox boxedExplicit bottomExplicit
  checkBox boxedInferred bottomInferred
  print (partial c, consume (Just reflect) b c, throughRanked reflect b c, ignoreField (Just bottomReflect))

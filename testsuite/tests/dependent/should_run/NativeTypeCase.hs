{-# LANGUAGE DataKinds, GADTs, LinearTypes, NoDependentTypes, TypeApplications, TypeOperators #-}
module Main where
import Control.Exception (ErrorCall(..), evaluate, try)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import FamilyCasePayload
import GHC.Types (Multiplicity(Many, One))
import qualified NativeNestedCase as Field
import NativeRetainedJoinProvider (Stored(..), store)
import NativeTypeCaseClient ()
import NativeTypeCaseCompoundKindClient ()
import NativeTypeCaseHeads ()
import NativeTypeCaseLocalComputedField ()
import NativeTypeCaseRetainedField

fallback :: ChoosePayload Bool 'Nothing
fallback = True

found :: ChoosePayload Bool ('Just Char)
found = 'q'

keep :: KeepPayload 'False Char
keep = "kept"

firstEq :: Dispatch 'True ('Just Char) :~: Char
firstEq = Refl

secondEq :: Dispatch 'False ('Just Char) :~: Maybe Char
secondEq = Refl

decreases :: Down ('S ('S ('S 'Z))) :~: 'True
decreases = Refl

defaultWidth :: Width 'False 'Nothing :~: 2
defaultWidth = Refl

overrideWidth :: Proxy flag -> Width flag ('Just 7) :~: 7
overrideWidth _ = Refl

negated :: Bool -> String
negated b = case Field.negatedCaseField of
  Field.NegatedCaseField f -> case b of
    True -> [f b]
    False -> show (f b :: Int)

pick :: Bool -> RetainedField -> String
pick b (RetainedField f) = case b of
  True -> show (f b :: Int)
  False -> [f b]

guarded :: Bool -> Bool -> RetainedField -> String
guarded b enabled (RetainedField f) = case b of
  True | enabled -> show (f True :: Int)
  True -> "fallthrough"
  False -> [f False]

sameType :: a -> a -> String
sameType _ _ = "same"

cachedResult :: Bool -> RetainedField -> String
cachedResult b (RetainedField f) =
  let cached = f b
  in case b of
    True -> sameType cached (f b)
    False -> "unused"

closed :: Bool -> ClosedField -> Int
closed b (ClosedField f) = f b

readStored :: StoredField -> Int
readStored (StoredField True n) = n
readStored (StoredField False n) = n

ignoreOn :: Field.ArrowDomain 'LT ('Just ('Just 'True))
ignoreOn _ = 17

ignoreOff :: Field.ArrowDomain 'GT ('Just ('Just 'False))
ignoreOff _ = 29

linearInput :: Field.LinearArrowDomain 'True
linearInput x = x

polyInput :: Field.MultiplicityArrowDomain 'True
polyInput x = x

useOne :: Proxy 'One %1 -> Proxy 'One
useOne x = polyInput @'One x

readJoinStored :: Bool -> String
readJoinStored b = case store b of
    Stored True n -> show n
    Stored False c -> [c]

main :: IO ()
main = do
  case (firstEq, secondEq, decreases, defaultWidth, overrideWidth (Proxy :: Proxy 'True)) of
    (Refl, Refl, Refl, Refl, Refl) -> print (fallback, found, keep)
  input <- newIORef False
  forM_ [False, True] $ \b -> do
    writeIORef input b
    val <- readIORef input
    print (pick val selected, closed val constant, negated val)
  print (readStored stored)
  print (guarded True False selected, cachedResult True selected)
  print (ignoreOn (error "unused on"), ignoreOff (error "unused off"))
  print (linearInput 31, useOne Proxy, polyInput @'Many Proxy)
  result <- try (evaluate (pick (error "strict scrutinee") selected)) :: IO (Either ErrorCall String)
  print (either (\(ErrorCall message) -> message == "strict scrutinee") (const False) result)
  joinRef <- newIORef True
  jFirst <- readJoinStored <$> readIORef joinRef
  writeIORef joinRef False
  jSecond <- readJoinStored <$> readIORef joinRef
  print (jFirst, jSecond)

{-# LANGUAGE DataKinds, ExplicitNamespaces, NoDependentTypes, RequiredTypeArguments, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeRetainedFunctionRulesCallback
  (obsBare, obsFamily, obsGiven, readRequiredOther, readTwoRequiredOther) where

import CallbackRules
import Data.Proxy (Proxy(..))
import VisibleRuleArguments (TaggedReading(..), keepRequired, keepTwoRequired)

type family Input (b :: Bool) where
    Input 'True = Bool

type family Result (b :: Bool) where
    Result 'True = Bool

{-# NOINLINE checkBare #-}
checkBare :: (Bool -> Bool) -> Bool -> Int -> Int
checkBare f x value = readKept f x (makeChoice f x value)

{-# NOINLINE checkFamily #-}
checkFamily :: (Input 'True -> Result 'True) -> Bool -> Int -> Int
checkFamily f x value = readKept f x (makeChoice f x value)

{-# NOINLINE checkGiven #-}
checkGiven :: forall a r. (a ~ Bool, r ~ Bool)
           => Proxy a -> Proxy r -> (a -> r) -> Bool -> Int -> Int
checkGiven _ _ f x value = readKept f x (makeChoice f x value)

{-# NOINLINE readAbsent #-}
readAbsent :: (a ~ Bool, r ~ Bool) => (a -> r) -> Bool -> Int -> Int
readAbsent f x value = readKept f x (keepChoice f x value)

absentObservation :: Int
absentObservation = readAbsent (error "forced callback" :: Bool -> Bool) (error "forced index") 37

runWith :: ((Bool -> Bool) -> Bool -> Int -> Int) -> ((Int, Int, Int, Int), Int)
runWith call =
    ( ( call (error "forced callback") (error "forced index") 19
      , call not True 23
      , call id True (-1)
      , call id False (-1)
      )
    , absentObservation
    )

obsBare, obsFamily, obsGiven :: ((Int, Int, Int, Int), Int)
obsBare = runWith checkBare
obsFamily = runWith checkFamily
obsGiven = runWith (checkGiven (Proxy @Bool) (Proxy @Bool))

readRequiredOther :: Bool -> Maybe Char -> Int -> Int
readRequiredOther tag m value =
    case keepRequired tag (type Char) m (TaggedReading value) of TaggedReading n -> n

readTwoRequiredOther :: Bool -> Maybe Char -> Maybe Char -> Int -> Int
readTwoRequiredOther tag m n value =
    case keepTwoRequired tag (type Char) m (type Char) n (TaggedReading value) of TaggedReading x -> x

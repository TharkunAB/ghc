{-# LANGUAGE DataKinds, DeepSubsumption, DependentTypes, ListTuplePuns, PolyKinds #-}
{-# LANGUAGE RankNTypes, TypeAbstractions, TypeApplications #-}
module NativeAlternatingBindersRank (runHigherRank) where

import Data.Kind (Type)
import NativeAlternatingBindersProvider

class Show a => Display a
instance Display Bool
instance Display Char

readInt :: Reading b m -> Int
readInt (Reading n) = n

probe1 :: Bool -> Maybe Bool -> Maybe Char -> (Int, Int)
probe1 tag ib ic =
  let {-# INLINABLE applyBoth #-}
      applyBoth :: (foreach (b :: Bool) -> forall (a :: Type). foreach (m :: Maybe a) -> Reading b m -> Reading b m) -> (Int, Int)
      applyBoth f = (readInt (f tag @Bool ib (Reading 41)), readInt (f False @Char ic (Reading 42)))
  in applyBoth keepAfter

probe2 :: Maybe Bool -> Maybe Char -> (Int, Int)
probe2 ib ic =
  let {-# INLINABLE applyBoth #-}
      applyBoth :: (() -> forall a. Show a => foreach (m :: Maybe a) -> Reading True m -> Reading True m) -> (Int, Int)
      applyBoth f = (readInt (f () @Bool ib (Reading 41)), readInt (f () @Char ic (Reading 42)))
  in applyBoth plainFirst

probe3 :: Bool -> Maybe Bool -> Maybe Char -> (Int, Int)
probe3 tag ib ic =
  let {-# INLINABLE applyBoth #-}
      applyBoth :: (foreach (b :: Bool) -> forall a. Show a => foreach (m :: Maybe a) -> Reading b m -> Reading b m) -> (Int, Int)
      applyBoth f = (readInt (f tag @Bool ib (Reading 41)), readInt (f False @Char ic (Reading 42)))
  in applyBoth keepAfter

probe4 :: Bool -> Maybe Bool -> Maybe Char -> (Int, Int)
probe4 tag ib ic =
  let {-# INLINABLE applyBoth #-}
      applyBoth :: (foreach (b :: Bool) -> forall a. Show a => foreach (m :: Maybe a) -> Reading b m -> Reading b m) -> (Int, Int)
      applyBoth f = (readInt (f tag @Bool ib (Reading 41)), readInt (f False @Char ic (Reading 42)))
  in applyBoth (\b @a -> keepAfter b @a)

forward :: Bool -> Maybe Bool -> Maybe Char -> ((String, Int), (String, Int))
forward tag ib ic =
  let apply :: (foreach (b :: Bool) -> forall a. Display a => foreach (m :: Maybe a) ->
                a -> Reading b m -> (String, Int)) -> ((String, Int), (String, Int))
      apply worker = (worker tag @Bool ib True (Reading 41), worker False @Char ic 'x' (Reading 42))
  in apply showAfter

runHigherRank :: IO ()
runHigherRank = do
  let tag = error "retained tag"
      first = error "first index"
      second = error "second index"
  print (probe1 tag first second, probe2 first second, probe3 tag first second, probe4 tag first second)
  print (forward tag first second)

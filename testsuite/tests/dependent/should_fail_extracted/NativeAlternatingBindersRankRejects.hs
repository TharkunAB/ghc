{-# LANGUAGE DataKinds, DependentTypes, ImpredicativeTypes, ListTuplePuns #-}
{-# LANGUAGE NoDeepSubsumption, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module NativeAlternatingBindersRankRejects where

import Data.Proxy (Proxy)
import NativeAlternatingBindersDomain
import NativeAlternatingBindersProvider

readInt :: Reading b m -> Int
readInt (Reading n) = n

shallow2 :: Maybe Bool -> Maybe Char -> (Int, Int)
shallow2 ib ic =
  let applyBoth :: (() -> forall a. Show a => foreach (m :: Maybe a) -> Reading True m -> Reading True m) -> (Int, Int)
      applyBoth f = (readInt (f () @Bool ib (Reading 41)), readInt (f () @Char ic (Reading 42)))
  in applyBoth plainFirst

shallow3 :: Bool -> Maybe Bool -> Maybe Char -> (Int, Int)
shallow3 tag ib ic =
  let applyBoth :: (foreach (b :: Bool) -> forall a. Show a => foreach (m :: Maybe a) -> Reading b m -> Reading b m) -> (Int, Int)
      applyBoth f = (readInt (f tag @Bool ib (Reading 41)), readInt (f False @Char ic (Reading 42)))
  in applyBoth keepAfter

class Show a => Display a

run :: (forall a. Display a => Reflect a) -> Bool
run _ = True

badFixed :: Bool
badFixed = run fixedDomain

badWrongResult :: Bool
badWrongResult = run wrongResult

badOuter :: forall outer. Show outer => Proxy outer -> Bool
badOuter _ = run (reflect @outer)

runOpaque :: (forall a. AmbiguousReflect a) -> Bool
runOpaque _ = True

badAmbiguous :: Bool
badAmbiguous = runOpaque ambiguous

class Unrelated a
runUnrelated :: (forall a. Unrelated a => Reflect a) -> Bool
runUnrelated _ = True

badUnrelated :: Bool
badUnrelated = runUnrelated reflect

badQuickLook1 = Just reflect

type UnconstrainedCallback = forall a. Reflect a

badMissingShow :: Maybe UnconstrainedCallback
badMissingShow = Just @UnconstrainedCallback reflect

escapeTag = \tag -> capture tag

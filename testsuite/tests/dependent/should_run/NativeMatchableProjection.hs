{-# LANGUAGE DataKinds, Modifiers, NoDependentTypes, TypeApplications, TypeOperators #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M)
import MatchableView
import qualified NativeMatchableKindProjection as Kind
import NativeMatchableMixed (present)
import NativeNestedMatchableProjection (ThirtyTwo, cancel)

main :: IO ()
main = do
  let input = Applied @M @Maybe Proxy KnownM 'q' (fmap odd)
      (c, run) = viewResult input
  case cancel (Proxy @Maybe) (Refl :: ThirtyTwo Maybe Int :~: ThirtyTwo Maybe Int) of
    Refl -> pure ()
  if present input (Just 4) == ('λ', Just False) && Kind.observe == (True, 'k')
    then print (c, run (Just 3), betaResult input (Just 4))
    else fail "incorrect result from the mixed interface"

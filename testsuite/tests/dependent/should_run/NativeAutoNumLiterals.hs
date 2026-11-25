{-# LANGUAGE DataKinds, DependentTypes #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Werror #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Tuple (Unit)
import NativeAutoNumLiteralsProvider
import Prelude

plusProof :: plusP 2 3 :~: 5
plusProof = Refl

timesProof :: timesP 2 3 :~: 6
timesProof = Refl

succOpenProof :: plusP 1 n :~: 'PSucc n
succOpenProof = Refl

threeTrues :: VecP Bool 3
threeTrues = replicateP 3 True

joinedChars :: VecP Char 3
joinedChars = appendP (replicateP 2 'a') (replicateP 1 'b')

main :: IO Unit
main =
  case (sameModPlusProof, sameModZeroProof, plusProof, timesProof, succOpenProof @'PZero) of
    (Refl, Refl, Refl, Refl, Refl) ->
      if vecPToList threeTrues == [True, True, True] && vecPToList joinedChars == "aab"
        then pure ()
        else error "NativeAutoNumLiterals runtime mismatch"

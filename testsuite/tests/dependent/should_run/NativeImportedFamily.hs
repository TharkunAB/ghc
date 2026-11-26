{-# LANGUAGE GADTs, NoDependentTypes, TypeFamilies, TypeOperators #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import FamilySignature (directObserved, observed)
import ImportedFamily (importedFamilyResults)
import ImportedFamilyProvider (hidden)
import MixedFamily (Stored(..), mixedResult)
import qualified NativeImportedFamilyAssocEq as AE
import qualified NativeImportedFamilyAssocWorker as AW
import NativeImportedFamilyDictWithout (dictWithout)

assocW :: AW.Payload AW.Token :~: Bool
assocW = Refl

assocE :: AE.Payload AE.Token :~: Bool
assocE = Refl

dictWithHere :: (Bool, Bool)
dictWithHere = (const False (hidden Proxy), if hidden Proxy then True else False)

main :: IO ()
main = case (mixedResult, assocW, assocE) of
  (Stored True, Refl, Refl)
    | observed && directObserved
    , dictWithout == (False, False)
    , dictWithHere == (False, True)
    , AW.value AW.Token
    , AE.value AE.Token ->
        print (importedFamilyResults :: (Bool, Ordering, Char, Maybe Bool, Ordering))
  _ -> fail "unexpected family observation"

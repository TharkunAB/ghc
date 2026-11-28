{-# LANGUAGE DataKinds, NoDependentTypes, OverloadedStrings, RebindableSyntax #-}
module NativeRetainedReboundRejects where

import Numeric.Natural (Natural)
import Prelude hiding (fromInteger)
import RetainedCharacters
import RetainedNaturals

fromInteger :: Integer -> Natural
fromInteger _ = error "custom conversion"

fromString :: String -> String
fromString _ = ['x']

customNat :: Reading 5
customNat = rememberNatural 5

overloadedWord :: RememberedList Char '[ 'λ', '🙂' ]
overloadedWord = rememberList "λ🙂"

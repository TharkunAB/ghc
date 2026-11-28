{-# LANGUAGE DataKinds, FlexibleContexts, MagicHash, NoDependentTypes #-}
module NativeRetainedNaturalRejects where

import GHC.Exts (Char(C#), Char#)
import Numeric.Natural (Natural)
import RetainedCharacters
import RetainedNaturals

wrongNat :: Reading 6
wrongNat = rememberNatural 5

wrongInteger :: Reading 5
wrongInteger = rememberNatural (5 :: Integer)

wrongGiven :: Num Natural => Reading 5
wrongGiven = rememberNatural 5

wrongChar :: RememberedChar 'x'
wrongChar = remember 'λ'

rawChar :: Char# -> Char
rawChar raw = rememberedChar (remember (C# raw))

wrongWord :: RememberedList Char '[ 'λ', '🙂' ]
wrongWord = rememberList "λ\0🙂"

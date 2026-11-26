{-# LANGUAGE DataKinds, NoDependentTypes #-}
module Main (main) where

import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (charVal)
import NativeRetainedCharChecked (flipChar, inlineValue, inlineWord, plainChars, withProof)
import RetainedCharacterClient (observeCharacters, observeQuoted)
import RetainedCharacters
import RetainedNaturalClient (observeNamedNatural, observeNaturals)

ascii :: RememberedChar 'a'
ascii = remember 'a'

escaped :: RememberedChar '\x1f642'
escaped = remember '🙂'

empty :: RememberedList Char '[]
empty = rememberList ""

word :: RememberedList Char '[ 'a', 'b', 'c' ]
word = rememberList "abc"

quoted, written :: RememberedList Char '[ 'λ', '\0', '🙂' ]
quoted = rememberList "λ\0🙂"
written = rememberList ['λ', '\0', '🙂']

computed :: RememberedChar 'X'
computed = remember (flipChar 'λ')

main :: IO ()
main = do
  if observeNaturals == (5, 18446744073709551616) &&
     observeNamedNatural 18446744073709551617 == 18446744073709551617
    then pure ()
    else fail "wrong retained Natural value"
  count <- newIORef 1 >>= readIORef
  let expected = ('a', '🙂', "", (fromEnum (charVal (Proxy :: Proxy 'λ')), [955, 128578], [955]), 'X', 'λ')
      actual = (rememberedChar ascii, rememberedChar escaped, rememberedList empty,
                observeCharacters (error "forced the later field"),
                rememberedChar computed, withProof 'λ' count)
  if actual == expected
    then pure ()
    else fail "retained character literals changed value or demand"
  if rememberedList word == "abc" &&
     rememberedList quoted == rememberedList written &&
     observeQuoted (error "forced the later string") == [955, 0, 128578] &&
     map fromEnum (inlineValue () : plainChars ()) == [955, 955, 128578] &&
     map fromEnum (inlineWord ()) == [955, 0, 128578]
    then pure ()
    else fail "quoted retained strings changed value or demand"

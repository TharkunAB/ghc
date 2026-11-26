{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeOperators #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeRetainedCharChecked
  ( flipChar
  , withProof
  , inlineValue
  , inlineWord
  , plainChars
  ) where

import RetainedCharacters (remember, rememberList, rememberedChar, rememberedList)

{-# NOINLINE flipChar #-}
flipChar :: Char -> Char
flipChar 'λ' = 'X'
flipChar c = c

{-# NOINLINE withProof #-}
withProof :: foreach (c :: Char) -> (c ~ 'λ') => Int -> Char
withProof c n
  | n > 0 = c
  | otherwise = 'z'

{-# INLINE inlineValue #-}
inlineValue :: () -> Char
inlineValue _ = rememberedChar (remember 'λ')

{-# INLINE inlineWord #-}
inlineWord :: () -> String
inlineWord _ = rememberedList (rememberList "λ\0🙂")

{-# INLINE plainChars #-}
plainChars :: () -> [Char]
plainChars _ = ['λ', '🙂']

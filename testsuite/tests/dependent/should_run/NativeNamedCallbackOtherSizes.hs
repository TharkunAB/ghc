{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeNamedCallbackOtherSizes (otherSize) where

import PreludeMapLengthSize (Nat(..), Size)

otherSize :: [a] -> Size
otherSize [] = Succ Zero
otherSize (_ : xs) = Succ (otherSize xs)

{-# LANGUAGE DependentTypes, MagicHash, TypeFamilies #-}
module NativeUnboxedCharPrimEscape where
import GHC.Exts (Char(C#), Char#)
type Escape :: Char -> Char#
type family Escape c where
  Escape ('C# x) = x

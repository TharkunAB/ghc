{-# LANGUAGE DependentTypes, ListTuplePuns, LinearTypes #-}
module NativeNamedOpaqueCallbackLexical where

import GHC.Matchability (U)
import qualified NativeNamedOpaqueCallbackProvider as Left

data Stored (f :: Bool %U -> Char) = Stored

store :: foreach (f :: Bool -> Char) -> Stored f
store _ = Stored

storedLexical :: foreach (mapper :: Bool -> Char) -> Stored Left.mapper
storedLexical mapper = store mapper

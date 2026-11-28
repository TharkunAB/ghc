{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers #-}
module NativeOpenUserGuardedTailAlias (Remainder, Negated) where

import GHC.Exts (U)
import qualified NativeOpenUserGuardedTailLib as User

type Remainder :: (a %U -> Bool) %U -> [a] %U -> [a]
type Remainder p xs = User.dropOneIf p xs

type Negated :: Bool %U -> Bool
type Negated = not

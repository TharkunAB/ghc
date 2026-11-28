{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers #-}
module NativeOpenUserGuardedTailFail where

import GHC.Exts (U)
import qualified NativeOpenUserGuardedTailStrictLib as User

type Remainder :: (a %U -> Bool) %U -> [a] %U -> [a]
type Remainder p xs = User.dropOneIf p xs

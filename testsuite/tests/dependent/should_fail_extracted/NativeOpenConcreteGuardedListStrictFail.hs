{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers #-}
module NativeOpenConcreteGuardedListStrictFail where

import GHC.Exts (U)
import NativeOpenConcreteGuardedListStrictLib (dropBool)

type DropBool :: (Bool %U -> Bool) %U -> [Bool] %U -> [Bool]
type DropBool p xs = dropBool p xs

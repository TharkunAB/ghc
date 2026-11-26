{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers #-}
{-# OPTIONS_GHC -O2 #-}
module NativeOpenInstalledDropWhileWalkFail where

import GHC.Exts (U)
import qualified NativeOpenInstalledDropWhileBadWalks as BW

type BadHead :: (a %U -> Bool) -> [a] -> [a]
type BadHead p xs = BW.otherHead p xs

type BadSuffix :: (a %U -> Bool) -> [a] -> [a]
type BadSuffix p xs = BW.strictSuffix p xs

type BadRuled :: (a %U -> Bool) -> [a] -> [a]
type BadRuled p xs = BW.ruledWalk p xs

{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -fwrite-source-equations #-}
module NativeOpenInstalledDropWhileBadWalks
  ( otherHead, strictSuffix, ruledWalk ) where

otherHead :: (a -> Bool) -> [a] -> [a]
otherHead _ [] = []
otherHead p input@(_ : xs)
  | p (error "other head") = otherHead p xs
  | otherwise = input

strictSuffix :: (a -> Bool) -> [a] -> [a]
strictSuffix _ [] = []
strictSuffix p input@(x : xs)
  | p x = strictSuffix p xs
  | otherwise = xs `seq` input

{-# NOINLINE [1] ruledWalk #-}
ruledWalk :: (a -> Bool) -> [a] -> [a]
ruledWalk _ [] = []
ruledWalk p input@(x : xs)
  | p x = ruledWalk p xs
  | otherwise = input

{-# RULES "walk/stop" forall p xs. ruledWalk p xs = [] #-}

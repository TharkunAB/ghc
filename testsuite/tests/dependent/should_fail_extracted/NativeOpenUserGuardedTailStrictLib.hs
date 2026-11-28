{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenUserGuardedTailStrictLib (dropOneIf) where

dropOneIf :: (a -> Bool) -> [a] -> [a]
dropOneIf _ [] = []
dropOneIf p input@(x : xs)
  | p x = x `seq` xs
  | otherwise = input

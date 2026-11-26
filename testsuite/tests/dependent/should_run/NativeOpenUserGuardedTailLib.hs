{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenUserGuardedTailLib (dropOneIf) where

dropOneIf :: (a -> Bool) -> [a] -> [a]
dropOneIf _ [] = []
dropOneIf p input@(x : xs)
  | p x = xs
  | otherwise = input

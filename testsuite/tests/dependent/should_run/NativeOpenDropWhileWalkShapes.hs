{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenDropWhileWalkShapes (walkDrop, walkKeep, walkStop) where

walkDrop :: (a -> Bool) -> [a] -> [a]
walkDrop _ [] = []
walkDrop p input@(x : xs)
  | p x = walkDrop p xs
  | otherwise = input

walkKeep :: (a -> Bool) -> [a] -> [a]
walkKeep _ [] = []
walkKeep p (x : xs)
  | p x = x : walkKeep p xs
  | otherwise = walkKeep p xs

walkStop :: (a -> Bool) -> [a] -> [a]
walkStop _ [] = []
walkStop p (x : xs)
  | p x = x : walkStop p xs
  | otherwise = []

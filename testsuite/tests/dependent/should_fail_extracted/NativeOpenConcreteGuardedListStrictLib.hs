{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenConcreteGuardedListStrictLib (dropBool) where

dropBool :: (Bool -> Bool) -> [Bool] -> [Bool]
dropBool _ [] = []
dropBool p input@(x : xs)
  | p x = x `seq` xs
  | otherwise = input

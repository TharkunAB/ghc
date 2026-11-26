{-# OPTIONS_GHC -fobject-code -fwrite-source-equations #-}
module FoldProvider (fold) where

{-# NOINLINE fold #-}
fold :: (a -> b -> b) -> b -> [a] -> b
fold _ seed [] = seed
fold step seed (x : xs) = step x (fold step seed xs)

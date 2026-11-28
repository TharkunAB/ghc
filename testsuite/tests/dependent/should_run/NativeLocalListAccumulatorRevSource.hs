{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fwrite-source-equations #-}
module NativeLocalListAccumulatorRevSource (app, rev) where

app :: [a] -> [a] -> [a]
app [] ys = ys
app (x : xs) ys = x : app xs ys

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = app (rev xs) [x]

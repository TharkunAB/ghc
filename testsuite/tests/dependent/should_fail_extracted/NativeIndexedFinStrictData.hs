{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeIndexedFinStrictData where

data StrictRetained (xs :: [Bool]) where
    StrictRetained :: foreach (ys :: [Bool]) -> StrictRetained ys

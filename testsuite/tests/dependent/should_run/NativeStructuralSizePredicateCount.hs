{-# LANGUAGE DependentTypes #-}
module NativeStructuralSizePredicateCount (Count(..), same) where

data Count = Z | S Count

same :: Count -> Count -> Bool
same Z Z = True
same Z (S _) = False
same (S _) Z = False
same (S m) (S n) = same m n

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeTypeCaseScrutinee where

type F = case Missing of
  True -> Int
  False -> Bool

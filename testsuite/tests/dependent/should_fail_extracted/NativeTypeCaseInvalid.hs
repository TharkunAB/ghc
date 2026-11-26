{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeTypeCaseInvalid where

type Test = Bool
type F a = case a of
  'Test -> Int

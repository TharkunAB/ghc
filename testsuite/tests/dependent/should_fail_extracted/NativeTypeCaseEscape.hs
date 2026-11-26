{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeTypeCaseEscape where

type Bad a = case a of
  Just x -> x
  Nothing -> x

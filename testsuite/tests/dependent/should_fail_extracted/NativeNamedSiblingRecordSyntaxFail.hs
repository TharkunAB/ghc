{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
module NativeNamedSiblingRecordSyntaxFail where

data Bad where
  Bad :: foreach (n :: Bool) -> { badField :: Int } -> Bad

{-# LANGUAGE DependentTypes, TypeFamilies #-}
module NativeImportedFamilyAssocEq (Has(..), Token(..)) where

class Has t where
  type Payload t
  value :: t -> Payload t

data Token = Token

instance Has Token where
  type Payload Token = NativeImportedFamilyAssocEq.keep Bool
  value _ = True

keep :: a -> a
keep x = x

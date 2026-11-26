{-# LANGUAGE DependentTypes, TypeFamilies #-}
module NativeImportedFamilyAssocWorker (Has(..), Token(..)) where

class Has t where
  type Payload t
  value :: t -> Payload t

data Token = Token

keep :: a -> a
keep x = x

instance Has Token where
  type Payload Token = NativeImportedFamilyAssocWorker.keep Bool
  value _ = True

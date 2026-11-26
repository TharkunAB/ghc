{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeTypeCaseTupleKindEscape where

import GHC.TypeNats (Nat)

type Bad (dims :: (Bool, Nat)) =
  let (flag, _) = dims
  in case flag of
    'True -> missing
    'False -> Bool

{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeNamedSiblingDeclFieldFail where

import PackedVecLibrary (Nat, Vec)

data BadFieldType = BadFieldType
  { badLength :: Nat, badPayload :: (Vec Char badLength, Bool) }

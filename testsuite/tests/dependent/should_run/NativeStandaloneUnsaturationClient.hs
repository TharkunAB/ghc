{-# LANGUAGE NoDependentTypes, NoLiberalTypeSynonyms, UnsaturatedTypeFamilies #-}
module NativeStandaloneUnsaturationClient
  ( ordinary
  , familyVal
  , synonymBox
  , boxedFamily
  , saturatedAlias
  ) where

import NativeStandaloneUnsaturationProvider

ordinary :: Apply Maybe Bool
ordinary = Just True

familyVal :: Apply Identity Bool
familyVal = True

synonymBox :: Box Plain
synonymBox = Box (Just True)

boxedFamily :: Box Identity
boxedFamily = Box True

saturatedAlias :: Result
saturatedAlias = True

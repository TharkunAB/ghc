{-# LANGUAGE DataKinds, DependentTypes, GADTs, KindSignatures, ListTuplePuns #-}
module NativeNamedSiblingRecordHeadFail where

data Indexed (b :: Bool) = Indexed

data ResultHead (b :: Bool) where
  MkResultHead :: { selH :: Bool } -> ResultHead (selH 'True)

data FieldHead where
  MkFieldHead :: { selF :: Bool, payloadF :: Indexed (selF 'True) } -> FieldHead

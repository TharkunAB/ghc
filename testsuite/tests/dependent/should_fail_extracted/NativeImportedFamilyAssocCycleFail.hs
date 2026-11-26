{-# LANGUAGE DependentTypes, TypeFamilies #-}
module NativeImportedFamilyAssocCycleFail where

class Has t where
  type AssocPayload t
  val :: t -> AssocPayload t

data Token = Token

instance Has Token where
  type AssocPayload Token = NativeImportedFamilyAssocCycleFail.keepAssoc Bool
  val _ = True

keepAssoc :: a -> a
keepAssoc x = const x (val Token)

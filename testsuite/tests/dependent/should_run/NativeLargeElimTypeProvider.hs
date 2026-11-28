{-# LANGUAGE DataKinds, ExplicitNamespaces, DependentTypes, ListTuplePuns, GADTs, KindSignatures, ScopedTypeVariables, TypeOperators #-}
module NativeLargeElimTypeProvider
  ( Val(..)
  , valPred
  , valRel
  , tupleOf
  , BoxVal(..)
  , idArr
  ) where

import Data.Kind (Type)

data Val = Bot | Arr Val Val
  deriving (Eq, Show)

valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)

valRel :: Val -> Val -> Type
valRel Bot       w = type (valPred w)
valRel (Arr a b) w = type (valRel a w -> valRel b w)

tupleOf :: forall a. [a] -> Type
tupleOf []       = type ()
tupleOf (_ : xs) = type (a, tupleOf xs)

data BoxVal :: Val -> Type where
  MkBoxVal :: valPred v -> BoxVal v

idArr :: valPred ('Arr 'Bot 'Bot)
idArr x = x

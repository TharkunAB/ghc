{-# LANGUAGE DataKinds, NoDependentTypes, TypeFamilies #-}
module NativeOpenConcreteGuardedListCallbackFail where

import ConcreteGuardedListSource (DropBool)

newtype WrappedBool = WrappedBool Bool

type family WrappedPredicate (x :: WrappedBool) :: Bool where
  WrappedPredicate _ = 'True

type Wrong xs = DropBool WrappedPredicate xs

{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, GADTs, ImpredicativeTypes, ListTuplePuns #-}
{-# LANGUAGE PolyKinds, RankNTypes, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeAlternatingBindersDomain
  ( Reflect
  , reflect
  , bottomReflect
  , wrongResult
  , fixedDomain
  , AmbiguousReflect
  , ambiguous
  , withinScope
  , Captured
  , capture
  ) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

type Reflect a = foreach (x :: Maybe a) -> x :~: x

reflect :: forall a. Show a => Reflect a
reflect _ = Refl

bottomReflect :: forall a. Show a => Reflect a
bottomReflect = error "callback bottom"

wrongResult :: forall a. Show a => foreach (x :: Maybe a) -> Proxy x
wrongResult _ = Proxy

fixedDomain :: Reflect Bool
fixedDomain _ = Refl

type family Opaque a
type BuiltinReflect a = foreach (x :: Maybe (Opaque a)) -> x :~: x

class Extra a
type AmbiguousReflect a = Extra a => BuiltinReflect a

ambiguous :: forall a. AmbiguousReflect a
ambiguous _ = Refl

runScope :: (forall a. Show a => Reflect a) -> foreach (index :: Maybe Bool) -> index :~: index
runScope worker index = worker @Bool index

withinScope :: Maybe Bool -> Bool
withinScope index = case runScope reflect index of Refl -> True

data CaptureValue (tag :: Bool) (index :: Maybe a) = CaptureValue Int
type Captured tag = forall a. Show a => foreach (index :: Maybe a) -> CaptureValue tag index

capture :: foreach (tag :: Bool) -> Maybe (Captured tag)
capture tag = Just worker
  where
    worker :: Captured tag
    worker _ = CaptureValue 42

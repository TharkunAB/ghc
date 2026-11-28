{-# LANGUAGE DataKinds, DependentTypes, ExplicitForAll, PolyKinds,
             StandaloneKindSignatures, TypeApplications #-}
module NativeOpenConstructorResiduals
  ( Pick, Reordered, PickNested, FirstMaybe, BoxedFirst ) where

import Data.Kind (Type)
import qualified OpenConstructorWorker as W
import qualified SpineWorker as S

type Pick :: W.Choice Type Type -> Type
type Pick p = W.pick p

type Reordered :: W.Choice Bool Char -> Char
type Reordered p = W.reordered @Char @Bool p

type PickNested :: forall a. W.Nested a -> a -> a
type PickNested p fallback = W.pickNested p fallback

type FirstMaybe :: forall a. S.Spine a -> Maybe a
type FirstMaybe p = S.firstMaybe p

type BoxedFirst :: forall a. S.Spine a -> Maybe (Maybe a)
type BoxedFirst p = S.boxedFirst p

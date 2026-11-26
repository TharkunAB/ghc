{-# LANGUAGE DataKinds, DependentTypes, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module NativeOpenConstructorHelpersResidual
  ( First, Wrapped, Marked, Second, Erase, Fill ) where

import qualified SpineWorker as W

type First :: W.Spine Bool -> Maybe Bool
type First xs = W.firstViaHelper xs

type Wrapped :: W.Spine Bool -> Maybe (Maybe Bool)
type Wrapped xs = W.firstViaWrap xs

type Marked :: W.Spine Bool -> Maybe (W.Marked Bool)
type Marked xs = W.firstViaMark xs

type Second :: W.Spine Bool -> Maybe Bool
type Second xs = W.secondOrFirst xs

type Erase :: W.Spine Bool -> W.Spine Bool
type Erase xs = W.erase xs

type Fill :: W.Spine Bool -> Maybe Bool -> W.Spine (Maybe Bool)
type Fill xs tag = W.fill xs tag

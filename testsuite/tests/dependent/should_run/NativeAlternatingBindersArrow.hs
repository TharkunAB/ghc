{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ExplicitNamespaces #-}
{-# LANGUAGE ListTuplePuns, Modifiers, PolyKinds, RankNTypes, RequiredTypeArguments #-}
module NativeAlternatingBindersArrow
  ( Payload(..)
  , keep1
  , keep2
  , keep3
  , keep4
  , inferArrow
  ) where

import GHC.Exts (M, Matchability, U)

data Payload (tag :: Bool) (m :: Maybe a) = Payload Int

keep1 :: foreach (tag :: Bool) -> Payload tag (Nothing :: Maybe Bool) %U -> Payload tag (Nothing :: Maybe Bool)
keep1 _ x = x

keep2 :: foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %U -> Payload tag m
keep2 _ (type a) _ x = x

keep3 :: foreach (tag :: Bool) -> Payload tag (Nothing :: Maybe Bool) %U -> Payload tag (Nothing :: Maybe Bool)
keep3 False x = x
keep3 True x = x

keep4 :: (foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %M -> Payload tag m)
      -> foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %M -> Payload tag m
keep4 existing = existing

ignoreArrow :: forall (ma :: Matchability).
  (foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %ma -> Payload tag m) -> Int
ignoreArrow _ = 61

inferArrow :: Int
inferArrow = ignoreArrow (\_ (type a) _ x -> x)

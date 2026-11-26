{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns, Modifiers #-}
{-# LANGUAGE PolyKinds, RequiredTypeArguments, ScopedTypeVariables, TypeAbstractions #-}
module NativeAlternatingBindersArrowRejects where

import GHC.Exts (M)

data Payload (tag :: Bool) (m :: Maybe a) = Payload Int

badArrow1 :: foreach (tag :: Bool) -> Payload tag ('Nothing :: Maybe Bool) %M -> Payload tag ('Nothing :: Maybe Bool)
badArrow1 _ x = x

badArrow2 :: foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Payload tag m %M -> Payload tag m
badArrow2 _ (type a) _ x = x

badArrow3 :: foreach (tag :: Bool) -> Payload tag ('Nothing :: Maybe Bool) %M -> Payload tag ('Nothing :: Maybe Bool)
badArrow3 False x = x
badArrow3 True x = x

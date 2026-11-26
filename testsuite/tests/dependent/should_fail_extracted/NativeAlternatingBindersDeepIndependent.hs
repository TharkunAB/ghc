{-# LANGUAGE DataKinds, DeepSubsumption, DependentTypes, ListTuplePuns, PolyKinds, RankNTypes #-}
module NativeAlternatingBindersDeepIndependent where

import Data.Proxy (Proxy)
import NativeAlternatingBindersProvider (Reading(..))

bad :: foreach (left :: Bool) -> foreach (right :: Bool) ->
       (foreach (b :: Bool) -> forall a. foreach (m :: Maybe a) -> (Reading left m, Proxy b)) ->
       Reading right ('Nothing :: Maybe Bool)
bad _ _ worker =
  let use :: (foreach (b :: Bool) -> forall a. Show a =>
              foreach (m :: Maybe a) -> (Reading right m, Proxy b)) ->
             Reading right ('Nothing :: Maybe Bool)
      use _ = Reading 42
  in use worker

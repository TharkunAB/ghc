{-# LANGUAGE DataKinds, Modifiers, NoDependentTypes, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-}
module NativeOpenEitherFmapRule where

import CheckedEitherInput (Input)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeFmapEitherProof (MappedEither)

{-# RULES "fmap/either/branch" [~1] forall f (x :: Either Bool a). fmap f x = Left False #-}

proof :: forall b (f :: Maybe Bool %U -> b). Proxy f ->
  MappedEither f (Input 'True) :~: ('Right (f ('Just 'False)) :: Either Bool b)
proof _ = Refl

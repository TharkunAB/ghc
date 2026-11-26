{-# LANGUAGE DataKinds, Modifiers, NoDependentTypes, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenEitherFmapWrong where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeFmapEitherProof (MappedEither)

wrongRight :: forall a b (f :: a %U -> b) (x :: a).
  Proxy f -> Proxy x -> MappedEither f ('Right x :: Either Bool a) :~: ('Left 'False :: Either Bool b)
wrongRight _ _ = Refl

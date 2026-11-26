{-# LANGUAGE DataKinds, DependentTypes, FlexibleInstances, ListTuplePuns, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -Wno-orphans #-}
module NativeOpenEitherFmapOverlap where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import NativeOpenEitherFmapProducer (Result(..), Translate(..))

instance {-# INCOHERENT #-} Translate (Result Bool) where
  translate _ _ = Failed False
  count _ = 2

fmapRight :: forall e a b (f :: a %U -> b) (x :: a).
  Proxy f -> Proxy x -> translate f ('Succeeded x :: Result e a) :~: ('Succeeded (f x) :: Result e b)
fmapRight _ _ = Refl

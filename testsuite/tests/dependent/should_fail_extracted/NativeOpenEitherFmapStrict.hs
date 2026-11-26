{-# LANGUAGE DataKinds, DependentTypes, FlexibleInstances, ListTuplePuns, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenEitherFmapStrict where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)

data Result e a = Failed e | Succeeded a

class Translate t where
  translate :: (a -> b) -> t a -> t b

instance Translate (Result e) where
  translate _ (Failed e) = seq e (Failed e)
  translate f (Succeeded a) = Succeeded (f a)

fmapLeft :: forall e a b (f :: a %U -> b) (x :: e).
  Proxy f -> Proxy x -> translate f ('Failed x :: Result e a) :~: ('Failed x :: Result e b)
fmapLeft _ _ = Refl

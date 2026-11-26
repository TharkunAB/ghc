{-# LANGUAGE DataKinds, GADTs, Modifiers, NoDependentTypes, PolyKinds, TypeOperators #-}
module NativeMatchableProjectionUnproved where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (Matchability)

cancel :: forall (m :: Matchability) (f :: Type %m -> Type) x y.
  Proxy f -> f x :~: f y -> x :~: y
cancel _ Refl = Refl

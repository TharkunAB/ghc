{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenEitherFmapGiven where

import Data.Kind (Constraint)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified Prelude as P

data Dict (p :: Constraint) where
  Dict :: p => Dict p

masked :: forall e. Dict (P.Functor (P.Either e)) -> ()
masked Dict =
  let left :: forall a b (f :: a %U -> b) (x :: e).
        Proxy f -> Proxy x -> P.fmap f ('P.Left x :: P.Either e a) :~: ('P.Left x :: P.Either e b)
      left _ _ = Refl
  in ()

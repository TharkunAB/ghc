{-# LANGUAGE CPP #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fomit-interface-pragmas -fno-write-if-simplified-core #-}

-- | Retained dependent pairs used by the experimental sum syntax.
module GHC.Sigma (Sigma(..), first, second, withSigma) where

import GHC.Internal.Types (Type, U)
#if __GLASGOW_HASKELL__ >= 1001
import qualified GHC.Essentials as Rebindable
#endif

-- | A runtime witness paired with a payload at that witness.
data Sigma (a :: Type) (p :: a %U -> Type) where
  Pair :: forall (a :: Type) (p :: a %U -> Type).
          foreach (x :: a) -> p x -> Sigma a p

-- | Return the witness without evaluating the payload.
first :: Sigma a p -> a
first (Pair x _) = x

-- | Return the payload at the pair's witness without evaluating the witness.
second :: foreach (p :: Sigma a b) -> b (first p)
second (Pair _ y) = y

-- | Give the witness and the payload to a dependent consumer.
withSigma :: Sigma a p -> (foreach (x :: a) -> p x -> r) -> r
withSigma (Pair x y) use = use x y

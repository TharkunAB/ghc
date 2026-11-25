{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Propositional equality combinators and arrow matchability types.
module HaskLib.Equality
    ( (:~:) (..)
    , (:~~:) (..)
    , sym
    , trans
    , cong
    , rewrite
    , gcastWith
    , withEquality
    , Matchability (..)
    , M
    , U
    ) where

import Data.Type.Equality (type (~), (:~:) (..), (:~~:) (..), gcastWith, sym, trans)
import GHC.Matchability (M, Matchability (..), U)
import Prelude ()

-- | Apply a matchable or unmatchable function to both sides of an equality.
cong :: forall {a} {b} {m :: Matchability} (f :: a %m -> b) x y.
        x :~: y -> f x :~: f y
cong Refl = Refl

-- | Bring an equality constraint into scope for a continuation.
rewrite :: (a :~: b) -> ((a ~ b) => r) -> r
rewrite = gcastWith

-- | Compatibility alias for 'rewrite'.
withEquality :: (a :~: b) -> ((a ~ b) => r) -> r
withEquality = rewrite

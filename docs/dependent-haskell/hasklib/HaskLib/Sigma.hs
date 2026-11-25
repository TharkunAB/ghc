{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Dependent pairs and erased existential package construction.
module HaskLib.Sigma
    ( Sigma (..)
    , first
    , second
    , withSigma
    , pattern Pack
    ) where

import GHC.Exists (pattern Pack)
import GHC.Sigma (Sigma (..), first, withSigma)
import GHC.Sigma.Projection (second)
import Prelude ()

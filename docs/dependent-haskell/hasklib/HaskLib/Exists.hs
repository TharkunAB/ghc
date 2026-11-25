{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Erased existential packages, constraint pairing, witness tokens, and combinators.
module HaskLib.Exists
    ( Exists
    , pattern Pack
    , type (/\) (..)
    , discardEvidence
    , withEvidence
    , Witness (..)
    , withExists
    , mapExists
    , uncurryExists
    ) where

import GHC.Exists
    ( Exists
    , Witness (..)
    , discardEvidence
    , mapExists
    , uncurryExists
    , withEvidence
    , withExists
    , pattern Pack
    , type (/\) (..)
    )
import Prelude ()

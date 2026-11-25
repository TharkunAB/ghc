{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Public Dependent Haskell base surface for indexed data, packages, and proofs.
module HaskLib
    ( module HaskLib.List
    , module HaskLib.Equality
    , module HaskLib.Exists
    , module HaskLib.Fin
    , module HaskLib.Sigma
    , module HaskLib.Vec
    , eqSize
    , eqSizeRefl
    , natToInt
    , plusZero
    , plusSucc
    , plusComm
    , plusAssoc
    , congSucc
    , congFirst
    ) where

import HaskLib.Equality
import HaskLib.Exists
import HaskLib.Fin
import HaskLib.List
import HaskLib.Nat
    ( congFirst
    , congSucc
    , eqSize
    , eqSizeRefl
    , natToInt
    , plusAssoc
    , plusComm
    , plusSucc
    , plusZero
    )
import HaskLib.Sigma
import HaskLib.Vec
import Prelude ()

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Explicit construction, constraint pairing, and matching of erased
existential packages.

'Pack' is special syntax when ExperimentalDependentTypes is enabled.
-}
module GHC.Exists (
  pattern Pack,
  type (/\) (..),
  discardEvidence,
) where

import GHC.Internal.Types (Constraint, Type)
#if __GLASGOW_HASKELL__ >= 1001
import qualified GHC.Essentials as Rebindable
#endif

-- This declaration gives Pack a constructor name for ordinary import and
-- export processing. Its type is private and its constructor is not used
-- in the Core for existential packages.
data PackTag = Pack

infixr 2 /\ {- prevent CPP line continuation -}

-- | Pair a constraint dictionary with a value.
type (/\) :: Constraint -> Type -> Type
data c /\ a where
  MkCPair :: c => a -> c /\ a

-- | Return the value and ignore the stored constraint dictionary.
discardEvidence :: (c /\ a) -> a
discardEvidence (MkCPair x) = x

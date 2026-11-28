{-# LANGUAGE CPP #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Explicit construction, constraint pairing, witness tokens, and
combinators for erased existential packages (GHC Proposal #473).

'Pack' is special syntax when 'DependentTypes' or 'ExistentialTypes' is
enabled.
-}
module GHC.Exists (
  Exists,
  data Pack,
  type (/\) (..),
  discardEvidence,
  withEvidence,
  Witness (..),
  withExists,
  mapExists,
  uncurryExists,
) where

import GHC.Internal.Types (Constraint, Matchability, Type)
#if __GLASGOW_HASKELL__ >= 1001
import qualified GHC.Essentials as Rebindable
#endif

-- | An erased existential package parameterized by its witness predicate.
type Exists :: forall {k} {m :: Matchability}. (k %m -> Type) -> Type
type Exists p = exists x | p x

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
{-# INLINE discardEvidence #-}

-- | Open a paired constraint dictionary and value in a continuation.
withEvidence :: (c /\ a) -> (c => a -> r) -> r
withEvidence (MkCPair x) k = k x
{-# INLINE withEvidence #-}

-- | An erased witness token for a type @a@ of any kind @k@.
type Witness :: forall {k}. k -> Type
data Witness a where
  Witness :: forall {k} (a :: k). Witness a

-- | Open an explicit existential package and pass its erased witness and
-- payload to a continuation.
withExists
  :: forall {k} {m :: Matchability} (p :: k %m -> Type) r.
     (exists (x :: k) | p x)
  -> (forall (x :: k) -> p x -> r)
  -> r
withExists pkg k = case pkg of
  Pack (type x) payload -> k x payload
{-# INLINE withExists #-}

-- | Transform the payload of an explicit existential package at the same
-- erased witness.
mapExists
  :: forall {k} {mp :: Matchability} {mq :: Matchability}
            (p :: k %mp -> Type) (q :: k %mq -> Type).
     (forall (x :: k) -> p x -> q x)
  -> (exists (x :: k) | p x)
  -> (exists (x :: k) | q x)
mapExists f pkg = case pkg of
  Pack (type x) payload -> Pack (type x) (f x payload)
{-# INLINE mapExists #-}

-- | Open a nested two-witness explicit existential package and pass both
-- erased witnesses and the payload to a continuation.
uncurryExists
  :: forall {k1} {k2} {m1 :: Matchability} {m2 :: Matchability}
            (p :: k1 %m1 -> k2 %m2 -> Type) r.
     (exists (x :: k1) (y :: k2) | p x y)
  -> (forall (x :: k1) (y :: k2) -> p x y -> r)
  -> r
uncurryExists pkg k = case pkg of
  Pack (type x) inner -> case inner of
    Pack (type y) payload -> k x y payload
{-# INLINE uncurryExists #-}

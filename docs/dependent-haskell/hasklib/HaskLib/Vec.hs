{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Length-indexed vectors and list boundary adapters.
module HaskLib.Vec
    ( Vec (..)
    , SomeVec
    , toListVec
    , fromListVec
    , fromListExact
    , packListVec
    , replicateVec
    , appendVec
    , mapVec
    , zipVec
    , headVec
    , tailVec
    ) where

import GHC.Sigma (Sigma (Pair))
import HaskLib.List (size)
import HaskLib.Nat (Nat (..), plus)

-- | A list whose length is tracked by a Peano natural index.
data Vec a (n :: Nat) where
    VNil  :: Vec a 'Zero
    VCons :: a -> Vec a n -> Vec a ('Succ n)

deriving instance Eq a => Eq (Vec a n)
deriving instance Show a => Show (Vec a n)

-- | A runtime length paired with a vector of that length.
type SomeVec a = some (n :: Nat) | Vec a n

-- | Convert a vector to an ordinary list by dropping its length index.
toListVec :: Vec a n -> [a]
toListVec VNil         = []
toListVec (VCons x xs) = x : toListVec xs

-- | Convert an ordinary list to a vector indexed by 'size'.
fromListVec :: forall a. foreach (xs :: [a]) -> Vec a (size xs)
fromListVec []       = VNil
fromListVec (x : xs) = VCons x (fromListVec @a xs)

-- | Check that an ordinary list has the requested runtime length.
-- See Note [Exact-length list adapter].
fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n)
fromListExact Zero     []       = Just VNil
fromListExact Zero     (_ : _)  = Nothing
fromListExact (Succ _) []       = Nothing
fromListExact (Succ n) (x : xs) = case fromListExact n xs of
    Nothing   -> Nothing
    Just rest -> Just (VCons x rest)

-- | Pair an ordinary list's runtime length with its indexed vector.
packListVec :: [a] -> SomeVec a
packListVec xs = Pair (size xs) (fromListVec xs)

{- Note [Exact-length list adapter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function fromListExact checks a list spine against a runtime Nat bound.
When the bound is Zero and the list is non-empty, fromListExact returns
Nothing without evaluating the head or tail. When the lengths match,
fromListExact returns Just without evaluating the list elements.

Function packListVec accepts a list of any length. Both Pair fields stay
lazy until the caller inspects the length or the vector spine.
-}

-- | Build a vector of the requested runtime length by repeating one value.
replicateVec :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateVec Zero     _ = VNil
replicateVec (Succ n) x = VCons x (replicateVec @a n x)

-- | Append two vectors and add their length indices.
appendVec :: Vec a n -> Vec a m -> Vec a (plus n m)
appendVec VNil         ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

-- | Apply a function to each element while preserving the length index.
mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec _ VNil         = VNil
mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)

-- | Pair corresponding elements of two vectors that share one length index.
zipVec :: Vec a n -> Vec b n -> Vec (a, b) n
zipVec VNil         VNil         = VNil
zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)

-- | Extract the first element of a non-empty vector.
headVec :: Vec a ('Succ n) -> a
headVec (VCons x _) = x

-- | Drop the first element of a non-empty vector.
tailVec :: Vec a ('Succ n) -> Vec a n
tailVec (VCons _ xs) = xs

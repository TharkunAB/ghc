{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Bounded natural positions and safe vector indexing.
module HaskLib.Fin
    ( Fin (..)
    , lookupVec
    , mkFin
    , mkFinVec
    ) where

import HaskLib.Nat (Nat (..))
import HaskLib.Vec (Vec (..))

-- | A zero-based position strictly below a Peano natural bound.
data Fin (n :: Nat) where
    FZ :: Fin ('Succ n)
    FS :: Fin n -> Fin ('Succ n)

deriving instance Eq (Fin n)
deriving instance Show (Fin n)

-- | Read the element at a checked position without a fallback branch.
lookupVec :: Fin n -> Vec a n -> a
lookupVec FZ     (VCons x _)  = x
lookupVec (FS i) (VCons _ xs) = lookupVec i xs

-- | Convert a non-negative integer into a position below a runtime bound.
mkFin :: foreach (n :: Nat) -> Int -> Maybe (Fin n)
mkFin n position
    | position < 0 = Nothing
    | otherwise = go n position
  where
    go :: foreach (m :: Nat) -> Int -> Maybe (Fin m)
    go Zero _ = Nothing
    go (Succ m) k =
        if k == 0
            then Just FZ
            else case go m (k - 1) of
                Nothing      -> Nothing
                Just smaller -> Just (FS smaller)

-- | Convert a non-negative integer into a position within a vector's spine.
mkFinVec :: Vec a n -> Int -> Maybe (Fin n)
mkFinVec xs position
    | position < 0 = Nothing
    | otherwise = case xs of
        VNil -> Nothing
        VCons _ rest
            | position == 0 -> Just FZ
            | otherwise     -> case mkFinVec rest (position - 1) of
                Nothing      -> Nothing
                Just smaller -> Just (FS smaller)

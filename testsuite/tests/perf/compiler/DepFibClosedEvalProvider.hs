{-# LANGUAGE GHC2024 #-}

-- | Closed ground producers for compile-time evaluation in types.
--
-- This module omits DependentTypes so that GHC compiles these functions
-- without source equations. Type-level applications in client modules must
-- therefore execute through bytecode evaluation and value readback.
module DepFibClosedEvalProvider
  ( Peano (..)
  , PairP (..)
  , BinTree (..)
  , peanoOf
  , peanoAdd
  , fibNat
  , ackNat
  , fibPeano
  , buildTree
  , countTree
  , swapPairP
  ) where

import Numeric.Natural (Natural)

-- | Unary natural numbers for promoted DataCon readback checks.
data Peano
  = Zero
  | Succ Peano
  deriving (Eq, Show)

-- | Pair of unary natural numbers.
data PairP = MkPairP Peano Peano
  deriving (Eq, Show)

-- | Binary tree with unary natural leaves.
data BinTree
  = Leaf Peano
  | Branch BinTree BinTree
  deriving (Eq, Show)

-- | Convert a Natural to a unary Peano value.
peanoOf :: Natural -> Peano
peanoOf 0 = Zero
peanoOf n = Succ (peanoOf (n - 1))

-- | Add two unary Peano values.
peanoAdd :: Peano -> Peano -> Peano
peanoAdd Zero y = y
peanoAdd (Succ x) y = Succ (peanoAdd x y)

-- | Compute Fibonacci numbers by binary recursion to exercise bytecode calls.
fibNat :: Natural -> Natural
fibNat 0 = 0
fibNat 1 = 1
fibNat n = fibNat (n - 1) + fibNat (n - 2)

-- | Compute the Ackermann-Peter function on small ground arguments.
ackNat :: Natural -> Natural -> Natural
ackNat 0 n = n + 1
ackNat m 0 = ackNat (m - 1) 1
ackNat m n = ackNat (m - 1) (ackNat m (n - 1))

-- | Compute a unary Fibonacci result for deep DataCon readback.
fibPeano :: Natural -> Peano
fibPeano 0 = Zero
fibPeano 1 = Succ Zero
fibPeano n = peanoAdd (fibPeano (n - 1)) (fibPeano (n - 2))

-- | Build a balanced binary tree of the requested height.
buildTree :: Natural -> BinTree
buildTree 0 = Leaf (Succ Zero)
buildTree n =
  let sub = buildTree (n - 1)
  in Branch sub sub

-- | Sum all leaf values in a binary tree.
countTree :: BinTree -> Peano
countTree (Leaf k) = k
countTree (Branch l r) = peanoAdd (countTree l) (countTree r)

-- | Swap the two fields of a PairP value.
swapPairP :: PairP -> PairP
swapPairP (MkPairP a b) = MkPairP b a

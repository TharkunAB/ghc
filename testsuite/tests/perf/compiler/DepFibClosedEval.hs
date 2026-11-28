{-# LANGUAGE DependentTypes #-}

-- | Compile-time benchmark for closed bytecode evaluation in types.
-- See Note [Closed evaluation benchmark structure].
module DepFibClosedEval where

import Data.Type.Equality ((:~:) (Refl))
import Numeric.Natural (Natural)
import DepFibClosedEvalProvider

{- Note [Closed evaluation benchmark structure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DepFibClosedEval measures two closed-evaluation paths in the typechecker:
1. Imported functions from DepFibClosedEvalProvider carry no SourceTable.
   Type-level calls link the provider module and evaluate bytecode on closed
   ground arguments.
2. Local functions in this module use Int accumulators and arithmetic so that
   same-module closed bytecode precomputation executes them directly and reads
   their results back as promoted DataCon applications.
-}

-- | Compute Fibonacci numbers in the home module through Int arithmetic.
localFib :: Natural -> Natural
localFib 0 = 0
localFib 1 = 1
localFib n =
  let step :: Int
      step = fromIntegral (localFib (n - 1)) + fromIntegral (localFib (n - 2))
  in fromIntegral step

-- | Build a unary Peano Fibonacci number in the home module.
localFibPeano :: Natural -> Peano
localFibPeano n =
  let k :: Int
      k = fromIntegral (localFib n)
      toP :: Int -> Peano
      toP 0 = Zero
      toP m = Succ (toP (m - 1))
  in toP k

-- | Count Collatz steps to 1 on positive ground naturals.
collatzSteps :: Natural -> Natural
collatzSteps n =
  let go :: Int -> Int -> Int
      go acc k
        | k <= 1 = acc
        | even k = go (acc + 1) (k `div` 2)
        | otherwise = go (acc + 1) (3 * k + 1)
  in fromIntegral (go 0 (fromIntegral n))

-- | Build a binary tree in the home module.
localTree :: Natural -> BinTree
localTree n =
  let go :: Int -> BinTree
      go 0 = Leaf (Succ Zero)
      go k =
        let sub = go (k - 1)
        in Branch sub sub
  in go (fromIntegral n)

-- | Test evenness by mutual recursion in the home module.
localEven :: Natural -> Bool
localEven 0 = True
localEven n =
  let k :: Int
      k = fromIntegral n - 1
  in localOdd (fromIntegral k)

-- | Test oddness by mutual recursion in the home module.
localOdd :: Natural -> Bool
localOdd 0 = False
localOdd n =
  let k :: Int
      k = fromIntegral n - 1
  in localEven (fromIntegral k)

pFib14 :: fibNat 14 :~: 377
pFib14 = Refl

pFib15 :: fibNat 15 :~: 610
pFib15 = Refl

pFib16 :: fibNat 16 :~: 987
pFib16 = Refl

pFib17 :: fibNat 17 :~: 1597
pFib17 = Refl

pFib18 :: fibNat 18 :~: 2584
pFib18 = Refl

pAck34 :: ackNat 3 4 :~: 125
pAck34 = Refl

pAck35 :: ackNat 3 5 :~: 253
pAck35 = Refl

pAck36 :: ackNat 3 6 :~: 509
pAck36 = Refl

pPeanoFib10 :: fibPeano 10 :~: peanoOf 55
pPeanoFib10 = Refl

pPeanoFib11 :: fibPeano 11 :~: peanoOf 89
pPeanoFib11 = Refl

pPeanoFib12 :: fibPeano 12 :~: peanoOf 144
pPeanoFib12 = Refl

pTree6 :: countTree (buildTree 6) :~: peanoOf 64
pTree6 = Refl

pTree7 :: countTree (buildTree 7) :~: peanoOf 128
pTree7 = Refl

pSwap :: swapPairP (MkPairP (Succ Zero) (Succ (Succ (Succ Zero))))
           :~: MkPairP (Succ (Succ (Succ Zero))) (Succ Zero)
pSwap = Refl

pLocalFib14 :: localFib 14 :~: 377
pLocalFib14 = Refl

pLocalFib15 :: localFib 15 :~: 610
pLocalFib15 = Refl

pLocalFib16 :: localFib 16 :~: 987
pLocalFib16 = Refl

pLocalFib17 :: localFib 17 :~: 1597
pLocalFib17 = Refl

pLocalPeano10 :: localFibPeano 10 :~: peanoOf 55
pLocalPeano10 = Refl

pLocalPeano11 :: localFibPeano 11 :~: peanoOf 89
pLocalPeano11 = Refl

pLocalTree6 :: countTree (localTree 6) :~: peanoOf 64
pLocalTree6 = Refl

pLocalTree7 :: countTree (localTree 7) :~: peanoOf 128
pLocalTree7 = Refl

pCollatz27 :: collatzSteps 27 :~: 111
pCollatz27 = Refl

pCollatz97 :: collatzSteps 97 :~: 118
pCollatz97 = Refl

pEven42 :: localEven 42 :~: True
pEven42 = Refl

pOdd42 :: localOdd 42 :~: False
pOdd42 = Refl

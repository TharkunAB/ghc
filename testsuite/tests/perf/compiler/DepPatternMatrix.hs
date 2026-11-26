{-# LANGUAGE DependentTypes #-}

-- | Compile-time benchmark for multi-column GADT pattern-match refutation,
-- existential unpacking, and the advisory structural totality analyser.
-- See Note [Dependent pattern-matrix benchmark].
module DepPatternMatrix where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exists (Exists, data Pack, withExists)

{- Note [Dependent pattern-matrix benchmark]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DepPatternMatrix stresses three coupled passes:
1. GADT and foreach index refinement across 4-column pattern matrices where
   off-diagonal constructor combinations are uninhabited.
2. Pattern-match coverage checking (GHC.HsToCore.Pmc) under
   -Werror=incomplete-patterns, verifying that all impossible off-diagonal
   clauses are refuted without redundant catch-all equations.
3. Advisory structural totality checking (-freport-totality) across indexed
   recursive functions and existential unpackings.
-}

-- | Four-state protocol phase index.
data Phase = P0 | P1 | P2 | P3

-- | Singleton tag refining a runtime Phase index.
type STag :: Phase -> Type
data STag p where
  SP0 :: STag P0
  SP1 :: STag P1
  SP2 :: STag P2
  SP3 :: STag P3

-- | Peano natural numbers.
data Nat = Zero | Succ Nat

-- | Length-indexed vectors.
type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- | 16-constructor 2D transition matrix covering every (Phase, Phase) cell.
type Cell :: Phase -> Phase -> Type
data Cell p q where
  C00 :: Nat -> Cell P0 P0
  C01 :: Nat -> Cell P0 P1
  C02 :: Nat -> Cell P0 P2
  C03 :: Nat -> Cell P0 P3
  C10 :: Nat -> Cell P1 P0
  C11 :: Nat -> Cell P1 P1
  C12 :: Nat -> Cell P1 P2
  C13 :: Nat -> Cell P1 P3
  C20 :: Nat -> Cell P2 P0
  C21 :: Nat -> Cell P2 P1
  C22 :: Nat -> Cell P2 P2
  C23 :: Nat -> Cell P2 P3
  C30 :: Nat -> Cell P3 P0
  C31 :: Nat -> Cell P3 P1
  C32 :: Nat -> Cell P3 P2
  C33 :: Nat -> Cell P3 P3

-- | Next phase transition function used in type indices.
nextPhase :: Phase -> Phase
nextPhase P0 = P1
nextPhase P1 = P2
nextPhase P2 = P3
nextPhase P3 = P0

-- | Structural addition on Nat.
plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

-- | Refine a runtime foreach phase to its singleton tag.
refinePhase :: foreach (p :: Phase) -> STag p
refinePhase P0 = SP0
refinePhase P1 = SP1
refinePhase P2 = SP2
refinePhase P3 = SP3

-- | Four-column dependent matrix dispatch. For each (p, q), STag p and STag q
-- uniquely determine one of the 16 Cell constructors; the 240 off-diagonal
-- combinations are refuted by the pattern-match oracle.
evalCell4
  :: foreach (p :: Phase)
  -> foreach (q :: Phase)
  -> STag p
  -> STag q
  -> Cell p q
  -> Nat
evalCell4 P0 P0 SP0 SP0 (C00 n) = n
evalCell4 P0 P1 SP0 SP1 (C01 n) = Succ n
evalCell4 P0 P2 SP0 SP2 (C02 n) = Succ (Succ n)
evalCell4 P0 P3 SP0 SP3 (C03 n) = Succ (Succ (Succ n))
evalCell4 P1 P0 SP1 SP0 (C10 n) = Succ n
evalCell4 P1 P1 SP1 SP1 (C11 n) = n
evalCell4 P1 P2 SP1 SP2 (C12 n) = Succ (Succ n)
evalCell4 P1 P3 SP1 SP3 (C13 n) = Succ (Succ (Succ n))
evalCell4 P2 P0 SP2 SP0 (C20 n) = Succ (Succ n)
evalCell4 P2 P1 SP2 SP1 (C21 n) = Succ n
evalCell4 P2 P2 SP2 SP2 (C22 n) = n
evalCell4 P2 P3 SP2 SP3 (C23 n) = Succ (Succ (Succ n))
evalCell4 P3 P0 SP3 SP0 (C30 n) = Succ (Succ (Succ n))
evalCell4 P3 P1 SP3 SP1 (C31 n) = Succ (Succ n)
evalCell4 P3 P2 SP3 SP2 (C32 n) = Succ n
evalCell4 P3 P3 SP3 SP3 (C33 n) = n

-- | Compose two adjacent transition cells along a shared middle phase r.
composeCell
  :: foreach (p :: Phase)
  -> foreach (q :: Phase)
  -> foreach (r :: Phase)
  -> STag p
  -> STag q
  -> STag r
  -> Cell p q
  -> Cell q r
  -> Cell p r
composeCell P0 P0 P0 SP0 SP0 SP0 (C00 a) (C00 b) = C00 (plus a b)
composeCell P0 P0 P1 SP0 SP0 SP1 (C00 a) (C01 b) = C01 (plus a b)
composeCell P0 P0 P2 SP0 SP0 SP2 (C00 a) (C02 b) = C02 (plus a b)
composeCell P0 P0 P3 SP0 SP0 SP3 (C00 a) (C03 b) = C03 (plus a b)
composeCell P0 P1 P0 SP0 SP1 SP0 (C01 a) (C10 b) = C00 (plus a b)
composeCell P0 P1 P1 SP0 SP1 SP1 (C01 a) (C11 b) = C01 (plus a b)
composeCell P0 P1 P2 SP0 SP1 SP2 (C01 a) (C12 b) = C02 (plus a b)
composeCell P0 P1 P3 SP0 SP1 SP3 (C01 a) (C13 b) = C03 (plus a b)
composeCell P0 P2 P0 SP0 SP2 SP0 (C02 a) (C20 b) = C00 (plus a b)
composeCell P0 P2 P1 SP0 SP2 SP1 (C02 a) (C21 b) = C01 (plus a b)
composeCell P0 P2 P2 SP0 SP2 SP2 (C02 a) (C22 b) = C02 (plus a b)
composeCell P0 P2 P3 SP0 SP2 SP3 (C02 a) (C23 b) = C03 (plus a b)
composeCell P0 P3 P0 SP0 SP3 SP0 (C03 a) (C30 b) = C00 (plus a b)
composeCell P0 P3 P1 SP0 SP3 SP1 (C03 a) (C31 b) = C01 (plus a b)
composeCell P0 P3 P2 SP0 SP3 SP2 (C03 a) (C32 b) = C02 (plus a b)
composeCell P0 P3 P3 SP0 SP3 SP3 (C03 a) (C33 b) = C03 (plus a b)
composeCell P1 P0 P0 SP1 SP0 SP0 (C10 a) (C00 b) = C10 (plus a b)
composeCell P1 P0 P1 SP1 SP0 SP1 (C10 a) (C01 b) = C11 (plus a b)
composeCell P1 P0 P2 SP1 SP0 SP2 (C10 a) (C02 b) = C12 (plus a b)
composeCell P1 P0 P3 SP1 SP0 SP3 (C10 a) (C03 b) = C13 (plus a b)
composeCell P1 P1 P0 SP1 SP1 SP0 (C11 a) (C10 b) = C10 (plus a b)
composeCell P1 P1 P1 SP1 SP1 SP1 (C11 a) (C11 b) = C11 (plus a b)
composeCell P1 P1 P2 SP1 SP1 SP2 (C11 a) (C12 b) = C12 (plus a b)
composeCell P1 P1 P3 SP1 SP1 SP3 (C11 a) (C13 b) = C13 (plus a b)
composeCell P1 P2 P0 SP1 SP2 SP0 (C12 a) (C20 b) = C10 (plus a b)
composeCell P1 P2 P1 SP1 SP2 SP1 (C12 a) (C21 b) = C11 (plus a b)
composeCell P1 P2 P2 SP1 SP2 SP2 (C12 a) (C22 b) = C12 (plus a b)
composeCell P1 P2 P3 SP1 SP2 SP3 (C12 a) (C23 b) = C13 (plus a b)
composeCell P1 P3 P0 SP1 SP3 SP0 (C13 a) (C30 b) = C10 (plus a b)
composeCell P1 P3 P1 SP1 SP3 SP1 (C13 a) (C31 b) = C11 (plus a b)
composeCell P1 P3 P2 SP1 SP3 SP2 (C13 a) (C32 b) = C12 (plus a b)
composeCell P1 P3 P3 SP1 SP3 SP3 (C13 a) (C33 b) = C13 (plus a b)
composeCell P2 P0 P0 SP2 SP0 SP0 (C20 a) (C00 b) = C20 (plus a b)
composeCell P2 P0 P1 SP2 SP0 SP1 (C20 a) (C01 b) = C21 (plus a b)
composeCell P2 P0 P2 SP2 SP0 SP2 (C20 a) (C02 b) = C22 (plus a b)
composeCell P2 P0 P3 SP2 SP0 SP3 (C20 a) (C03 b) = C23 (plus a b)
composeCell P2 P1 P0 SP2 SP1 SP0 (C21 a) (C10 b) = C20 (plus a b)
composeCell P2 P1 P1 SP2 SP1 SP1 (C21 a) (C11 b) = C21 (plus a b)
composeCell P2 P1 P2 SP2 SP1 SP2 (C21 a) (C12 b) = C22 (plus a b)
composeCell P2 P1 P3 SP2 SP1 SP3 (C21 a) (C13 b) = C23 (plus a b)
composeCell P2 P2 P0 SP2 SP2 SP0 (C22 a) (C20 b) = C20 (plus a b)
composeCell P2 P2 P1 SP2 SP2 SP1 (C22 a) (C21 b) = C21 (plus a b)
composeCell P2 P2 P2 SP2 SP2 SP2 (C22 a) (C22 b) = C22 (plus a b)
composeCell P2 P2 P3 SP2 SP2 SP3 (C22 a) (C23 b) = C23 (plus a b)
composeCell P2 P3 P0 SP2 SP3 SP0 (C23 a) (C30 b) = C20 (plus a b)
composeCell P2 P3 P1 SP2 SP3 SP1 (C23 a) (C31 b) = C21 (plus a b)
composeCell P2 P3 P2 SP2 SP3 SP2 (C23 a) (C32 b) = C22 (plus a b)
composeCell P2 P3 P3 SP2 SP3 SP3 (C23 a) (C33 b) = C23 (plus a b)
composeCell P3 P0 P0 SP3 SP0 SP0 (C30 a) (C00 b) = C30 (plus a b)
composeCell P3 P0 P1 SP3 SP0 SP1 (C30 a) (C01 b) = C31 (plus a b)
composeCell P3 P0 P2 SP3 SP0 SP2 (C30 a) (C02 b) = C32 (plus a b)
composeCell P3 P0 P3 SP3 SP0 SP3 (C30 a) (C03 b) = C33 (plus a b)
composeCell P3 P1 P0 SP3 SP1 SP0 (C31 a) (C10 b) = C30 (plus a b)
composeCell P3 P1 P1 SP3 SP1 SP1 (C31 a) (C11 b) = C31 (plus a b)
composeCell P3 P1 P2 SP3 SP1 SP2 (C31 a) (C12 b) = C32 (plus a b)
composeCell P3 P1 P3 SP3 SP1 SP3 (C31 a) (C13 b) = C33 (plus a b)
composeCell P3 P2 P0 SP3 SP2 SP0 (C32 a) (C20 b) = C30 (plus a b)
composeCell P3 P2 P1 SP3 SP2 SP1 (C32 a) (C21 b) = C31 (plus a b)
composeCell P3 P2 P2 SP3 SP2 SP2 (C32 a) (C22 b) = C32 (plus a b)
composeCell P3 P2 P3 SP3 SP2 SP3 (C32 a) (C23 b) = C33 (plus a b)
composeCell P3 P3 P0 SP3 SP3 SP0 (C33 a) (C30 b) = C30 (plus a b)
composeCell P3 P3 P1 SP3 SP3 SP1 (C33 a) (C31 b) = C31 (plus a b)
composeCell P3 P3 P2 SP3 SP3 SP2 (C33 a) (C32 b) = C32 (plus a b)
composeCell P3 P3 P3 SP3 SP3 SP3 (C33 a) (C33 b) = C33 (plus a b)

-- | Advance one cyclic step along nextPhase.
stepCell :: foreach (p :: Phase) -> STag p -> Nat -> Cell p (nextPhase p)
stepCell P0 SP0 n = C01 n
stepCell P1 SP1 n = C12 n
stepCell P2 SP2 n = C23 n
stepCell P3 SP3 n = C30 n

-- | Four-fold nextPhase cycle returns to the starting Phase.
fourCycleId :: foreach (p :: Phase) -> nextPhase (nextPhase (nextPhase (nextPhase p))) :~: p
fourCycleId P0 = Refl
fourCycleId P1 = Refl
fourCycleId P2 = Refl
fourCycleId P3 = Refl

-- | Path indexed by start phase, end phase, and step count.
type Path :: Phase -> Phase -> Nat -> Type
data Path p q n where
  PathNil  :: Path p p Zero
  PathCons :: STag p -> Cell p q -> Path q r n -> Path p r (Succ n)

-- | Sum all weights along an indexed path.
pathWeight :: Path p q n -> Nat
pathWeight PathNil = Zero
pathWeight (PathCons _ cell rest) = plus (cellPayload cell) (pathWeight rest)

-- | Extract the Nat payload of any Cell constructor.
cellPayload :: Cell p q -> Nat
cellPayload (C00 n) = n
cellPayload (C01 n) = n
cellPayload (C02 n) = n
cellPayload (C03 n) = n
cellPayload (C10 n) = n
cellPayload (C11 n) = n
cellPayload (C12 n) = n
cellPayload (C13 n) = n
cellPayload (C20 n) = n
cellPayload (C21 n) = n
cellPayload (C22 n) = n
cellPayload (C23 n) = n
cellPayload (C30 n) = n
cellPayload (C31 n) = n
cellPayload (C32 n) = n
cellPayload (C33 n) = n

-- | Existentially hide the destination phase of an outgoing edge.
type OutEdge :: Phase -> Type
type OutEdge p = Exists (Cell p)

-- | Pack a cyclic outgoing edge using Pack existential syntax.
packStepEdge :: foreach (p :: Phase) -> Nat -> OutEdge p
packStepEdge p n =
  let cell :: Cell p (nextPhase p)
      cell = stepCell p (refinePhase p) n
  in Pack (type (nextPhase p)) cell

-- | Unpack a vector of existential outgoing edges and accumulate their weights.
sumOutEdges :: Vec (OutEdge p) n -> Nat
sumOutEdges VNil = Zero
sumOutEdges (VCons edge rest) =
  withExists edge (\_ cell -> plus (cellPayload cell) (sumOutEdges rest))

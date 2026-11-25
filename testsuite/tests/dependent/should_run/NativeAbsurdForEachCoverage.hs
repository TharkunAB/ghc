{-# LANGUAGE DataKinds, DependentTypes, EmptyCase, GADTs, LambdaCase, ListTuplePuns, TypeOperators #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Werror #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Type.Equality ((:~:) (Refl))
import Prelude

-- | Peano natural numbers.
data Nat = Zero | Succ Nat
  deriving (Eq, Show)

-- | Convert a Peano natural number to 'Int'.
natToInt :: Nat -> Int
natToInt Zero     = 0
natToInt (Succ n) = 1 + natToInt n

-- | Length-indexed vectors.
data Vec a (n :: Nat) where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

-- | Convert a length-indexed vector to a plain list.
vecToList :: Vec a n -> [a]
vecToList VNil         = []
vecToList (VCons x xs) = x : vecToList xs

-- | Bounded natural numbers.
data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

-- | Convert a bounded natural number to 'Int'.
finToInt :: Fin n -> Int
finToInt FZ     = 0
finToInt (FS i) = 1 + finToInt i

-- | Relational length-equality witness.
data SameLen (m :: Nat) (n :: Nat) where
  SLNil  :: SameLen 'Zero 'Zero
  SLCons :: SameLen m n -> SameLen ('Succ m) ('Succ n)

-- | Two-column @foreach@ zip omitting impossible @Zero (Succ _)@ and @(Succ _) Zero@
-- branches via propositional equality @m :~: n@.
sameVecZip :: foreach (m n :: Nat) -> Vec a m -> Vec b n -> (m :~: n) -> Vec (a, b) m
sameVecZip Zero     Zero     VNil         VNil         Refl = VNil
sameVecZip (Succ m) (Succ n) (VCons x xs) (VCons y ys) Refl =
  VCons (x, y) (sameVecZip m n xs ys Refl)

-- | Two-column @foreach@ zip omitting impossible off-diagonal branches via an
-- indexed GADT witness @SameLen m n@.
sameLenZip :: foreach (m n :: Nat) -> SameLen m n -> Vec a m -> Vec b n -> Vec (a, b) m
sameLenZip Zero     Zero     SLNil      VNil         VNil         = VNil
sameLenZip (Succ m) (Succ n) (SLCons s) (VCons x xs) (VCons y ys) =
  VCons (x, y) (sameLenZip m n s xs ys)

-- | Three-column @foreach@ zip omitting all six off-diagonal triples.
triVecZip
  :: foreach (m n p :: Nat)
  -> Vec a m -> Vec b n -> Vec c p
  -> (m :~: n) -> (n :~: p)
  -> Vec (a, b, c) m
triVecZip Zero     Zero     Zero     VNil         VNil         VNil         Refl Refl = VNil
triVecZip (Succ m) (Succ n) (Succ p) (VCons x xs) (VCons y ys) (VCons z zs) Refl Refl =
  VCons (x, y, z) (triVecZip m n p xs ys zs Refl Refl)

-- | Single-column @foreach@ omitting impossible @Zero@ via @n :~: Succ k@.
onlySuccHead :: foreach (n :: Nat) -> Vec a n -> (n :~: 'Succ k) -> a
onlySuccHead (Succ _) (VCons x _) Refl = x

-- | Variable @foreach@ binder refined by GADT @Vec a (Succ n)@.
vHead :: foreach (n :: Nat) -> Vec a ('Succ n) -> a
vHead _ (VCons x _) = x

-- | Multi-clause variable @foreach@ prefix discriminated solely by GADT arguments,
-- exercising transitive @TermVarTy@ Given compatibility in PMC.
vZipCountByGadt :: foreach (m n :: Nat) -> (m :~: n) -> Vec a m -> Vec b n -> Int
vZipCountByGadt _ _ Refl VNil        VNil        = 0
vZipCountByGadt m _ Refl (VCons _ _) (VCons _ _) = natToInt m

-- | Explicit empty dependent @case@ on an uninhabited @foreach (i :: Fin Zero)@.
absurdFin :: foreach (i :: Fin 'Zero) -> a
absurdFin i = case i of {}

-- | Empty @\case@ on an uninhabited @foreach (i :: Fin Zero)@.
absurdFinLam :: foreach (i :: Fin 'Zero) -> a
absurdFinLam = \case {}

-- | Empty @case@ on @Fin n@ refined by an earlier @n :~: Zero@ GADT match.
absurdFinUnderEq :: foreach (n :: Nat) -> (n :~: 'Zero) -> Fin n -> a
absurdFinUnderEq _ Refl i = case i of {}

-- | Empty @case@ on cyclic @n :~: Succ n@ refuted by strict constructor occurs check.
absurdCyclicEq :: foreach (n :: Nat) -> (n :~: 'Succ n) -> a
absurdCyclicEq _ eq = case eq of {}

main :: IO ()
main = do
  let n2 = Succ (Succ Zero)
      vA = VCons 'a' (VCons 'b' VNil)
      vB = VCons (10 :: Int) (VCons 20 VNil)
      vC = VCons True (VCons False VNil)
      sl2 = SLCons (SLCons SLNil)
      z1 = vecToList (sameVecZip n2 n2 vA vB Refl)
      z2 = vecToList (sameLenZip n2 n2 sl2 vA vB)
      z3 = vecToList (triVecZip n2 n2 n2 vA vB vC Refl Refl)
      h1 = onlySuccHead @(Char) @(Succ Zero) n2 vA Refl
      h2 = vHead (Succ Zero) vA
      c0 = vZipCountByGadt Zero Zero Refl (VNil :: Vec Int 'Zero) (VNil :: Vec Bool 'Zero)
      c2 = vZipCountByGadt n2 n2 Refl vA vB
      f1 = finToInt (FS FZ :: Fin ('Succ ('Succ 'Zero)))
      _unused :: (Int, Int, Int, Int)
      _unused = (absurdFin, absurdFinLam, absurdFinUnderEq Zero Refl, absurdCyclicEq Zero)
        `seq` (0, 0, 0, 0)
  ok <- evaluate
    ( z1 == [('a', 10), ('b', 20)]
    && z2 == [('a', 10), ('b', 20)]
    && z3 == [('a', 10, True), ('b', 20, False)]
    && h1 == 'a'
    && h2 == 'a'
    && c0 == 0
    && c2 == 2
    && f1 == 1
    )
  if not ok
    then error "NativeAbsurdForEachCoverage: value mismatch"
    else pure ()

  -- Verify the synthesized empty case on an off-diagonal branch forces the
  -- impossible equality witness cleanly at runtime.
  res <- try (evaluate (vecToList (sameVecZip Zero (Succ Zero)
                                     (VNil :: Vec Char 'Zero)
                                     (VCons (1 :: Int) VNil)
                                     (error "absurd-eq-forced"))))
         :: IO (Either ErrorCall [(Char, Int)])
  case res of
    Left _  -> pure ()
    Right _ -> error "NativeAbsurdForEachCoverage: expected absurd witness exception"

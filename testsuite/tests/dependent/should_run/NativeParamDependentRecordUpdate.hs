{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main where

import Data.Char (toUpper)
import Data.Type.Equality ((:~:) (Refl))
import NativeParamDependentRecordUpdateProvider
import NativeParamDependentRecordUpdateOrd (checkOrdinaryStrict)

-- Invariant payload update on parameterized Columns a.
mapColumnsInvariant :: (a -> a) -> Columns a -> Columns a
mapColumnsInvariant f r = r { cells = mapVec f (cells r) }

-- Type-changing payload update on parameterized Columns a -> Columns b.
mapColumnsChanging :: (a -> b) -> Columns a -> Columns b
mapColumnsChanging f r = r { cells = mapVec f (cells r) }

-- Dependent foreach signature payload update.
replaceCells :: forall a b. foreach (r :: Columns a) -> Vec b (storedLength r) -> Columns b
replaceCells r xs = r { cells = xs }

-- Partial type-changing payload update on PairCols a b -> PairCols c b (leaving rightCells lazy).
mapLeftOnly :: (a -> c) -> PairCols a b -> PairCols c b
mapLeftOnly f p = p { leftCells = mapVec f (leftCells p) }

-- Consistent multi-field type-changing update on SameCols a -> SameCols b.
mapSameBoth :: (a -> b) -> SameCols a -> SameCols b
mapSameBoth f s = s { firstCells = mapVec f (firstCells s)
                    , secondCells = mapVec f (secondCells s) }

-- Witness-replacing update changing both stored witness and element type.
clearColumns :: Columns a -> Columns b
clearColumns r = r { storedLength = Z, cells = VNil }

-- Witness-preserving update reusing omitted payload lazily (including visible type application).
keepColumns :: forall a. Columns a -> Columns a
keepColumns r = r { storedLength = storedLength @a r }

-- Witness-replacing update under a nominal equality proof reusing omitted secondCells lazily.
transportFirst :: foreach (p :: SameCols a) (q :: SameCols a)
               -> (sameLength p :~: sameLength q) -> SameCols a
transportFirst p q Refl = p { sameLength = sameLength q, firstCells = firstCells q }

main :: IO ()
main = do
  let c0 = Columns (S (S Z)) (VCons 'a' (VCons 'b' VNil))
      c1 = mapColumnsInvariant toUpper c0
      c2 = mapColumnsChanging (== 'A') c1
      c3 = replaceCells c2 (mapVec (\b -> if b then (10 :: Int) else 20) (cells c2))
      c4 = clearColumns c3 :: Columns String
      c5 = keepColumns (Columns (S Z) (VCons 'z' VNil))
      lazyLen :: Nat
      lazyLen = undefined
      lazyOk = keepColumns (Columns lazyLen undefined :: Columns Bool) `seq` True

      p0 = PairCols (S Z) (VCons (42 :: Int) VNil) (undefined :: Vec Bool ('S 'Z))
      p1 = mapLeftOnly show p0

      s0 = SameCols (S Z) (VCons 'x' VNil) (VCons 'y' VNil)
      s1 = mapSameBoth (== 'x') s0
      s3 = transportFirst s0 s0 Refl

      ok = natToInt (storedLength c1) == 2
        && toList (cells c1) == "AB"
        && toList (cells c2) == [True, False]
        && toList (cells c3) == [10, 20]
        && natToInt (storedLength c4) == 0
        && null (toList (cells c4))
        && toList (cells c5) == "z"
        && lazyOk
        && toList (leftCells p1) == ["42"]
        && toList (zipVec (firstCells s1) (secondCells s1)) == [(True, False)]
        && toList (zipVec (firstCells s3) (secondCells s3)) == [('x', 'y')]
  ordOk <- checkOrdinaryStrict
  print (ok && ordOk)

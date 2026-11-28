{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, PolyKinds, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import qualified ExistentialVec as E
import qualified IndexedVec as V
import qualified NativeClosedWitnessedBatchExtra as W
import qualified RectangularTable as T
import qualified WitnessedBatch as B

evenBatch :: B.build 4 :~: 'B.Chunk ('E.SS 'E.SZ) ('V.VCons 4 'V.VNil)
                            ('B.Chunk ('E.SS ('E.SS 'E.SZ))
                                ('V.VCons 5 ('V.VCons 6 'V.VNil)) 'B.End)
evenBatch = Refl

oddBatch :: B.build 3 :~: 'B.Chunk ('E.SS ('E.SS 'E.SZ)) ('V.VCons 3 ('V.VCons 4 'V.VNil)) 'B.End
oddBatch = Refl

mask :: B.maskedRow 4 :~: 'B.MaskedRow ('E.SS ('E.SS 'E.SZ))
                            ('V.VCons 4 ('V.VCons 5 'V.VNil))
                            'x' ('V.VCons 'False ('V.VCons 'True 'V.VNil))
mask = Refl

empty :: B.keyedRow 0 :~: 'B.KeyedRow 'e' 'V.VNil 'V.VNil
empty = Refl

row :: B.keyedRow 4 :~: 'B.KeyedRow 'r' ('V.VCons 'x' ('V.VCons 'y' 'V.VNil))
                                        ('V.VCons 4 ('V.VCons 5 'V.VNil))
row = Refl

margins :: B.tableMargins 0 :~: 'B.TableMargins 'V.VNil ('V.VCons 'x' 'V.VNil)
                                                'V.VNil ('V.VCons 0 'V.VNil)
margins = Refl

type Two x y = 'V.VCons x ('V.VCons y 'V.VNil)
type Three x y z = 'V.VCons x (Two y z)

table :: T.sample 4 :~: 'T.Table (Two 'r' 's') (Three 'x' 'y' 'z')
                                (Two (Three 1 2 3) (Three 4 5 6))
table = Refl

unlabelled :: T.withoutRowLabels (T.sample 4) :~: 'T.ColumnTable
                (Three 'x' 'y' 'z') (Two (Three 1 2 3) (Three 4 5 6))
unlabelled = Refl

squareProof :: T.square 1 :~: 'T.Square (Two (Two 1 2) (Two 3 4))
squareProof = Refl

emptySquare :: T.square 0 :~: 'T.Square 'V.VNil
emptySquare = Refl

matrixProof :: T.matrix (T.sample 4) :~: 'T.Matrix
                (Two (Three 1 2 3) (Three 4 5 6)) (Three 'x' 'y' 'z')
matrixProof = Refl

emptyMatrix :: T.matrix (W.noRows 'z') :~: 'T.Matrix 'V.VNil ('V.VCons 'z' 'V.VNil)
emptyMatrix = Refl

labelsAfterCells :: W.labelsLast 4 :~: 'W.LabelledLast
                    (Three 'x' 'y' 'z') (Two (Three 1 2 3) (Three 4 5 6)) (Two 'r' 's')
labelsAfterCells = Refl

unlabelledEmpty :: T.withoutRowLabels (W.noRows 'z') :~: 'T.ColumnTable ('V.VCons 'z' 'V.VNil) 'V.VNil
unlabelledEmpty = Refl

unlabelledNoCols :: T.withoutRowLabels (W.noCols 's') :~: 'T.ColumnTable 'V.VNil ('V.VCons 'V.VNil 'V.VNil)
unlabelledNoCols = Refl

emptyRows :: W.noRows 'z' :~: 'T.Table 'V.VNil ('V.VCons 'z' 'V.VNil) 'V.VNil
emptyRows = Refl

emptyCols :: W.noCols 's' :~: 'T.Table ('V.VCons 's' 'V.VNil) 'V.VNil ('V.VCons 'V.VNil 'V.VNil)
emptyCols = Refl

totals :: T.totals (T.sample 4) :~: 'B.TableMargins (Two 'r' 's') (Three 'x' 'y' 'z')
                                                   (Two 6 15) (Three 5 7 9)
totals = Refl

firstWidthProof :: B.firstWidth (B.partial 9) :~: 1
firstWidthProof = Refl

rowWidthProof :: B.rowWidth (B.partialRow 9) :~: 1
rowWidthProof = Refl

skipped :: [Int]
skipped = B.selectedValues (B.MaskedRow (E.SS E.SZ) (V.VCons (error "unused value") V.VNil)
                            (error "unused label") (V.VCons False V.VNil))

main :: IO ()
main = do
  case (unlabelled, squareProof, emptySquare, matrixProof, emptyMatrix, labelsAfterCells, unlabelledEmpty, unlabelledNoCols) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> pure ()
  print (evenBatch, oddBatch, mask, firstWidthProof, rowWidthProof,
         B.firstWidth (B.build 4), B.firstWidth (B.build 3), B.firstWidth (B.partial 9),
         B.selectedValues (B.maskedRow 4), skipped)
  let short :: B.KeyedRow Int
      short = B.KeyedRow (error "unused tag") (V.VCons 'x' V.VNil)
                         (V.VCons 8 (error "unused tail"))
  print (row, empty, B.lookupColumn 'y' (B.keyedRow 4), B.lookupColumn 'x' short)
  let partial :: B.TableMargins Int
      partial = B.TableMargins (error "unused row labels") (V.VCons 'x' V.VNil)
                               (error "unused row totals") (V.VCons 7 V.VNil)
  print (margins, B.lookupTotals 'r' 'y' (B.tableMargins 4), snd (B.lookupTotals 'r' 'x' partial))
  let noRows = T.Table V.VNil (V.VCons 'z' V.VNil) V.VNil
      noCols = T.Table (V.VCons 's' V.VNil) V.VNil (V.VCons V.VNil V.VNil)
  print (table, emptyRows, emptyCols, totals,
         T.selectedTotals (T.sample 7), T.selectedTotals noRows, T.selectedTotals noCols)

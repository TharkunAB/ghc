{-# LANGUAGE DataKinds, EmptyCase, GADTs, NoDependentTypes #-}
module NativeGadtRetainedSumCoverage where

import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider

knownZero :: DirectVec a 'Z -> Bool
knownZero (DirectVec Z _) = True

knownSuccessor :: DirectVec a ('S n) -> a
knownSuccessor (DirectVec (S _) (VCons x _)) = x

unknownZero :: DirectVec a n -> Bool
unknownZero (DirectVec Z _) = True

knownUnreachable :: DirectVec a 'Z -> Bool
knownUnreachable (DirectVec (S _) _) = False
knownUnreachable (DirectVec _ _) = True

knownMatrix :: Matrix a 'Z ('S columns) -> Bool
knownMatrix (Matrix Z (S _) _) = True
knownMatrix (MatrixByColumns (S _) Z _) = True

knownMarked :: MarkedRows a ('S rows) 'True -> a
knownMarked (MarkedRows True (S _) (VCons x _)) = x

knownSquare :: SquareMatrix a 'Z 'Z -> Bool
knownSquare (Square Z _) = True

disjointSquare :: SquareMatrix a 'Z ('S 'Z) -> ()
disjointSquare x = case x of {}

disjointSquareMatch :: SquareMatrix a 'Z ('S 'Z) -> ()
disjointSquareMatch (Square _ _) = ()

impossibleSuccessor :: SuccVec a 'Z -> ()
impossibleSuccessor x = case x of {}

knownPredecessor :: SuccVec a ('S 'Z) -> Bool
knownPredecessor (SuccVec Z _) = True

unknownPredecessor :: SuccVec a n -> Bool
unknownPredecessor (SuccVec Z _) = True

knownTile :: Tile a ('Rectangle 'Z ('S 'Z)) -> Bool
knownTile (Tile Z (S _) _) = True

unknownTile :: Tile a shape -> Bool
unknownTile (Tile Z _ _) = True

knownNested :: DirectVec a ('S n) -> a
knownNested (DirectVec _ (VCons x _)) = x

knownDifferentFields :: Blocked a ('Block ('S n) mark 'Z) -> a
knownDifferentFields (Blocked _ _ _ (VCons x _) VNil) = x

unknownNested :: Blocked a ('Block n mark depth) -> a
unknownNested (Blocked _ _ _ (VCons x _) _) = x

knownNestedBool :: Blocked Bool ('Block ('S n) mark depth) -> Bool
knownNestedBool (Blocked _ _ _ (VCons True _) _) = True

knownNestedInaccessible :: Blocked a ('Block 'Z mark depth) -> Bool
knownNestedInaccessible (Blocked _ _ _ (VCons _ _) _) = False
knownNestedInaccessible (Blocked _ _ _ _ _) = True

missingNamed :: Phantom -> Bool
missingNamed (Pair Z _) = False

missingApplied :: Packed Bool -> Bool
missingApplied (Pair Z _) = False

missingOrdinary :: OrdinaryOptional Bool -> Bool
missingOrdinary (Just _) = False

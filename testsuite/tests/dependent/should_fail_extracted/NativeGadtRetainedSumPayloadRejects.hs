{-# LANGUAGE BangPatterns, DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
module NativeGadtRetainedSumPayloadRejects where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider

badOuter :: Nested Bool
badOuter = makeNested (S Z) Z (VCons True VNil) VNil VNil

badInner :: Nested Bool
badInner = makeNested (S Z) (S Z) (VCons True VNil) (VCons False VNil) VNil

badBranch :: Later a b -> Vec a requested
badBranch (Later _ (Pair _ (xs, _))) = xs

badEscape :: Packed Bool -> Vec Bool caller
badEscape (Pair _ xs) = forward xs

badIndependent :: Packed Bool -> Packed Bool -> Bool
badIndependent (Pair _ xs) (Pair _ ys) = sameLength xs ys

badElement :: DirectVec a n -> Vec b n -> DirectVec a n
badElement (DirectVec n _) xs = DirectVec n xs

badLength :: DirectVec a n -> Vec a m -> DirectVec a n
badLength (DirectVec n _) xs = DirectVec n xs

crossed :: DirectVec a n -> DirectVec a m -> Vec a m
crossed (DirectVec !size xs) right = case right of
  DirectVec !size _ -> xs

wrongDirect :: DirectVec a n -> Maybe (n :~: 'Z)
wrongDirect (DirectVec (S _) _) = Just Refl
wrongDirect (DirectVec Z _) = Just Refl

wrongTile :: Tile a ('Rectangle 'Z ('S 'Z)) -> Vec (Vec a 'Z) ('S 'Z)
wrongTile (Tile _ _ xs) = xs

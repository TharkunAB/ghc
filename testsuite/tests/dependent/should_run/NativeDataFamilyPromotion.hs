{-# LANGUAGE Haskell2010, NoDependentTypes, DataKinds, GADTs, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import NativeDataFamilyStrictCheck (runStrictCheck)
import NativeDataFamilyTuplePromotion (Selected, Refined, proof, refinementProof, pairIndex, reflected)
import RetainedConstructor (emitted, lazy, withNatural, withRefinement)
import RetainedTuple (keep, observe, reflexive)
import TupleProvider (Tuple(Tup2), At(..), fields)

proofAfterImport :: Selected :~: 'True
proofAfterImport = Refl

refinementAfterImport :: Refined :~: 'True
refinementAfterImport = Refl

seen :: Tuple Bool Bool -> (Bool, Bool)
seen t = case observe t of (At, pair) -> pair

lazyTuple :: Tuple Bool Bool -> Bool
lazyTuple t = case keep t of At -> case reflexive t of Refl -> True

main :: IO ()
main = case (proof, proofAfterImport, refinementProof, refinementAfterImport, pairIndex) of
    (Refl, Refl, Refl, Refl, At) -> do
        print (fields (Tup2 True False))
        putStrLn reflected
        print (seen (Tup2 True False), seen (Tup2 False True), lazyTuple (error "opaque tuple was forced"))
        strictRes <- runStrictCheck
        print strictRes
        print (snd (emitted ()), snd (withNatural ()),
               snd (observe (Tup2 False True)),
               lazy (error "constructor field was demanded"),
               snd (withRefinement ()))

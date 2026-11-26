{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, MagicHash, StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeDataFamilyTuplePromotion (Selected, Refined, proof, refinementProof, pairIndex, reflected) where

import Data.Type.Equality ((:~:)(Refl))
import Type.Reflection (typeRep)
import TupleProvider

type Inspect :: Tuple Bool Bool -> Bool
type family Inspect x where
    Inspect ('Tup2 'True b) = b
    Inspect ('Tup2 'False _) = 'False

type Selected = Inspect ('Tup2 'True 'True)

proof :: Selected :~: 'True
proof = Refl

type family HasRefinement (x :: C (Int, Bool)) :: Bool where
    HasRefinement 'Refines = 'True

type Refined = HasRefinement ('Refines :: C (Int, Bool))

refinementProof :: Refined :~: 'True
refinementProof = Refl

pairIndex :: At ('Tup2 'True 'False :: Tuple Bool Bool)
pairIndex = At

_emptyIndex :: At ('Tup0 :: Tuple)
_emptyIndex = At

_singleIndex :: At ('Tup1 'True :: Tuple Bool)
_singleIndex = At

reflected :: String
reflected = show (typeRep @('Tup2 'True 'False :: Tuple Bool Bool))

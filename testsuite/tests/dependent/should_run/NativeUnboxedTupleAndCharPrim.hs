{-# LANGUAGE DataKinds, MagicHash, NoDependentTypes, TypeOperators, UnboxedTuples #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import NativeUnboxedTupleAndCharPrimProvider
import NativeUnboxedTupleNoPuns (p1, p2)

clientEq :: EqCharLit 'a' :~: 'True
clientEq = Refl

clientFst :: FstU PairSyn :~: 'True
clientFst = Refl

clientSwap :: FstU (SwapU PairSyn) :~: 'x'
clientSwap = Refl

main :: IO ()
main =
  case ( charLitRefl
       , eqCharHit
       , eqCharMiss
       , eqCharOther
       , matchCharVarProof
       , matchCharWildProof
       , matchCharAnnotProof
       , fstUProof
       , sndUProof
       , swapUFstProof
       , swapUSndProof
       , clientEq
       , clientFst
       , clientSwap
       , p1
       , p2
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> pure ()

{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeIndexedConsProofBad where
import NativeIndexedConsProof (Seen(..), see)
import qualified PackedVecLibrary as P

seenCons :: forall n. foreach (x :: Bool) -> foreach (xs :: P.Vec Bool n) -> Seen ('P.VCons x xs)
seenCons x xs = SeenCons @n @x @xs

badHead :: forall (n :: P.Nat). foreach (input :: P.Vec Bool n) -> Seen input
badHead input = case input of
    P.VNil -> SeenNil
    P.VCons _ xs -> seenCons False xs

badIndep :: foreach (left :: P.Vec Bool ('P.S 'P.Z)) ->
            foreach (right :: P.Vec Bool ('P.S 'P.Z)) -> (Seen left, Seen right)
badIndep left _ = case left of
    P.VCons x xs -> (seenCons x xs, seenCons x xs)

{-# NOINLINE seeOperand #-}
seeOperand :: forall n. foreach (input :: P.Vec Bool n) -> Seen input
seeOperand input = see input

badWidth :: Seen 'P.VNil
badWidth = seeOperand @'P.Z (P.VCons True P.VNil)

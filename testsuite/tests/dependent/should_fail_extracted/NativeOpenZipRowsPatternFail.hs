{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, KindSignatures #-}
module NativeOpenZipRowsPatternFail where

import PackedVecLibrary (Vec(..))
import ZipLengthSource (zipLength)

shadowed :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
         -> Vec (a, b) (zipLength xs ys)
shadowed [] _ = VNil
shadowed [] [] = VNil
shadowed (_ : _) [] = VNil
shadowed (x : xs) (y : ys) = VCons (x, y) (shadowed xs ys)

data Phase = Empty | First | Last
data Packet (phase :: Phase) (value :: Bool) = Packet

missingPair :: foreach (phase :: Phase) -> foreach (value :: Bool) -> Packet phase value
missingPair Empty _ = Packet
missingPair First False = Packet
missingPair First True = Packet
missingPair Last False = Packet

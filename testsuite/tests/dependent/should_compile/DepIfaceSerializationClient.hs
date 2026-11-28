{-# LANGUAGE DependentTypes #-}
module DepIfaceSerializationClient where

import Data.Type.Equality ((:~:)(Refl))
import DepIfaceSerialization

flipProof :: flipBit False :~: True
flipProof = Refl

monusProof :: foreach (m :: Nat) -> foreach (n :: Nat)
           -> monus (Succ m) (Succ n) :~: monus m n
monusProof _ _ = Refl

replaceProof :: foreach (e :: Expr Empty Base)
             -> replace (At New) e :~: e
replaceProof _ = Refl

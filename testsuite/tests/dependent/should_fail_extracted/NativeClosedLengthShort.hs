{-# LANGUAGE DataKinds, DependentTypes, GADTs, KindSignatures, ListTuplePuns #-}
{-# LANGUAGE TypeOperators #-}
module NativeClosedLengthShort where

import ClosedLengthProducer (length)
import GHC.TypeNats (Nat, type (+))
import Prelude hiding (length)

data Vec (n :: Nat) a where
  VNil :: Vec 0 a
  (:.) :: a -> Vec n a -> Vec (n + 1) a

infixr 5 :.

v :: Vec (length [1,2,3]) Bool
v = (True :. False :. VNil) :: Vec 2 Bool

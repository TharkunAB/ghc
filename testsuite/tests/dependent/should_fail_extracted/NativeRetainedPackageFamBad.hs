{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeRetainedPackageFamBad where
import Data.Kind (Type)
import PackedVecLibrary (Nat(..), PackedVec(..), Vec(..))
import RetainedNat (Retained)

type family Calculate (n :: Nat) :: Type
type WrongFamily = Retained Calculate

data Shifted (p :: Nat -> Type) where
    Shift :: foreach (n :: Nat) -> p ('S n) -> Shifted p
type family ShiftWitness (q :: Shifted p) :: Nat where
    ShiftWitness ('Shift n _) = n

type WrongIdx = 'PackVec 'Z ('VCons 'True 'VNil)

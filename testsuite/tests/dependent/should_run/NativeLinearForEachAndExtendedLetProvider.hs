{-# LANGUAGE DependentTypes, LinearTypes #-}
module NativeLinearForEachAndExtendedLetProvider
  ( Nat (..)
  , Vec (..)
  , SingBool (..)
  , LinNat (..)
  , SingLinNat (..)
  , vCons1
  , vSumLinear
  , idRetainedLinear
  , linNatSingleton
  , dupInTypeLinearInTerm
  , idRetainedMany
  , idRetainedPolyMult
  , idRetainedImplicitMult
  , pairRetainedLinear
  , unannotLinear
  , vacuousLinear
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Tuple (Tuple2)
import GHC.Types (Multiplicity (..))

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil  :: Vec a Zero
  VCons :: a %1 -> Vec a n %1 -> Vec a (Succ n)

type SingBool :: Bool -> Type
data SingBool b where
  STrue  :: SingBool True
  SFalse :: SingBool False

data LinNat where
  LZero :: LinNat
  LSucc :: LinNat %1 -> LinNat

type SingLinNat :: LinNat -> Type
data SingLinNat n where
  SLZero :: SingLinNat LZero
  SLSucc :: SingLinNat k %1 -> SingLinNat (LSucc k)

vCons1 :: foreach (n :: Nat) -> a %1 -> Vec a n %1 -> Vec a (Succ n)
vCons1 _ x xs = VCons x xs

vSumLinear :: Vec Int n -> Int
vSumLinear VNil         = 0
vSumLinear (VCons x xs) = x + vSumLinear xs

idRetainedLinear :: foreach (x %1 :: Bool) -> SingBool x
idRetainedLinear True  = STrue
idRetainedLinear False = SFalse

linNatSingleton :: foreach (n %1 :: LinNat) -> SingLinNat n
linNatSingleton LZero     = SLZero
linNatSingleton (LSucc k) = SLSucc (linNatSingleton k)

dupInTypeLinearInTerm :: foreach (x %1 :: Nat) -> Proxy (x, x) -> Nat
dupInTypeLinearInTerm x Proxy = x

idRetainedMany :: foreach (x %Many :: Bool) -> Tuple2 Bool (SingBool x)
idRetainedMany x = (x, idRetainedLinear x)

idRetainedPolyMult :: forall (m :: Multiplicity). foreach (x %m :: Nat) -> Proxy (x, x) -> Nat
idRetainedPolyMult x Proxy = x

idRetainedImplicitMult :: foreach (x %(m :: Multiplicity) :: Nat) -> Proxy x -> Nat
idRetainedImplicitMult x Proxy = x

pairRetainedLinear :: foreach (x y %1 :: Nat) -> Proxy (x, y) -> Tuple2 Nat Nat
pairRetainedLinear x y Proxy = (x, y)

unannotLinear :: foreach (x %1) -> Proxy (x :: Nat) -> Nat
unannotLinear x Proxy = x

vacuousLinear :: foreach (_ %1 :: Nat) -> Int %1 -> Tuple2 Nat Int
vacuousLinear n k = (n, k)

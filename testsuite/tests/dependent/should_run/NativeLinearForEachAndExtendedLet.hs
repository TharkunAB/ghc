{-# LANGUAGE DependentTypes, LinearTypes #-}
module Main (main) where

import Data.Proxy (Proxy (..))
import GHC.Tuple (Tuple2, Unit)
import GHC.Types (Multiplicity (..))
import NativeLinearForEachAndExtendedLetProvider
import NativeStandaloneExtendedLet (polySyn)

singBoolToBool :: SingBool b -> Bool
singBoolToBool STrue  = True
singBoolToBool SFalse = False

singLinNatRoundtrip :: SingLinNat n %1 -> LinNat
singLinNatRoundtrip SLZero     = LZero
singLinNatRoundtrip (SLSucc s) = LSucc (singLinNatRoundtrip s)

linNatToInt :: LinNat -> Int
linNatToInt LZero     = 0
linNatToInt (LSucc k) = 1 + linNatToInt k

consumeLinearNat :: Nat %1 -> Nat
consumeLinearNat n = dupInTypeLinearInTerm n Proxy

consumePolyOne :: Nat %1 -> Nat
consumePolyOne n = idRetainedPolyMult @One n Proxy

consumePolyMany :: Nat -> Tuple2 Nat Nat
consumePolyMany n = (idRetainedPolyMult @Many n Proxy, n)

letPairCheck :: Int
letPairCheck =
  let type PairOf a = Tuple2 a a
      type Quad a = PairOf (PairOf a)
      x :: PairOf Int
      x = (10, 20)
      y = ((3, 4) :: PairOf Int)
      q :: Quad Int
      q = (x, y)
      (p1, p2) = id @(PairOf (PairOf Int)) q
  in fst p1 + snd p1 + fst p2 + snd p2

letTypeOnlyCheck :: Int
letTypeOnlyCheck =
  let type Endo a = a -> a
  in ((\z -> z + 1) :: Endo Int) 41

whereRetainedIndexCheck :: forall a. foreach (n :: Nat) -> a -> Vec a n -> Tuple2 a (Vec a n)
whereRetainedIndexCheck n def xs = (def, helper xs)
  where
    type LocalEnv = Vec a n
    type TaggedVec b = Tuple2 b (Vec b n)
    helper :: LocalEnv -> LocalEnv
    helper ys = snd (tagger ys)
    tagger :: Vec a n -> TaggedVec a
    tagger zs = (def, zs)

doAndInfixSynCheck :: IO Int
doAndInfixSynCheck = do
  let type a :+: b = Either a b
  let v1 :: Int :+: Bool
      v1 = Left 7
  let v2 :: Int :+: Bool
      v2 = Right True
  let decode :: (Int :+: Bool) -> Int
      decode (Left k)  = k
      decode (Right b) = if b then 8 else 0
  pure (decode v1 + decode v2)

main :: IO Unit
main = polySyn (Proxy @Int) `seq` do
  let v1 = vCons1 Zero 10 VNil
      v2 = vCons1 (Succ Zero) 25 v1
      b1 = singBoolToBool (idRetainedLinear True)
      b2 = singBoolToBool (idRetainedLinear False)
      ln = linNatToInt (singLinNatRoundtrip (linNatSingleton (LSucc (LSucc LZero))))
      c1 = consumeLinearNat (Succ (Succ Zero))
      (mb, ms) = idRetainedMany True
      p1 = consumePolyOne (Succ Zero)
      p2 = consumePolyMany (Succ (Succ (Succ Zero)))
      im = idRetainedImplicitMult @One (Succ Zero) Proxy
      pr = pairRetainedLinear Zero (Succ Zero) Proxy
      ua = unannotLinear (Succ (Succ Zero)) Proxy
      vc = vacuousLinear Zero 99
      (_, wv) = whereRetainedIndexCheck (Succ (Succ Zero)) (0 :: Int) v2
  doRes <- doAndInfixSynCheck
  print
    ( vSumLinear wv
    , (b1, b2, mb, singBoolToBool ms)
    , ln
    , (c1, p1, p2, im, pr, ua, vc)
    , (letPairCheck, letTypeOnlyCheck, doRes)
    )

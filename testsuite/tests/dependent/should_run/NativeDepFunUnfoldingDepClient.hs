{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables #-}
module NativeDepFunUnfoldingDepClient (runDepClient) where

import Data.Proxy (Proxy (..))
import NativeDepFunUnfoldingProvider

useHeadLazy :: Int
useHeadLazy =
  let badNat :: Nat
      badNat = error "strict runtime Nat index"
      badTail :: Vec Int badNat
      badTail = error "strict runtime Vec tail"
      vec = inlVarApp badNat 42 badTail
      vec1 = inlAliasDefault (S Z) (inlVarApp Z (inlVecHead badNat vec) VNil)
  in inlVecHead Z vec1

usePackLazy :: Int
usePackLazy =
  let badVal = error "strict packed payload"
      (_, packed) = inlPackVec True badVal
  in inlUnpackSum False Proxy packed

usePackActive :: Int
usePackActive =
  let (_, packed) = inlPackVec True 20
  in inlUnpackSum True Proxy packed

useBoolAndTop :: Int
useBoolAndTop =
  let n :: Int
      n = inlBoolCase True 9
  in snd (inlTopIndex True n)

useScopedAndInlinable :: (Int, Int, Int, Int)
useScopedAndInlinable =
  let flag = True
      t1 = case inlScopedDict flag 42 of Tagged v -> v
      v1 = inlinableScaleHead Z (3 :: Int) (VCons 40 VNil)
      t2 = case inlinableScopedDict flag of Tagged v -> v
      s  = recVecSum (S (S Z)) (VCons 2 (VCons 4 VNil))
  in (t1, v1, t2, s)

runDepClient :: [Int]
runDepClient =
  let (a, b, c, d) = useScopedAndInlinable
  in [useHeadLazy, usePackLazy, usePackActive, useBoolAndTop, a, b, c, d]

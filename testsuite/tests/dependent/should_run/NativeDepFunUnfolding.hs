{-# LANGUAGE Haskell2010, NoDependentTypes, DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}
module Main (main) where

import Data.Proxy (Proxy (..))
import NativeDepFunUnfoldingDepClient (runDepClient)
import NativeDepFunUnfoldingProvider

lazyHeadH2010 :: Int
lazyHeadH2010 =
  let go :: Nat -> Int
      go badNat =
        let badTail = error "strict H2010 tail"
        in inlVecHead badNat (inlVarApp badNat 42 badTail)
  in go (error "strict H2010 Nat index")

lazyPackH2010 :: Int
lazyPackH2010 =
  let badPayload = error "strict H2010 payload"
      (_, packed) = inlPackVec True badPayload
  in inlUnpackSum False Proxy packed

activePackH2010 :: Int
activePackH2010 =
  let (_, packed) = inlPackVec True 20
  in inlUnpackSum True Proxy packed

topIdxH2010 :: Int
topIdxH2010 = snd (inlTopIndex True 10)

scopedAndSpecH2010 :: [Int]
scopedAndSpecH2010 =
  let flag = True
      a = case inlScopedDict flag 42 of Tagged v -> v
      b = inlinableScaleHead Z (3 :: Int) (VCons 40 VNil)
      c = case inlinableScopedDict flag of Tagged v -> v
      d = recVecSum (S (S Z)) (VCons 2 (VCons 4 VNil))
  in [a, b, c, d]

main :: IO ()
main = do
  let h2010Vals = [lazyHeadH2010, lazyPackH2010, activePackH2010, topIdxH2010] ++ scopedAndSpecH2010
  if runDepClient == h2010Vals
    then mapM_ print h2010Vals
    else error ("Mismatch: " ++ show (runDepClient, h2010Vals))

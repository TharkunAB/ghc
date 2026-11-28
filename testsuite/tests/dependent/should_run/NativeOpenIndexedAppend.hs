{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (ErrorCall(..), SomeException, evaluate, try)
import Control.Monad (unless, void)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenIndexedAppendExtra (extraProofsOk)
import OpenVectorAppend (appendConsAt, appendEmptyAt, appendEmptyAtBoth)
import OrdinaryOpenVectorAppend
  ( appendContents, consContents, observeAppendCons, observeAppendEmpty, observeAppendPair )
import qualified PackedVecLibrary as P

raises :: a -> IO Bool
raises value = do
  result <- try (void (evaluate value)) :: IO (Either SomeException ())
  pure (case result of Left _ -> True; Right _ -> False)

pairAtZero :: P.Vec Bool 'P.Z -> P.Vec Bool 'P.Z -> Bool
pairAtZero xs ys = case appendEmptyAtBoth P.Z xs ys of
  (Refl, Refl) -> True

pairAtOne :: P.Vec Bool ('P.S 'P.Z) -> P.Vec Bool ('P.S 'P.Z) -> Bool
pairAtOne xs ys = case appendEmptyAtBoth (P.S P.Z) xs ys of
  (Refl, Refl) -> True

pairAtTwo :: P.Vec Bool ('P.S ('P.S 'P.Z)) -> P.Vec Bool ('P.S ('P.S 'P.Z)) -> Bool
pairAtTwo xs ys = case appendEmptyAtBoth (P.S (P.S P.Z)) xs ys of
  (Refl, Refl) -> True

main :: IO ()
main = do
  unless extraProofsOk (fail "extra proofs failed")
  xs <- newIORef (P.VCons True (P.VCons False P.VNil)) >>= readIORef
  let proofOnly :: P.Vec Bool n -> Bool
      proofOnly v = case appendEmptyAt v of Refl -> True
  print (observeAppendEmpty, appendContents xs, proofOnly (error "forced proof input" :: P.Vec Bool 'P.Z))
  putStrLn observeAppendCons
  print (consContents False P.VNil (P.VCons True P.VNil))
  let bottom = error "forced vector" :: P.Vec Bool 'P.Z
      bottomOne = error "forced vector" :: P.Vec Bool ('P.S 'P.Z)
      bottomTwo = error "forced vector" :: P.Vec Bool ('P.S ('P.S 'P.Z))
  zero <- newIORef P.Z >>= readIORef
  one <- newIORef (P.S (error "forced predecessor")) >>= readIORef
  cut <- try (evaluate (observeAppendPair (error "forced cut")))
  let demandedCut = case cut of
        Left (ErrorCall "forced cut") -> True
        _ -> False
  print (pairAtZero bottom bottom, pairAtOne bottomOne bottomOne, pairAtTwo bottomTwo bottomTwo,
         observeAppendPair zero, observeAppendPair one, demandedCut)
  let consProof :: a -> P.Vec a n -> P.Vec a m -> Bool
      consProof hd tl ys = case appendConsAt hd tl ys of Refl -> True
  unless (consProof (error "forced proof head") bottom bottom) (fail "missing recursive proof")
  left <- raises (P.appendVec bottom P.VNil)
  unless left (fail "append did not inspect its left operand")
  case P.appendVec (P.VCons (error "forced head") bottom) bottom of
    P.VCons headValue tailValue -> do
      headDemand <- raises headValue
      leftTailDemand <- raises tailValue
      unless (headDemand && leftTailDemand) (fail "append changed left field demand")
  case P.appendVec (P.VCons True P.VNil) bottom of
    P.VCons True tailValue -> do
      rightDemand <- raises tailValue
      unless rightDemand (fail "append changed right field demand")
    P.VCons False _ -> fail "append changed the head"

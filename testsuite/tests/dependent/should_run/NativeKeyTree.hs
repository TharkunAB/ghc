{-# LANGUAGE GADTs, NoDependentTypes #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeKeyTreeProofFirst as PF
import qualified NativeKeyTreeTypesSplice as TS
import qualified NativeKeyTreeWorkersSplice as WS
import NativeKeyTreeUncheckedNodes ()
import SameGroupKeyTreeProof

main :: IO ()
main = do
  let tree :: Tree Int Bool
      tree = Branch (error "forced key") (Tip False)
               (Branch 7 (Tip (error "forced leaf")) (Tip True))
      bad :: Bool -> Char
      bad _ = error "forced callback"
      change :: Bool -> Char
      change True = 'y'
      change False = 'n'
  case (PF.nodesMap bad (PF.Tip True), TS.nodesMap bad (TS.Tip True), WS.nodesMap bad (WS.Tip True)) of
    (Refl, Refl, Refl) -> do
      print (forkCount tree)
      print (nodesMap bad tree)
      print $ case mapValues change (Branch 9 (Tip True) (Tip False)) of
        Branch k (Tip a) (Tip b) -> (k :: Int, a, b)
        _ -> error "wrong shape"

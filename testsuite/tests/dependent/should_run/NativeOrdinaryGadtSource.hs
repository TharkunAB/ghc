{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import NativeOrdinaryGadtSourceMixed (mixed)
import NativeOrdinaryGadtSourceTree

open :: foreach (x :: Tree) -> focus ('Join ('Arrow x 'Empty) 'Other) :~: 'Join x 'Other
open _ = Refl

main :: IO ()
main = case (open (Arrow Empty Other), mixed True (False, EQ)) of
  (Refl, Refl) -> case pairRoundTrip (False, EQ) of
    Refl -> do
      let bottom = error "forced the second argument" :: Tree
          node = Join bottom bottom
      if not (fst (later False False bottom)) && fst (later True False node)
        then putStrLn "Refl"
        else fail "unexpected result from the later column"

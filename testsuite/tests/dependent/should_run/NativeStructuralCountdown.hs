{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import NativeStructuralCountdownLib

next :: foreach (n :: Step) -> countdown ('Next n) :~: (n ': countdown n)
next _ = Refl

main :: IO ()
main = case (empty, next Zero, countdown (Next (error "predecessor"))) of
  (Refl, Refl, _ : _) -> putStrLn "Refl"
  _ -> fail "expected a cons"

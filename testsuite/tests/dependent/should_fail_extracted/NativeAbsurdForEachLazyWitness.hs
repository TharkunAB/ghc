{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module NativeAbsurdForEachLazyWitness where
import Data.Type.Equality ((:~:))
data Nat = Zero | Succ Nat
lazyWitnessBad :: foreach (m n :: Nat) -> (m :~: n) -> Int
lazyWitnessBad Zero     Zero     _ = 0
lazyWitnessBad (Succ _) (Succ _) _ = 1

{-# LANGUAGE GHC2024, NoDependentTypes #-}
module CoercionForallWitness where

type role Index nominal
data Index (a :: k) = Index Int

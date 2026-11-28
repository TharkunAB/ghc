{-# LANGUAGE DataKinds, NoDependentTypes, ExplicitForAll, GADTs, KindSignatures #-}
module NativeGadtImplicitSourceOrdinary where

data Tree = Tip | Fork Tree

worker :: Tree -> Tree
worker _ = error "no checked equation"

data Edge (a :: Tree) (b :: Tree) where
  Step :: Edge a (worker a)

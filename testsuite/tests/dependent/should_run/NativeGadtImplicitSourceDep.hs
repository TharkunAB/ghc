{-# LANGUAGE DataKinds, DependentTypes, ExplicitForAll, GADTs, KindSignatures #-}
module NativeGadtImplicitSourceDep where

data Tree = Tip | Fork Tree

worker :: Tree -> Tree
worker Tip = Tip
worker (Fork n) = n

data Edge (a :: Tree) (b :: Tree) where
  Step :: Edge a (worker a)

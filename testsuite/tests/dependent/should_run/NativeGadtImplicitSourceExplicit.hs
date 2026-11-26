{-# LANGUAGE DataKinds, DependentTypes, ExplicitForAll, GADTs, KindSignatures #-}
module NativeGadtImplicitSourceExplicit where

data Tree = Tip | Fork Tree

worker :: Tree -> Tree
worker _ = error "no checked equation"

data Edge (a :: Tree) (b :: Tree) where
  Step :: forall (worker :: Tree -> Tree) a. Edge a (worker a)

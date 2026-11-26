{-# LANGUAGE DataKinds, DependentTypes, NoRequiredTypeArguments, ExplicitForAll, GADTs, KindSignatures #-}
module NativeGadtImplicitSourceNoReq where

data Tree = Tip | Fork Tree

worker :: Tree -> Tree
worker Tip = Tip
worker (Fork n) = n

data Edge (a :: Tree) (b :: Tree) where
  Step :: Edge a (worker a)

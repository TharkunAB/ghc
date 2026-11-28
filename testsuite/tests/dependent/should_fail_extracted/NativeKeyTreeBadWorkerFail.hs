{-# LANGUAGE DependentTypes, GADTs, LambdaCase, ScopedTypeVariables,
             TypeApplications, TypeOperators #-}
module NativeKeyTreeBadWorkerFail where

import Data.Type.Equality ((:~:)(Refl), gcastWith)

data Tree k v = Tip v | Branch k (Tree k v) (Tree k v)
data Size = Zero | Succ Size

plus :: Size -> Size -> Size
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

mapValues :: forall b k a. (a -> b) -> Tree k a -> Tree k b
mapValues f (Tip x) = Tip (f x)
mapValues f (Branch k l r) = Branch k (mapValues f l) (mapValues f r)

nodesLoop :: forall a k. Tree k a -> Size
nodesLoop (Tip _) = Zero
nodesLoop (Branch k l r) = plus (nodesLoop @a @k (Branch k l r)) (Succ (nodesLoop @a @k r))

nodesMapLoop :: forall k a b. foreach (f :: a -> b) -> foreach (t :: Tree k a)
             -> nodesLoop (mapValues f t) :~: nodesLoop t
nodesMapLoop _ _ = Refl

nodes :: forall a k. Tree k a -> Size
nodes (Tip _) = Zero
nodes (Branch _ l r) = plus (nodes @a @k l) (Succ (nodes @a @k r))

wrongMap :: forall k a b. foreach (f :: a -> b) -> foreach (t :: Tree k a)
         -> nodes (mapValues f t) :~: Succ (nodes t)
wrongMap f = \case
  Tip _ -> Refl
  Branch _ l r -> gcastWith (wrongMap f l) (gcastWith (wrongMap f r) Refl)

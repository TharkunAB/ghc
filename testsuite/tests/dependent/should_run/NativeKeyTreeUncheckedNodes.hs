{-# LANGUAGE DependentTypes, ScopedTypeVariables, TypeApplications #-}
module NativeKeyTreeUncheckedNodes (Tree(..), Size(..), mapValues, nodes, forkCount) where

data Tree k v = Tip v | Branch k (Tree k v) (Tree k v)
data Size = Zero | Succ Size

plus :: Size -> Size -> Size
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

mapValues :: forall b k a. (a -> b) -> Tree k a -> Tree k b
mapValues f (Tip x) = Tip (f x)
mapValues f (Branch k l r) = Branch k (mapValues f l) (mapValues f r)

nodes :: forall a k. Tree k a -> Size
nodes (Tip _) = Zero
nodes (Branch k l r) = plus (nodes @a @k (Branch k l r)) (Succ (nodes @a @k r))

forkCount :: Tree k v -> Int
forkCount = fromSize . nodes
  where
    fromSize Zero = 0
    fromSize (Succ n) = 1 + fromSize n

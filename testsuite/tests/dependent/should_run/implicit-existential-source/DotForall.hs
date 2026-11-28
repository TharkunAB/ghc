{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, RankNTypes #-}

module DotForall where

import Vector

type Rejected = exists (n :: Nat). forall a. Vec n a

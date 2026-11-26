{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs #-}
module BarProvider (Bar) where

import Vector

type Bar a = exists (n :: Nat) | Vec n a

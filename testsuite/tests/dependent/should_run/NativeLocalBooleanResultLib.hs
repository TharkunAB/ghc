{-# LANGUAGE DataKinds, DependentTypes, GADTs #-}
module NativeLocalBooleanResultLib
  ( Index(..), Syntax(..), Step(..), substitute ) where

data Index = Z | S Index
data Syntax = Var Index | Done | Nest Syntax

same :: Index -> Index -> Bool
same Z Z = True
same Z (S _) = False
same (S _) Z = False
same (S x) (S y) = same x y

substitute :: Syntax -> Index -> Syntax -> Syntax
substitute (Var x) y replacement = case same x y of
  True -> replacement
  False -> Var x
substitute Done _ _ = Done
substitute (Nest term) y replacement = Nest (substitute term y replacement)

data Step (name :: Index) (body :: Syntax) (argument :: Syntax) (result :: Syntax) where
  Reduce :: Step x t v (substitute t x v)
  Unfold :: forall x t. Step x t (Nest t) (substitute t x (Nest t))

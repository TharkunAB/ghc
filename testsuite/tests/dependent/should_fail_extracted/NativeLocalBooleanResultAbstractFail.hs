{-# LANGUAGE DataKinds, DependentTypes, GADTs #-}
module NativeLocalBooleanResultAbstractFail
  ( Index(..), Syntax, StepAbstract(..), subAbstract ) where

data Index = Z | S Index
data Syntax = Var Index | Done | Nest Syntax

same :: Index -> Index -> Bool
same Z Z = True
same Z (S _) = False
same (S _) Z = False
same (S x) (S y) = same x y

subAbstract :: Syntax -> Index -> Syntax -> Syntax
subAbstract (Var x) y replacement = case same x y of
  True -> replacement
  False -> Var x
subAbstract Done _ _ = Done
subAbstract (Nest term) y replacement = Nest (subAbstract term y replacement)

data StepAbstract (name :: Index) (body :: Syntax) (argument :: Syntax) (result :: Syntax) where
  ReduceA :: StepAbstract x t v (subAbstract t x v)

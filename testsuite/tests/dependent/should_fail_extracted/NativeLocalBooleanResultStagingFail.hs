{-# LANGUAGE DataKinds, DependentTypes, GADTs #-}
module NativeLocalBooleanResultStagingFail
  ( Index(..), Syntax(..), StepPartial(..), StepLoop(..), subPartial, subLoop ) where

data Index = Z | S Index
data Syntax = Var Index | Done | Nest Syntax

same :: Index -> Index -> Bool
same Z Z = True
same Z (S _) = False
same (S _) Z = False
same (S x) (S y) = same x y

subPartial :: Syntax -> Index -> Syntax -> Syntax
subPartial (Var x) y replacement = case same x y of
  True -> replacement
  False -> Var x
subPartial (Nest term) y replacement = Nest (subPartial term y replacement)

subLoop :: Syntax -> Index -> Syntax -> Syntax
subLoop (Var x) y replacement = case same x y of
  True -> replacement
  False -> Var x
subLoop Done _ _ = Done
subLoop (Nest term) y replacement = Nest (subLoop (Nest term) y replacement)

data StepPartial (name :: Index) (body :: Syntax) (argument :: Syntax) (result :: Syntax) where
  ReduceP :: StepPartial x t v (subPartial t x v)

data StepLoop (name :: Index) (body :: Syntax) (argument :: Syntax) (result :: Syntax) where
  ReduceL :: StepLoop x t v (subLoop t x v)

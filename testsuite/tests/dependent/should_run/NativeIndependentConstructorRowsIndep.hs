{-# LANGUAGE DependentTypes #-}
module NativeIndependentConstructorRowsIndep (Term(..), Label(..), decorate) where

data Term = Empty | Atom Label | Next Term
data Label = Here | There Label

{-# NOINLINE matches #-}
matches :: Label -> Label -> Bool
matches Here Here = True
matches Here (There _) = False
matches (There _) Here = False
matches (There _) (There _) = True

decorate :: Term -> Label -> Term -> Term
decorate Empty _ _ = Empty
decorate (Atom x) label value = case matches x label of
  True -> value
  False -> Atom x
decorate (Next t) label value = Next (decorate t label value)

{-# LANGUAGE DependentTypes, ListTuplePuns, LambdaCase #-}
module NativeTypeCaseRetainedField where

data RetainedField where
  RetainedField :: (foreach (b :: Bool) -> (case b of { True -> Int; False -> Char })) -> RetainedField

selected :: RetainedField
selected = RetainedField (\case True -> 11; False -> 'x')

data ClosedField where
  ClosedField :: (foreach (b :: Bool) -> (case True of { True -> Int; False -> Char })) -> ClosedField

constant :: ClosedField
constant = ClosedField (\b -> if b then 11 else 23)

data StoredField where
  StoredField :: foreach (b :: Bool) -> (case b of { True -> Int; False -> Int }) -> StoredField

stored :: StoredField
stored = StoredField False 23

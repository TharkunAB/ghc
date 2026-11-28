{-# LANGUAGE FlexibleInstances, NoDependentTypes #-}
module NativeOpenTheseFmapUniformRow (URow(..)) where

data URow e t a = UEmpty | UFlagged t a | UBoth e t a
  deriving Show

instance Functor (URow Bool Bool) where
  fmap _ row = row `seq` UEmpty

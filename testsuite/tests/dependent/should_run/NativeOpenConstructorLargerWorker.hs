{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module NativeOpenConstructorLargerWorker
  ( Larger(..), choose, Product(..), project, Marker(..), retain, Deep(..), innermost ) where

data Larger a = Larger (Maybe a) a | Empty
choose :: Larger a -> a -> a
choose (Larger (Just x) _) _ = x
choose (Larger Nothing _) x = x
choose Empty x = x

data Product a = Product a
project :: Product a -> a
project (Product x) = x

data Marker a = Marker
retain :: Marker a -> a -> a
retain Marker x = x

data Deep a = Deep (Maybe (Maybe a))
innermost :: Deep a -> a -> a
innermost (Deep outer) fallback = case outer of
  Nothing -> fallback
  Just inner -> case inner of
    Nothing -> fallback
    Just x -> x

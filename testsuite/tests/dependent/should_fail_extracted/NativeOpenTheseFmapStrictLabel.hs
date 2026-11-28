{-# LANGUAGE DataKinds, DependentTypes, FlexibleInstances, GADTs, ListTuplePuns, TypeOperators #-}
module NativeOpenTheseFmapStrictLabel where

import Data.Type.Equality ((:~:)(Refl))

data StrictRow e t a = SEmpty | SBoth e t a

instance Functor (StrictRow Int Bool) where
  fmap _ SEmpty = SEmpty
  fmap f (SBoth e t a) = e `seq` SBoth e t (f a)

claim :: foreach (n :: Int) -> foreach (f :: Bool -> Bool) -> foreach (x :: Bool) ->
  Prelude.fmap f ('SBoth n 'True x) :~: 'SBoth n 'True (f x)
claim _ _ _ = Refl

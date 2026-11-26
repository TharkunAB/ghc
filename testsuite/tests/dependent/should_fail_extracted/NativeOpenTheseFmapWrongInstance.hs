{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables, TypeOperators #-}
module NativeOpenTheseFmapWrongInstance where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Row (Row(..))
import These (These(..))

wrongInstance :: forall (a :: Type). foreach (p :: a -> Bool) ->
  foreach (flag :: Bool) -> foreach (x :: a) ->
  fmap p ('Flagged flag x :: Row Bool Bool a) :~: 'Flagged flag (p x)
wrongInstance _ _ _ = Refl

alwaysEmpty :: foreach (p :: Int -> Bool) -> foreach (r :: Row Bool Bool Int) ->
  Prelude.fmap p r :~: ('Empty :: Row Bool Bool Bool)
alwaysEmpty _ _ = Refl

wrongCallback :: forall (a :: Type) (b :: Type).
  foreach (p :: a -> b) -> foreach (q :: a -> b) ->
  foreach (label :: Char) -> foreach (x :: a) ->
  fmap p ('These label x) :~: 'These label (q x)
wrongCallback _ _ _ _ = Refl

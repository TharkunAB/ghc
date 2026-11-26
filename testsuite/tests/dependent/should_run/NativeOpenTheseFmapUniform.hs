{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module NativeOpenTheseFmapUniform
  ( URow(..)
  , uEmpty
  , uFlagged
  , uBoth
  , thisProof
  , thatProof
  ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenTheseFmapUniformRow (URow(..))
import These (These(..))

uEmpty :: foreach (p :: Int -> Bool) ->
  Prelude.fmap p ('UEmpty :: URow Bool Bool Int) :~: ('UEmpty :: URow Bool Bool Bool)
uEmpty _ = Refl

uFlagged :: foreach (p :: Int -> Bool) -> foreach (flag :: Bool) -> foreach (x :: Int) ->
  Prelude.fmap p ('UFlagged flag x :: URow Bool Bool Int) :~: ('UEmpty :: URow Bool Bool Bool)
uFlagged _ _ _ = Refl

uBoth :: foreach (p :: Int -> Bool) -> foreach (x :: Int) ->
  Prelude.fmap p ('UBoth 'True 'False x :: URow Bool Bool Int) :~: ('UEmpty :: URow Bool Bool Bool)
uBoth _ _ = Refl

thisProof :: forall (a :: Type). foreach (p :: a -> Bool) -> foreach (label :: Char) ->
  fmap p ('This label :: These Char a) :~: 'This label
thisProof _ _ = Refl

thatProof :: forall (a :: Type). foreach (p :: a -> Bool) -> foreach (x :: a) ->
  fmap p ('That x :: These Char a) :~: 'That (p x)
thatProof _ _ = Refl

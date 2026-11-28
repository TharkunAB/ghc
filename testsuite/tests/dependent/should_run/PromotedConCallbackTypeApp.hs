{-# LANGUAGE DataKinds, NoDependentTypes, TypeApplications #-}
-- A promoted constructor with a function field accepts a matchable type
-- constructor in a visible type application, as in GHC without
-- DependentTypes.
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Type.Reflection (typeRep)

data Endo a = Endo (a -> a)
data Two = Two (Type -> Type) (Type -> Type)

main :: IO ()
main = do
  print (Proxy @('Endo Maybe))
  print (typeRep @('Endo Maybe))
  print (typeRep @('Two Maybe []))

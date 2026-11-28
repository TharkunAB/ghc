{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, TypeFamilies, TypeOperators #-}
module NativeSynonymReify (checkFamily) where

import Data.Type.Equality ((:~:)(Refl))
import NativeSynonymReifyClient (Copied, check, checkPlain)

type family Identity a where
  Identity a = a

checkFamily :: Copied Identity Bool :~: Bool
checkFamily = (check, checkPlain) `seq` Refl

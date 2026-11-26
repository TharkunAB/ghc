{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers, NoUndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-}
module NativeTypeLambdaEtaTermination where

import Data.Kind (Type)
import GHC.Matchability (U)

type Apply :: (Type %U -> Type) %U -> Type %U -> Type
type family Apply f cell where
  Apply f cell = f cell
type Applied f = \cell -> Apply f cell

type Stamped tag = \cell -> Stamp tag cell
type family Stamp tag cell where
  Stamp tag cell = Stamped tag cell

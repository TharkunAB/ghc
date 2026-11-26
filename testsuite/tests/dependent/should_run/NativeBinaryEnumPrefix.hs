{-# LANGUAGE DataKinds, DependentTypes, Modifiers, TypeFamilies #-}
module NativeBinaryEnumPrefix where

import GHC.Matchability (U)
import qualified NativeBinaryEnumCert as C

type family Finish (f :: Bool %U -> Bool) (b :: Bool) where
  Finish f b = f b

type Skipped b = Finish (C.choose 'True) b
type Carried b = Finish (C.choose 'False) b
type Unary = Finish C.flipBit 'False
type BothFalse b = C.bothDemanded 'False b
type StrictFalse b = C.explicitStrict 'False b
type StrictTrue b = C.explicitStrict 'True b

{-# LANGUAGE DataKinds, FlexibleInstances, NoDependentTypes, TypeApplications #-}
module NativeUnsaturatedBadInstanceHead where

import GHC.Exts (FUN, Multiplicity(Many), U)
import NativeUnsaturatedProvider (Select)

class C (f :: FUN @U 'Many Bool Bool)
instance C (Select 'True)

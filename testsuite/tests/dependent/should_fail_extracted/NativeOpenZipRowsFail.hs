{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             GADTs, TypeOperators #-}
module NativeOpenZipRowsFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenZipRowsVariants (countRight)
import PackedVecLibrary (Nat(..))
import ZipLengthSource (zipLength)

wrongDemand :: forall (xs :: [Char]). zipLength xs ('[] :: [Bool]) :~: 'Z
wrongDemand = Refl

rightDemand :: forall (xs :: [Char]). countRight xs ('[] :: [Bool]) :~: 'Z
rightDemand = Refl

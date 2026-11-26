{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             GADTs, TypeOperators #-}
module NativeOpenZipRowsNondecreasingFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenZipRowsVariants (loopLength)
import PackedVecLibrary (Nat(..))

nondecreasing :: forall (ys :: [Char]). loopLength '[] ys :~: 'Z
nondecreasing = Refl

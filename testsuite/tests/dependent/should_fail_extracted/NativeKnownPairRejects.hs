{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ImpredicativeTypes, NoDependentTypes #-}
module NativeKnownPairRejects where

import GHC.Sigma (Sigma (Pair))
import NativeKnownPairProvider

wrong :: Open Bool
wrong = Pair True (No 9)

independent :: forall a. Open a
independent = Pair True (Yes 1)

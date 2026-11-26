{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
module NativeConstrainedSumEarlyDomain where

import Data.Proxy (Proxy)
import NativeConstrainedSumProvider (Choice (Shape))

data EarlyStaged a where
  EarlyStaged :: Shape a Char -> (a ~ Bool) => Proxy a -> EarlyStaged a

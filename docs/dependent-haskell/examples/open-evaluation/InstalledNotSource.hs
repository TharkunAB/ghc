{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, StandaloneKindSignatures #-}
module InstalledNotSource (NegateAll) where

import qualified OpenMapWorker as W
import qualified Prelude as P

-- | Complement each element with the ordinary function exported by Prelude.
type NegateAll :: [P.Bool] -> [P.Bool]
type NegateAll xs = W.each P.not xs

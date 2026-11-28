{-# LANGUAGE DependentTypes #-}
module SumOfSizes (plusExists) where

import Data.Nat.Dependent (Nat, plus)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))

-- | Return the sum of two runtime sizes with its equality proof.
plusExists :: foreach (m :: Nat) -> foreach (n :: Nat)
           -> some (k :: Nat) | k :~: plus m n
plusExists m n = Pair (plus m n) Refl

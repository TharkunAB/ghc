{-# LANGUAGE DependentTypes #-}
module MutatedClient where
import Data.Type.Equality ((:~:) (Refl))
import MutatedProvider (Nat(..), eqNat)
import Prelude (Bool(..))

badProof :: foreach (n :: Nat) -> eqNat n n :~: True
badProof Zero     = Refl
badProof (Succ n) = badProof n

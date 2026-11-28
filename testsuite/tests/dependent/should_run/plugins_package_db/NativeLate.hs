{-# LANGUAGE DependentTypes #-}
module NativeLate where
import Data.Type.Equality ((:~:) (Refl))
data Nat = Zero | Succ Nat
reflNat :: foreach (n :: Nat) -> n :~: n
reflNat _ = Refl

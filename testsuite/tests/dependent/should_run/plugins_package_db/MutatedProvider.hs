{-# LANGUAGE DependentTypes #-}
module MutatedProvider (Nat(..), eqNat) where
import Prelude (Bool(..))

data Nat = Zero | Succ Nat

eqNat :: Nat -> Nat -> Bool
eqNat Zero     Zero     = True
eqNat Zero     (Succ _) = False
eqNat (Succ _) Zero     = False
eqNat (Succ m) (Succ n) = eqNat m n

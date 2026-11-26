{-# LANGUAGE DependentTypes #-}
module NoPluginLocal where
import Data.Type.Equality ((:~:))
import ObsProvider (Nat(..))

badLoop :: Nat -> Nat
badLoop n = badLoop n

law :: foreach (n :: Nat) -> badLoop n :~: Zero
law = error "unreachable"

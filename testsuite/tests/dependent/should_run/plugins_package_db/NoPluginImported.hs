{-# LANGUAGE DependentTypes #-}
module NoPluginImported where
import Data.Type.Equality ((:~:))
import ObsProvider (Nat(..), plus, whileAdd)

law :: foreach (m :: Nat) (n :: Nat) -> whileAdd m n :~: plus m n
law = error "unreachable"

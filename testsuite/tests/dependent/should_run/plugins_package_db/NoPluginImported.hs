{-# LANGUAGE DependentTypes #-}
module NoPluginImported where
import Data.Type.Equality ((:~:)(Refl))
import ObsProvider (Nat(..), plus, whileAdd)

law :: foreach (m :: Nat) (n :: Nat) -> whileAdd m n :~: plus m n
law m n = Refl

{-# LANGUAGE DependentTypes, LinearTypes #-}
module NativeLinearForEachDiscard where
import Data.Proxy (Proxy (..))
import GHC.Tuple (Unit)
import NativeLinearForEachAndExtendedLetProvider (Nat)
discardLinear :: foreach (x %1 :: Nat) -> Proxy x -> Unit
discardLinear _ Proxy = ()

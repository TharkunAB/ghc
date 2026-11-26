{-# LANGUAGE DependentTypes, LinearTypes #-}
module NativeLinearForEachDup where
import Data.Proxy (Proxy (..))
import GHC.Tuple (Tuple2)
import NativeLinearForEachAndExtendedLetProvider (Nat)
dupLinear :: foreach (x %1 :: Nat) -> Proxy x -> Tuple2 Nat Nat
dupLinear x Proxy = (x, x)

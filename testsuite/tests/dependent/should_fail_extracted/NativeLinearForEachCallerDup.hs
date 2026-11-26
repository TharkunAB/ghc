{-# LANGUAGE DependentTypes, LinearTypes #-}
module NativeLinearForEachCallerDup where
import Data.Proxy (Proxy (..))
import GHC.Tuple (Tuple2)
import NativeLinearForEachAndExtendedLetProvider (Nat, dupInTypeLinearInTerm)
callerDup :: Nat %1 -> Tuple2 Nat Nat
callerDup n = (dupInTypeLinearInTerm n Proxy, n)

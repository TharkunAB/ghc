{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeMixedPackageInterfaceIncompatible where
import Data.Proxy
import GHC.Exists
import GHC.Sigma (Sigma(Pair))

type Both = some (b :: Bool) | (Proxy b, Proxy b)
type NeedsTrue = exists (b :: Bool) | (Proxy b, Proxy 'True)

bad :: Both -> NeedsTrue
bad (Pair _ fields) = Pack _ fields

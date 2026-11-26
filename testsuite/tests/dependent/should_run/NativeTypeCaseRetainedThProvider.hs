{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeCaseRetainedThProvider where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import NativeRetainedTH (Select, quoted)

shadowed :: forall (b :: Type). Proxy b -> foreach (b :: Bool) -> Select b
shadowed _ = quoted

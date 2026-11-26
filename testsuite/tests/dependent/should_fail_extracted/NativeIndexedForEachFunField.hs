{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
module NativeIndexedForEachFunField where
import NativeIndexedForEachCtorProvider (Nat(..), UTerm)
data BadFun (n :: Nat) where
  MkBadFun :: foreach (f :: UTerm n -> UTerm n) -> BadFun n

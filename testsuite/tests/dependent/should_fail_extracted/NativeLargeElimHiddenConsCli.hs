{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeLargeElimHiddenConsCli where
import NativeLargeElimHiddenConsCli_Prov (Val, flatPred)
use :: foreach (v :: Val) -> flatPred v -> ()
use _ _ = ()

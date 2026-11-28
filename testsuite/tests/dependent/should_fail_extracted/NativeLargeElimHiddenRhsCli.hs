{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeLargeElimHiddenRhsCli where
import NativeLargeElimHiddenRhsCli_Prov (Val(..), flatPred)
use :: foreach (v :: Val) -> flatPred v -> ()
use _ _ = ()

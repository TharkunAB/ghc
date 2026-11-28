{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns #-}
module NativeLargeElimHiddenRhsCli_Prov (Val(..), flatPred) where
import Data.Kind (Type)
data Secret = Secret
data Val = Bot | Arr Val Val
flatPred :: Val -> Type
flatPred Bot       = type Secret
flatPred (Arr _ _) = type ()

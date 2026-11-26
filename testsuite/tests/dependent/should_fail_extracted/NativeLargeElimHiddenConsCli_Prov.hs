{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns #-}
module NativeLargeElimHiddenConsCli_Prov (Val, flatPred) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
flatPred :: Val -> Type
flatPred Bot       = type ()
flatPred (Arr _ _) = type Int

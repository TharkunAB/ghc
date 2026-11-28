{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns #-}
module NativeLargeElimNonTermStep where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred (Arr a b))

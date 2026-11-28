{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns #-}
module NativeLargeElimNonTermBase where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type (valPred Bot)
valPred (Arr a b) = type (valPred a -> valPred b)

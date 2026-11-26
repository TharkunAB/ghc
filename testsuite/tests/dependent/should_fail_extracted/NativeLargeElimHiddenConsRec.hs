{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns #-}
module NativeLargeElimHiddenConsRec (Val, valPred) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)

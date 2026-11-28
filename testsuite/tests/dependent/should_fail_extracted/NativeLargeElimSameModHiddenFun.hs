{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, GADTs, ListTuplePuns #-}
module NativeLargeElimSameModHiddenFun (Val(..), Box(..)) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)
data Box (v :: Val) = MkBox (valPred v)

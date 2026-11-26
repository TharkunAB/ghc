{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns, TemplateHaskell #-}
module NativeLargeElimHiddenRhsRec (Val(..), valPred) where
import Data.Kind (Type)
data Secret = Secret
$(pure [])
data Val = Bot | Arr Val Val
valPred :: Val -> Type
valPred Bot       = type Secret
valPred (Arr a b) = type (valPred a -> valPred b)

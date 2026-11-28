{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NativeLargeElimRuleConflict where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
data Val = Bot | Arr Val Val
{-# NOINLINE valPred #-}
valPred :: Val -> Type
valPred Bot       = type ()
valPred (Arr a b) = type (valPred a -> valPred b)
{-# RULES "hijackValPred" forall x. valPred x = valPred Bot #-}
bad :: valPred 'Bot :~: ()
bad = Refl

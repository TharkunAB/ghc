{-# LANGUAGE ExplicitNamespaces, GADTs, Modifiers, NoDependentTypes, TypeOperators #-}
module NativePercentArrowAliasProvider (TyFun, type (~>)) where
import Data.Kind (Type)
data TyFun a b
type a ~> b = TyFun a b -> Type

{-# LANGUAGE DependentTypes, ListTuplePuns, RankNTypes, TypeFamilies #-}
module NativeRetainedFamDomAliasEscape where
import ConstrainedElements (Collects(..))
import Data.Kind (Type)
import Data.Proxy (Proxy)
type Scoped c = Collects c => Proxy (Elem c) -> Bool
type Escapes = forall (c :: Type). Scoped c -> Proxy (Elem c)

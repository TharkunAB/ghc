{-# LANGUAGE DependentTypes, FlexibleContexts, FunctionalDependencies, ListTuplePuns, MultiParamTypeClasses, QuantifiedConstraints, TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
module NativeConstrainedFamEqScopeWrong where
import Data.Coerce (Coercible)
import Data.Kind (Type)
import NativeConstrainedFamilyProvider (Allowed, Collects(..), KeyElement, Output(..))
type Forget a = Bool
class Output c e => Via c e | c -> e where
    type Hidden c
    type Hidden c = Explicit c Bool
class SourceOnly c where
    type Copy c
    type Copy c = KeyElement c
class Collects a => Witness (a :: Type) (b :: Type) where
    type Sibling a b
    type Sibling a b = Forget (Elem b)
class (Coercible a b, Collects b) => Equal (a :: Type) (b :: Type) where
    type Rewritten a b
    type Rewritten a b = Elem a
class (forall x. Allowed x => Collects (f x)) => Quantified (f :: Type -> Type) (a :: Type) where
    type Projected f a
    type Projected f a = Elem (f a)

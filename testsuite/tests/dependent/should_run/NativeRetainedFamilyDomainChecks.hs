{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, FlexibleContexts, ListTuplePuns, Modifiers, RankNTypes, RequiredTypeArguments, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module NativeRetainedFamilyDomainChecks (Scoped, scoped, project, ok, forgotten, Uses(..), prefix) where

import ConstrainedElements (Collects(..), FieldKey, KnownIndex(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Matchability (U)

type Scoped = foreach (tag :: Bool) -> Collects (FieldKey tag) => Proxy (Elem (FieldKey tag))
scoped :: Scoped
scoped _ = Proxy

project :: forall k -> forall (c :: k) -> Collects c => Proxy (Elem c)
project (type _) (type _) = Proxy

ok :: Proxy Bool
ok = project (type Type) (type [Bool])

type Ignore a = forall b. Show b => b -> String
forgotten :: Ignore (Elem [Bool])
forgotten x = show x

data Uses (f :: Maybe Bool %U -> Maybe Bool) = Uses
prefix :: Uses Echo
prefix = Uses

{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns, RequiredTypeArguments, TypeFamilies #-}
module NativeRetainedFamDomMissingForeach where
import ConstrainedElements (Collects(..), FieldKey)
import Data.Proxy (Proxy(..))
required :: forall (tag :: Bool) -> Proxy (Elem (FieldKey tag))
required (type _) = Proxy
missing :: foreach (tag :: Bool) -> Proxy (Elem (FieldKey tag))
missing _ = Proxy

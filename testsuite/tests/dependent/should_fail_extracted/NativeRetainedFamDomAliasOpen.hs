{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeRetainedFamDomAliasOpen where
import ConstrainedElements (Collects(..))
type Missing a = Elem [a]

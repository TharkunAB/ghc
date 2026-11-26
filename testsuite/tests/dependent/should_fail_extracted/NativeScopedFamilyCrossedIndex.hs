{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables, TypeFamilies #-}
module NativeScopedFamilyCrossedIndex where
import ScopedFamilyMethodProvider
crossed :: forall c. Collects c
        => foreach (x :: c) -> foreach (y :: c) -> Element c -> TaggedEntry (Element c) (compareOrigin x)
crossed _ y payload = inspectEntry y payload

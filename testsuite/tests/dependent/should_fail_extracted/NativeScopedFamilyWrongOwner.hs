{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables, TypeFamilies #-}
module NativeScopedFamilyWrongOwner where
import ScopedFamilyMethodProvider
class OtherClass c where
    otherCompare :: c -> Ordering
wrongOwner :: forall c. OtherClass c
           => foreach (x :: c) -> Element c -> TaggedEntry (Element c) (otherCompare x)
wrongOwner _ _ = error "missing owner"

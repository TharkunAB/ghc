{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables #-}
module NativeScopedFamilyAmbiguous where
import ScopedFamilyMethodProvider
missingPayload :: forall c a. (Collects c, Show a) =>
                  foreach (x :: c) -> TaggedEntry Int (compareOrigin x)
missingPayload x = inspectPayload x (5 :: Int)

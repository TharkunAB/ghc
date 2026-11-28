{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables #-}
module NativeScopedDictAmbigOk (missingPayloadOk) where
import ScopedFamilyMethodProvider
missingPayloadOk :: forall c a. (Collects c, Show a) =>
                    foreach (x :: c) -> TaggedEntry Int (compareOrigin x)
missingPayloadOk x = inspectPayload x (5 :: Int)

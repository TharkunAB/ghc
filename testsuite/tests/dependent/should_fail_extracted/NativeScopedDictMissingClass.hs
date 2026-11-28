{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns #-}
module NativeScopedDictMissingClass where
import NativeScopedDictExportProvider
missing :: a -> Bool
missing x = case chooseTagged x of
    Tagged value -> value

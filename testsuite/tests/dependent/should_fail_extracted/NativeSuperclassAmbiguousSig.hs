{-# LANGUAGE DependentTypes, GADTs #-}
module NativeSuperclassAmbiguousSig where
import NativeTypeCaseDotSectionsProvider
badSig :: (SubChoose a, AltSubChoose a) => foreach (x :: a) -> Tagged (choose x)
badSig x = tagBool (choose x)

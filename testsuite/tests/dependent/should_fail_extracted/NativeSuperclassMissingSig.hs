{-# LANGUAGE DependentTypes, GADTs #-}
module NativeSuperclassMissingSig where
import NativeTypeCaseDotSectionsProvider
badSig :: Eq a => foreach (x :: a) -> Tagged (choose x)
badSig _ = tagBool False

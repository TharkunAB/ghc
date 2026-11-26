{-# LANGUAGE DependentTypes, GADTs #-}
module NativeSuperclassMissingCall where
import NativeTypeCaseDotSectionsProvider
bad :: Eq a => a -> Bool
bad x = case chooseTagged x of
  Tagged v -> v

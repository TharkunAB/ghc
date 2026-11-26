{-# LANGUAGE DependentTypes, GADTs #-}
module NativeSuperclassAmbiguousCall where
import NativeTypeCaseDotSectionsProvider
bad :: (SubChoose a, AltSubChoose a) => a -> Bool
bad x = case chooseTagged x of
  Tagged v -> v

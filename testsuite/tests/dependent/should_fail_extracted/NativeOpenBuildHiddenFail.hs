{-# LANGUAGE DataKinds, DependentTypes, StandaloneKindSignatures #-}
module NativeOpenBuildHiddenFail where

import Data.Kind (Type)
import qualified NativeOpenBuildControls as Ctrl (Shell, otherBuild)

type HiddenMake :: Type -> Type -> Ctrl.Shell Type Type
type HiddenMake a b = Ctrl.otherBuild a b

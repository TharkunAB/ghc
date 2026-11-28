{-# LANGUAGE DataKinds, DependentTypes, StandaloneKindSignatures #-}
module NativeOpenBuildStrictFail where

import Data.Kind (Type)
import qualified NativeOpenBuildControls as Ctrl

type StrictMake :: Type -> Type -> Ctrl.Shell Type Type
type StrictMake a b = Ctrl.strictBuild a b

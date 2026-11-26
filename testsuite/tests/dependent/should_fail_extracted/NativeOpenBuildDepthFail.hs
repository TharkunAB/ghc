{-# LANGUAGE DataKinds, DependentTypes, StandaloneKindSignatures #-}
module NativeOpenBuildDepthFail where

import Data.Kind (Type)
import qualified NativeOpenBuildControls as Ctrl

type Diverging :: Type -> Type -> Ctrl.Shell Type Type
type Diverging a b = Ctrl.diverging a b

type TooSlow :: Type -> Type -> Ctrl.Shell Type Type
type TooSlow a b = Ctrl.slowBoth a b

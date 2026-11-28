{-# LANGUAGE DataKinds, DependentTypes, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Werror=unused-imports #-}
module NativeOpenBuildVisible (MakeVisible) where

import Data.Kind (Type)
import qualified OpenBuildWorker as B (Shell, build)
import qualified OpenBuildWorker as C (Shell(Shell))

type MakeVisible :: Type -> Type -> B.Shell Type Type
type MakeVisible a b = B.build a b

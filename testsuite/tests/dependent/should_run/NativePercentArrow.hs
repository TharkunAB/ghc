{-# LANGUAGE DataKinds, GADTs, Modifiers, NoDependentTypes, PolyKinds, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, StandaloneKindSignatures, TypeApplications #-}
module NativePercentArrow where
import Data.Kind (Type)
import GHC.Exts (M)
import NativePercentArrowOnlyModifiers ()
import NativePercentArrowProvider ()

type InvisibleKind = forall (a :: Type). @M a %M -> Type
data InvisibleSymbol :: InvisibleKind
type VisibleKind = forall (a :: Type) -> @M Type
type VisibleSymbol :: VisibleKind
data VisibleSymbol a

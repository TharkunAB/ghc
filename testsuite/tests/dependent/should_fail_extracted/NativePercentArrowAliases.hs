{-# LANGUAGE DataKinds, ExplicitNamespaces, GADTs, Modifiers, NoDependentTypes, PolyKinds, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, StandaloneKindSignatures, TypeApplications, TypeOperators #-}
module NativePercentArrowAliases where
import Data.Kind (Type)
import GHC.Exts (M, U)
import NativePercentArrowAliasProvider (type (~>))

data LocalTyFun a b
type LocalArrow a b = LocalTyFun a b -> Type
data SymbolLocal :: LocalArrow Type Type
data SymbolImported :: Type ~> Type
data SymbolWired :: (->) @U Type Type
data SymbolTrailing :: Type -> LocalArrow Type Type
type Identity a = a
data SymbolForwarded :: Identity ((->) @U Type Type)

type InvisibleKind = forall (a :: Type). a %M -> Type
data InvisibleSymbol :: InvisibleKind
type VisibleKind = forall (a :: Type) -> Type
type VisibleSymbol :: VisibleKind
data VisibleSymbol a

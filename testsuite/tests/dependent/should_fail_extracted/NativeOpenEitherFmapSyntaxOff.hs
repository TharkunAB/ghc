{-# LANGUAGE DataKinds, Modifiers, NoDependentTypes, StandaloneKindSignatures #-}
module NativeOpenEitherFmapSyntaxOff where

import GHC.Exts (U)
import qualified Prelude as P

type MappedEither :: forall e a b. (a %U -> b) -> P.Either e a -> P.Either e b
type MappedEither f x = P.fmap f x

{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures, PolyKinds #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module NativeAutoNumMTransProvider (Endo, MTrans, Tag(..), Lifting(..), idEndo) where
import Data.Kind (Type)
type Endo = Type -> Type
type MTrans = (Type -> Type) -> (Type -> Type)
data Tag :: Endo where
  MkTag :: Tag Int
class Lifting (t :: MTrans) where
  liftUnit :: Monad m => m a -> t m a
idEndo :: Endo
idEndo x = x

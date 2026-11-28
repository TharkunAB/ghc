{-# LANGUAGE DataKinds, KindSignatures, NoDependentTypes, RankNTypes #-}
module NativeOpenMatchabilityFixed (Varianced) where
import Data.Kind (Type)
data Var = Op
type Varianced = forall (var :: Var). Type

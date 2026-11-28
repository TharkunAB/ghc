{-# LANGUAGE DependentTypes, LiberalTypeSynonyms, ListTuplePuns #-}
module RuleProof (Callback, proof, observed) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import FoldProvider (fold)
import GHC.Exts (Matchability(Matchable), U)
import RuleBridge (value)

type Callback (f :: Type %('Matchable) -> Type) = f Bool

type Fold :: (a %U -> b %U -> b) -> b -> [a] -> b
type Fold step seed xs = fold step seed xs

proof :: Proxy step -> Proxy seed -> Fold step seed '[] :~: seed
proof _ _ = Refl

observed :: Int
observed = value

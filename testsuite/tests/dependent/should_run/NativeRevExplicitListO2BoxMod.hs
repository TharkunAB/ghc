{-# LANGUAGE DataKinds, Modifiers, NoDependentTypes, PolyKinds, RankNTypes,
             StandaloneKindSignatures #-}
module NativeRevExplicitListO2BoxMod (RKindU, BoxU, BoxM, matchDeferOk) where

import Data.Kind (Type)
import GHC.Matchability (U)

type RKindU = forall x. @U Maybe x %U -> Maybe x

type BoxU :: RKindU -> Type
data BoxU f

type BoxM :: (forall x. Maybe x -> Maybe x) -> Type
data BoxM f

matchDeferOk :: forall (r :: forall x. Maybe x -> Maybe x). BoxU r -> ()
matchDeferOk _ = ()

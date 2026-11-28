{-# LANGUAGE DataKinds, ExplicitForAll, FlexibleInstances, Modifiers, MultiParamTypeClasses #-}
{-# LANGUAGE NoDependentTypes, PolyKinds, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module NativeUnsaturatedPattern (Box, C(..), local, matchable) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M, Matchability, U)

type family UnApp a where
  UnApp (f x) = x

matchable :: UnApp (Maybe Bool) :~: Bool
matchable = Refl

type family Flip (m :: Matchability) where
  Flip U = M
  Flip M = U

type family Initial (flag :: Bool) :: Matchability where
  Initial flag = U

data Box (flag :: Bool) (f :: Type %(Flip (Initial flag)) -> Type)

class C (flag :: Bool) box t where
  tag :: Proxy flag -> Proxy box -> Proxy t -> Bool

instance forall flag (f :: Type %(Flip (Initial flag)) -> Type) x.
         C flag (Box flag f) (f x) where
  tag _ _ _ = True

local :: Bool
local = tag (Proxy @'True) (Proxy @(Box 'True Maybe)) (Proxy @(Maybe Bool))

type family Loop (flag :: Bool) :: Matchability where
  Loop flag = Loop flag

class Unused t
instance forall (unused :: Type %(Loop 'True) -> Type). Unused Int

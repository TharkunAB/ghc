{-# LANGUAGE DataKinds, DependentTypes, ExplicitForAll, GADTs, TypeFamilies,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fexpose-all-unfoldings #-}
module NativeClosedMethodApartnessObserved
  ( Box(..), boxed, aliasProof, familyProof, inlinable, manifest, observation, quantified, run ) where

import Data.Type.Equality ((:~:)(Refl))

type Image = Prelude.fmap Prelude.not ('Just 'True)
type family Family where
  Family = Prelude.fmap Prelude.not ('Just 'True)

{-# OPAQUE aliasProof #-}
aliasProof :: Image :~: 'Just 'False
aliasProof = Refl

{-# OPAQUE familyProof #-}
familyProof :: Family :~: 'Just 'False
familyProof = Refl

data Box where
  Box :: (Family :~: 'Just 'False) -> Box

{-# OPAQUE boxed #-}
boxed :: Box
boxed = Box familyProof

{-# INLINABLE inlinable #-}
inlinable :: String
inlinable = case familyProof of Refl -> "inlinable"

{-# OPAQUE manifest #-}
manifest :: 'True :~: 'True
manifest = case aliasProof of Refl -> Refl

{-# OPAQUE observation #-}
observation :: String
observation = case familyProof of Refl -> "closed observation"

{-# OPAQUE quantified #-}
quantified :: forall (p :: Family :~: 'Just 'False). String
quantified = case familyProof of Refl -> "quantified"

{-# OPAQUE run #-}
run :: Bool -> Maybe Bool
run x = Prelude.fmap not (Just x)

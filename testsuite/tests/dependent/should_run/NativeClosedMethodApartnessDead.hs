{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -fomit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeClosedMethodApartnessDead (runDead) where

import Data.Type.Equality ((:~:)(Refl))
import NativeClosedMethodApartnessOrigin (ImageAlias, ImageFamily)

proof :: (ImageAlias :~: 'Just 'False, ImageFamily :~: 'Just 'False)
proof = (Refl, Refl)

{-# RULES "dead/constructor" [~1] forall f. fmap f (Just False) = Nothing #-}

runDead :: IO ()
runDead = do
  print proof
  print (fmap not (Just True), fmap not (Just False), fmap not Nothing :: Maybe Bool)

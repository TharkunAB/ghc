{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -fomit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeClosedMethodApartnessEmpty (runEmpty) where

import Data.Type.Equality ((:~:)(Refl))
import NativeClosedMethodApartnessOrigin (ImageAlias, ImageFamily)

proof :: (ImageAlias :~: 'Just 'False, ImageFamily :~: 'Just 'False)
proof = (Refl, Refl)

{-# RULES "empty/constructor" [~1] forall f. fmap f Nothing = Just (f True) #-}

runEmpty :: IO ()
runEmpty = do
  print proof
  print (fmap not (Just True), fmap not (Just False), fmap not Nothing :: Maybe Bool)

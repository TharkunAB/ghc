{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -fomit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeClosedMethodApartnessInactive (runInactive) where

import Data.Type.Equality ((:~:)(Refl))
import NativeClosedMethodApartnessOrigin
  ( ImageAlias, ImageFamily, ImageMixed, ImageMultiple, ImageNested
  , ImageOpaqueAlias, ImageOpaqueCallback )

proof :: ( ImageAlias :~: 'Just 'False
         , ImageFamily :~: 'Just 'False
         , ImageNested :~: 'Just 'False
         , ImageOpaqueAlias :~: 'Just 'False
         , ImageOpaqueCallback :~: 'Just 'False
         , ImageMultiple :~: '( 'Just 'False, 'Just 'True)
         , ImageMixed :~: '( 'Just 'False, 'Just 'False) )
proof = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

{-# RULES "inactive/constructor" [~] forall f. fmap f (Just True) = Nothing #-}

runInactive :: IO ()
runInactive = case proof of
  (Refl, Refl, Refl, Refl, Refl, Refl, Refl) ->
    print (fmap not (Just True), fmap not (Just False), fmap not Nothing :: Maybe Bool)

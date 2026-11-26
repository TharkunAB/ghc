{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeClosedMethodApartnessCoarseFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeClosedMethodApartnessOrigin
  ( ImageMixed, ImageMultiple, ImageNested, ImageOpaqueAlias, ImageOpaqueCallback )

{-# RULES "dead/coarse" [~1] forall f. fmap f (Just False) = Nothing #-}

badNested :: ImageNested :~: 'Just 'False
badNested = Refl

badOpaqueAlias :: ImageOpaqueAlias :~: 'Just 'False
badOpaqueAlias = Refl

badOpaqueCallback :: ImageOpaqueCallback :~: 'Just 'False
badOpaqueCallback = Refl

badMultiple :: ImageMultiple :~: '( 'Just 'False, 'Just 'True)
badMultiple = Refl

badMixed :: ImageMixed :~: '( 'Just 'False, 'Just 'False)
badMixed = Refl

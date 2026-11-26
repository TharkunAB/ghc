{-# LANGUAGE DependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O1 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenIdentityRuleFail where

import Prelude
import qualified Prelude as P

unqualified :: forall a. a -> id a
unqualified x = x

qualified :: forall a. a -> P.id a
qualified x = x

{-# RULES "change observed identity" forall x. P.id x = P.undefined #-}

{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-}
module NativeExistentialSourceRuleOwner where
import Data.Kind (Type)
type Hidden = exists (a :: Type). Maybe a
packValue :: Bool -> Hidden
packValue x = Just x
constant :: Hidden
constant = Just True
{-# RULES "package/id" forall (x :: Bool). id (Just x :: Hidden) = constant #-}

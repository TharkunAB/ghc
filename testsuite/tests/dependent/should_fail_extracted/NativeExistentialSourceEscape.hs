{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
module NativeExistentialSourceEscape where

import GHC.Exists
import Provider

bad = case supplied of Pack (type a) x -> x

badWildcard = case supplied of Pack _ x -> x

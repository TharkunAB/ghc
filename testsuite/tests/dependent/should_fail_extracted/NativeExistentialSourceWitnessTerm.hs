{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
module NativeExistentialSourceWitnessTerm where

import GHC.Exists
import Provider

bad = case supplied of Pack (type a) _ -> a

{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
module NativeExistentialSourceScope where

import Data.Proxy
import GHC.Exists
import Provider

bad = (case supplied of Pack (type a) _ -> (), Proxy @a)

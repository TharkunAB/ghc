{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
module NativeExistentialSourceWrongPayload where

import GHC.Exists
import Provider

bad :: Hidden
bad = Pack (type Bool) (Just 'a')

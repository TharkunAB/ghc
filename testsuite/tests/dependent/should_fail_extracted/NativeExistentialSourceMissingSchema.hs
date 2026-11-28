{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
module NativeExistentialSourceMissingSchema where

import GHC.Exists

bad = Pack (type Bool) (Just True)

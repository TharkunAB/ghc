{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
module NativeExistentialSourceWrongSchema where

import GHC.Exists

bad = Pack @Bool (type Bool) (Just True)

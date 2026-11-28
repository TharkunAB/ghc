{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
module MissingSchema where

import GHC.Exists

bad = Pack (type Bool) (Just True)

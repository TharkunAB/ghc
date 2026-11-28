{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
module NativeExistentialSourceOverSat where

import GHC.Exists
import Provider

bad = (Pack @Hidden Bool (Just True)) ()

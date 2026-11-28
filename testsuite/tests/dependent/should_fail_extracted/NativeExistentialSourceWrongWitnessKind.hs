{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE RequiredTypeArguments #-}
module NativeExistentialSourceWrongWitnessKind where

import GHC.Exists
import Provider

bad :: Hidden
bad = Pack (type 'True) Nothing

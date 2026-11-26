{-# LANGUAGE DataKinds, NoDependentTypes, TypeApplications #-}
module NativeOpenRetryInterfaceMismatch where

import Data.Proxy (Proxy(..))
import NativeOpenRetryInterfaceProducer (upper)

wrongInput :: Proxy 'A'
wrongInput = upper (Proxy @'b')

wrongResult :: Proxy 'Z'
wrongResult = upper (Proxy @'a')

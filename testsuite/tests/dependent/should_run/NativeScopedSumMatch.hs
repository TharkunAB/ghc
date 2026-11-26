{-# LANGUAGE NoDependentTypes #-}
module NativeScopedSumMatch (checked) where

import Data.Proxy (Proxy(..))
import NativeScopedSumMatchClient (converted, fallback)

checked :: Bool
checked = (converted (Proxy :: Proxy Int), fallback) `seq` True

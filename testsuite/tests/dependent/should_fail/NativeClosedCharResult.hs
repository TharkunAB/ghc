{-# LANGUAGE DataKinds, DependentTypes #-}
module NativeClosedCharResult where

import qualified Data.Char
import Data.Proxy (Proxy)

unsupported :: Proxy (Data.Char.ord 'x')
unsupported = undefined

{-# LANGUAGE NoDependentTypes #-}
module NativeClosedCharLib (code, nonempty) where

import Data.Char (ord)
import Numeric.Natural (Natural)

code :: Char -> Natural
code c = fromIntegral (ord c)

nonempty :: [Natural] -> Bool
nonempty = not . null

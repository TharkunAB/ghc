{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Producer where
import Data.Proxy
import Numeric.Natural
import Rule ()
import Value
keep :: foreach (b :: Bool) -> Proxy b
keep _ = Proxy
pair :: Natural -> (Natural, Char)
pair n = (n + score [], error "unused right field")

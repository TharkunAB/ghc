{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeOpenPairLinkableWorker (choose, marker) where

import qualified NativeOpenPairLinkableAux as Aux

marker :: Bool
marker = Aux.marker

{-# NOINLINE [1] choose #-}
choose :: (a, a) -> a
choose (first, _) = first

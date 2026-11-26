{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module NativeOpenBuildDisjointWorker (swapDisjoint) where

{-# INLINE swapDisjoint #-}
swapDisjoint :: (a, b) -> (b, a)
swapDisjoint (x, y) = (y, x)

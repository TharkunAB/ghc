{-# LANGUAGE BangPatterns, NoDependentTypes #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas -fexpose-all-unfoldings #-}
module NativeOpenConstructorStrictWorker (Choice(..), pick) where

data Choice a b = LeftFields !a b | RightFields b a

{-# INLINE pick #-}
pick :: Choice a b -> b
pick (LeftFields _ y) = y
pick (RightFields y _) = y

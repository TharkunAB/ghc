{-# LANGUAGE BangPatterns, NoDependentTypes #-}
module NativeFunctionIndexIsoIdentity (identity, strictIdentity) where

identity :: a -> a
identity x = x

strictIdentity :: a -> a
strictIdentity !x = x

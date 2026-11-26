{-# LANGUAGE DependentTypes, ListTuplePuns, Strict #-}
module NativeComposedStrictLocal (strictLocal) where

strictLocal :: Int
strictLocal = to (error "strict-forced")
  where
    to x = const 1 (id x)

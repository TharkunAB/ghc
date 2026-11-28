{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module NativeOpenConstructorHelpersProvider (Choice(..), choose) where

data Choice a = Pick a a | Only a

choose :: Choice a -> a
choose (Pick _ y) = y
choose (Only y) = y

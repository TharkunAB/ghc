{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module NativeOpenConstructorHelpersBridge (Box(..), box) where

import qualified NativeOpenConstructorHelpersProvider as P

data Box a = Box (P.Choice a)

box :: Box a -> Maybe a
box (Box p) = Just (P.choose p)

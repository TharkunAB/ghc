{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import NativeOpenConstructorDisjointRules ()
import NativeOpenConstructorExtraProofs ()
import OpenConstructorSource (left, nested, right)

main :: IO ()
main = print (left True 'x', right 'y' False, nested (7 :: Int) 'z')

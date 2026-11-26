{-# LANGUAGE DataKinds, DependentTypes #-}
module NativeSourceTablePartial where

type Answer = oneRow 'False

oneRow :: Bool -> Bool
oneRow False = True

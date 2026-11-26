{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module NativeOpenMapForward (flipBool) where

flipBool :: Bool -> Bool
flipBool = not

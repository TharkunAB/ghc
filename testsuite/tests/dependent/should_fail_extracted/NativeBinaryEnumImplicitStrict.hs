{-# LANGUAGE NoDependentTypes, Strict #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module NativeBinaryEnumImplicitStrict where

choose :: Bool -> Bool -> Bool
choose False b = b
choose True _b = False

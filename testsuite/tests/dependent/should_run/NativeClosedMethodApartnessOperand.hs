{-# LANGUAGE DataKinds, NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -fomit-interface-pragmas -fno-write-if-simplified-core #-}
module NativeClosedMethodApartnessOperand (Input, opaqueNot) where

type Input = 'Just 'True

opaqueNot :: Bool -> Bool
opaqueNot = not

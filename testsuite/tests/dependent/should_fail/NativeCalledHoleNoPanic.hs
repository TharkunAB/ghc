{-# LANGUAGE DependentTypes #-}
module NativeCalledHoleNoPanic where

data N = Z | S N

data Token = Token

tokenAt :: N -> Token
tokenAt Z = Token
tokenAt (S p) = _

tokenGrow :: N -> N -> Token
tokenGrow y Z = tokenAt y
tokenGrow y (S n) = tokenGrow (S y) n

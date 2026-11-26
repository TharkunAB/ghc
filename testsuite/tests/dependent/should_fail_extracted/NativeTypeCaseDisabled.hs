{-# LANGUAGE NoDependentTypes #-}
module NativeTypeCaseDisabled where

type F a = case a of
  True -> Int
  False -> Bool

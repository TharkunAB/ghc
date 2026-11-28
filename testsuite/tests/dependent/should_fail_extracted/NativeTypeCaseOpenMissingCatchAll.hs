{-# LANGUAGE DependentTypes #-}
module NativeTypeCaseOpenMissingCatchAll where
import Data.Kind (Type)
bad :: foreach (a :: Type) -> Maybe a
bad a = case a of
  Int  -> Just 0
  Bool -> Just True

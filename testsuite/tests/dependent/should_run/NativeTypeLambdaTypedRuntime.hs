{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TemplateHaskell #-}
module NativeTypeLambdaTypedRuntime (ordinary) where

import NativeTypeLambdaTypedCallbacks (named)

data Cell (present :: Bool) where
  Filled :: Int -> Cell 'True

getCell :: Cell 'True -> Int
getCell (Filled n) = n

ordinary :: Either String Int
ordinary = fmap (getCell . Filled) $$(named)

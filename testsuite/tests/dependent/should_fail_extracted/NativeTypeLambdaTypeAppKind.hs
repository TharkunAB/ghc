{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TemplateHaskell, TypeApplications #-}
module NativeTypeLambdaTypeAppKind where

import Data.Proxy (Proxy(..))
import TypeLambdaArgument (rowFor)

badSpliceKind = rowFor @($([t| \err cell -> Either 'True cell |]) String) Proxy (Right 42) (Left "flag")
badKind = rowFor @(\cell -> Either 'True cell) Proxy (Right 42) (Left "flag")

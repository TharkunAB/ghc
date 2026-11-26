{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TemplateHaskell, TypeApplications #-}
module NativeTypeLambdaTypeAppScopeRejects where

import Data.Proxy (Proxy(..))
import TypeLambdaArgument (rowFor)

badSpliceScope = rowFor @($([t| \err cell -> Either missing cell |]) String) Proxy (Right 42) (Left "flag")
badFree = rowFor @missing Proxy (Right 42) (Left "flag")
badFreeLambda = rowFor @(\cell -> Either missing cell) Proxy (Right 42) (Left "flag")

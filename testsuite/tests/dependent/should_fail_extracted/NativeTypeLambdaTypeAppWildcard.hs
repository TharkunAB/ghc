{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, NamedWildCards, TypeApplications #-}
module NativeTypeLambdaTypeAppWildcard where

import Data.Proxy (Proxy(..))
import TypeLambdaArgument (rowFor)

badAnonWildcard = rowFor @(\cell -> Either _ cell) Proxy (Right 42) (Left "flag")
badNamedWildcard = rowFor @(\cell -> Either _problem cell) Proxy (Right 42) (Left "flag")

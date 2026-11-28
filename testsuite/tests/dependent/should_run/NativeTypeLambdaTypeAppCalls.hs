{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module NativeTypeLambdaTypeAppCalls
  ( shadowed
  , spliced
  , ordinaryWildcard
  , ordinarySplice
  , splicedMulti
  , captured
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Language.Haskell.TH.Syntax (addModFinalizer)
import TypeLambda (Fields)
import TypeLambdaArgument (rowFor)

shadowed :: forall (cell :: Type) (e :: Type). Proxy cell -> e -> Fields '[Either e Int, Either e Bool]
shadowed _ msg = rowFor @(\cell -> Either e cell) Proxy (Right 42) (Left msg)

spliced :: Fields '[Either String Int, Either String Bool]
spliced = $([| rowFor @(\cell -> Either String cell) Proxy (Right 42) (Left "quoted flag") |])

ordinaryWildcard :: Int
ordinaryWildcard = id @_ 17

ordinarySplice :: Int
ordinarySplice = id @($([t| Int |])) 18

splicedMulti :: forall (e :: Type). e -> Fields '[Either e Int, Either e Bool]
splicedMulti msg =
  rowFor @($(do
    addModFinalizer (pure ())
    [t| \err cell -> Either err cell |]) e)
    Proxy (Right 42) (Left msg)

captured :: forall (err :: Type) (cell :: Type). Proxy err -> Proxy cell -> Fields '[Either err Int, Either err Bool]
captured _ _ = $([| rowFor @(\cell -> Either err cell) Proxy (Right 42) (Right True) |])

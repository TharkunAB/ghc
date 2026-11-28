{-# LANGUAGE DataKinds, NoDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import NativeOpaqueCallbackAliasBridge (bridge)
import NativeOpaqueCallbackAliasProof (accepts, runCallback, token)
import qualified NativeOpaqueCallbackAliasProvider as Provider
import qualified NativeNamedOpaqueCallbackAlias as NamedAlias
import qualified NativeNamedOpaqueCallbackProvider as NamedOther
import NativeNamedOpaqueCallbackPublic (called, owns, ownsCall, stored)

main :: IO ()
main = do
    let mapper = Provider.mapper
        otherMapper = Provider.otherMapper
        bottomMapper = error "forced unused callback" :: Bool -> Char
        bottomInput = error "forced unused callback input" :: Bool
    print (bridge Provider.mapper, accepts mapper True (token mapper True))
    print (accepts bottomMapper bottomInput (token bottomMapper bottomInput))
    print (fst (runCallback mapper True), fst (runCallback otherMapper True))
    result <- try @ErrorCall (evaluate (fst (runCallback mapper False)))
    case result of
        Left err | "mapper demanded its input" `isPrefixOf` displayException err -> pure ()
        _ -> fail "expected the actual callback to run"
    print (owns NamedAlias.mapper (stored NamedOther.otherMapper),
           ownsCall NamedAlias.mapper True (called True))

{-# LANGUAGE DataKinds, NoDependentTypes #-}
module NativeOpaqueCallbackAliasReject where

import NativeOpaqueCallbackAliasProof (Witness, accepts, token)
import qualified NativeOpaqueCallbackAliasProvider as Provider
import qualified NativeOpaqueCallbackAliasProvider as SameProvider

localMapper :: Bool -> Char
localMapper _ = error "forced local callback"

directLocal :: Bool
directLocal = accepts localMapper True (token localMapper True)

differentFunction :: Bool
differentFunction = let f = Provider.mapper; g = Provider.otherMapper
                    in accepts f True (token g True)

differentAlias :: Bool
differentAlias = let f = Provider.mapper; g = SameProvider.mapper
                 in accepts f True (token g True)

literalResult :: Witness 'l' -> Bool
literalResult _ = True

guessedReduction :: Bool
guessedReduction = let f = Provider.mapper in literalResult (token f True)

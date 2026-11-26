{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ImpredicativeTypes, NoDependentTypes #-}
module NativeConstrainedSumMissingDomain where

import Data.Proxy (Proxy (..))
import GHC.Sigma (Sigma (Pair), first)
import NativeConstrainedSumProvider (Allowed, Forward (Forwarded), make)

badMissing :: Bool
badMissing = case make (Proxy :: Proxy Int) of Pair tag _ -> tag

readForwarded :: Allowed a => Proxy a -> Forwarded [a] -> Bool
readForwarded _ = first

badConstructorDomain :: Bool
badConstructorDomain = readForwarded (Proxy :: Proxy Int) (Pair True (error "missing constructor domain"))

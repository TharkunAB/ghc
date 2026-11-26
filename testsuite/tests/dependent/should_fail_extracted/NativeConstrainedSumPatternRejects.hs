{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ImpredicativeTypes, NoDependentTypes #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module NativeConstrainedSumPatternRejects where

import Data.Coerce (Coercible)
import Data.Proxy (Proxy)
import GHC.Sigma (Sigma (Pair))
import NativeAssociatedSumProvider (DefaultSum (Defaulted), Tagged (..))
import NativeConstrainedSumProvider (Choice (Shape))

instance DefaultSum Char

badStuck :: (a ~ Bool) => Shape a Float -> Bool
badStuck pkg = case pkg of Pair tag _ -> tag

badNewtype :: (a ~ Bool) => Shape a Int -> Bool
badNewtype pkg = case pkg of Pair tag _ -> tag

badRepresentation :: (Choice a, Coercible a Bool) => Proxy a -> Shape a Char -> Bool
badRepresentation _ pkg = case pkg of Pair tag _ -> tag

readPayload :: Defaulted Char -> (Bool, Char)
readPayload (Pair tag payload) = (tag, case payload of Yes c -> c; No c -> c)

badConstructorPayload :: (Bool, Char)
badConstructorPayload = readPayload (Pair False (Yes 'x'))

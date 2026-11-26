{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE ImpredicativeTypes, NoDependentTypes, TypeFamilies, UndecidableInstances #-}
module NativeAssociatedPayloadClientRejects where

import AssociatedPayloadSum (Allowed, PayloadFormat (Packet))
import ComposedPayloadSum (Second (..), distinctValue, payloadSchema)
import Data.Proxy (Proxy (Proxy))
import GHC.Sigma (Sigma (Pair))

readGlobal :: Packet Bool -> (Bool, String)
readGlobal (Pair False n) = (False, show (n :: Int))
readGlobal (Pair True c) = (True, [c])

permittedTag :: Allowed a => Proxy a -> Packet [a] -> Bool
permittedTag _ (Pair tag _) = tag

wrongPayload :: (Bool, String)
wrongPayload = readGlobal (Pair False 'w')

missingPermit :: Bool
missingPermit = permittedTag (Proxy :: Proxy Int) (Pair True (error "missing permit"))

class MissingSecond a
instance {-# OVERLAPPING #-} MissingSecond Int => Second [Int] where
  type SecondBody [Int] False = String
  type SecondBody [Int] True = Bool

inferred = payloadSchema distinctValue

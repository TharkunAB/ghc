{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, KindSignatures,
             ScopedTypeVariables #-}
module NativeOpenIdentityStrictFail where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified NativeOpenIdentityOmitted as Omitted
import qualified OpenProducer

strictArg :: forall a b. a -> b -> OpenProducer.strictSecond a b
strictArg _ y = y

strictPairField :: forall a b. a -> b -> OpenProducer.strictPairFirst '(a, b)
strictPairField x _ = x

stuckPair :: forall (p :: (Type, Type)). Proxy p -> Proxy (OpenProducer.strictPairFirst p)
stuckPair _ = undefined

missingBody :: forall a. Num a => Omitted.identity a
missingBody = 42

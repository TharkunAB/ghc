{-# LANGUAGE DataKinds, DependentTypes, TypeOperators #-}
module NativeBinaryEnumStrictModFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeBinaryEnumImplicitStrict as IS

badStrictMod :: Proxy b -> IS.choose 'False b :~: b
badStrictMod _ = Refl

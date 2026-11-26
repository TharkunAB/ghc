{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
module NativeTypeLambdaEtaMatchable where

import Data.Type.Equality ((:~:)(Refl))

badMatchable :: Maybe :~: (\cell -> Maybe cell)
badMatchable = Refl

{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoDependentTypes, TypeOperators #-}
module NativeAutoNumOptInOff where
import Data.Type.Equality ((:~:)(Refl))
import NativeAutoNumLiteralsProvider (Peano(..))
bad :: (0 :: Peano) :~: 'PZero
bad = Refl

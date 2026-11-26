{-# LANGUAGE DataKinds, DependentTypes #-}
module NativeAutoNumExplicitFam (explicitFour) where
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeNats.Experimental (FromNatural)
import NativeAutoNumLiteralsProvider (Peano(..))

explicitFour :: FromNatural Peano 4 :~: 'PSucc ('PSucc ('PSucc ('PSucc 'PZero)))
explicitFour = Refl

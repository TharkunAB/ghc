{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeClosedReturnedPairFalseProof where
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedReturnedPairLib as P
bad :: P.pair 1 :~: '(3, 1)
bad = Refl

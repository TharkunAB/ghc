{-# LANGUAGE DataKinds, NoDependentTypes, TypeApplications, TypeOperators #-}
module NativePercentArrowPprMismatch where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, M, U, Multiplicity(One))

bad :: FUN @M 'One Int Bool :~: FUN @U 'One Int Bool
bad = Refl

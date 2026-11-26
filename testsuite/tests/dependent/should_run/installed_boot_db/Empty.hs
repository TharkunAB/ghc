{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module Empty where
import Bridge (NegateAll)
import Data.Type.Equality ((:~:)(Refl))
empty :: NegateAll '[] :~: '[]
empty = Refl

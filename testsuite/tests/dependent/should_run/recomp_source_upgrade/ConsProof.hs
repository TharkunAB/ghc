{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module ConsProof where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapSource (PreludeNegateAll)

proof :: Proxy xs -> PreludeNegateAll ('True ': xs) :~: ('False ': PreludeNegateAll xs)
proof _ = Refl

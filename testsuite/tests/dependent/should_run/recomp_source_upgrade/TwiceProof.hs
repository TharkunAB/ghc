{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module TwiceProof where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import TwiceSource (Twice)

proof :: Proxy xs -> Twice ('True ': xs) :~: ('True ': Twice xs)
proof _ = Refl

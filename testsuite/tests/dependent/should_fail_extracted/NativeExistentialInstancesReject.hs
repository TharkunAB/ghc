{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, NoDependentTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeExistentialInstancesReject where
import Data.Proxy (Proxy(..))
import Data.Type.Equality
import ErasedStream (Stream, fromList)

type family IsBoolean p where
    IsBoolean (Stream Bool) = 'True
    IsBoolean p = 'False

class FirstElement a stream where firstElement :: Proxy a -> stream -> Int
instance {-# OVERLAPPABLE #-} FirstElement a (Stream a) where firstElement _ _ = 1
instance {-# OVERLAPPING #-} FirstElement Bool (Stream Bool) where firstElement _ _ = 2

class StreamOnly stream where streamOnly :: stream -> Int
instance StreamOnly (Stream a) where streamOnly _ = 0

undetermined :: Int
undetermined = streamOnly @(Stream Bool) (fromList [True])

conflict :: Int
conflict = firstElement @Bool @(Stream Char) Proxy (fromList "x")

type family Element a
stuck :: IsBoolean (Stream (Element Int)) :~: 'False
stuck = Refl

open :: proxy a -> IsBoolean (Stream a) :~: 'False
open _ = Refl

inverse :: Stream a :~: Stream b -> a :~: b
inverse Refl = Refl

{-# LANGUAGE Haskell2010, NoPolyKinds, TypeFamilies, TypeOperators #-}
module NoPolyKindsOpenFamMatchability where

import Data.Type.Equality ((:~:)(Refl))
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import NoPolyKindsOpenFamMatchabilityA (Rewrapping(..), TransExternal)

newtype Point f a = P (f a)
newtype Vector t = V [t]

instance Rewrapping (Point f a) where
  type Unwrapped (Point f a) = f a

type instance TransExternal (Vector t) b = CInt -> Ptr t -> b

type family TransLocal c b
type instance TransLocal (Vector t) b = CInt -> Ptr t -> b
type instance TransLocal [t] b = b

checkUnwrapped :: Unwrapped (Point Maybe Int) :~: Maybe Int
checkUnwrapped = Refl

checkExternal :: TransExternal (Vector Double) (IO ()) :~: (CInt -> Ptr Double -> IO ()) -> ()
checkExternal _ = ()

checkLocal :: TransLocal (Vector Double) Bool :~: (CInt -> Ptr Double -> Bool)
checkLocal = Refl
